extern crate alloc;

use alloc::collections::BTreeMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module, FuncId};

use cranelift::codegen::ir::{FuncRef, SigRef};
use cranelift::codegen::verifier::verify_function;

use parser::ast::ASTNode;

mod parser;

#[derive(Debug, Clone)]
struct Function {
    name : String,
    return_type : String,
    args : Vec<(String, String)>, // 0: type, 1: name
    body : ASTNode,
}

#[derive(Debug, Clone)]
struct Struct {
    name : String,
    vars : Vec<(String, String)>, // 0: type, 1: name
}

#[derive(Debug, Clone)]
struct Program {
    structs : BTreeMap<String, Struct>,
    funcs : BTreeMap<String, Function>,
}

impl Program {
    fn new(ast : &ASTNode) -> Result<Program, String>
    {
        let mut structs = BTreeMap::new();
        let mut funcs = BTreeMap::new();
        
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "structdef"
            {
                let name = child.child(0)?.child(0)?.text.clone();
                let mut vars = Vec::new();
                for prop in child.child(1)?.get_children()?
                {
                    let prop_type = prop.child(0)?.child(0)?.text.clone();
                    let prop_name = prop.child(1)?.child(0)?.text.clone();
                    vars.push((prop_type, prop_name));
                }
                
                structs.insert(name.clone(), Struct { name, vars });
            }
        }
        
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "funcdef"
            {
                let return_type = child.child(0)?.child(0)?.text.clone();
                let name = child.child(1)?.child(0)?.text.clone();
                
                let mut args = Vec::new();
                for arg in child.child(2)?.get_children()?
                {
                    let arg_type = arg.child(0)?.child(0)?.text.clone();
                    let arg_name = arg.child(1)?.child(0)?.text.clone();
                    
                    args.push((arg_type, arg_name));
                }
                
                let body = child.child(3)?.clone();
                
                funcs.insert(name.clone(), Function { name, return_type, args, body });
            }
        }
        Ok(Program { structs, funcs })
    }
}

fn main()
{
    let ir_grammar = include_str!("parser/irgrammar.txt");
    let program_text = include_str!("parser/irexample.txt");
    let mut parser = parser::Parser::new_from_grammar(&ir_grammar).unwrap();
    let program_lines : Vec<String> = program_text.lines().map(|x| x.to_string()).collect();
    let tokens = parser.tokenize(&program_lines, false).unwrap();
    let ast = parser.parse_program(&tokens, &program_lines, false).unwrap().unwrap();
    
    let program = Program::new(&ast).unwrap();
    
    let mut ast_debug = format!("{:#?}", program);
    ast_debug = ast_debug.replace("    ", " ");
    ast_debug = ast_debug.split("\n").filter(|x|
           !x.contains(" position:")
        && !x.contains(" line:")
        && !x.contains(" children: None")
        && !x.contains(" ),")
        && !x.contains(" },")
        && !x.contains(" ],")
        && !x.ends_with(" [")
        && !x.ends_with(" (")
        && *x != "}"
        ).map(|x|
            x.replace("{}", "None").replace(" {", ":").replace("children: Some(", "children:")
        ).collect::<Vec<_>>().join("\n");
    println!("{}\n", ast_debug);
    
    let builder = JITBuilder::new(cranelift_module::default_libcall_names());
    let mut module = JITModule::new(builder.unwrap());
    
    let mut builder_context = FunctionBuilderContext::new();
    let mut ctx = module.make_context();
    
    let mut func_decs = BTreeMap::new();
    
    for f in &program.funcs
    {
        let f_name = f.0;
        let function = f.1;
        
        let mut signature = module.make_signature();
        
        let mut variables = BTreeMap::new();
        let mut parameters = BTreeMap::new();
        {
            for arg in &function.args
            {
                let var_type = arg.0.clone();
                let var_name = arg.1.clone();
                
                match var_type.as_str()
                {
                    "f32" =>
                    {
                        signature.params.push(AbiParam::new(types::F32));
                    }
                    _ => panic!("asdf"),
                }
                
                if variables.contains_key(&var_name)
                {
                    panic!("error: variable {} redeclared", var_name);
                }
                parameters.insert(var_name, var_type);
            }
        }
        // FIXME: multi-return
        match function.return_type.as_str()
        {
            "f32" => signature.returns.push(AbiParam::new(types::F32)),
            _ => panic!("asdf"),
        }
        
        let id = module.declare_function(&f_name, Linkage::Export, &signature).unwrap();
        
        func_decs.insert(f_name.clone(), (id, signature, variables, parameters, function.return_type.clone()));
    }
    
    for f in &program.funcs
    {
        let f_name = f.0;
        let function = f.1;
        
        let (id, signature, mut variables, parameters, _) = func_decs.get(f_name).unwrap().clone();
        
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        builder.func.signature = signature.clone();
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        
        for (i, param) in parameters.iter().enumerate()
        {
            let var_type = param.1.clone();
            let var_name = param.0.clone();
            let var = Variable::new(i);
            match var_type.as_str()
            {
                "f32" =>
                {
                    builder.declare_var(var, types::F32);
                }
                _ => panic!("unknown parameter type {}", var_type),
            }
            let tmp = builder.block_params(block)[i];
            builder.def_var(var, tmp);
            println!("inserting {} into vars with type {}", var_name, var_type);
            if variables.contains_key(&var_name)
            {
                panic!("error: variable {} redeclared", var_name);
            }
            variables.insert(var_name, (var_type, var));
        }
        
        function.body.visit(&mut |node : &ASTNode|
        {
            let mut i = 0;
            if node.is_parent() && node.text == "declaration"
            {
                let var_type = node.child(0).unwrap().child(0).unwrap().text.clone();
                let var_name = node.child(1).unwrap().child(0).unwrap().text.clone();
                let var = Variable::new(i);
                // FIXME: use actual type
                builder.declare_var(var, types::F32);
                if variables.contains_key(&var_name)
                {
                    panic!("error: variable {} redeclared", var_name);
                }
                variables.insert(var_name, (var_type, var));
                i += 1;
            }
            return false;
        });
        
        let mut funcrefs = BTreeMap::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            let mut i = 0;
            if node.is_parent() && node.text == "rvarname"
            {
                let name = &node.child(0).unwrap().text;
                if !variables.contains_key(name) && func_decs.contains_key(name)
                {
                    let (id, signature, variables, parameters, _) = func_decs.get(name).unwrap();
                    let funcref = module.declare_func_in_func(*id, builder.func);
                    let sigref = builder.import_signature(signature.clone());
                    funcrefs.insert(name.clone(), (funcref, sigref));
                }
            }
            return false;
        });
        
        
        struct Environment<'a, 'b, 'c, 'd>
        {
            stack      : &'a mut Vec<(String, Value)>,
            funcstack  : &'a mut Vec<(FuncRef, SigRef)>,
            variables  : &'a BTreeMap<String, (String, Variable)>,
            builder    : &'b mut FunctionBuilder<'c>,
            program    : &'d Program,
            root_block : &'a Block,
            func_decs  : &'a BTreeMap<String, (FuncId, Signature, BTreeMap<String, (String, Variable)>, BTreeMap<String, String>, String)>,
            funcrefs   : &'a BTreeMap<String, (FuncRef, SigRef)>,
        }
        
        let mut stack = Vec::new(); // 0: type, 1: variable handle
        let mut funcstack = Vec::new(); // 0: type, 1: variable handle
        
        let mut env = Environment { stack : &mut stack, funcstack : &mut funcstack, variables : &variables, builder : &mut builder, program : &program, root_block : &block, func_decs : &func_decs, funcrefs : &funcrefs };
        fn compile<'a, 'b>(env : &'a mut Environment, node : &'b ASTNode)
        {
            println!("looking at {} to compile!!!", node.text);
            if node.is_parent()
            {
                match node.text.as_str()
                {
                    "statementlist" | "statement" | "instruction" =>
                    {
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child);
                        }
                    }
                    "return" =>
                    {
                        let mut returns = Vec::new();
                        // FIXME: check types
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child);
                            returns.push(env.stack.pop().unwrap().1);
                        }
                        env.builder.ins().return_(&returns);
                    }
                    "declaration" =>
                    {
                        return;
                    }
                    "rvarname" =>
                    {
                        let name = &node.child(0).unwrap().text;
                        println!("{}", name);
                        if env.variables.contains_key(name)
                        {
                            let var = &env.variables[name];
                            println!("{:?}", var);
                            env.stack.push((var.0.clone(), env.builder.use_var(var.1)));
                        }
                        else if env.funcrefs.contains_key(name)
                        {
                            let (funcref, sigref) = env.funcrefs.get(name).unwrap();
                            env.funcstack.push((*funcref, *sigref));
                        }
                        else
                        {
                            panic!("unrecognized identifier {}", name);
                        }
                    }
                    "funcargs_head" =>
                    {
                        compile(env, node.child(0).unwrap());
                        let (funcref, sigref) = env.funcstack.pop().unwrap();
                        
                        compile(env, node.child(1).unwrap());
                        let num_args = node.child(1).unwrap().child_count().unwrap();
                        
                        let mut args = Vec::new();
                        // FIXME: check types
                        for i in 0..num_args
                        {
                            let (type_, val) = env.stack.pop().unwrap();
                            args.push(val);
                        }
                        
                        let funcaddr = env.builder.ins().func_addr(types::I64, funcref);
                        
                        let inst = env.builder.ins().call_indirect(sigref, funcaddr, &args);
                        let results = env.builder.inst_results(inst);
                        for result in results
                        {
                            // FIXME: use actual type
                            env.stack.push(("f32".to_string(), *result));
                        }
                    }
                    "funcargs" =>
                    {
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child);
                        }
                    }
                    "number" =>
                    {
                        // FIXME: use actual type
                        let text = &node.child(0).unwrap().text;
                        let val : f32 = text.parse().unwrap();
                        let res = env.builder.ins().f32const(val);
                        env.stack.push(("f32".to_string(), res));
                    }
                    text => 
                    {
                        println!("compiling {} ...", text);
                        if text.starts_with("binexpr")
                        {
                            compile(env, node.child(0).unwrap());
                            compile(env, node.child(2).unwrap());
                            let op = &node.child(1).unwrap().child(0).unwrap().text;
                            let right = env.stack.pop().unwrap();
                            let left  = env.stack.pop().unwrap();
                            // FIXME: check types
                            let res = match op.as_str()
                            {
                                "+" => env.builder.ins().fadd(left.1, right.1),
                                _ => panic!("unhandled binary operator {}", op)
                            };
                            env.stack.push(("f32".to_string(), res));
                        }
                        else
                        {
                            panic!("unhandled AST node {}", text);
                        }
                    }
                }
            }
            else
            {
                /*
                match node.text.as_str()
                {
                    "name" =>
                    {
                        let var = variables[node.child(0).unwrap()]
                        builder.use_var()
                    }
                }*/
            }
        }
        compile(&mut env, &function.body);
        
        builder.seal_all_blocks();
        builder.finalize();
        
        module.define_function(id, &mut ctx).unwrap();
        
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&ctx.func, &flags);
        println!("{}", ctx.func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }
        println!("function compiled with no errors!");
        
        module.clear_context(&mut ctx);
    }
    module.finalize_definitions();
    for (f_name, (id, signature, _, _, _)) in func_decs.iter()
    {
        let code = module.get_finalized_function(*id);
        // FIXME: SAFETY: use actual function signature
        let func = unsafe
        {
            core::mem::transmute::<_, fn(f32, f32) -> f32>(code)
        };
        let a = 3.14;
        let b = 150.0012;
        println!("{}({},{}) = {}", f_name, a, b, func(a, b));
    }
}
