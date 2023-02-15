extern crate alloc;

use alloc::collections::BTreeMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};

use cranelift_codegen::verifier::verify_function;

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
    
    //println!("{:#?}", program);
    
    let builder = JITBuilder::new(cranelift_module::default_libcall_names());
    let mut module = JITModule::new(builder.unwrap());
    
    let mut builder_context = FunctionBuilderContext::new();
    let mut ctx = module.make_context();
    
    
    let mut func_ids = BTreeMap::new();
    
    for f in &program.funcs
    {
        let f_name = f.0;
        let function = f.1;
        
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
                        ctx.func.signature.params.push(AbiParam::new(types::F32));
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
        match function.return_type.as_str()
        {
            "f32" => ctx.func.signature.returns.push(AbiParam::new(types::F32)),
            _ => panic!("asdf"),
        }
        
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
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
        
        let mut stack = Vec::<(String, Value)>::new(); // 0: type, 1: variable handle
        
        struct Environment<'a, 'b, 'c, 'd>
        {
            stack      : &'a mut Vec<(String, Value)>,
            variables  : &'a BTreeMap<String, (String, Variable)>,
            builder    : &'b mut FunctionBuilder<'c>,
            program    : &'d Program,
            root_block : &'a Block,
        }
        
        let mut env = Environment { stack : &mut stack, variables : &variables, builder : &mut builder, program : &program, root_block : &block };
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
                    "rvariable" =>
                    {
                        let name = &node.child(0).unwrap().text;
                        println!("{}", name);
                        let var = &env.variables[name];
                        println!("{:?}", var);
                        env.stack.push((var.0.clone(), env.builder.use_var(var.1)));
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
        
        
        let id = module.declare_function(&f_name, Linkage::Export, &ctx.func.signature).unwrap();
        func_ids.insert(f_name.clone(), id);
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
    for (f_name, id) in func_ids.iter()
    {
        let code = module.get_finalized_function(*id);
        println!("{:?}", code);
    }
}
