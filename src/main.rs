extern crate alloc;

use alloc::collections::BTreeMap;

use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};

use cranelift::codegen::ir::FuncRef;
use cranelift::codegen::verifier::verify_function;

use crate::codegen::ir::StackSlot;

use parser::ast::ASTNode;

mod parser;


#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeData
{
    Primitive,
    Struct(Vec<(String, Type, usize)>), // property name, property type, location within struct
    Pointer(Box<Type>),
    FuncPointer(Box<FunctionSig>),
    Array(Box<Type>, usize),
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct Type
{
    name : String,
    data : TypeData,
}

impl ToString for Type
{
    fn to_string(&self) -> String
    {
        match &self.data
        {
            TypeData::Primitive => self.name.clone(),
            TypeData::Pointer(inner) => format!("ptr({})", inner.to_string()),
            TypeData::Array(inner, size) => format!("array({}, {})", inner.to_string(), size),
            TypeData::Struct(_) => self.name.clone(),
            TypeData::FuncPointer(sig) => sig.to_string(),
        }
    }
}

impl Type
{
    pub (crate) fn visit(&self, f : &mut dyn FnMut(&Type) -> bool)
    {
        if !f(self)
        {
            match &self.data
            {
                TypeData::Primitive => {}
                TypeData::Pointer(inner) =>
                {
                    inner.visit(f);
                }
                TypeData::Array(inner, _) =>
                {
                    inner.visit(f);
                }
                TypeData::Struct(subs) =>
                {
                    for (_, sub, _) in subs
                    {
                        sub.visit(f);
                    }
                }
                TypeData::FuncPointer(sig) =>
                {
                    sig.return_type.visit(f);
                    for arg in &sig.args
                    {
                        arg.visit(f);
                    }
                }
            }
        }
    }
    fn to_ptr(&self) -> Type
    {
        Type { name : "ptr".to_string(), data : TypeData::Pointer(Box::new(self.clone())) }
    }
    fn deref_ptr(&self) -> Type
    {
        match &self.data
        {
            TypeData::Pointer(inner) =>
            {
                return *inner.clone();
            }
            _ => {}
        }
        panic!("error: attempted to dereference non-pointer type `{}`", self.to_string());
    }
    fn array_to_inner(&self) -> Type
    {
        match &self.data
        {
            TypeData::Array(inner, _size) =>
            {
                return *inner.clone();
            }
            _ => {}
        }
        panic!("error: attempted to get dereference array type `{}`", self.to_string());
    }
    fn is_struct(&self) -> bool
    {
        matches!(self.data, TypeData::Struct(_))
    }
    fn is_array(&self) -> bool
    {
        matches!(self.data, TypeData::Array(_, _))
    }
    fn from_functionsig(funcsig : &FunctionSig) -> Type
    {
        Type { name : "funcptr".to_string(), data : TypeData::FuncPointer(Box::new(funcsig.clone())) }
    }
    fn size(&self) -> u32
    {
        match self.name.as_str()
        {
            "u8"  => 1,
            "u16" => 2,
            "u32" => 4,
            "u64" => 8,
            
            "i8"  => 1,
            "i16" => 2,
            "i32" => 4,
            "i64" => 8,
            
            "f32" => 4,
            "f64" => 8,
            
            // FIXME: target-specific pointer size
            "ptr" => 8,
            "funcptr" => 8,
            
            _ => match &self.data
            {
                TypeData::Array(inner, count) =>
                {
                    inner.size() * *count as u32
                }
                TypeData::Struct(subs) =>
                {
                    let mut r = 0;
                    for (_, sub, _) in subs
                    {
                        r += sub.size();
                    }
                    r
                }
                _ => panic!("internal error: failed to cover type when getting size"),
            }
        }
    }
    fn to_cranetype(&self) -> Option<cranelift::prelude::Type>
    {
        if matches!(self.data, TypeData::Primitive | TypeData::Pointer(_) | TypeData::FuncPointer(_))
        {
            match self.name.as_str()
            {
                "u8"  => Some(types::I8),
                "u16" => Some(types::I16),
                "u32" => Some(types::I32),
                "u64" => Some(types::I64),
                
                "i8"  => Some(types::I8),
                "i16" => Some(types::I16),
                "i32" => Some(types::I32),
                "i64" => Some(types::I64),
                
                "f32" => Some(types::F32),
                "f64" => Some(types::F64),
                
                // FIXME: target-specific pointer size
                "ptr" => Some(types::I64),
                "funcptr" => Some(types::I64),
                
                // unknown/broken types
                _ => None,
            }
        }
        else
        {
            Some(types::I64) // arrays and structs are pointers
        }
    }
    fn to_abi(&self) -> Option<AbiParam>
    {
        self.to_cranetype().map(|x| AbiParam::new(x))
    }
}
    
fn parse_type(types : &BTreeMap<String, Type>, node : &ASTNode) -> Result<Type, String>
{
    match (node.is_parent(), node.text.as_str())
    {
        (true, "type") => parse_type(types, node.child(0).unwrap()),
        (true, "fundamental_type") => Ok(types.get(&node.child(0).unwrap().text).unwrap().clone()),
        (true, "funcptr_type") =>
        {
            let return_node = node.child(0).unwrap();
            if let Ok(return_type) = parse_type(types, return_node)
            {
                let mut args = Vec::new();
                for arg in node.child(1).unwrap().get_children().unwrap()
                {
                    if let Ok(r) = parse_type(types, arg)
                    {
                        args.push(r);
                    }
                    else
                    {
                        return Err(format!("error: failed to parse function pointer type signature (culprit: `{}`)", arg.text));
                    }
                }
                let sig = FunctionSig { return_type, args };
                Ok(Type::from_functionsig(&sig))
            }
            else
            {
                Err(format!("error: failed to parse function pointer type signature (culprit: `{}`)", return_node.text))
            }
        }
        (true, "struct_type") =>
        {
            let name = &node.child(0).unwrap().text;
            if let Some(named_type) = types.get(name)
            {
                if let TypeData::Struct(struct_data) = &named_type.data
                {
                    return Ok(named_type.clone());
                }
            }
            Err(format!("error: unknown type `{}`", name))
        }
        (true, "array_type") =>
        {
            let res = parse_type(types, node.child(0).unwrap());
            if let Ok(type_) = res
            {
                let count_text = &node.child(1).unwrap().child(0).unwrap().text;
                let count : u64 = count_text.parse().unwrap();
                Ok(Type { name : "array".to_string(), data : TypeData::Array(Box::new(type_.clone()), count as usize) })
            }
            else
            {
                res
            }
        }
        // FIXME
        (_, name) => Err(format!("error: non-fundemental types not yet supported (culprit: `{}`)", name)),
    }
}

#[derive(Debug, Clone)]
struct Function
{
    name : String,
    return_type : Type,
    args : Vec<(Type, String)>,
    body : ASTNode,
}
#[derive(Debug, Clone, PartialEq, Eq)]
struct FunctionSig
{
    return_type : Type,
    args : Vec<Type>,
}
impl ToString for FunctionSig
{
    fn to_string(&self) -> String
    {
        let return_type = self.return_type.to_string();
        let mut args = String::new();
        let arg_count = self.args.len();
        for (i, arg) in self.args.iter().enumerate()
        {
            args += &arg.to_string();
            if i+1 < arg_count
            {
                args += ", ";
            }
        }
        format!("fnptr({}, ({}))", return_type, args)
    }
}
impl FunctionSig
{
    fn to_signature<T : Module>(&self, module : &T) -> Signature
    {
        let mut signature = module.make_signature();
        
        for var_type in &self.args
        {
            let var_abi = var_type.to_abi();
            if var_abi.is_none()
            {
                let name = &var_type.name;
                panic!("error: non-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", name, name);
            }
            signature.params.push(var_abi.unwrap().clone());
        }
        
        let return_abi = self.return_type.to_abi();
        if return_abi.is_none()
        {
            let name = &self.return_type.name;
            panic!("error: funcsig-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", name, name);
        }
        signature.returns.push(return_abi.unwrap().clone());
        println!("made func sig {:?}", signature);
        signature
    }
}
impl Function
{
    fn to_sig(&self) -> FunctionSig
    {
        FunctionSig { return_type : self.return_type.clone(), args : self.args.iter().map(|x| x.0.clone()).collect() }
    }
}
#[derive(Debug, Clone)]
struct Program
{
    funcs : BTreeMap<String, Function>,
}

impl Program
{
    fn new(types: &mut BTreeMap<String, Type>, ast : &ASTNode) -> Result<Program, String>
    {
        let mut funcs = BTreeMap::new();
        
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "structdef"
            {
                let name = child.child(0)?.child(0)?.text.clone();
                let mut struct_data = Vec::new();
                let mut offset : usize = 0;
                for prop in child.child(1)?.get_children()?
                {
                    let prop_type = parse_type(&types, prop.child(0)?).unwrap();
                    let prop_name = prop.child(1)?.child(0)?.text.clone();
                    // FIXME: don't add fields with the name "_" (for padding)
                    // FIXME: throw an error on misaligned fields
                    struct_data.push((prop_name, prop_type.clone(), offset));
                    offset += prop_type.size() as usize;
                }
                
                //Struct(Vec<(String, Type, usize)>), // property name, property type, location within struct
                let struct_type = Type { name : name.clone(), data : TypeData::Struct(struct_data) };
                
                types.insert(name.clone(), struct_type);
            }
        }
        
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "funcdef"
            {
                let return_type = parse_type(&types, child.child(0)?).unwrap();
                let name = child.child(1)?.child(0)?.text.clone();
                
                let mut args = Vec::new();
                for arg in child.child(2)?.get_children()?
                {
                    let arg_type = parse_type(&types, arg.child(0)?).unwrap();
                    let arg_name = arg.child(1)?.child(0)?.text.clone();
                    
                    args.push((arg_type, arg_name));
                }
                
                let body = child.child(3)?.clone();
                
                funcs.insert(name.clone(), Function { name, return_type, args, body });
            }
        }
        Ok(Program { funcs })
    }
}

fn main()
{
    // only holds primitives and structs, not pointers or arrays, those are constructed dynamically
    let mut types = BTreeMap::new();
    
    types.insert("u8" .to_string(), Type { name : "u8" .to_string(), data : TypeData::Primitive });
    types.insert("u16".to_string(), Type { name : "u16".to_string(), data : TypeData::Primitive });
    types.insert("u32".to_string(), Type { name : "u32".to_string(), data : TypeData::Primitive });
    types.insert("u64".to_string(), Type { name : "u64".to_string(), data : TypeData::Primitive });
    
    types.insert("i8" .to_string(), Type { name : "i8" .to_string(), data : TypeData::Primitive });
    types.insert("i16".to_string(), Type { name : "i16".to_string(), data : TypeData::Primitive });
    types.insert("i32".to_string(), Type { name : "i32".to_string(), data : TypeData::Primitive });
    types.insert("i64".to_string(), Type { name : "i64".to_string(), data : TypeData::Primitive });
    
    types.insert("f32".to_string(), Type { name : "f32".to_string(), data : TypeData::Primitive });
    types.insert("f64".to_string(), Type { name : "f64".to_string(), data : TypeData::Primitive });
    
    
    let ir_grammar = include_str!("parser/irgrammar.txt");
    let program_text = include_str!("parser/irexample.txt");
    let mut parser = parser::Parser::new_from_grammar(&ir_grammar).unwrap();
    let program_lines : Vec<String> = program_text.lines().map(|x| x.to_string()).collect();
    let tokens = parser.tokenize(&program_lines, false).unwrap();
    let ast = parser.parse_program(&tokens, &program_lines, false).unwrap().unwrap();
    
    let program = Program::new(&mut types, &ast).unwrap();
    
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
        
        let funcsig = function.to_sig();
        
        let signature = funcsig.to_signature(&module);
        
        let mut variables = BTreeMap::new();
        let mut parameters = BTreeMap::new();
        
        for arg in &function.args
        {
            let var_type = arg.0.clone();
            let var_name = arg.1.clone();
            
            if parameters.contains_key(&var_name)
            {
                panic!("error: variable {} redeclared", var_name);
            }
            parameters.insert(var_name, var_type);
        }
        
        let id = module.declare_function(&f_name, Linkage::Export, &signature).unwrap();
        
        func_decs.insert(f_name.clone(), (id, funcsig, variables, parameters, function.return_type.clone()));
    }
    
    for f in &program.funcs
    {
        
        let f_name = f.0;
        let function = f.1;
        
        let (id, funcsig, mut variables, parameters, _) = func_decs.get(f_name).unwrap().clone();
        let signature = funcsig.to_signature(&module);
        
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        builder.func.signature = signature.clone();
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        
        // declare arguments
        let mut i = variables.len();
        for (j, param) in parameters.iter().enumerate()
        {
            let var_type = param.1.clone();
            let var_name = param.0.clone();
            let slot = builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size : var_type.size() });
            
            let tmp = builder.block_params(block)[j];
            builder.ins().stack_store(tmp, slot, 0);
            println!("inserting {} into vars with type {}", var_name, var_type.name);
            if variables.contains_key(&var_name)
            {
                panic!("error: variable {} redeclared", var_name);
            }
            variables.insert(var_name, (var_type, slot));
            
            i += 1;
        }
        
        // declare variables
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && node.text == "declaration"
            {
                let var_type = parse_type(&types, &node.child(0).unwrap()).unwrap();
                let var_name = node.child(1).unwrap().child(0).unwrap().text.clone();
                let slot = builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size : var_type.size() });
                
                if variables.contains_key(&var_name)
                {
                    panic!("error: variable {} redeclared", var_name);
                }
                variables.insert(var_name, (var_type, slot));
                i += 1;
            }
            false
        });
        
        // collect referred function signatures
        let mut funcrefs = BTreeMap::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            let mut i = 0;
            if node.is_parent() && node.text == "rvarname"
            {
                let name = &node.child(0).unwrap().text;
                if !variables.contains_key(name) && func_decs.contains_key(name)
                {
                    let (id, funcsig, _, _, _) = func_decs.get(name).unwrap();
                    let funcref = module.declare_func_in_func(*id, builder.func);
                    // FIXME: need a better way to get a SigRef
                    funcrefs.insert(name.clone(), (funcsig.clone(), funcref));
                }
            }
            false
        });
        
        struct Environment<'a, 'b, 'c, 'e>
        {
            stack      : &'a mut Vec<(Type, Value)>,
            variables  : &'a BTreeMap<String, (Type, StackSlot)>,
            builder    : &'b mut FunctionBuilder<'c>,
            module     : &'e mut JITModule,
            funcrefs   : &'a BTreeMap<String, (FunctionSig, FuncRef)>,
            types      : &'a BTreeMap<String, Type>,
        }
        
        let mut stack = Vec::new();
        
        let mut env = Environment { stack : &mut stack, variables : &variables, builder : &mut builder, module : &mut module, funcrefs : &funcrefs, types : &types };
        fn compile<'a, 'b>(env : &'a mut Environment, node : &'b ASTNode, want_pointer : bool)
        {
            if node.is_parent()
            {
                match node.text.as_str()
                {
                    "statementlist" | "statement" | "instruction" | "parenexpr" | "arrayindex" =>
                    {
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child, want_pointer);
                        }
                    }
                    "return" =>
                    {
                        let mut returns = Vec::new();
                        // FIXME: check types
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child, false);
                            returns.push(env.stack.pop().unwrap().1);
                        }
                        env.builder.ins().return_(&returns);
                    }
                    "declaration" =>
                    {
                        return;
                    }
                    "lvar" =>
                    {
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child, true);
                        }
                    }
                    "lvar_name" =>
                    {
                        let name = &node.child(0).unwrap().child(0).unwrap().text;
                        if env.variables.contains_key(name)
                        {
                            let (type_, slot) = &env.variables[name];
                            let addr = env.builder.ins().stack_addr(types::I64, *slot, 0);
                            env.stack.push((type_.to_ptr(), addr));
                        }
                        else
                        {
                            panic!("error: unrecognized variable `{}`", name);
                        }
                    }
                    "arrayindex_head" =>
                    {
                        compile(env, node.child(0).unwrap(), true);
                        compile(env, node.child(1).unwrap(), false);
                        let (offset_type, offset_val) = env.stack.pop().unwrap();
                        let (base_type, base_addr) = env.stack.pop().unwrap();
                        
                        if offset_type.name == "i64"
                        {
                            // TODO: do multi-level accesses in a single load operation instead of several
                            // FIXME: double check that nested types work properly
                            let inner_type = base_type.array_to_inner();
                            let inner_size = inner_type.size();
                            let inner_offset = env.builder.ins().imul_imm(offset_val, inner_size as i64);
                            let inner_addr = env.builder.ins().iadd(base_addr, inner_offset);
                            if want_pointer
                            {
                                if inner_type.is_struct()
                                {
                                    env.stack.push((inner_type.clone(), inner_addr));
                                }
                                else
                                {
                                    env.stack.push((inner_type.to_ptr(), inner_addr));
                                }
                            }
                            else
                            {
                                let val = env.builder.ins().load(inner_type.to_cranetype().unwrap(), MemFlags::trusted(), inner_addr, 0);
                                env.stack.push((inner_type.clone(), val));
                            }
                        }
                        else
                        {
                            panic!("error: can't offset into arrays except with type i64 (used type `{}`)", offset_type.name)
                        }
                    }
                    "indirection_head" =>
                    {
                        compile(env, node.child(0).unwrap(), true);
                        let (struct_type, struct_addr) = env.stack.pop().unwrap();
                        
                        let right_name = &node.child(1).unwrap().child(0).unwrap().text;
                        
                        if let Some(found) = match &struct_type.data
                        {
                            TypeData::Struct(ref props) =>
                            {
                                props.iter().find(|x| x.0 == *right_name)
                            }
                            _ => panic!("error: tried to use indirection (.) operator on non-struct"),
                        }
                        {
                            // TODO: do multi-level struct accesses (e.g. mat.x_vec.x) in a single load operation instead of several
                            // FIXME: double check that nested structs work properly
                            let inner_type = &found.1;
                            if want_pointer || inner_type.is_struct()
                            {
                                let offset = env.builder.ins().iconst(types::I64, found.2 as i64);
                                let inner_addr = env.builder.ins().iadd(struct_addr, offset);
                                if inner_type.is_struct()
                                {
                                    env.stack.push((inner_type.clone(), inner_addr));
                                }
                                else
                                {
                                    env.stack.push((inner_type.to_ptr(), inner_addr));
                                }
                            }
                            else
                            {
                                let val = env.builder.ins().load(inner_type.to_cranetype().unwrap(), MemFlags::trusted(), struct_addr, found.2 as i32);
                                env.stack.push((inner_type.clone(), val));
                            }
                        }
                        else
                        {
                            panic!("error: no such property {} in struct type {}", right_name, struct_type.name);
                        }
                    }
                    "binstate" =>
                    {
                        compile(env, node.child(0).unwrap(), true);
                        compile(env, node.child(2).unwrap(), false);
                        
                        // always =
                        //let op = &node.child(1).unwrap().child(0).unwrap().text;
                        
                        let (type_val, val) = env.stack.pop().unwrap();
                        let (type_left_ptr, left_addr) = env.stack.pop().unwrap();
                        println!("binstate in {:?}", node);
                        let type_left = type_left_ptr.deref_ptr();
                        assert!(type_val == type_left);
                        env.builder.ins().store(MemFlags::trusted(), val, left_addr, 0);
                    }
                    "rvarname" =>
                    {
                        let name = &node.child(0).unwrap().text;
                        println!("{}", name);
                        if env.variables.contains_key(name)
                        {
                            let (type_, slot) = &env.variables[name];
                            println!("{:?}", (type_, slot));
                            let addr = env.builder.ins().stack_addr(types::I64, *slot, 0);
                            // FIXME: make this universal somehow (currently semi duplicated with indirection_head)
                            if type_.is_struct() || type_.is_array()
                            {
                                env.stack.push((type_.clone(), addr));
                            }
                            else if want_pointer
                            {
                                env.stack.push((type_.to_ptr(), addr));
                            }
                            else
                            {
                                let val = env.builder.ins().load(type_.to_cranetype().unwrap(), MemFlags::trusted(), addr, 0);
                                env.stack.push((type_.clone(), val));
                            }
                        }
                        else if env.funcrefs.contains_key(name)
                        {
                            let (funcsig, funcref) = env.funcrefs.get(name).unwrap();
                            let funcaddr = env.builder.ins().func_addr(types::I64, *funcref);
                            env.stack.push((Type::from_functionsig(funcsig), funcaddr));
                        }
                        else
                        {
                            panic!("unrecognized identifier {}", name);
                        }
                    }
                    "funcargs_head" =>
                    {
                        println!("compiling func call");
                        compile(env, node.child(0).unwrap(), false);
                        let (type_, funcaddr) = env.stack.pop().unwrap();
                        match type_.data
                        {
                            TypeData::FuncPointer(funcsig) =>
                            {
                                let sig = funcsig.to_signature(&env.module);
                                let sigref = env.builder.import_signature(sig);
                                
                                compile(env, node.child(1).unwrap(), false);
                                let num_args = node.child(1).unwrap().child_count().unwrap();
                                
                                let mut args = Vec::new();
                                for arg_type in &funcsig.args
                                {
                                    let (type_, val) = env.stack.pop().unwrap();
                                    if type_ != *arg_type
                                    {
                                        panic!("mismatched types in call to function: expected `{}`, got `{}`", arg_type.to_string(), type_.to_string());
                                    }
                                    args.push(val);
                                }
                                
                                println!("calling func with sigref {} and sig {}", sigref, funcsig.to_string());
                                let inst = env.builder.ins().call_indirect(sigref, funcaddr, &args);
                                let results = env.builder.inst_results(inst);
                                println!("number of results {}", results.len());
                                for (result, type_) in results.iter().zip([funcsig.return_type])
                                {
                                    env.stack.push((type_.clone(), *result));
                                }
                            }
                            _ => panic!("tried to fall non-function expression as a function")
                        }
                        println!("done compiling func call");
                    }
                    "funcargs" =>
                    {
                        for child in node.get_children().unwrap()
                        {
                            compile(env, child, false);
                        }
                    }
                    "float" =>
                    {
                        let text = &node.child(0).unwrap().text;
                        let location = text.rfind("f").unwrap();
                        let parts = text.split_at(location);
                        let text = parts.0;
                        match parts.1
                        {
                            "f32" =>
                            {
                                let val : f32 = text.parse().unwrap();
                                let res = env.builder.ins().f32const(val);
                                env.stack.push((env.types.get("f32").unwrap().clone(), res));
                            }
                            "f64" =>
                            {
                                let val : f64 = text.parse().unwrap();
                                let res = env.builder.ins().f64const(val);
                                env.stack.push((env.types.get("f64").unwrap().clone(), res));
                            }
                            _ => panic!("unknown float suffix pattern {}", parts.1)
                        }
                    }
                    "integer" =>
                    {
                        let text = &node.child(0).unwrap().text;
                        let mut location = text.rfind("i");
                        if location.is_none()
                        {
                            location = text.rfind("u");
                        }
                        if location.is_none()
                        {
                            panic!("internal error: unknown integer type literal suffix");
                        }
                        let location = location.unwrap();
                        let parts = text.split_at(location);
                        let text = parts.0;
                        match parts.1
                        {
                            // FIXME: check if we need to use sign extension (probably don't)
                            "u8" =>
                            {
                                let val : u8 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I8, val as i64);
                                env.stack.push((env.types.get("u8").unwrap().clone(), res));
                            }
                            "u16" =>
                            {
                                let val : u16 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I16, val as i64);
                                env.stack.push((env.types.get("u16").unwrap().clone(), res));
                            }
                            "u32" =>
                            {
                                let val : u32 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I32, val as i64);
                                env.stack.push((env.types.get("u32").unwrap().clone(), res));
                            }
                            "u64" =>
                            {
                                let val : u64 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I64, val as i64);
                                env.stack.push((env.types.get("u64").unwrap().clone(), res));
                            }
                            "i8" =>
                            {
                                let val : i8 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I8, val as i64);
                                env.stack.push((env.types.get("i8").unwrap().clone(), res));
                            }
                            "i16" =>
                            {
                                let val : i16 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I16, val as i64);
                                env.stack.push((env.types.get("i16").unwrap().clone(), res));
                            }
                            "i32" =>
                            {
                                let val : i32 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I32, val as i64);
                                env.stack.push((env.types.get("i32").unwrap().clone(), res));
                            }
                            "i64" =>
                            {
                                let val : i64 = text.parse().unwrap();
                                let res = env.builder.ins().iconst(types::I64, val as i64);
                                env.stack.push((env.types.get("i64").unwrap().clone(), res));
                            }
                            _ => panic!("unknown float suffix pattern {}", parts.1)
                        }
                    }
                    text => 
                    {
                        println!("compiling {} ...", text);
                        if text.starts_with("binexpr")
                        {
                            compile(env, node.child(0).unwrap(), false);
                            compile(env, node.child(2).unwrap(), false);
                            let op = &node.child(1).unwrap().child(0).unwrap().text;
                            let right = env.stack.pop().unwrap();
                            let left  = env.stack.pop().unwrap();
                            // FIXME: check types
                            let res = match op.as_str()
                            {
                                "+" => env.builder.ins().fadd(left.1, right.1),
                                "-" => env.builder.ins().fsub(left.1, right.1),
                                _ => panic!("unhandled binary operator {}", op)
                            };
                            env.stack.push((env.types.get("f32").unwrap().clone(), res));
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
        compile(&mut env, &function.body, false);
        
        builder.seal_all_blocks();
        builder.finalize();
        
        println!("{}", ctx.func.display());
        
        module.define_function(id, &mut ctx).unwrap();
        
        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&ctx.func, &flags);
        
        if let Err(errors) = res
        {
            panic!("{}", errors);
        }
        println!("function {} compiled with no errors!", function.name);
        
        module.clear_context(&mut ctx);
    }
    module.finalize_definitions().unwrap();
    
    /*
    macro_rules! do_call {
        ($(($b:ty) ($a:expr)),* -> $r:ty) =>
        (
        
        );
    }
    */
    
    for (f_name, (id, _, _, _, _)) in func_decs.iter()
    {
        let code = module.get_finalized_function(*id);
        // FIXME: SAFETY: use actual function signature
        let func = unsafe
        {
            core::mem::transmute::<_, unsafe extern "C" fn(f32, f32) -> f32>(code)
        };
        let a = 3.14;
        let b = 150.0012;
        // FIXME: SAFETY: WARNING: EVIL: THIS INVOKES UB BECAUSE WE DON'T CHECK FUNCTION SIGNATURES
        unsafe
        {
            println!("running function {}...", f_name);
            println!("{}({}, {}) = {}", f_name, a, b, func(a, b));
        }
        // dump code to console for later manual disassembly
        //unsafe
        //{
        //    let mut ptr : *mut u8 = core::mem::transmute::<_, _>(code);
        //    while true
        //    {
        //        print!("{:02X} ", *ptr);
        //        ptr = ptr.offset(1);
        //    }
        //}
    }
}
