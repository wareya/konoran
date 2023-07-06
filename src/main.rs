extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use inkwell::types::{AnyType, BasicType};

use parser::ast::ASTNode;

mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeData
{
    Void,
    Primitive,
    Struct(Vec<(String, Type, usize)>), // property name, property type, location within struct
    Pointer(Box<Type>),
    VirtualPointer(Box<Type>),
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
            TypeData::Void => self.name.clone(),
            TypeData::Primitive => self.name.clone(),
            TypeData::Pointer(inner) => format!("ptr({})", inner.to_string()),
            TypeData::VirtualPointer(inner) => format!("vptr({})", inner.to_string()),
            TypeData::Array(inner, size) => format!("array({}, {})", inner.to_string(), size),
            TypeData::Struct(_) => self.name.clone(),
            TypeData::FuncPointer(sig) => sig.to_string(),
        }
    }
}

impl Type
{
    fn to_string_rusttype(&self, is_ptr : bool) -> String
    {
        match &self.data
        {
            TypeData::Void => if is_ptr { "core::ffi::c_void" } else { "()" }.to_string(),
            TypeData::Primitive => self.name.clone(),
            TypeData::Pointer(inner) => format!("*mut {}", inner.to_string_rusttype(true)),
            TypeData::VirtualPointer(inner) => format!("<unrepresented>"),
            TypeData::Array(inner, size) => format!("<unrepresented>"),
            TypeData::Struct(_) => format!("*mut core::ffi::c_void"),
            TypeData::FuncPointer(sig) => sig.to_string_rusttype(),
        }
    }
    fn to_array(&self, count : usize) -> Type
    {
        Type { name : "array".to_string(), data : TypeData::Array(Box::new(self.clone()), count as usize) }
    }
    fn to_ptr(&self) -> Type
    {
        Type { name : "ptr".to_string(), data : TypeData::Pointer(Box::new(self.clone())) }
    }
    fn to_vptr(&self) -> Type
    {
        Type { name : "vptr".to_string(), data : TypeData::VirtualPointer(Box::new(self.clone())) }
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
    fn deref_vptr(&self) -> Type
    {
        match &self.data
        {
            TypeData::VirtualPointer(inner) =>
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
        panic!("error: attempted to use indexing on non-array type `{}`", self.to_string());
    }
    fn is_struct(&self) -> bool
    {
        matches!(self.data, TypeData::Struct(_))
    }
    fn is_array(&self) -> bool
    {
        matches!(self.data, TypeData::Array(_, _))
    }
    fn is_composite(&self) -> bool
    {
        self.is_struct() || self.is_array()
    }
    fn is_pointer(&self) -> bool
    {
        matches!(self.data, TypeData::Pointer(_))
    }
    fn is_virtual_pointer(&self) -> bool
    {
        matches!(self.data, TypeData::VirtualPointer(_))
    }
    fn is_float(&self) -> bool
    {
        self.name == "f32" || self.name == "f64"
    }
    fn is_int(&self) -> bool
    {
        ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64"].contains(&self.name.as_str())
    }
    fn is_int_signed(&self) -> bool
    {
        ["i8", "i16", "i32", "i64"].contains(&self.name.as_str())
    }
    fn is_int_unsigned(&self) -> bool
    {
        ["u8", "u16", "u32", "u64"].contains(&self.name.as_str())
    }
    fn is_void(&self) -> bool
    {
        matches!(self.data, TypeData::Void)
    }
    fn from_functionsig(funcsig : &FunctionSig) -> Type
    {
        Type { name : "funcptr".to_string(), data : TypeData::FuncPointer(Box::new(funcsig.clone())) }
    }
    fn size(&self) -> u32
    {
        match self.name.as_str()
        {
            "void" => 0,
            
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
                    inner.aligned_size() * *count as u32
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
    fn align(&self) -> u32
    {
        let align_size = match &self.data
        {
            TypeData::Array(inner, _) => inner.align(),
            TypeData::Struct(subs) =>
            {
                let mut maximum = 0;
                for (_, sub, _) in subs
                {
                    maximum = maximum.max(sub.align());
                }
                maximum
            }
            _ => self.size()
        };
        2_u32.pow(if align_size > 1 { (align_size-1).ilog2() + 1 } else { 0 })
    }
    fn aligned_size(&self) -> u32
    {
        let mut size = self.size();
        let mut align = self.align();
        size += align - 1;
        size /= align;
        size *= align;
        size
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
                if count * type_.aligned_size() as u64 == 0
                {
                    return Err(format!("error: zero-size arrays are not allowed"));
                }
                Ok(type_.to_array(count as usize))
            }
            else
            {
                res
            }
        }
        (true, "ptr_type") =>
        {
            let res = parse_type(types, node.child(0).unwrap());
            res.map(|x| x.to_ptr())
        }
        // FIXME
        (_, name) => Err(format!("error: unsupported type (culprit: `{}`)", name)),
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
    fn to_string_rusttype(&self) -> String
    {
        let return_type = self.return_type.to_string_rusttype(false);
        let mut args = String::new();
        let arg_count = self.args.len();
        for (i, arg) in self.args.iter().enumerate()
        {
            args += &arg.to_string_rusttype(false);
            if i+1 < arg_count
            {
                args += ", ";
            }
        }
        if return_type == "()"
        {
            format!("unsafe extern \"C\" fn({})", args)
        }
        else
        {
            format!("unsafe extern \"C\" fn({}) -> {}", args, return_type)
        }
    }
}
impl Function
{
    fn to_sig(&self) -> FunctionSig
    {
        FunctionSig { return_type : self.return_type.clone(), args : self.args.iter().map(|x| x.0.clone()).collect() }
    }
}
    
fn get_backend_type<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, type_ : &Type) -> inkwell::types::AnyTypeEnum<'c>
{
    if let Some(backend_type) = backend_types.get(&type_.name)
    {
        *backend_type
    }
    else
    {
        match &type_.data
        {
            TypeData::Void => panic!("internal error: tried to recreate void type"),
            TypeData::Primitive => panic!("internal error: tried to recreate primitive type"),
            TypeData::Pointer(inner) | TypeData::VirtualPointer(inner) =>
            {
                let backend_inner = get_backend_type(backend_types, &inner);
                let ptr_type = if let Ok(basic) = inkwell::types::BasicTypeEnum::try_from(backend_inner)
                {
                    basic.ptr_type(inkwell::AddressSpace::default())
                }
                else if let Ok(func) = inkwell::types::FunctionType::try_from(backend_inner)
                {
                    func.ptr_type(inkwell::AddressSpace::default())
                }
                else
                {
                    panic!("error: can't build pointers of type {}", type_.name)
                }.into();
                backend_types.insert(type_.name.clone(), ptr_type);
                ptr_type
            }
            TypeData::Array(inner, size) => panic!("TODO"),
            TypeData::Struct(struct_data) => panic!("TODO"),
            TypeData::FuncPointer(sig) => panic!("TODO"),
        }
    }
}

fn get_function_type<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, sig : &FunctionSig) -> inkwell::types::FunctionType<'c>
{
    let sig_type = Type::from_functionsig(sig);
    if let Some(backend_type) = backend_types.get(&sig_type.name)
    {
        return backend_type.into_function_type();
    }
    
    let mut params = Vec::new();
    
    for var_type in &sig.args
    {
        if var_type.is_void()
        {
            panic!("error: void function arguments are not allowed");
        }
        if let Ok(backend_type) = inkwell::types::BasicTypeEnum::try_from(get_backend_type(backend_types, &var_type))
        {
            params.push(backend_type.into());
        }
        else
        {
            panic!("error: non-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", var_type.name, var_type.name);
        }
    }
    
    let return_type = get_backend_type(backend_types, &sig.return_type);
    
    let func_type = if let Ok(basic) = inkwell::types::BasicTypeEnum::try_from(return_type)
    {
        basic.fn_type(&params, false)
    }
    else if let Ok(void) = inkwell::types::VoidType::try_from(return_type)
    {
        void.fn_type(&params, false)
    }
    else
    {
        panic!("error: can't build functions that return type {}", sig.return_type.name)
    };
    
    backend_types.insert(sig_type.name, func_type.into());
    
    func_type
}

#[derive(Debug, Clone)]
struct Program
{
    funcs : BTreeMap<String, Function>,
}

impl Program
{
    fn new(types : &mut BTreeMap<String, Type>, ast : &ASTNode) -> Result<Program, String>
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
                    if prop_name != "_" // for placeholders only
                    {
                        if prop_type.name == "void"
                        {
                            panic!("error: void struct properties are not allowed");
                        }
                        let align = prop_type.align() as usize;
                        
                        if offset%align != 0
                        {
                            panic!("error: property {} of struct type {} is not aligned (should be aligned to {} bytes; actual offset was {}, for a misalignment of {} bytes)\nNOTE: add padding before this property like `array(u8, {}) _;`", prop_name, name, align, offset, offset%align, align - offset%align);
                        }
                        
                        struct_data.push((prop_name, prop_type.clone(), offset));
                    }
                    offset += prop_type.size() as usize;
                }
                
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

struct Environment<'a, 'b, 'c, 'd>
{
    builder    : &'b inkwell::builder::Builder<'c>,
    module     : &'d inkwell::module::Module<'c>,
    types      : &'a BTreeMap<String, Type>,
}

impl<'a, 'b, 'c, 'd> Environment<'a, 'b, 'c, 'd>
{
    /*
    fn stack_push(&mut self, stuff : (Type, Value))
    {
        self.stack.push((stuff.0, Some(stuff.1), None))
    }
    fn stack_pop(&mut self) -> Option<(Type, Value)>
    {
        let stuff = self.stack.pop().unwrap();
        Some((stuff.0, stuff.1.unwrap()))
    }
    */
}

fn main()
{
    let mut context = inkwell::context::Context::create();
    
    let type_table = [
        ("void".to_string(), Type { name : "void".to_string(), data : TypeData::Void }, context.void_type().as_any_type_enum()),
        
        ("u8" .to_string(), Type { name : "u8" .to_string(), data : TypeData::Primitive }, context.f32_type().as_any_type_enum()),
        ("u16".to_string(), Type { name : "u16".to_string(), data : TypeData::Primitive }, context.f64_type().as_any_type_enum()),
        ("u32".to_string(), Type { name : "u32".to_string(), data : TypeData::Primitive }, context. i8_type().as_any_type_enum()),
        ("u64".to_string(), Type { name : "u64".to_string(), data : TypeData::Primitive }, context.i16_type().as_any_type_enum()),
        
        ("i8" .to_string(), Type { name : "i8" .to_string(), data : TypeData::Primitive }, context.i32_type().as_any_type_enum()),
        ("i16".to_string(), Type { name : "i16".to_string(), data : TypeData::Primitive }, context.i64_type().as_any_type_enum()),
        ("i32".to_string(), Type { name : "i32".to_string(), data : TypeData::Primitive }, context. i8_type().as_any_type_enum()),
        ("i64".to_string(), Type { name : "i64".to_string(), data : TypeData::Primitive }, context.i16_type().as_any_type_enum()),
        
        ("f32".to_string(), Type { name : "f32".to_string(), data : TypeData::Primitive }, context.f32_type().as_any_type_enum()),
        ("f64".to_string(), Type { name : "f64".to_string(), data : TypeData::Primitive }, context.f64_type().as_any_type_enum()),
    ];
    
    // only holds frontend types, and only for primitives and structs, not pointers or arrays, those are constructed dynamically
    let mut types = BTreeMap::new();
    // holds backend types for everything, including pointers and arrays
    let mut backend_types = BTreeMap::new();
    for (a, b, c) in type_table.iter()
    {
        types.insert(a.clone(), b.clone());
        backend_types.insert(a.clone(), *c);
    }
    
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
    
    
    let mut imports : BTreeMap<String, (*const u8, FunctionSig)> = BTreeMap::new();
    fn import_function<T>(types: &BTreeMap<String, Type>, parser : &mut parser::Parser, imports : &mut BTreeMap<String, (*const u8, FunctionSig)>, name : &str, pointer : T, pointer_usize : usize, type_string : &str)
    {
        let type_lines = vec!(type_string.to_string());
        let type_tokens = parser.tokenize(&type_lines, false).unwrap();
        let type_ast = parser.parse_with_root_node_type(&type_tokens, &type_lines, false, "type").unwrap().unwrap();
        let type_ = parse_type(&types, &type_ast).unwrap();
        if let TypeData::FuncPointer(funcsig) = type_.data
        {
            let want_type_string = std::any::type_name::<T>(); // FIXME: not guaranteed to be stable across rust versions
            let type_string_rust = funcsig.to_string_rusttype();
            assert!(want_type_string == type_string_rust, "types do not match:\n{}\n{}\n", want_type_string, type_string_rust);
            assert!(want_type_string.starts_with("unsafe "), "function pointer type must be unsafe");
            
            let ptr = pointer_usize as *const u8;
            imports.insert(name.to_string(), (ptr, *funcsig));
        }
        else
        {
            panic!("type string must be function type");
        }
    }
    //import_function::<unsafe extern "C" fn(*mut u8, u64) -> ()>(&types, &mut parser, &mut imports, "print_bytes", print_bytes, print_bytes as usize, "funcptr(void, (ptr(u8), u64))");
    
    let context = Context::create();
    let module = context.create_module("main");
    
    //let mut builder = JITBuilder::with_flags(&settings, cranelift_module::default_libcall_names()).unwrap();
    //for (f_name, (pointer, funcsig)) in &imports
    //{
    //    builder.symbol(f_name, *pointer);
    //}
    
    let mut module = context.create_module("main");
    
    //let mut func_types = BTreeMap::new();
    //let mut func_sizes = BTreeMap::new();
    //let mut func_disasm = BTreeMap::new();
    let mut func_decs = BTreeMap::new();
    
    for (f_name, (pointer, funcsig)) in &imports
    {
        let func_type = get_function_type(&mut backend_types, &funcsig);
        let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::AvailableExternally));
        func_decs.insert(f_name.clone(), (func_val, funcsig.clone()));
    }
    
    for (f_name, function) in &program.funcs
    {
        let funcsig = function.to_sig();
        let func_type = get_function_type(&mut backend_types, &funcsig);
        let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::External));
        func_decs.insert(f_name.clone(), (func_val, funcsig));
    }
    
    for f in &program.funcs
    {
        let f_name = f.0;
        let function = f.1;
        
        let (func_val, funcsig) = func_decs.get(f_name).unwrap().clone();
        let func_type = get_function_type(&mut backend_types, &funcsig);
        
        let block = context.append_basic_block(func_val, "entry");
        let builder = context.create_builder();
        builder.position_at_end(block);
        
        let mut variables = BTreeMap::new();
        
        // declare arguments
        let mut i = 0;
        for (j, param) in function.args.iter().enumerate()
        {
            let var_type = param.0.clone();
            let var_name = param.1.clone();
            
            let backend_type = get_backend_type(&mut backend_types, &var_type);
            if let Ok(basic_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
            {
                let slot = builder.build_alloca(basic_type, &var_name);
                
                let val = func_val.get_nth_param(j as u32).unwrap();
                builder.build_store(slot, val);
                
                if variables.contains_key(&var_name) || func_decs.contains_key(&var_name)
                {
                    panic!("error: parameter {} redeclared", var_name);
                }
                variables.insert(var_name, (var_type, slot, None::<inkwell::values::BasicValueEnum>));
            }
            else
            {
                panic!("error: variables of type {} are not allowed", var_type.name);
            }
            
            i += 1;
        }
        
        // declare variables
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && (node.text == "declaration" || node.text == "fulldeclaration" || node.text == "constdeclaration")
            {
                let var_type = parse_type(&types, &node.child(0).unwrap()).unwrap();
                let var_name = node.child(1).unwrap().child(0).unwrap().text.clone();
                
                let backend_type = get_backend_type(&mut backend_types, &var_type);
                if let Ok(basic_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
                {
                    let slot = builder.build_alloca(basic_type, &var_name);
                    
                    if variables.contains_key(&var_name) || func_decs.contains_key(&var_name)
                    {
                        panic!("error: parameter {} redeclared", var_name);
                    }
                    variables.insert(var_name, (var_type, slot, None));
                }
                else
                {
                    panic!("error: variables of type {} are not allowed", var_type.name);
                }
                
                i += 1;
            }
            false
        });
        
        // collect labels (blocks)
        let mut blocks = HashMap::new();
        let mut blocks_vec = Vec::new();
        let mut next_block = HashMap::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && node.text == "label"
            {
                let name = &node.child(0).unwrap().child(0).unwrap().text;
                if !blocks.contains_key(name)
                {
                    let block = context.append_basic_block(func_val, name);
                    blocks.insert(name.clone(), block);
                    if let Some(prev) = blocks_vec.last()
                    {
                        next_block.insert(*prev, block);
                    }
                    blocks_vec.push(block);
                }
                else
                {
                    panic!("error: redeclared block {}", name);
                }
            }
            false
        });
        
        //let mut stack = Vec::new();
        
        let mut env = Environment { builder : &builder, module : &module, types : &types };
        
        println!("compiling function {}...", function.name);
    }
    
    println!("module:\n{}", module.to_string());
}
