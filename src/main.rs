extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};
use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;
use inkwell::types::{AnyType, BasicType};
use inkwell::values::{AnyValue, BasicValue};

use parser::ast::ASTNode;

mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeData
{
    Void,
    Primitive,
    Struct(Vec<(String, Type)>), // property name, property type
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
            TypeData::Pointer(inner) => *inner.clone(),
            _ => panic!("error: attempted to dereference non-pointer type `{}`", self.to_string()),
        }
    }
    fn deref_vptr(&self) -> Type
    {
        match &self.data
        {
            TypeData::VirtualPointer(inner) => *inner.clone(),
            _ => panic!("error: attempted to dereference non-pointer type `{}`", self.to_string()),
        }
    }
    fn array_to_inner(&self) -> Type
    {
        match &self.data
        {
            TypeData::Array(inner, _size) => *inner.clone(),
            _ => panic!("error: attempted to use indexing on non-array type `{}`", self.to_string()),
        }
    }
    fn struct_to_info(&self) -> Vec<(String, Type)>
    {
        match &self.data
        {
            TypeData::Struct(info) => info.clone(),
            _ => panic!("internal error: attempted to access struct info of non-struct type `{}`", self.to_string()),
        }
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
            Err(format!("error: unknown struct type `{}`", name))
        }
        (true, "array_type") =>
        {
            let res = parse_type(types, node.child(0).unwrap());
            if let Ok(type_) = res
            {
                let count_text = &node.child(1).unwrap().child(0).unwrap().text;
                let count : u64 = count_text.parse().unwrap();
                if count == 0 || type_.name == "void"
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

fn store_size_of_type<'a>(target_data : &inkwell::targets::TargetData, backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'a>>, type_ : &Type) -> u64
{
    if type_.is_void()
    {
        return 0;
    }
    let backend_type = get_backend_type(backend_types, type_);
    target_data.get_store_size(&backend_type)
}
fn alloc_size_of_type<'a>(target_data : &inkwell::targets::TargetData, backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'a>>, type_ : &Type) -> u64
{
    if type_.is_void()
    {
        return 0;
    }
    let backend_type = get_backend_type(backend_types, type_);
    target_data.get_abi_size(&backend_type)
}
fn whyyyyyy_kgafnagerriawgiugsafbiu438438094<'c>(sdkawuidsguisagugarewudsga : inkwell::types::BasicTypeEnum<'c>) -> inkwell::context::ContextRef<'c>
{
    match sdkawuidsguisagugarewudsga
    {
        inkwell::types::BasicTypeEnum::ArrayType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        inkwell::types::BasicTypeEnum::FloatType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        inkwell::types::BasicTypeEnum::IntType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        inkwell::types::BasicTypeEnum::PointerType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        inkwell::types::BasicTypeEnum::StructType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        inkwell::types::BasicTypeEnum::VectorType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
    }
}
fn get_backend_type<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, type_ : &Type) -> inkwell::types::AnyTypeEnum<'c>
{
    let key = type_.to_string();
    
    if let Some(backend_type) = backend_types.get(&key)
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
                    panic!("error: can't build pointers of type {}", key)
                }.into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::Array(inner, size) =>
            {
                let backend_inner = get_backend_type_sized(backend_types, &inner);
                let ptr_type = backend_inner.array_type(*size as u32).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::Struct(struct_data) =>
            {
                let mut types = Vec::new();
                for (name, type_) in struct_data
                {
                    let backend_type = get_backend_type_sized(backend_types, &type_);
                    types.push(backend_type);
                }
                if let Some(first) = types.first()
                {
                    let context = whyyyyyy_kgafnagerriawgiugsafbiu438438094(*first);
                    let ptr_type = context.struct_type(&types, false).into();
                    backend_types.insert(key, ptr_type);
                    ptr_type
                }
                else
                {
                    panic!("error: structs cannot be empty");
                }
            }
            TypeData::FuncPointer(sig) => panic!("TODO"),
        }
    }
}

fn get_backend_type_sized<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, type_ : &Type) -> inkwell::types::BasicTypeEnum<'c>
{
    let backend_type = get_backend_type(backend_types, &type_);
    if let Ok(basic_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
    {
        basic_type
    }
    else
    {
        panic!("error: tried to use a non-sized type in a context where only sized types are allowed");
    }
}

fn get_function_type<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, sig : &FunctionSig) -> inkwell::types::FunctionType<'c>
{
    let sig_type = Type::from_functionsig(sig);
    let key = sig_type.to_string();
    if let Some(backend_type) = backend_types.get(&key)
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
    
    //println!("testing return type {:?} for {:?}...", return_type, sig.return_type);
    
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
    
    //println!("func type is  {:?}", func_type);
    
    backend_types.insert(key, func_type.into());
    
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
                        struct_data.push((prop_name, prop_type.clone()));
                    }
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

struct Environment<'a, 'b, 'c, 'd, 'f>
{
    context       : &'c inkwell::context::Context,
    stack         : Vec<(Type, inkwell::values::BasicValueEnum<'c>)>,
    variables     : BTreeMap<String, (Type, inkwell::values::PointerValue<'c>)>,
    builder       : &'b inkwell::builder::Builder<'c>,
    module        : &'d inkwell::module::Module<'c>,
    func_decs     : &'a BTreeMap<String, (inkwell::values::FunctionValue<'c>, FunctionSig)>,
    types         : &'a BTreeMap<String, Type>,
    backend_types : &'a mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>,
    func_val      : inkwell::values::FunctionValue<'c>,
    blocks        : HashMap<String, inkwell::basic_block::BasicBlock<'c>>,
    ptr_int_type  : inkwell::types::IntType<'c>,
    target_data   : &'f inkwell::targets::TargetData,
}

#[derive(Clone, Debug, Copy, PartialEq)]
enum WantPointer {
    None,
    Real,
    Virtual,
}
fn compile<'a, 'b>(env : &'a mut Environment, node : &'b ASTNode, want_pointer : WantPointer)
{
    // used to build constants for some lowerings, and to cast to bool
    let u8_type_frontend = env.types.get("u8").unwrap();
    let u8_type = env.backend_types.get("u8").unwrap().into_int_type();
    let u64_type = env.backend_types.get("u64").unwrap().into_int_type();
    
    macro_rules! push_val_or_ptr
    {
        ($type:expr, $addr:expr) =>
        {
            if want_pointer == WantPointer::Real
            {
                env.stack.push(($type.to_ptr(), $addr.into()));
            }
            else if want_pointer == WantPointer::Virtual
            {
                env.stack.push(($type.to_vptr(), $addr.into()));
            }
            else
            {
                let basic_type = get_backend_type_sized(&mut env.backend_types, &$type);
                let val = env.builder.build_load(basic_type, $addr, "");
                env.stack.push(($type.clone(), val));
            }
        }
    }
    if node.is_parent()
    {
        match node.text.as_str()
        {
            "funcbody" =>
            {
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
                if env.builder.get_insert_block().unwrap().get_terminator().is_none()
                {
                    panic!("error: functions must explicitly return, even if their return type is void\n(on line {})", node.line);
                }
            }
            "statementlist" | "statement" | "instruction" =>
            {
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
            }
            "parenexpr" | "arrayindex" =>
            {
                for child in node.get_children().unwrap()
                {
                    compile(env, child, want_pointer);
                }
            }
            "unusedcomma" => {},
            "return" =>
            {
                let mut returns = Vec::new();
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                    let (type_, val) = env.stack.pop().unwrap();
                    returns.push((type_, val));
                }
                if let Some((_, val)) = returns.get(0)
                {
                    // FIXME: check types
                    //assert!();
                    env.builder.build_return(Some(val));
                }
                else
                {
                    env.builder.build_return(None);
                }
            }
            "declaration" =>
            {
                return;
            }
            "fulldeclaration" =>
            {
                let name = &node.child(1).unwrap().child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (type_var, slot) = env.variables[name].clone();
                    
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let (type_val, val) = env.stack.pop().unwrap();
                    
                    assert!(type_val == type_var, "fulldec type failure, {:?} vs {:?}, line {}", type_val, type_var, node.line);
                    
                    let instval = env.builder.build_store(slot, val);
                    let v = instval.get_volatile();
                    if false
                    {
                        println!("volatile: {:?}", v);
                    }
                }
                else
                {
                    panic!("internal error: failed to find variable in full declaration");
                }
            }
            "binstate" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let (type_val, val) = env.stack.pop().unwrap();
                let (type_left_incomplete, left_addr) = env.stack.pop().unwrap();
                
                let type_left = if type_left_incomplete.is_virtual_pointer()
                {
                    type_left_incomplete.deref_vptr()
                }
                else
                {
                    panic!("tried to assign to fully evaluated expression (not a variable or virtual pointer) {:?}", type_left_incomplete);
                };
                assert!(type_val == type_left);
                
                if let Ok(addr) = inkwell::values::PointerValue::try_from(left_addr)
                {
                    env.builder.build_store(addr, val);
                }
                else
                {
                    panic!("tried to assign to fully evaluated expression (not a variable or virtual pointer) {:?}", type_left_incomplete);
                }
            }
            "lvar" =>
            {
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::Virtual);
                }
            }
            "lvar_name" =>
            {
                let name = &node.child(0).unwrap().child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (type_, slot) = env.variables[name].clone();
                    
                    assert!(want_pointer != WantPointer::None);
                    
                    push_val_or_ptr!(type_, slot);
                }
                else
                {
                    panic!("error: unrecognized variable `{}`", name);
                }
            }
            "rvarname" =>
            {
                let name = &node.child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (type_, slot) = env.variables[name].clone();
                    
                    push_val_or_ptr!(type_, slot);
                }
                else if let Some((func_val, funcsig)) = env.func_decs.get(name)
                {
                    let func_ptr = func_val.as_global_value().as_pointer_value();
                    env.stack.push((Type::from_functionsig(funcsig), func_ptr.into()));
                }
                else
                {
                    panic!("unrecognized identifier {}", name);
                }
            }
            "arrayindex_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::Virtual);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                
                //println!("compiling arrayindex head");
                
                let (offset_type, offset_val) = env.stack.pop().unwrap();
                let (base_type, base_addr) = env.stack.pop().unwrap();
                let base_type = base_type.deref_vptr();
                
                if offset_type.name == "i64"
                {
                    // FIXME: double check that nested types work properly
                    //println!("\ntype: {:?}", base_type);
                    //println!("\nval: {:?}\n", base_addr);
                    
                    let inner_type = base_type.array_to_inner();
                    let inner_backend_type = get_backend_type_sized(&mut env.backend_types, &inner_type);
                    let inner_addr = unsafe
                    {
                        env.builder.build_in_bounds_gep(inner_backend_type, base_addr.into_pointer_value(), &[offset_val.into_int_value()], "")
                    };
                    
                    push_val_or_ptr!(inner_type, inner_addr);
                    
                }
                else
                {
                    panic!("error: can't offset into arrays except with type i64 (used type `{}`)", offset_type.name)
                }
            }
            "indirection_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::Virtual);
                let (struct_type, struct_addr) = env.stack.pop().unwrap();
                let struct_type = struct_type.deref_vptr();
                let backend_type = get_backend_type_sized(&mut env.backend_types, &struct_type);
                
                let right_name = &node.child(1).unwrap().child(0).unwrap().text;
                
                if let Some(found) = match &struct_type.data {
                    TypeData::Struct(ref props) => props.iter().enumerate().find(|x| x.1.0 == *right_name),
                    _ => panic!("error: tried to use indirection (.) operator on non-struct"),
                }
                {
                    let inner_index = found.0;
                    let inner_type = &found.1.1;
                    let struct_addr = struct_addr.into_pointer_value();
                    
                    let index_addr = unsafe
                    {
                        env.builder.build_struct_gep::<inkwell::types::BasicTypeEnum>(backend_type.into(), struct_addr, inner_index as u32, "").unwrap()
                    };
                    
                    push_val_or_ptr!(inner_type, index_addr);
                }
                else
                {
                    panic!("error: no such property {} in struct type {}", right_name, struct_type.name);
                }
            }
            "funcargs_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, funcaddr) = env.stack.pop().unwrap();
                match type_.data
                {
                    TypeData::FuncPointer(funcsig) =>
                    {
                        let func_type = get_function_type(&mut env.backend_types, &funcsig);
                        
                        let stack_len_start = env.stack.len();
                        compile(env, node.child(1).unwrap(), WantPointer::None);
                        let stack_len_end = env.stack.len();
                        
                        let num_args = node.child(1).unwrap().child_count().unwrap();
                        assert!(num_args == stack_len_end - stack_len_start);
                        assert!(num_args == funcsig.args.len(), "incorrect number of arguments to function on line {}", node.line);
                        
                        let mut args = Vec::new();
                        for (i, arg_type) in funcsig.args.iter().rev().enumerate()
                        {
                            let (type_, val) = env.stack.pop().unwrap();
                            if type_ != *arg_type
                            {
                                panic!("mismatched types for parameter {} in call to function {:?} on line {}: expected `{}`, got `{}`", i+1, funcaddr, node.line, arg_type.to_string(), type_.to_string());
                            }
                            args.push(val.into());
                        }
                        
                        args.reverse();
                        
                        //println!("calling func with sigref {} and sig {}", sigref, funcsig.to_string());
                        let callval = env.builder.build_indirect_call(func_type, funcaddr.into_pointer_value(), &args, "");
                        let result = callval.try_as_basic_value().left();
                        //println!("number of results {}", results.len());
                        for (result, type_) in result.iter().zip([funcsig.return_type])
                        {
                            env.stack.push((type_.clone(), *result));
                        }
                    }
                    _ => panic!("error: tried to use non-function expression as a function")
                }
                //println!("done compiling func call");
            }
            "funcargs" =>
            {
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
            }
            "array_literal" =>
            {
                let stack_size = env.stack.len();
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
                let array_length = env.stack.len() - stack_size;
                //println!("array length: {}", array_length);
                let mut vals = Vec::new();
                let mut element_type = None;
                
                for _ in 0..array_length
                {
                    let (type_, val) = env.stack.pop().unwrap();
                    if element_type.is_none()
                    {
                        element_type = Some(type_.clone());
                    }
                    if Some(type_.clone()) != element_type
                    {
                        panic!("error: array literals must entirely be of a single type");
                    }
                    vals.push(val);
                }
                vals.reverse();
                
                if element_type.is_some()
                {
                    let element_type = element_type.unwrap();
                    let array_type = element_type.to_array(array_length);
                    let backend_type = get_backend_type_sized(&mut env.backend_types, &array_type);
                    let element_backend_type = get_backend_type_sized(&mut env.backend_types, &element_type);
                    
                    let size = u64_type.const_int(array_length as u64, false);
                    let slot = env.builder.build_array_alloca(element_backend_type, size, "");
                    
                    let mut offset = 0;
                    for val in vals
                    {
                        let offset_val = u64_type.const_int(offset as u64, false);
                        let offset_addr = unsafe
                        {
                            env.builder.build_in_bounds_gep(element_backend_type, slot, &[offset_val], "")
                        };
                        
                        env.builder.build_store(offset_addr, val);
                        offset += 1;
                    }
                    
                    push_val_or_ptr!(array_type, slot);
                }
                else
                {
                    panic!("error: zero-length array literals are not allowed");
                }
            }
            "struct_literal" =>
            {
                let stack_size = env.stack.len();
                
                let struct_type = parse_type(&env.types, &node.child(0).unwrap()).unwrap();
                let struct_member_types = struct_type.struct_to_info().iter().map(|x| x.1.clone()).collect::<Vec<_>>();
                //println!("struct type: {:?}", struct_type);
                
                for child in &node.get_children().unwrap()[1..]
                {
                    compile(env, child, WantPointer::None);
                }
                let member_count = env.stack.len() - stack_size;
                assert!(member_count == struct_member_types.len());
                //println!("struct member count: {}", member_count);
                
                let mut vals = Vec::new();
                for _ in 0..member_count
                {
                    let (type_, val) = env.stack.pop().unwrap();
                    vals.push((type_, val));
                }
                vals.reverse();
                
                let stack_val_types = vals.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
                assert!(struct_member_types == stack_val_types);
                
                let backend_type = get_backend_type_sized(&mut env.backend_types, &struct_type);
                
                //let size = alloc_size_of_type(&env.target_data, env.backend_types, &struct_type);
                //println!("{:?}", backend_type);
                let slot = env.builder.build_alloca(backend_type, "");
                for (index, (type_, val)) in vals.into_iter().enumerate()
                {
                    let index_addr = unsafe
                    {
                        env.builder.build_struct_gep::<inkwell::types::BasicTypeEnum>(backend_type.into(), slot, index as u32, "").unwrap()
                    };
                    
                    assert!(type_ == stack_val_types[index]);
                    
                    env.builder.build_store(index_addr, val);
                }
                
                push_val_or_ptr!(struct_type, slot);
            }
            "float" =>
            {
                let text = &node.child(0).unwrap().text;
                let location = text.rfind("f").unwrap();
                let parts = text.split_at(location);
                let text = parts.0;
                if let Some(type_) = env.types.get(parts.1)
                {
                    let backend_type = get_backend_type(&mut env.backend_types, &type_);
                    if let Ok(float_type) = inkwell::types::FloatType::try_from(backend_type)
                    {
                        match parts.1
                        {
                            "f32" =>
                            {
                                let val : f32 = text.parse().unwrap();
                                let res = float_type.const_float(val as f64);
                                env.stack.push((type_.clone(), res.into()));
                            }
                            "f64" =>
                            {
                                let val : f64 = text.parse().unwrap();
                                let res = float_type.const_float(val);
                                env.stack.push((type_.clone(), res.into()));
                            }
                            _ => panic!("unknown float suffix pattern {}", parts.1)
                        }
                    }
                    else
                    {
                        panic!("unknown float suffix pattern {}", parts.1)
                    }
                }
                else
                {
                    panic!("unknown float suffix pattern {}", parts.1)
                }
            }
            "integer" =>
            {
                let text = &node.child(0).unwrap().text;
                
                let mut location = text.rfind("u");
                let mut signed = false;
                if location.is_none()
                {
                    location = text.rfind("i");
                    signed = true;
                }
                if location.is_none()
                {
                    panic!("internal error: unknown integer type literal suffix");
                }
                let location = location.unwrap();
                let parts = text.split_at(location);
                let text = parts.0;
                
                let mut is_hex = false;
                let text = if text.find("0x") == Some(0)
                {
                    is_hex = true;
                    let parts = text.split_at(2);
                    parts.1.to_string()
                }
                else if text.find("-0x") == Some(0)
                {
                    is_hex = true;
                    let parts = text.split_at(2);
                    "-".to_string() + parts.1
                }
                else
                {
                    text.to_string()
                };
                
                if let Some(type_) = env.types.get(parts.1)
                {
                    let backend_type = get_backend_type(&mut env.backend_types, &type_);
                    if let Ok(int_type) = inkwell::types::IntType::try_from(backend_type)
                    {
                        let res = int_type.const_int(match (is_hex, parts.1)
                        {
                            (false, "u8")  => text.parse::< u8>().unwrap() as u64,
                            (false, "u16") => text.parse::<u16>().unwrap() as u64,
                            (false, "u32") => text.parse::<u32>().unwrap() as u64,
                            (false, "u64") => text.parse::<u64>().unwrap() as u64,
                            (false, "i8")  => text.parse::< i8>().unwrap() as u64,
                            (false, "i16") => text.parse::<i16>().unwrap() as u64,
                            (false, "i32") => text.parse::<i32>().unwrap() as u64,
                            (false, "i64") => text.parse::<i64>().unwrap() as u64,
                            (true , "u8")  =>  u8::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "u16") => u16::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "u32") => u32::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "u64") => u64::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "i8")  =>  i8::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "i16") => i16::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "i32") => i32::from_str_radix(&text, 16).unwrap() as u64,
                            (true , "i64") => i64::from_str_radix(&text, 16).unwrap() as u64,
                            _ => panic!("unknown int suffix pattern {}", parts.1)
                        }, signed);
                        env.stack.push((type_.clone(), res.into()));
                    }
                    else
                    {
                        panic!("unknown int suffix pattern {}", parts.1)
                    }
                }
                else
                {
                    panic!("unknown int suffix pattern {}", parts.1)
                }
            }
            "unary" =>
            {
                let op = &node.child(0).unwrap().child(0).unwrap().text;
                //println!("---- compiling unary operator `{}`", op);
                if op.as_str() == "&"
                {
                    compile(env, node.child(1).unwrap(), WantPointer::Real);
                    let (type_, val) = env.stack.pop().unwrap();
                    if !type_.is_pointer()
                    {
                        panic!("error: tried to get address of non-variable");
                    }
                    env.stack.push((type_, val));
                }
                else
                {
                    compile(env, node.child(1).unwrap(), WantPointer::None);
                    let (type_, val) = env.stack.pop().unwrap();
                    match type_.name.as_str()
                    {
                        "ptr" =>
                        {
                            if want_pointer == WantPointer::Virtual
                            {
                                if op.as_str() != "*"
                                {
                                    panic!("error: can't use operator `{}` on type `{}`", op, type_.name);
                                }
                                env.stack.push((type_.deref_ptr().to_vptr(), val));
                            }
                            else
                            {
                                let inner_type = type_.deref_ptr();
                                if inner_type.is_void()
                                {
                                    panic!("can't dereference void pointers");
                                }
                                let basic_type = get_backend_type_sized(&mut env.backend_types, &inner_type);
                                let res = match op.as_str()
                                {
                                    "*" => env.builder.build_load(basic_type, val.into_pointer_value(), ""),
                                    _ => panic!("error: can't use operator `{}` on type `{}`", op, type_.name)
                                };
                                env.stack.push((inner_type, res));
                            }
                        }
                        "f32" | "f64" =>
                        {
                            let res = match op.as_str()
                            {
                                "+" => val,
                                "-" => env.builder.build_float_neg(val.into_float_value(), "").into(),
                                _ => panic!("error: can't use operator `{}` on type `{}`", op, type_.name)
                            };
                            env.stack.push((type_.clone(), res));
                        }
                        // TODO reimplement
                        /*
                        "i8" | "i16" | "i32" | "i64" |
                        "u8" | "u16" | "u32" | "u64" =>
                        {
                            let res = match op.as_str()
                            {
                                "+" => val,
                                "-" => env.builder.ins().ineg(val),
                                "!" => env.builder.ins().icmp_imm(IntCC::Equal, val, 0),
                                "~" => env.builder.ins().bnot(val),
                                _ => panic!("error: can't use operator `{}` on type `{}`", op, type_.name)
                            };
                            env.stack.push((type_.clone(), res));
                        }
                        */
                        _ => panic!("error: type `{}` is not supported by unary operators", type_.name)
                    }
                }
            }
            "label" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                if let Some(block) = env.blocks.get(label)
                {
                    env.builder.build_unconditional_branch(*block);
                    env.builder.position_at_end(*block);
                }
                else
                {
                    panic!("error: no such block {}", label);
                }
            }
            "goto" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                let next_block = env.context.append_basic_block(env.func_val, "");
                if let Some(then_block) = env.blocks.get(label)
                {
                    env.builder.build_unconditional_branch(*then_block);
                    env.builder.position_at_end(next_block);
                }
                else
                {
                    panic!("error: no such label {}", label);
                }
            }
            // TODO: all the other C-style control flow mechanisms
            "ifcondition" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, val) = env.stack.pop().unwrap();
                let label = &node.child(1).unwrap().child(0).unwrap().text;
                // anonymous block for "else" case
                let else_block = env.context.append_basic_block(env.func_val, "");
                if let Some(then_block) = env.blocks.get(label)
                {
                    let backend_type = get_backend_type(&mut env.backend_types, &type_);
                    if let (Ok(int_type), Ok(int_val)) = (inkwell::types::IntType::try_from(backend_type), inkwell::values::IntValue::try_from(val))
                    {
                        let zero = int_type.const_int(0, true);
                        let int_val = env.builder.build_int_compare(inkwell::IntPredicate::NE, int_val, zero, "");
                        let instval = env.builder.build_conditional_branch(int_val, *then_block, else_block);
                        env.builder.position_at_end(else_block);
                    }
                    else
                    {
                        panic!("error: tried to branch on non-integer expression");
                    }
                }
                else
                {
                    panic!("error: no such label {}", label);
                }
            }
            "bitcast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack.pop().unwrap();
                
                let right_type = parse_type(&env.types, &node.child(1).unwrap()).unwrap();
                
                let (left_size, right_size) = (store_size_of_type(&env.target_data, env.backend_types, &left_type), store_size_of_type(&env.target_data, env.backend_types, &right_type));
                let ptr_size = env.target_data.get_store_size(&env.ptr_int_type);
                
                // FIXME: platform-specific pointer size
                let basic_type = get_backend_type_sized(&mut env.backend_types, &right_type);
                if left_size == right_size || (right_size == ptr_size && left_type.is_composite())
                {
                    let ret = env.builder.build_bitcast(left_val, basic_type, "");
                    env.stack.push((right_type, ret));
                }
                else
                {
                    panic!("error: unsupported bitcast from type {} to type {} (types must have the same size and be sized to be bitcasted)", left_type.to_string(), right_type.to_string());
                }
            }
            "cast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack.pop().unwrap();
                
                let right_type = parse_type(&env.types, &node.child(1).unwrap()).unwrap();
                let target_backend_type = get_backend_type(&mut env.backend_types, &right_type);
                
                // cast as own type (replace type, aka do nothing)
                if left_type.name == right_type.name
                {
                    env.stack.push((right_type, left_val));
                }
                // cast from pointer to pointer (replace type)
                else if left_type.is_pointer() && right_type.is_pointer()
                {
                    env.stack.push((right_type, left_val));
                }
                // cast between float types"
                else if (left_type.name == "f32" && right_type.name == "f64") || (left_type.name == "f64" && right_type.name == "f32")
                {
                    if let (Ok(left_val), Ok(float_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::FloatType::try_from(target_backend_type))
                    {
                        let ret = env.builder.build_float_cast(left_val, float_type, "");
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic!("internal error: float cast internal and backend type mismatch");
                    }
                }
                // TODO reimplement all these casts
                /*
                // cast between types of same size, non-float. bitcast.
                else if !left_type.is_float() && !right_type.is_float() && left_type.size() == right_type.size()
                {
                    let ret = env.builder.ins().bitcast(target_cranetype, MemFlags::new(), left_val);
                    env.stack_push((right_type, ret));
                }
                // cast from float to int (must be int, not pointer)
                else if left_type.is_int_signed() && right_type.is_float()
                {
                    let ret = env.builder.ins().fcvt_from_sint(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                else if left_type.is_int_unsigned() && right_type.is_float()
                {
                    let ret = env.builder.ins().fcvt_from_uint(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast from int to float (must be int, not pointer)
                else if left_type.is_float() && right_type.is_int_signed()
                {
                    let ret = env.builder.ins().fcvt_to_sint(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                else if left_type.is_float() && right_type.is_int_unsigned()
                {
                    let ret = env.builder.ins().fcvt_to_uint(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast from smaller signed to larger unsigned
                else if left_type.size() < right_type.size() && left_type.is_int_signed() && right_type.is_int_unsigned()
                {
                    let ret = env.builder.ins().sextend(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast from smaller signed to larger unsigned
                else if left_type.size() < right_type.size() && left_type.is_int_unsigned() && right_type.is_int_signed()
                {
                    let ret = env.builder.ins().uextend(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast to larger int type, signed
                else if left_type.size() < right_type.size() && left_type.is_int_signed() && right_type.is_int_signed()
                {
                    let ret = env.builder.ins().sextend(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast to larger int type, unsigned
                else if left_type.size() < right_type.size() && left_type.is_int_unsigned() && right_type.is_int_unsigned()
                {
                    let ret = env.builder.ins().uextend(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                // cast to smaller int type
                else if left_type.size() > right_type.size() && left_type.is_int() && right_type.is_int()
                {
                    let ret = env.builder.ins().ireduce(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                */
                else
                {
                    panic!("unsupported cast from type {} to type {}", left_type.to_string(), right_type.to_string());
                }
            }
            text => 
            {
                if text.starts_with("binexpr")
                {
                    compile(env, node.child(0).unwrap(), WantPointer::None);
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let op = &node.child(1).unwrap().child(0).unwrap().text;
                    let (right_type, right_val)  = env.stack.pop().unwrap();
                    let (left_type , left_val )  = env.stack.pop().unwrap();
                    
                    let is_u = left_type.name.starts_with("u");
                    
                    match (left_type.name.as_str(), right_type.name.as_str())
                    {
                        ("f32", "f32") | ("f64", "f64") =>
                        {
                            let left_val = left_val.into_float_value();
                            let right_val = right_val.into_float_value();
                            match op.as_str()
                            {
                                "+" | "-" | "*" | "/" | "%" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "+" => env.builder.build_float_add(left_val, right_val, ""),
                                        "-" => env.builder.build_float_sub(left_val, right_val, ""),
                                        "*" => env.builder.build_float_mul(left_val, right_val, ""),
                                        "/" => env.builder.build_float_div(left_val, right_val, ""),
                                        /*
                                        // TODO reimplement
                                        "%" =>
                                        {
                                            let times = env.builder.ins().fdiv(left_val, right_val);
                                            let floored = env.builder.ins().floor(times);
                                            let n = env.builder.ins().fmul(floored, right_val);
                                            env.builder.ins().fsub(left_val, n)
                                        }
                                        */
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack.push((left_type.clone(), res.into()));
                                }
                                
                                ">" | "<" | ">=" | "<=" | "==" | "!=" =>
                                {
                                    let op = match op.as_str()
                                    {
                                        "==" => inkwell::FloatPredicate::OEQ,
                                        "!=" => inkwell::FloatPredicate::ONE,
                                        "<"  => inkwell::FloatPredicate::OLT,
                                        "<=" => inkwell::FloatPredicate::OLE,
                                        ">"  => inkwell::FloatPredicate::OGT,
                                        ">=" => inkwell::FloatPredicate::OGE,
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    let res = env.builder.build_float_compare(op, left_val, right_val, "");
                                    let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "");
                                    env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                }
                                _ => panic!("operator {} not supported on type pair {}, {}", op, left_type.name, right_type.name)
                            }
                        }
                        ("i8", "i8") | ("i16", "i16") | ("i32", "i32") | ("i64", "i64") |
                        ("u8", "u8") | ("u16", "u16") | ("u32", "u32") | ("u64", "u64") =>
                        {
                            let left_val = left_val.into_int_value();
                            let right_val = right_val.into_int_value();
                            match op.as_str()
                            {
                                "&&" | "||" | "and" | "or" =>
                                {
                                    let zero = left_val.get_type().const_int(0, true);
                                    let left_bool  = env.builder.build_int_compare(inkwell::IntPredicate::NE, left_val , zero, "");
                                    let left_bool  = env.builder.build_int_cast_sign_flag(left_bool , u8_type, false, "");
                                    let right_bool = env.builder.build_int_compare(inkwell::IntPredicate::NE, right_val, zero, "");
                                    let right_bool = env.builder.build_int_cast_sign_flag(right_bool, u8_type, false, "");
                                    
                                    let res = match op.as_str()
                                    {
                                        "||" | "or"  => env.builder.build_or (left_bool, right_bool, ""),
                                        "&&" | "and" => env.builder.build_and(left_bool, right_bool, ""),
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack.push((u8_type_frontend.clone(), res.into()));
                                }
                                "|" | "&" | "^" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "|" => env.builder.build_or (left_val, right_val, ""),
                                        "&" => env.builder.build_and(left_val, right_val, ""),
                                        "^" => env.builder.build_xor(left_val, right_val, ""),
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack.push((left_type.clone(), res.into()));
                                }
                                "+" | "-" | "*" | "/" | "%" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "+" => env.builder.build_int_add(left_val, right_val, ""),
                                        "-" => env.builder.build_int_sub(left_val, right_val, ""),
                                        "*" => env.builder.build_int_mul(left_val, right_val, ""),
                                        "/" => if is_u
                                        {
                                            env.builder.build_int_unsigned_div(left_val, right_val, "")
                                        }
                                        else
                                        {
                                            env.builder.build_int_signed_div(left_val, right_val, "")
                                        },
                                        /*
                                        // TODO reimplement
                                        "%" =>
                                        {
                                            let times = if is_u
                                            {
                                                env.builder.ins().udiv(left_val, right_val)
                                            }
                                            else
                                            {
                                                env.builder.ins().sdiv(left_val, right_val)
                                            };
                                            let n = env.builder.ins().imul(times, right_val);
                                            env.builder.ins().isub(left_val, n)
                                        }
                                        */
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack.push((left_type.clone(), res.into()));
                                }
                                
                                ">" | "<" | ">=" | "<=" | "==" | "!=" =>
                                {
                                    let op = match op.as_str()
                                    {
                                        "==" => inkwell::IntPredicate::EQ,
                                        "!=" => inkwell::IntPredicate::NE,
                                        "<"  => if left_type.is_int_signed() { inkwell::IntPredicate::SLT } else  { inkwell::IntPredicate::ULT },
                                        "<=" => if left_type.is_int_signed() { inkwell::IntPredicate::SLE } else  { inkwell::IntPredicate::ULE },
                                        ">"  => if left_type.is_int_signed() { inkwell::IntPredicate::SGT } else  { inkwell::IntPredicate::UGT },
                                        ">=" => if left_type.is_int_signed() { inkwell::IntPredicate::SGE } else  { inkwell::IntPredicate::UGE },
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    let res = env.builder.build_int_compare(op, left_val, right_val, "");
                                    let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "");
                                    env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                }
                                
                                _ => panic!("operator {} not supported on type {}", op, left_type.name)
                            }
                        }
                        _ => panic!("unhandled type pair `{}`, `{}`", left_type.name, right_type.name)
                    }
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
        panic!("unhandled variable access");
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

const VERBOSE : bool = false;

fn main()
{
    println!("startup...");
    
    let start = std::time::Instant::now();
    let mut context = inkwell::context::Context::create();
    
    let type_table = [
        ("void".to_string(), Type { name : "void".to_string(), data : TypeData::Void }, context.void_type().into()),
        
        ("u8" .to_string(), Type { name : "u8" .to_string(), data : TypeData::Primitive }, context. i8_type().into()),
        ("u16".to_string(), Type { name : "u16".to_string(), data : TypeData::Primitive }, context.i16_type().into()),
        ("u32".to_string(), Type { name : "u32".to_string(), data : TypeData::Primitive }, context.i32_type().into()),
        ("u64".to_string(), Type { name : "u64".to_string(), data : TypeData::Primitive }, context.i64_type().into()),
        
        ("i8" .to_string(), Type { name : "i8" .to_string(), data : TypeData::Primitive }, context. i8_type().into()),
        ("i16".to_string(), Type { name : "i16".to_string(), data : TypeData::Primitive }, context.i16_type().into()),
        ("i32".to_string(), Type { name : "i32".to_string(), data : TypeData::Primitive }, context.i32_type().into()),
        ("i64".to_string(), Type { name : "i64".to_string(), data : TypeData::Primitive }, context.i64_type().into()),
        
        ("f32".to_string(), Type { name : "f32".to_string(), data : TypeData::Primitive }, context.f32_type().into()),
        ("f64".to_string(), Type { name : "f64".to_string(), data : TypeData::Primitive }, context.f64_type().into()),
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
    let tokens = parser.tokenize(&program_lines, true).unwrap();
    let ast = parser.parse_program(&tokens, &program_lines, true).unwrap().unwrap();
    
    let program = Program::new(&mut types, &ast).unwrap();
    
    let mut imports : BTreeMap<String, (*const u8, FunctionSig)> = BTreeMap::new();
    fn import_function<T>(types: &BTreeMap<String, Type>, parser : &mut parser::Parser, imports : &mut BTreeMap<String, (*const u8, FunctionSig)>, name : &str, pointer : T, pointer_usize : usize, type_string : &str)
    {
        let type_lines = vec!(type_string.to_string());
        let type_tokens = parser.tokenize(&type_lines, true).unwrap();
        let type_ast = parser.parse_with_root_node_type(&type_tokens, &type_lines, true, "type").unwrap().unwrap();
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
    
    unsafe extern "C" fn print_bytes(bytes : *mut u8, count : u64) -> ()
    {
        unsafe
        {
            for i in 0..count
            {
                print!("{:02X} ", *bytes.offset(i as isize));
            }
            print!("\n");
        }
    }
    import_function::<unsafe extern "C" fn(*mut u8, u64) -> ()>(&types, &mut parser, &mut imports, "print_bytes", print_bytes, print_bytes as usize, "funcptr(void, (ptr(u8), u64))");
    
    unsafe extern "C" fn print_float(a : f64) -> ()
    {
        println!("{}", a);
    }
    import_function::<unsafe extern "C" fn(f64) -> ()>(&types, &mut parser, &mut imports, "print_float", print_float, print_float as usize, "funcptr(void, (f64))");
    
    println!("startup done! time: {}", start.elapsed().as_secs_f64());
    
    let start = std::time::Instant::now();
    println!("compiling...");
    
    let context = Context::create();
    
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
    
    let intrinsic_imports = [
        ("sqrt", "funcptr(f64, (f64))"),
    ];
    
    for (name, type_name) in intrinsic_imports
    {
        let type_lines = vec!(type_name.to_string());
        let tokens = parser.tokenize(&type_lines, true).unwrap();
        let type_ast = parser.parse_with_root_node_type(&tokens, &type_lines, true, "type").unwrap().unwrap();
        let type_ = parse_type(&types, &type_ast).unwrap();
        if let TypeData::FuncPointer(funcsig) = type_.data
        {
            let sqrt_intrinsic = inkwell::intrinsics::Intrinsic::find(&format!("llvm.{}", name)).unwrap();
            
            let mut types = Vec::new();
            for arg_type in &funcsig.args
            {
                types.push(get_backend_type_sized(&mut backend_types, &arg_type));
            }
            //let f64_type = &types.get("f64").unwrap();
            let sqrt_function = sqrt_intrinsic.get_declaration(&module, &types).unwrap();
            
            func_decs.insert(name.to_string(), (sqrt_function, *funcsig.clone()));
        }
    }
    
    if VERBOSE
    {
        println!("module:\n{}", module.to_string());
        println!("adding global mappings...");
    }
    
    let opt_level = OptimizationLevel::Aggressive;
    //let opt_level = OptimizationLevel::None;
    
    let start = std::time::Instant::now();
    let mut executor = module.create_jit_execution_engine(opt_level).unwrap();
    let target_data = executor.get_target_data();
    for (f_name, (pointer, funcsig)) in &imports
    {
        executor.add_global_mapping(&func_decs.get(f_name).unwrap().0, *pointer as usize);
    }
    let ptr_int_type = context.ptr_sized_int_type(&target_data, None);
    println!("executor build time: {}", start.elapsed().as_secs_f64());
    
    
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
                let slot = if let TypeData::Array(inner_type, size) = &var_type.data
                {
                    let size = get_backend_type_sized(&mut backend_types, types.get("u64").unwrap()).into_int_type().const_int(*size as u64, false);
                    let inner_basic_type = get_backend_type_sized(&mut backend_types, &inner_type);
                    builder.build_array_alloca(inner_basic_type, size, &var_name)
                }
                else
                {
                    builder.build_alloca(basic_type, &var_name)
                };
                
                let val = func_val.get_nth_param(j as u32).unwrap();
                builder.build_store(slot, val);
                
                if variables.contains_key(&var_name)
                {
                    panic!("error: parameter {} redeclared", var_name);
                }
                variables.insert(var_name, (var_type, slot));
            }
            else
            {
                panic!("error: variables of type {} are not allowed", var_type.name);
            }
            
            i += 1;
        }
        
        // TODO don't declare variables ahead of time, declare them on the go, and support variable scoping
        // declare variables
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && (node.text == "declaration" || node.text == "fulldeclaration")
            {
                let var_type = parse_type(&types, &node.child(0).unwrap()).unwrap();
                let var_name = node.child(1).unwrap().child(0).unwrap().text.clone();
                
                let basic_type = get_backend_type_sized(&mut backend_types, &var_type);
                
                let slot = if let TypeData::Array(inner_type, size) = &var_type.data
                {
                    let size = get_backend_type_sized(&mut backend_types, types.get("u64").unwrap()).into_int_type().const_int(*size as u64, false);
                    let inner_basic_type = get_backend_type_sized(&mut backend_types, &inner_type);
                    builder.build_array_alloca(inner_basic_type, size, &var_name)
                }
                else
                {
                    builder.build_alloca(basic_type, &var_name)
                };
                
                if variables.contains_key(&var_name)// || func_decs.contains_key(&var_name)
                {
                    panic!("error: variable {} redeclared", var_name);
                }
                variables.insert(var_name, (var_type, slot));
                
                i += 1;
            }
            false
        });
        
        // collect labels (blocks)
        let mut blocks = HashMap::new();
        let mut blocks_vec = Vec::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && node.text == "label"
            {
                let name = &node.child(0).unwrap().child(0).unwrap().text;
                if !blocks.contains_key(name)
                {
                    let block = context.append_basic_block(func_val, name);
                    blocks.insert(name.clone(), block);
                    blocks_vec.push(block);
                }
                else
                {
                    panic!("error: redeclared block {}", name);
                }
            }
            false
        });
        
        let stack = Vec::new();
        let mut env = Environment { context : &context, stack, variables, builder : &builder, module : &module, func_decs : &func_decs, types : &types, backend_types : &mut backend_types, func_val, blocks, ptr_int_type, target_data };
        
        //println!("\n\ncompiling function {}...", function.name);
        //println!("{}", function.body.pretty_debug());
        compile(&mut env, &function.body, WantPointer::None);
    }
    
    println!("compilation time: {}", start.elapsed().as_secs_f64());
    
    //println!("features: {}", inkwell::targets::TargetMachine::get_host_cpu_features());
    
    let start = std::time::Instant::now();
    
    // set up IR-level optimization pass manager
    let pass_manager = {
        let config = inkwell::targets::InitializationConfig::default();
        inkwell::targets::Target::initialize_native(&config).unwrap();
        
        let builder = inkwell::passes::PassManagerBuilder::create();
        builder.set_optimization_level(opt_level);
        builder.set_inliner_with_threshold(1);
        builder.set_size_level(2);
        
        let pass_manager = inkwell::passes::PassManager::create(());
        builder.populate_module_pass_manager(&pass_manager);
        
        pass_manager
    };
    println!("pass manager build time: {}", start.elapsed().as_secs_f64());
    
    
    let start = std::time::Instant::now();
    if VERBOSE
    {
        println!("doing IR optimizations...");
    }
    pass_manager.run_on(&module);
    
    println!("done doing IR optimizations. time: {}", start.elapsed().as_secs_f64());
    if VERBOSE
    {
        println!("module after doing IR optimizations:\n{}", module.to_string());
    }
    
    macro_rules! get_func
    {
        ($name:expr, $T:ty) =>
        {
            {
                let dec = func_decs.get(&$name.to_string()).unwrap();
                let type_string = dec.1.to_string_rusttype();
                
                let want_type_string = std::any::type_name::<$T>(); // FIXME: not guaranteed to be stable across rust versions
                assert!(want_type_string == type_string, "types do not match:\n{}\n{}\n", want_type_string, type_string);
                assert!(want_type_string.starts_with("unsafe "), "function pointer type must be unsafe");
                
                executor.get_function::<$T>(&$name).unwrap()
            }
        }
    }
    
    unsafe
    {
        let name = "nbody_bench";
        let f = get_func!(name, unsafe extern "C" fn() -> ());
        
        let start = std::time::Instant::now();
        println!("running {}...", name);
        let out = f.call();
        let elapsed_time = start.elapsed();
        
        println!("{}() = {:?}", name, out);
        println!("time: {}", elapsed_time.as_secs_f64());
    }
}
