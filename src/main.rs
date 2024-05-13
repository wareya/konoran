#![allow(clippy::let_unit_value)] // seim-generic debugging code
#![allow(clippy::len_zero)] // subjective readability
#![allow(clippy::comparison_to_empty)] // subjective readability
#![allow(clippy::single_char_pattern)] // subjective readability
#![allow(clippy::unnecessary_unwrap)] // block ordering
#![allow(clippy::if_same_then_else)] // block ordering readability
#![allow(clippy::collapsible_else_if)] // block ordering readability
#![allow(clippy::match_like_matches_macro)] // refactoring ease
#![allow(clippy::suspicious_else_formatting)] // false positive around commented-out code

extern crate alloc;

use alloc::rc::Rc;
use core::cell::RefCell;

use alloc::collections::BTreeMap;
use std::collections::HashMap;

use inkwell::passes::PassBuilderOptions;
use inkwell::types::*;
use inkwell::values::{BasicValue, BasicValueEnum, AsValueRef};
use inkwell::targets::{Target, TargetMachine, TargetTriple, TargetData, CodeModel, RelocMode};

use parser::ast::ASTNode;

/*

TODO list:

high:
- bit shift operators
- ternary operator

low:
- standard io functions
- decay_to_ptr $expr$ for arrays (what C does automatically, but manually) (currently done by manually casting)
- float operation intrisics (sin/cos/tan, floor/ceil/round, exp/log/pow)

maybe:
- varargs in declarations (not definitions) (for printf mainly)
- implement other control flow constructs than just "if -> goto"
- have proper scoped variable declarations, not function-level variable declarations


FIXMEs:

- sizeof might not compile properly on non-64-bit because idk if it needs to be manually upcasted to u64 or not
- suboptimal codegen with some temporary allocas due to lack of hoisting into entry block 

*/

mod parser;

#[derive(Debug, Clone, PartialEq, Eq)]
enum TypeData
{
    IncompleteStruct,
    Void,
    Primitive,
    Struct(Vec<(String, Type)>), // property name, property type
    Pointer(Rc<RefCell<Type>>, bool), // bool is whether the pointer is volatile
    VirtualPointer(Box<Type>, bool), // bool is whether the pointer is volatile
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
            TypeData::Pointer(inner, _) => format!("ptr({})", inner.borrow().to_string()),
            TypeData::VirtualPointer(inner, _) => format!("vptr({})", inner.to_string()),
            TypeData::Array(inner, size) => format!("array({}, {})", inner.to_string(), size),
            TypeData::Struct(_) => self.name.clone(),
            TypeData::IncompleteStruct => self.name.clone(),
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
            TypeData::Pointer(inner, _) => format!("*mut {}", inner.borrow().to_string_rusttype(true)),
            TypeData::VirtualPointer(_, _) => "<unrepresented>".to_string(),
            TypeData::Array(_, _) => "<unrepresented>".to_string(),
            TypeData::Struct(_) => "<unrepresented>".to_string(),
            TypeData::IncompleteStruct => "<unrepresented>".to_string(),
            TypeData::FuncPointer(sig) => sig.to_string_rusttype(),
        }
    }
    fn to_array(&self, count : usize) -> Type
    {
        Type { name : "array".to_string(), data : TypeData::Array(Box::new(self.clone()), count) }
    }
    fn to_ptr(&self, volatile : bool) -> Type
    {
        Type { name : "ptr".to_string(), data : TypeData::Pointer(Rc::new(RefCell::new(self.clone())), volatile) }
    }
    fn ptr_to_vptr(&self) -> Result<Type, String>
    {
        match &self.data
        {
            TypeData::Pointer(inner, is_volatile) => Ok(Type { name : "vptr".to_string(), data : TypeData::VirtualPointer(Box::new(inner.borrow().clone()), *is_volatile) }),
            _ => Err("internal error: tried to convert non-ptr to vptr".to_string())
        }
    }
    fn to_vptr(&self, volatile : bool) -> Type
    {
        Type { name : "vptr".to_string(), data : TypeData::VirtualPointer(Box::new(self.clone()), volatile) }
    }
    fn deref_ptr(&self) -> Result<(Type, bool), String>
    {
        match &self.data
        {
            TypeData::Pointer(inner, volatile) => Ok((inner.borrow().clone(), *volatile)),
            _ => Err(format!("error: attempted to dereference non-pointer type `{}`", self.to_string())),
        }
    }
    fn deref_vptr(&self) -> Result<(Type, bool), String>
    {
        match &self.data
        {
            TypeData::VirtualPointer(inner, volatile) => Ok((*inner.clone(), *volatile)),
            _ => Err(format!("error: attempted to virtually dereference non-virtual-pointer type `{}`", self.to_string())),
        }
    }
    fn array_to_inner(&self) -> Result<Type, String>
    {
        match &self.data
        {
            TypeData::Array(inner, _size) => Ok(*inner.clone()),
            _ => Err(format!("error: attempted to use indexing on non-array type `{}`", self.to_string())),
        }
    }
    fn struct_to_info(&self) -> Result<Vec<(String, Type)>, String>
    {
        match &self.data
        {
            TypeData::Struct(info) => Ok(info.clone()),
            _ => Err(format!("internal error: attempted to access struct info of non-struct type `{}`", self.to_string())),
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
        matches!(self.data, TypeData::Pointer(_, _))
    }
    fn is_pointer_or_fpointer(&self) -> bool
    {
        match &self.data
        {
            TypeData::Pointer(_, _) => true,
            TypeData::FuncPointer(_) => true,
            _ => false,
        }
    }
    fn is_virtual_pointer(&self) -> bool
    {
        matches!(self.data, TypeData::VirtualPointer(_, _))
    }
    fn is_float(&self) -> bool
    {
        self.name == "f32" || self.name == "f64"
    }
    fn is_int(&self) -> bool
    {
        ["u8", "u16", "u32", "u64", "i8", "i16", "i32", "i64"].contains(&self.name.as_str())
    }
    fn size(&self) -> u8
    {
        match self.name.as_str()
        {
            "u8"  | "i8"  => 1,
            "u16" | "i16" => 2,
            "u32" | "i32" => 4,
            "u64" | "i64" => 8,
            _ => panic!("internal error: tried to get the size of a non-primitive-int type")
        }
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
                if let TypeData::Struct(_) = &named_type.data
                {
                    return Ok(named_type.clone());
                }
                else if let TypeData::IncompleteStruct = &named_type.data
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
                if count == 0 || type_.name == "void"
                {
                    return Err("error: zero-size arrays are not allowed".to_string());
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
            let mut res = parse_type(types, node.child(0).unwrap());
            if let Ok(res) = res.as_mut()
            {
                if matches!(res.data, TypeData::Struct(_))
                {
                    res.data = TypeData::IncompleteStruct;
                }
            }
            res.map(|x| x.to_ptr(false))
        }
        // FIXME // why was this fixme here?
        (_, name) => Err(format!("error: unsupported type (culprit: `{}`)", name)),
    }
}

#[derive(Debug, Clone)]
struct Function
{
#[allow(dead_code)]
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

#[allow(dead_code)]
fn store_size_of_type(target_data : &TargetData, backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum>, types : &BTreeMap<String, Type>, type_ : &Type) -> u64
{
    if type_.is_void()
    {
        return 0;
    }
    let backend_type = get_backend_type(backend_types, types, type_);
    target_data.get_store_size(&backend_type)
}
#[allow(dead_code)]
fn alloc_size_of_type(target_data : &TargetData, backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum>, types : &BTreeMap<String, Type>, type_ : &Type) -> u64
{
    if type_.is_void()
    {
        return 0;
    }
    let backend_type = get_backend_type(backend_types, types, type_);
    target_data.get_abi_size(&backend_type)
}
fn get_any_type_context(sdkawuidsguisagugarewudsga : inkwell::types::BasicTypeEnum) -> inkwell::context::ContextRef
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
fn get_any_type_poison(sdkawuidsguisagugarewudsga : inkwell::types::BasicTypeEnum) -> inkwell::values::BasicValueEnum
{
    match sdkawuidsguisagugarewudsga
    {
        inkwell::types::BasicTypeEnum::ArrayType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        inkwell::types::BasicTypeEnum::FloatType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        inkwell::types::BasicTypeEnum::IntType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        inkwell::types::BasicTypeEnum::PointerType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        inkwell::types::BasicTypeEnum::StructType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        inkwell::types::BasicTypeEnum::VectorType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
    }
}
/*
fn get_any_type_align<'c>(sdkawuidsguisagugarewudsga : inkwell::types::BasicTypeEnum<'c>) -> inkwell::values::IntValue<'c>
{
    match sdkawuidsguisagugarewudsga
    {
        inkwell::types::BasicTypeEnum::ArrayType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
        inkwell::types::BasicTypeEnum::FloatType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
        inkwell::types::BasicTypeEnum::IntType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
        inkwell::types::BasicTypeEnum::PointerType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
        inkwell::types::BasicTypeEnum::StructType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
        inkwell::types::BasicTypeEnum::VectorType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_alignment(),
    }
}
*/
fn get_backend_type<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, types : &BTreeMap<String, Type>, type_ : &Type) -> inkwell::types::AnyTypeEnum<'c>
{
    let key = type_.to_string();
    
    if let Some(backend_type) = backend_types.get(&key)
    {
        *backend_type
    }
    else
    {
        let context = get_any_type_context(inkwell::types::BasicTypeEnum::try_from(*backend_types.values().next().unwrap()).unwrap());
        
        match &type_.data
        {
            TypeData::Void => panic!("internal error: tried to recreate void type"),
            TypeData::Primitive => panic!("internal error: tried to recreate primitive type"),
            TypeData::Pointer(_, _) =>
            {
                let ptr_type = context.ptr_type(inkwell::AddressSpace::default()).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::VirtualPointer(_, _) =>
            {
                let ptr_type = context.ptr_type(inkwell::AddressSpace::default()).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::Array(inner, size) =>
            {
                let ptr_type = get_backend_type_sized(backend_types, types, inner).array_type(*size as u32).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::IncompleteStruct =>
            {
                let complete_type = types.get(&type_.name).unwrap_or_else(|| panic!("internal error: tried to use incomplete struct type"));
                if let TypeData::Struct(_) = &complete_type.data
                {
                    get_backend_type(backend_types, types, complete_type)
                }
                else
                {
                    panic!("internal error: struct type broken");
                }
            }
            TypeData::Struct(struct_data) =>
            {
                let mut prop_types = Vec::new();
                for (_, type_) in struct_data
                {
                    prop_types.push(get_backend_type_sized(backend_types, types, type_));
                }
                if prop_types.is_empty()
                {
                    panic!("internal error: structs cannot be empty");
                }
                let ptr_type = context.struct_type(&prop_types, true).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
            TypeData::FuncPointer(_sig) =>
            {
                let ptr_type = context.ptr_type(inkwell::AddressSpace::default()).into();
                backend_types.insert(key, ptr_type);
                ptr_type
            }
        }
    }
}

fn get_backend_type_sized<'c>(backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, types : &BTreeMap<String, Type>, type_ : &Type) -> inkwell::types::BasicTypeEnum<'c>
{
    let backend_type = get_backend_type(backend_types, types, type_);
    if let Ok(basic_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
    {
        basic_type
    }
    else
    {
        panic!("error: tried to use a non-sized type in a context where only sized types are allowed");
    }
}

fn get_function_type<'c>(function_types : &mut BTreeMap<String, inkwell::types::FunctionType<'c>>, backend_types : &mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>, types : &BTreeMap<String, Type>, sig : &FunctionSig, options : &HashMap<&'static str, bool>) -> inkwell::types::FunctionType<'c>
{
    let sig_type = Type::from_functionsig(sig);
    let key = sig_type.to_string();
    if let Some(backend_type) = function_types.get(&key)
    {
        return *backend_type;
    }
    
    let mut params = Vec::new();
    
    for var_type in &sig.args
    {
        let mut var_type = var_type.clone();
        if var_type.is_void()
        {
            panic!("error: void function arguments are not allowed");
        }
        if var_type.is_composite() && !options.contains_key("bypass_agg_memcpy")
        {
            var_type = var_type.to_ptr(false);
        }
        let backend_type = get_backend_type(backend_types, types, &var_type);
        if let Ok(backend_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
        {
            params.push(backend_type.into());
        }
        else
        {
            panic!("error: non-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", var_type.name, var_type.name);
        }
    }
    
    
    let mut _return_type = sig.return_type.clone();
    if _return_type.is_composite() && !options.contains_key("bypass_agg_memcpy")
    {
        let backend_type = get_backend_type_sized(backend_types, types, &_return_type.to_ptr(false));
        params.push(backend_type.into());
        _return_type = Type { name : "void".to_string(), data : TypeData::Void };
    }
    let return_type = get_backend_type(backend_types, types, &_return_type);
    
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
        panic!("error: can't build functions that return type {} ({:?})", sig.return_type.name, return_type)
    };
    
    // FIXME // why was this fixme here?
    function_types.insert(key, func_type);
    
    func_type
}

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Visibility
{
    Private,
    Local,
    Export,
    ImportLocal,
    Import,
}

#[derive(Debug, Clone)]
struct Program
{
    funcs : BTreeMap<String, (Function, Visibility)>,
    func_imports : BTreeMap<String, (FunctionSig, Visibility)>,
    globals : BTreeMap<String, (Type, Option<ASTNode>, Visibility)>,
    constants : BTreeMap<String, (Type, ASTNode)>,
    globals_order : Vec<String>,
}

fn struct_check_recursive(types : &BTreeMap<String, Type>, root_type : &Type, type_ : &Type)
{
    match &type_.data
    {
        TypeData::Struct(struct_data) =>
            for inner_type in struct_data
            {
                if inner_type.1.name == root_type.name
                {
                    panic!("struct {} is directly recursive; directly recursive structs are forbidden", root_type.name);
                }
                struct_check_recursive(types, root_type, &inner_type.1);
            }
        TypeData::IncompleteStruct =>
        {
            let complete_type = types.get(&type_.name).unwrap_or_else(|| panic!("error: tried to use incomplete struct type"));
            if complete_type.name == root_type.name
            {
                panic!("struct {} is directly recursive; directly recursive structs are forbidden", root_type.name);
            }
            if let TypeData::Struct(_) = &complete_type.data
            {
                struct_check_recursive(types, root_type, &complete_type);
            }
            else
            {
                panic!("internal error: struct type broken");
            }
        }
        _ => {}
    }
}

impl Program
{
    fn new(types : &mut BTreeMap<String, Type>, ast : &ASTNode) -> Result<Program, String>
    {
        let mut funcs = BTreeMap::new();
        let mut func_imports = BTreeMap::new();
        let mut globals = BTreeMap::new();
        let mut constants = BTreeMap::new();
        let mut globals_order = Vec::new();
        
        //println!("starting struct defs...");
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "structdef"
            {
                let name = child.child(0)?.child(0)?.text.clone();
                types.insert(name.clone(), Type{name, data : TypeData::IncompleteStruct});
            }
        }
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "structdef"
            {
                let name = child.child(0)?.child(0)?.text.clone();
                
                let mut struct_data = Vec::new();
                for prop in child.child(1)?.get_children()?
                {
                    let prop_type = parse_type(types, prop.child(0)?).unwrap();
                    let prop_name = prop.child(1)?.child(0)?.text.clone();
                    if prop_type.name == "void"
                    {
                        panic!("error: void struct properties are not allowed");
                    }
                    struct_data.push((prop_name, prop_type.clone()));
                }
                
                let struct_type = Type { name : name.clone(), data : TypeData::Struct(struct_data) };
                
                struct_check_recursive(types, &struct_type, &struct_type);
                
                types.insert(name, struct_type);
            }
        }
        
        //println!("starting func defs...");
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "importfunc"
            {
                //println!("import func");
                let visibility = if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.text.clone() == "import_extern"
                {
                    Visibility::Import
                }
                else // "using"
                {
                    Visibility::ImportLocal
                };
                
                let return_type = parse_type(types, child.child(1)?).unwrap();
                let name = child.child(2)?.child(0)?.text.clone();
                
                let mut args = Vec::new();
                for arg in child.child(3)?.get_children()?
                {
                    let arg_type = parse_type(types, arg.child(0)?).unwrap();
                    args.push(arg_type);
                }
                
                let funcsig = FunctionSig { return_type, args };
                
                func_imports.insert(name.clone(), (funcsig, visibility));
            }
            if child.is_parent() && child.text == "funcdef"
            {
                //println!("func def");
                let visibility = if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.text.clone() == "export_extern"
                {
                    Visibility::Export
                }
                else if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.text.clone() == "private"
                {
                    Visibility::Private
                }
                else // default
                {
                    Visibility::Local
                };
                
                let return_type = parse_type(types, child.child(1)?).unwrap();
                let name = child.child(2)?.child(0)?.text.clone();
                
                let mut args = Vec::new();
                for arg in child.child(3)?.get_children()?
                {
                    let arg_type = parse_type(types, arg.child(0)?).unwrap();
                    let arg_name = arg.child(1)?.child(0)?.text.clone();
                    
                    args.push((arg_type, arg_name));
                }
                
                let body = child.child(4)?.clone();
                
                funcs.insert(name.clone(), (Function { name, return_type, args, body }, visibility));
            }
        }
        
        //println!("starting global defs...");
        for child in ast.get_children()?
        {
            if child.is_parent() && child.text == "importglobal"
            {
                let visibility = if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.text.clone() == "import_extern"
                {
                    Visibility::Import
                }
                else // "using"
                {
                    Visibility::ImportLocal
                };
                
                let type_ = parse_type(types, child.child(1)?).unwrap();
                let name = child.child(2)?.child(0)?.text.clone();
                globals.insert(name.clone(), (type_, None, visibility));
                globals_order.push(name.clone());
            }
            else if child.is_parent() && (child.text == "globaldeclaration" || child.text == "globalfulldeclaration")
            {
                let visibility = if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.child(0)?.text.clone() == "export_extern"
                {
                    Visibility::Export
                }
                else if child.child(0)?.child_count().unwrap() != 0 && child.child(0)?.child(0)?.child(0)?.text.clone() == "private"
                {
                    Visibility::Private
                }
                else // default
                {
                    Visibility::Local
                };
                
                let type_ = parse_type(types, child.child(1)?).unwrap();
                let name = child.child(2)?.child(0)?.text.clone();
                
                if child.text == "globaldeclaration"
                {
                    globals.insert(name.clone(), (type_, None, visibility));
                    globals_order.push(name.clone());
                }
                else
                {
                    let initializer = child.child(3)?.clone();
                    globals.insert(name.clone(), (type_, Some(initializer), visibility));
                    globals_order.push(name.clone());
                }
            }
            else if child.is_parent() && child.text == "constexpr_globalfulldeclaration"
            {
                let type_ = parse_type(types, child.child(0)?).unwrap();
                let name = child.child(1)?.child(0)?.text.clone();
                
                let initializer = child.child(2)?.clone();
                constants.insert(name.clone(), (type_, initializer));
                globals_order.push(name.clone());
            }
        }
        Ok(Program { funcs, func_imports, globals, constants, globals_order })
    }
}

struct Environment<'a, 'b, 'c, 'e>
{
    source_text    : &'e Vec<String>,
    context        : &'c inkwell::context::Context,
    module         : &'a inkwell::module::Module<'c>,
    stack          : Vec<(Type, inkwell::values::BasicValueEnum<'c>)>,
    variables      : BTreeMap<String, (Type, inkwell::values::PointerValue<'c>)>,
    constants      : BTreeMap<String, (Type, inkwell::values::BasicValueEnum<'c>)>,
    builder        : &'b inkwell::builder::Builder<'c>,
    func_decs      : &'a BTreeMap<String, (inkwell::values::FunctionValue<'c>, FunctionSig)>,
    global_decs    : &'a BTreeMap<String, (Type, inkwell::values::GlobalValue<'c>, Option<inkwell::values::FunctionValue<'c>>)>,
    intrinsic_decs : &'a BTreeMap<String, (inkwell::values::FunctionValue<'c>, FunctionSig)>,
    types          : &'a BTreeMap<String, Type>,
    backend_types  : &'a mut BTreeMap<String, inkwell::types::AnyTypeEnum<'c>>,
    function_types : &'a mut BTreeMap<String, inkwell::types::FunctionType<'c>>,
    func_val       : inkwell::values::FunctionValue<'c>,
    blocks         : HashMap<String, inkwell::basic_block::BasicBlock<'c>>,
    entry_block    : inkwell::basic_block::BasicBlock<'c>,
    ptr_int_type   : inkwell::types::IntType<'c>,
    target_data    : &'a TargetData,
    
    anon_globals   : HashMap<inkwell::values::PointerValue<'c>, inkwell::values::BasicValueEnum<'c>>,
    
    return_type    : Option<Type>,
    hoisted_return : Option<(Type, inkwell::values::PointerValue<'c>)>,
    just_returned  : bool,
    
    options        : &'e HashMap<&'static str, bool>,
}

#[derive(Clone, Debug, Copy, PartialEq)]
enum WantPointer {
    None,
    Real,
    Virtual,
}
fn check_struct_incomplete(env : &mut Environment, type_ : &mut Type)
{
    match &mut type_.data
    {
        TypeData::IncompleteStruct =>
        {
            let new_type = env.types.get(&type_.name).unwrap_or_else(|| panic!("tried to use incomplete struct type {}", type_.name));
            type_.data = new_type.data.clone();
        }
        TypeData::VirtualPointer(ref mut inner_type, _) => check_struct_incomplete(env, inner_type),
        TypeData::Pointer(inner_type, _) => check_struct_incomplete(env, &mut inner_type.borrow_mut()),
        _ => {}
    }
    
}
fn val_is_const(mut is_const : bool, val : inkwell::values::BasicValueEnum) -> bool
{
    match val
    {
        BasicValueEnum::ArrayValue(v)   => is_const = is_const && v.is_const(),
        BasicValueEnum::IntValue(v)     => is_const = is_const && v.is_const(),
        BasicValueEnum::FloatValue(v)   => is_const = is_const && v.is_const(),
        BasicValueEnum::PointerValue(v) => is_const = is_const && v.is_const(),
        BasicValueEnum::StructValue(v)  => is_const = is_const && { unsafe { llvm_sys::core::LLVMIsConstant(v.as_value_ref()) == 1 }},
        _ => is_const = false,
    }
    is_const
}
fn basic_const_array<'ctx>(type_ : inkwell::types::BasicTypeEnum<'ctx>, vals : &[inkwell::values::BasicValueEnum<'ctx>]) -> inkwell::values::ArrayValue<'ctx>
{
    match type_
    {
        BasicTypeEnum::ArrayType(v)   => v.const_array(&(vals.iter().map(|x| (*x).try_into().unwrap()).collect::<Vec<_>>())),
        BasicTypeEnum::IntType(v)     => v.const_array(&(vals.iter().map(|x| (*x).try_into().unwrap()).collect::<Vec<_>>())),
        BasicTypeEnum::FloatType(v)   => v.const_array(&(vals.iter().map(|x| (*x).try_into().unwrap()).collect::<Vec<_>>())),
        BasicTypeEnum::PointerType(v) => v.const_array(&(vals.iter().map(|x| (*x).try_into().unwrap()).collect::<Vec<_>>())),
        BasicTypeEnum::StructType(v)  => v.const_array(&(vals.iter().map(|x| (*x).try_into().unwrap()).collect::<Vec<_>>())),
        _ => panic!("internal error: unsupported type for array"),
    }
}

macro_rules! build_memcpy
{
    ($builder:expr, $module:expr, $dst:expr, $src:expr, $len:expr, $volatile:expr) =>
    {{
        let intrinsic = inkwell::intrinsics::Intrinsic::find("llvm.memcpy.p0.i64").unwrap();
        let u64_type = $module.get_context().i64_type();
        let i1_type = $module.get_context().bool_type();
        let zero_bool = i1_type.const_int(0, true);
        let one_bool = i1_type.const_int(1, true);
        
        let vol_bool = if $volatile { one_bool } else { zero_bool }.into();
        let dst_type : PointerType = $dst.get_type().try_into().unwrap();
        let src_type : PointerType = $src.get_type().try_into().unwrap();
        if dst_type.get_address_space() != src_type.get_address_space()
        {
            let function = intrinsic.get_declaration(&$module, &[dst_type.into(), dst_type.into(), u64_type.into()]).unwrap();
            let src = $builder.build_address_space_cast($src.try_into().unwrap(), dst_type, "").unwrap();
            $builder.build_direct_call(function, &[$dst.into(), src.into(), $len, vol_bool], "")
            //let function = intrinsic.get_declaration($module, &[src_type.into(), src_type.into(), u64_type.into()]).unwrap();
            //let dst = $builder.build_address_space_cast($dst.try_into().unwrap(), src_type, "").unwrap();
            //$builder.build_direct_call(function, &[dst.into(), $src.into(), $len, vol_bool], "")
        }
        else
        {
            let function = intrinsic.get_declaration(&$module, &[dst_type.into(), src_type.into(), u64_type.into()]).unwrap();
            $builder.build_direct_call(function, &[$dst.into(), $src.into(), $len, vol_bool], "")
        }
    }}
}
fn compile(env : &mut Environment, node : &ASTNode, want_pointer : WantPointer)
{
    // used to build constants for some lowerings, and to cast to bool
    let u8_type_frontend = env.types.get("u8").unwrap();
    let i1_type = env.backend_types.get("i1").unwrap().into_int_type();
    let u8_type = env.backend_types.get("u8").unwrap().into_int_type();
    let u64_type = env.backend_types.get("u64").unwrap().into_int_type();
    let ptr_type = env.context.ptr_type(inkwell::AddressSpace::default());
    
    let zero_bool = i1_type.const_int(0, true);
    let one_bool = i1_type.const_int(1, true);
    
    macro_rules! panic_error
    {
        ($($t:tt)*) =>
        {{
            eprintln!("\x1b[91mError:\x1b[0m {}", format!($($t)*));
            eprintln!("at line {}, column {}", node.line, node.position);
            
            let s = env.source_text[node.line-1].clone();
            let ix = s.char_indices().map(|(p, _)| p).collect::<Vec<_>>();
            let start = ix[node.position - 1];
            let end = (start + node.span).min(s.len());
            let a = &s[0..start];
            let b = &s[start..end];
            let c = &s[end..s.len()];
            
            eprintln!("{}\x1b[96m{}\x1b[0m{}", a, b, c);
            
            for _ in 0..node.position-1
            {
                eprint!("\x1b[93m-");
            }
            eprintln!("^\x1b[0m");
            panic!($($t)*);
        }};
    }
    macro_rules! unwrap_or_panic
    {
        ($($t:tt)*) =>
        {{
            let x = ($($t)*);
            let mut unwrappable = false;
            let _ = x.as_ref().inspect(|_| unwrappable = true);
            if unwrappable
            {
                x.unwrap()
            }
            else
            {
                panic_error!("{:?}", x);
            }
        }}
    }
    
    macro_rules! emit_alloca
    {
        ($type:expr, $name:expr) =>
        {{
            // get_insert_block store current block
            let block = env.builder.get_insert_block().unwrap();
            // position_at_end to entry block
            env.builder.position_at_end(env.entry_block);
            // insert alloca
            let slot = env.builder.build_alloca($type, $name).unwrap();
            // jump back
            env.builder.position_at_end(block);
            
            slot
        }}
    }
    
    macro_rules! push_val_or_ptr
    {
        ($type:expr, $addr:expr, $volatile:expr) =>
        {{
            let basic_type = get_backend_type_sized(env.backend_types, env.types, &$type);
            if want_pointer == WantPointer::Real
            {
                env.stack.push(($type.to_ptr($volatile), $addr.into()));
            }
            else if want_pointer == WantPointer::Virtual
            {
                env.stack.push(($type.to_vptr($volatile), $addr.into()));
            }
            else if $type.is_composite() && !env.options.contains_key("bypass_agg_memcpy")
            {
                let slot = emit_alloca!(basic_type, "__temp_synthetic_");
                let backend_type = get_backend_type_sized(env.backend_types, env.types, &$type);
                let len = backend_type.size_of().unwrap().into();
                build_memcpy!(env.builder, env.module, slot, $addr, len, $volatile).unwrap();
                env.stack.push(($type, slot.into()));
            }
            else
            {
                let val = env.builder.build_load(basic_type, $addr, "").unwrap();
                val.as_instruction_value().unwrap().set_volatile($volatile).unwrap();
                env.stack.push(($type.clone(), val));
            }
        }}
    }
    macro_rules! store_or_memcpy
    {
        ($type_var:expr, $slot:expr, $type_val:expr, $val:expr, $volatile:expr) =>
        {{
            if $type_val.is_composite() && !env.options.contains_key("bypass_agg_memcpy")
            {
                assert!($type_var == $type_val);
                let backend_type = get_backend_type_sized(env.backend_types, env.types, &$type_var);
                let len = backend_type.size_of().unwrap().into();
                
                if val_is_const(true, $val)
                {
                    if $val.is_pointer_value() // already assigned to global const, load straight form its pointer
                    {
                        build_memcpy!(env.builder, env.module, $slot, $val, len, $volatile).unwrap();
                    }
                    else
                    {
                        let global = env.module.add_global(backend_type, Some(inkwell::AddressSpace::default()), "");
                        global.set_constant(true);
                        global.set_initializer(&$val);
                        
                        global.set_linkage(inkwell::module::Linkage::Private);
                        
                        let val = global.as_pointer_value();
                        build_memcpy!(env.builder, env.module, $slot, val, len, $volatile).unwrap();
                    }
                }
                else
                {
                    build_memcpy!(env.builder, env.module, $slot, $val, len, $volatile).unwrap();
                }
            }
            // FIXME maybe always do this if the store is volatile...?
            else
            {
                let store = env.builder.build_store($slot, $val).unwrap();
                store.set_volatile($volatile).unwrap();
            }
        }}
    }
    
    macro_rules! make_anonymous_const_global
    {
        ($initializer:expr) =>
        {{
            let global = env.module.add_global($initializer.get_type(), Some(inkwell::AddressSpace::default()), "");
            global.set_constant(true);
            global.set_initializer(&$initializer);
            global.set_linkage(inkwell::module::Linkage::Private);
            env.anon_globals.insert(global.as_pointer_value(), $initializer.into());
            
            global
        }}
    }
    
    if node.is_parent()
    {
        if env.just_returned
        {
            // returns can only happen at the very end of a block
            // and the end of a block can only have one flow control mechanism
            // (so we can't explicitly jump to this new anonymous block we're making; it's dead code)
            env.builder.position_at_end(env.context.append_basic_block(env.func_val, ""));
            env.just_returned = false;
        }
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
                    panic_error!("error: functions must explicitly return, even if their return type is void\n(on line {})", node.line);
                }
            }
            "statementlist" | "statement" | "instruction" =>
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
            "parenexpr" | "arrayindex" =>
                for child in node.get_children().unwrap()
                {
                    compile(env, child, want_pointer);
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
                if let Some((type_val, val)) = returns.first()
                {
                    if env.return_type != Some(type_val.clone())
                    {
                        panic_error!("error: tried to return wrong type from function");
                    }
                    if let Some((return_type, return_slot)) = &env.hoisted_return
                    {
                        let return_backend_type = get_backend_type_sized(env.backend_types, env.types, return_type);
                        let len = return_backend_type.size_of().unwrap().into();
                        build_memcpy!(env.builder, env.module, *return_slot, *val, len, false).unwrap();
                        
                        env.builder.build_return(None).unwrap();
                    }
                    else
                    {
                        env.builder.build_return(Some(val)).unwrap();
                    }
                }
                else
                {
                    if env.return_type != env.types.get("void").cloned()
                    {
                        panic_error!("error: tried to return nothing from function that requires a return value");
                    }
                    env.builder.build_return(None).unwrap();
                }
                env.just_returned = true;
            }
            "declaration" => { }
            "fulldeclaration" =>
            {
                let name = &node.child(1).unwrap().child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (mut type_var, slot) = env.variables[name].clone();
                    check_struct_incomplete(env, &mut type_var);
                    
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let (type_val, val) = unwrap_or_panic!(env.stack.pop());
                    
                    assert!(type_val == type_var, "fulldec type failure, {:?} vs {:?}, line {}", type_val, type_var, node.line);
                    
                    store_or_memcpy!(type_var, slot, type_val, val, false);
                }
                else
                {
                    panic_error!("internal error: failed to find variable in full declaration");
                }
            }
            "constexpr_fulldeclaration" =>
            {
                let name = &node.child(1).unwrap().child(0).unwrap().text;
                if !env.constants.contains_key(name)
                {
                    let type_var = parse_type(env.types, node.child(0).unwrap()).unwrap();
                    //let basic_type = get_backend_type_sized(&mut env.backend_types, &env.types, &type_var);
                    
                    compile(env, node.child(2).unwrap(), want_pointer);
                    let (type_val, val) = unwrap_or_panic!(env.stack.pop());
                    
                    assert!(type_val == type_var, "fulldec type failure, {:?} vs {:?}, line {}", type_val, type_var, node.line);
                    
                    if !val_is_const(true, val)
                    {
                        panic_error!("error: constexpr contains non-constant parts");
                    }
                    
                    env.constants.insert(name.clone(), (type_var, val));
                }
                else
                {
                    panic_error!("error: tried to redeclare constant {}", name);
                }
            }
            "binstate" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let (type_val, val) = env.stack.pop().unwrap();
                let (type_left_incomplete, left_addr) = env.stack.pop().unwrap();
                
                let (type_left, volatile) = if type_left_incomplete.is_virtual_pointer()
                {
                    unwrap_or_panic!(type_left_incomplete.deref_vptr())
                }
                else
                {
                    panic_error!("tried to assign to fully evaluated expression (not a variable or virtual pointer) {:?}", type_left_incomplete);
                };
                assert!(type_val == type_left, "binstate type failure, {:?} vs {:?}, line {}", type_val, type_left, node.line);
                
                if let Ok(addr) = inkwell::values::PointerValue::try_from(left_addr)
                {
                    //let store = env.builder.build_store(addr, val).unwrap();
                    //store.set_volatile(volatile).unwrap();
                    store_or_memcpy!(type_left, addr, type_val, val, volatile);
                }
                else
                {
                    panic_error!("tried to assign to fully evaluated expression (not a variable or virtual pointer) {:?}", type_left_incomplete);
                }
            }
            "lvar" =>
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::Virtual);
                }
            "lvar_name" =>
            {
                let name = &node.child(0).unwrap().child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (mut type_, slot) = env.variables[name].clone();
                    check_struct_incomplete(env, &mut type_);
                    
                    assert!(want_pointer != WantPointer::None);
                    
                    push_val_or_ptr!(type_, slot, false);
                }
                else if env.global_decs.contains_key(name)
                {
                    panic_error!("error: found global but not implemented yet");
                }
                else if env.constants.contains_key(name)
                {
                    panic_error!("error: cannot assign to constant `{}`", name);
                }
                else
                {
                    panic_error!("error: unrecognized variable `{}`", name);
                }
            }
            "rvarname" =>
            {
                let name = &node.child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (mut type_, slot) = env.variables[name].clone();
                    check_struct_incomplete(env, &mut type_);
                    
                    push_val_or_ptr!(type_, slot, false);
                }
                else if env.constants.contains_key(name)
                {
                    let (mut type_, val) = env.constants[name].clone();
                    check_struct_incomplete(env, &mut type_);
                    
                    env.stack.push((type_, val));
                }
                else if let Some((type_, val, _)) = env.global_decs.get(name)
                {
                    let mut type_ = type_.clone();
                    check_struct_incomplete(env, &mut type_);
                    
                    push_val_or_ptr!(type_, val.as_pointer_value(), false);
                }
                else if let Some((func_val, funcsig)) = env.func_decs.get(name)
                {
                    let func_ptr = func_val.as_global_value().as_pointer_value();
                    env.stack.push((Type::from_functionsig(funcsig), func_ptr.into()));
                }
                else
                {
                    panic_error!("unrecognized identifier {}", name);
                }
            }
            "arrayindex_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::Virtual);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                
                let (offset_type, offset_val) = env.stack.pop().unwrap();
                let (base_type, base_addr) = env.stack.pop().unwrap();
                let (base_type, volatile) = unwrap_or_panic!(base_type.deref_vptr());
                
                if offset_type.name != "i64"
                {
                    panic_error!("error: can't offset into arrays except with type i64 (used type `{}`)", offset_type.name)
                }
                
                let mut inner_type = unwrap_or_panic!(base_type.array_to_inner());
                check_struct_incomplete(env, &mut inner_type);
                
                let inner_backend_type = get_backend_type_sized(env.backend_types, env.types, &inner_type);
                let inner_addr = unsafe { env.builder.build_in_bounds_gep(inner_backend_type, base_addr.into_pointer_value(), &[offset_val.into_int_value()], "").unwrap() };
                
                push_val_or_ptr!(inner_type, inner_addr, volatile);
            }
            "indirection_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::Virtual);
                let (struct_type, struct_addr) = env.stack.pop().unwrap();
                let (mut struct_type, volatile) = unwrap_or_panic!(struct_type.deref_vptr());
                check_struct_incomplete(env, &mut struct_type);
                let backend_type = get_backend_type_sized(env.backend_types, env.types, &struct_type);
                
                let right_name = &node.child(1).unwrap().child(0).unwrap().text;
                
                let found = match &struct_type.data
                {
                    TypeData::Struct(ref props) => props.iter().enumerate().find(|x| x.1.0 == *right_name),
                    _ => panic_error!("error: tried to use indirection (.) operator on non-struct {:?}", struct_type),
                }.unwrap_or_else(|| panic_error!("error: no such property {} in struct type {}", right_name, struct_type.name));
                
                let inner_index = found.0;
                let mut inner_type = found.1.1.clone();
                check_struct_incomplete(env, &mut inner_type);
                
                let struct_addr = struct_addr.into_pointer_value();
                
                let index_addr = env.builder.build_struct_gep::<inkwell::types::BasicTypeEnum>(backend_type, struct_addr, inner_index as u32, "").unwrap();
                
                push_val_or_ptr!(inner_type, index_addr, volatile);
            }
            "funcargs_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, funcaddr) = env.stack.pop().unwrap();
                match type_.data
                {
                    TypeData::FuncPointer(funcsig) =>
                    {
                        let hoisted_return = funcsig.return_type.is_composite() && !env.options.contains_key("bypass_agg_memcpy");
                        
                        let func_type = get_function_type(env.function_types, env.backend_types, env.types, &funcsig, env.options);
                        
                        let stack_len_start = env.stack.len();
                        compile(env, node.child(1).unwrap(), WantPointer::None);
                        let stack_len_end = env.stack.len();
                        
                        let num_args = node.child(1).unwrap().child_count().unwrap();
                        assert!(num_args == stack_len_end - stack_len_start);
                        assert!(num_args == funcsig.args.len(), "incorrect number of arguments to function on line {}", node.line);
                        
                        let mut args = Vec::new();
                        let mut arg_types = Vec::new();
                        for (i, arg_type) in funcsig.args.iter().rev().enumerate()
                        {
                            let (type_, mut val) = env.stack.pop().unwrap();
                            if type_ != *arg_type
                            {
                                panic_error!("mismatched types for parameter {} in call to function {:?} on line {}: expected `{}`, got `{}`", i+1, funcaddr, node.line, arg_type.to_string(), type_.to_string());
                            }
                            if val.is_pointer_value() && val.into_pointer_value().get_type().get_address_space() != inkwell::AddressSpace::default()
                            {
                                val = env.builder.build_address_space_cast(val.try_into().unwrap(), ptr_type, "").unwrap().into();
                            }
                            args.push(val.into());
                            arg_types.push(arg_type.clone());
                        }
                        args.reverse();
                        
                        if hoisted_return
                        {
                            let return_backend_type = get_backend_type_sized(env.backend_types, env.types, &funcsig.return_type);
                            let mut slot = emit_alloca!(return_backend_type, "");
                            if slot.get_type().get_address_space() != inkwell::AddressSpace::default()
                            {
                                slot = env.builder.build_address_space_cast(slot.try_into().unwrap(), ptr_type, "").unwrap().into();
                            }
                            args.push(slot.into());
                            arg_types.push(funcsig.return_type.clone());
                        }
                        
                        //println!("calling func with sigref {} and sig {}", sigref, funcsig.to_string());
                        let callval = env.builder.build_indirect_call(func_type, funcaddr.into_pointer_value(), &args, "").unwrap();
                        let result = callval.try_as_basic_value().left();
                        //println!("number of results {}", results.len());
                        if !hoisted_return
                        {
                            for (result, type_) in result.iter().zip([funcsig.return_type])
                            {
                                env.stack.push((type_.clone(), *result));
                            }
                        }
                        else
                        {
                            //println!("building access to hoisted return... {:?}", args.last().unwrap());
                            env.stack.push((arg_types.last().unwrap().clone(), (*args.last().unwrap()).try_into().unwrap()));
                        }
                    }
                    _ => panic_error!("error: tried to use non-function expression as a function")
                }
            }
            "funcargs" =>
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                }
            "intrinsic" =>
            {
                let intrinsic_name = &node.child(0).unwrap().child(0).unwrap().text;
                let (funcaddr, funcsig) = env.intrinsic_decs.get(intrinsic_name)
                    .unwrap_or_else(|| panic_error!("error: tried to call non-existent intrinsic {}", intrinsic_name));
                
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
                        panic_error!("mismatched types for parameter {} in call to intrinsic {:?} on line {}: expected `{}`, got `{}`", i+1, funcaddr, node.line, arg_type.to_string(), type_.to_string());
                    }
                    args.push(val.into());
                }
                
                args.reverse();
                
                // intrinsics whose function signatures lie because they have hidden arguments
                match intrinsic_name.as_str()
                {
                    "memcpy"      => args.push(zero_bool.into()),
                    "memmove"     => args.push(zero_bool.into()),
                    "memset"      => args.push(zero_bool.into()),
                    "memcpy_vol"  => args.push(one_bool.into()),
                    "memmove_vol" => args.push(one_bool.into()),
                    "memset_vol"  => args.push(one_bool.into()),
                    _ => {}
                }
                
                //println!("calling func with sigref {} and sig {}", sigref, funcsig.to_string());
                let callval = env.builder.build_direct_call(*funcaddr, &args, "").unwrap();
                let result = callval.try_as_basic_value().left();
                //println!("number of results {}", results.len());
                for (result, type_) in result.iter().zip([&funcsig.return_type])
                {
                    env.stack.push((type_.clone(), *result));
                }
                //println!("done compiling func call");
            }
            "constexpr" =>
            {
                compile(env, node.child(0).unwrap(), want_pointer);
                if !val_is_const(true, env.stack.last().unwrap().1)
                {
                    panic_error!("error: constexpr contains non-constant parts; {:?}", env.stack.last());
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
                let mut element_backend_type = None;
                let mut is_const = true;
                for _ in 0..array_length
                {
                    let (type_, mut val) = env.stack.pop().unwrap();
                    if element_type.is_none()
                    {
                        element_type = Some(type_.clone());
                        element_backend_type = Some(get_backend_type_sized(env.backend_types, env.types, &type_));
                    }
                    if Some(type_.clone()) != element_type
                    {
                        panic_error!("error: array literals must entirely be of a single type");
                    }
                    is_const = val_is_const(is_const, val);
                    if val_is_const(is_const, val) && type_.is_composite() && val.is_pointer_value()
                    {
                        val = *env.anon_globals.get(&val.into_pointer_value()).unwrap();
                    }
                    else if type_.is_composite() && val.is_pointer_value()
                    {
                        val = env.builder.build_load(element_backend_type.unwrap(), val.into_pointer_value(), "").unwrap();
                    }
                    vals.push(val);
                }
                vals.reverse();
                
                let element_type = element_type.unwrap_or_else(|| panic_error!("error: zero-length array literals are not allowed"));
                let element_backend_type = element_backend_type.unwrap();
                let array_type = element_type.to_array(array_length);
                let array_backend_type = get_backend_type_sized(env.backend_types, env.types, &array_type);
                
                if !is_const
                {
                    //let slot = env.builder.build_array_alloca(element_backend_type, size, "").unwrap();
                    let slot = emit_alloca!(array_backend_type, "");
                    
                    for (offset, val) in vals.into_iter().enumerate()
                    {
                        let offset_val = u64_type.const_int(offset as u64, false);
                        let offset_addr = unsafe { env.builder.build_in_bounds_gep(element_backend_type, slot, &[offset_val], "").unwrap() };
                        
                        env.builder.build_store(offset_addr, val).unwrap();
                    }
                    
                    push_val_or_ptr!(array_type, slot, false);
                }
                else
                {
                    //println!("-- {:?}", vals);
                    let val = basic_const_array(element_backend_type, &vals);
                    let global = make_anonymous_const_global!(val);
                    
                    if want_pointer != WantPointer::None
                    {
                        push_val_or_ptr!(array_type.clone(), global.as_pointer_value(), false);
                    }
                    else
                    {
                        env.stack.push((array_type.clone(), global.as_pointer_value().into()));
                    }
                }
            }
            "struct_literal" =>
            {
                let stack_size = env.stack.len();
                
                let struct_type = parse_type(env.types, node.child(0).unwrap()).unwrap();
                let all_struct_members = unwrap_or_panic!(struct_type.struct_to_info()).to_vec();
                let mut struct_member_types = Vec::new();
                let mut struct_members = Vec::new();
                for (index, (name, type_)) in all_struct_members.iter().enumerate()
                {
                    if name == "_"
                    {
                        continue;
                    }
                    struct_member_types.push(type_.clone());
                    struct_members.push((type_.clone(), index));
                }
                //println!("struct type: {:?}", struct_type);
                
                for child in &node.get_children().unwrap()[1..]
                {
                    compile(env, child, WantPointer::None);
                }
                let member_count = env.stack.len() - stack_size;
                assert!(member_count == struct_member_types.len());
                //println!("struct member count: {}", member_count);
                
                let mut is_const = true;
                let mut vals = Vec::new();
                for _ in 0..member_count
                {
                    let (type_, mut val) = env.stack.pop().unwrap();
                    is_const = val_is_const(is_const, val);
                    if val_is_const(is_const, val) && type_.is_composite() && val.is_pointer_value()
                    {
                        val = *env.anon_globals.get(&val.into_pointer_value()).unwrap();
                    }
                    else if type_.is_composite() && val.is_pointer_value()
                    {
                        let val_backend_type = get_backend_type_sized(env.backend_types, env.types, &type_);
                        val = env.builder.build_load(val_backend_type, val.into_pointer_value(), "").unwrap();
                    }
                    vals.push((type_, val));
                }
                vals.reverse();
                
                let stack_val_types = vals.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
                assert!(struct_member_types == stack_val_types);
                
                let backend_type = get_backend_type_sized(env.backend_types, env.types, &struct_type);
                
                if !is_const
                {
                    let slot = emit_alloca!(backend_type, "");
                    for (index, (type_, val)) in vals.into_iter().enumerate()
                    {
                        let real_index = struct_members[index].1;
                        let index_addr = env.builder.build_struct_gep::<inkwell::types::BasicTypeEnum>(backend_type, slot, real_index as u32, "").unwrap();
                        
                        assert!(type_ == stack_val_types[index]);
                        
                        env.builder.build_store(index_addr, val).unwrap();
                    }
                    
                    push_val_or_ptr!(struct_type, slot, false);
                }
                else
                {
                    let mut newvals = Vec::new();
                    let mut i = 0;
                    for (name, type_) in all_struct_members.iter()
                    {
                        if name == "_"
                        {
                            let backend_type = get_backend_type_sized(env.backend_types, env.types, type_);
                            let zero_val = get_any_type_poison(backend_type);
                            newvals.push(zero_val);
                        }
                        else
                        {
                            let (inner_type_, val) = &vals[i];
                            assert!(*type_ == stack_val_types[i]);
                            assert!(*type_ == *inner_type_);
                            newvals.push(*val);
                            
                            i += 1;
                        }
                    }
                    let val = backend_type.into_struct_type().const_named_struct(&newvals);
                    let global = make_anonymous_const_global!(val);
                    
                    if want_pointer != WantPointer::None
                    {
                        push_val_or_ptr!(struct_type, global.as_pointer_value(), false);
                    }
                    else
                    {
                        env.stack.push((struct_type, global.as_pointer_value().into()));
                    }
                }
            }
            "float" =>
            {
                let text = &node.child(0).unwrap().text;
                let location = text.rfind("f").unwrap();
                let parts = text.split_at(location);
                let text = parts.0;
                let type_ = env.types.get(parts.1).unwrap_or_else(|| panic_error!("unknown float suffix pattern {}", parts.1));
                let backend_type = get_backend_type(env.backend_types, env.types, type_);
                let float_type = inkwell::types::FloatType::try_from(backend_type).unwrap_or_else(|()| panic_error!("unknown float suffix pattern {}", parts.1));
                
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
                    _ => panic_error!("unknown float suffix pattern {}", parts.1)
                }
            }
            "char" =>
            {
                let text = &node.child(0).unwrap().text;
                let mut c = text.chars().nth(1).unwrap();
                if c == '\\'
                {
                    c = match text.chars().nth(2).unwrap()
                    {
                        '\\' => '\\',
                        '\'' => '\'',
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        _ => panic_error!("unknown char escape code"),
                    }
                }
                let val = u32::from(c);
                if text.ends_with("u32")
                {
                    let type_ = env.types.get("u32").unwrap();
                    let backend_type = get_backend_type(env.backend_types, env.types, type_);
                    let int_type = backend_type.into_int_type();
                    let res = int_type.const_int(val as u64, false);
                    env.stack.push((type_.clone(), res.into()));
                }
                else
                {
                    if val > 0xFF
                    {
                        panic_error!("u8 char literal has a value greater than 255");
                    }
                    let type_ = env.types.get("u8").unwrap();
                    let backend_type = get_backend_type(env.backend_types, env.types, type_);
                    let int_type = backend_type.into_int_type();
                    let res = int_type.const_int(val as u64, false);
                    env.stack.push((type_.clone(), res.into()));
                }
            }
            "string" =>
            {
                let text = &node.child(0).unwrap().text;
                let text = text.split_once('"').unwrap().1;
                let (text, suffix) = text.rsplit_once('"').unwrap();
                let mut full_text = "".to_string();
                let mut char_iter = text.chars();
                let mut maybe_c = char_iter.next();
                while let Some(mut c) = maybe_c
                {
                    if c == '\\'
                    {
                        c = match char_iter.next()
                        {
                            Some('\\') => '\\',
                            Some('\'') => '\'',
                            Some('n') => '\n',
                            Some('r') => '\r',
                            Some('t') => '\t',
                            _ => panic_error!("unknown or missing string escape code"),
                        }
                    }
                    full_text.push(c);
                    maybe_c = char_iter.next();
                }
                let mut bytes = full_text.into_bytes();
                if !suffix.ends_with("nonull")
                {
                    bytes.push(0);
                }
                
                let vals = bytes.iter().map(|x| u8_type.const_int(*x as u64, false).into()).collect::<Vec<_>>();
                let array_val = basic_const_array(u8_type.into(), &vals);
                let type_ = u8_type_frontend.to_ptr(false);
                if suffix.starts_with("array")
                {
                    if want_pointer != WantPointer::None
                    {
                        let global = make_anonymous_const_global!(array_val);
                        env.stack.push((type_.clone(), global.as_pointer_value().into()));
                    }
                    else
                    {
                        let array_type = u8_type_frontend.to_array(bytes.len());
                        env.stack.push((array_type.clone(), array_val.into()));
                    }
                }
                else
                {
                    if want_pointer != WantPointer::None
                    {
                        panic_error!("tried to get pointer of pointer-to-string literal");
                    }
                    let global = make_anonymous_const_global!(array_val);
                    env.stack.push((type_.clone(), global.as_pointer_value().into()));
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
                    panic_error!("internal error: unknown integer type literal suffix");
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
                
                let type_ = env.types.get(parts.1).unwrap_or_else(|| panic_error!("unknown int suffix pattern {}", parts.1));
                let backend_type = get_backend_type(env.backend_types, env.types, type_);
                let int_type = inkwell::types::IntType::try_from(backend_type).unwrap_or_else(|()| panic_error!("unknown int suffix pattern {}", parts.1));
                let res = int_type.const_int(match (is_hex, parts.1)
                {
                    (false, "u8")  => text.parse::< u8>().unwrap() as u64,
                    (false, "u16") => text.parse::<u16>().unwrap() as u64,
                    (false, "u32") => text.parse::<u32>().unwrap() as u64,
                    (false, "u64") => text.parse::<u64>().unwrap(),
                    (false, "i8")  => text.parse::< i8>().unwrap() as u64,
                    (false, "i16") => text.parse::<i16>().unwrap() as u64,
                    (false, "i32") => text.parse::<i32>().unwrap() as u64,
                    (false, "i64") => text.parse::<i64>().unwrap() as u64,
                    (true , "u8")  =>  u8::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "u16") => u16::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "u32") => u32::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "u64") => u64::from_str_radix(&text, 16).unwrap(),
                    (true , "i8")  =>  i8::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "i16") => i16::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "i32") => i32::from_str_radix(&text, 16).unwrap() as u64,
                    (true , "i64") => i64::from_str_radix(&text, 16).unwrap() as u64,
                    _ => panic_error!("unknown int suffix pattern {}", parts.1)
                }, signed);
                env.stack.push((type_.clone(), res.into()));
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
                        panic_error!("error: tried to get address of non-variable");
                    }
                    env.stack.push((type_, val));
                }
                else
                {
                    compile(env, node.child(1).unwrap(), WantPointer::None);
                    let (mut type_, val) = env.stack.pop().unwrap();
                    match type_.name.as_str()
                    {
                        "ptr" =>
                        {
                            if want_pointer == WantPointer::Virtual
                            {
                                if op.as_str() == "!"
                                {
                                    let res = env.builder.build_is_null(inkwell::values::PointerValue::try_from(val).unwrap(), "").unwrap();
                                    let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "").unwrap();
                                    env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                }
                                else if op.as_str() == "*"
                                {
                                    env.stack.push((unwrap_or_panic!(type_.ptr_to_vptr()), val));
                                }
                                else if op.as_str() == "@"
                                {
                                    match &mut type_.data
                                    {
                                        TypeData::Pointer(_, ref mut is_volatile) => *is_volatile = true,
                                        _ => panic_error!("internal error: broken pointer volatility test"),
                                    }
                                    println!("marked pointer as volatile");
                                    env.stack.push((type_, val));
                                    
                                }
                                else
                                {
                                    panic_error!("error: can't use operator `{}` on type `{}`", op, type_.name);
                                }
                            }
                            else
                            {
                                let (inner_type, volatile) = unwrap_or_panic!(type_.deref_ptr());
                                let basic_type = get_backend_type_sized(env.backend_types, env.types, &inner_type);
                                match op.as_str()
                                {
                                    "!" =>
                                    {
                                        let res = env.builder.build_is_null(inkwell::values::PointerValue::try_from(val).unwrap(), "").unwrap();
                                        let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "").unwrap();
                                        env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                    }
                                    "*" =>
                                    {
                                        if inner_type.is_void()
                                        {
                                            panic_error!("can't dereference void pointers");
                                        }
                                        let res = env.builder.build_load(basic_type, val.into_pointer_value(), "").unwrap();
                                        res.as_instruction_value().unwrap().set_volatile(volatile).unwrap();
                                        env.stack.push((inner_type, res));
                                    }
                                    "@" =>
                                    {
                                        match &mut type_.data
                                        {
                                            TypeData::Pointer(_, ref mut is_volatile) => *is_volatile = true,
                                            _ => panic_error!("internal error: broken pointer volatility test"),
                                        }
                                        env.stack.push((type_, val));
                                    }
                                    _ => panic_error!("error: can't use operator `{}` on type `{}`", op, type_.name)
                                };
                            }
                        }
                        "f32" | "f64" =>
                        {
                            let res = match op.as_str()
                            {
                                "+" => val,
                                "-" => env.builder.build_float_neg(val.into_float_value(), "").unwrap().into(),
                                _ => panic_error!("error: can't use operator `{}` on type `{}`", op, type_.name)
                            };
                            env.stack.push((type_.clone(), res));
                        }
                        "i8" | "i16" | "i32" | "i64" |
                        "u8" | "u16" | "u32" | "u64" =>
                        {
                            let res = match op.as_str()
                            {
                                "+" => val,
                                "-" => env.builder.build_int_neg(val.into_int_value(), "").unwrap().into(),
                                "~" => env.builder.build_not(val.into_int_value(), "").unwrap().into(),
                                "!" =>
                                {
                                    let backend_type = get_backend_type(env.backend_types, env.types, &type_);
                                    if let (Ok(int_type), Ok(int_val)) = (inkwell::types::IntType::try_from(backend_type), inkwell::values::IntValue::try_from(val))
                                    {
                                        let zero = int_type.const_int(0, true);
                                        let int_val = env.builder.build_int_compare(inkwell::IntPredicate::EQ, int_val, zero, "").unwrap().into();
                                        int_val
                                    }
                                    else
                                    {
                                        panic_error!("internal error: integer was not integer in ! operator");
                                    }
                                }
                                _ => panic_error!("error: can't use operator `{}` on type `{}`", op, type_.name)
                            };
                            env.stack.push((type_.clone(), res));
                        }
                        _ => panic_error!("error: type `{}` is not supported by unary operators", type_.name)
                    }
                }
            }
            "label" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                let block = env.blocks.get(label).unwrap_or_else(|| panic_error!("error: no such block {}", label));
                env.builder.build_unconditional_branch(*block).unwrap();
                env.builder.position_at_end(*block);
            }
            "goto" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                let then_block = env.blocks.get(label).unwrap_or_else(|| panic_error!("error: no such label {}", label));
                env.builder.build_unconditional_branch(*then_block).unwrap();
                env.builder.position_at_end(env.context.append_basic_block(env.func_val, ""));
            }
            // TODO: all the other C-style control flow mechanisms
            "ifcondition" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, val) = env.stack.pop().unwrap();
                let label = &node.child(1).unwrap().child(0).unwrap().text;
                // anonymous block for "else" case
                let else_block = env.context.append_basic_block(env.func_val, "");
                let then_block = env.blocks.get(label).unwrap_or_else(|| panic_error!("error: no such label {}", label));
                let backend_type = get_backend_type(env.backend_types, env.types, &type_);
                if let (Ok(int_type), Ok(int_val)) = (inkwell::types::IntType::try_from(backend_type), inkwell::values::IntValue::try_from(val))
                {
                    let zero = int_type.const_int(0, true);
                    let int_val = env.builder.build_int_compare(inkwell::IntPredicate::NE, int_val, zero, "").unwrap();
                    env.builder.build_conditional_branch(int_val, *then_block, else_block).unwrap();
                    env.builder.position_at_end(else_block);
                }
                else if let (Ok(_ptr_type), Ok(ptr_val)) = (inkwell::types::PointerType::try_from(backend_type), inkwell::values::PointerValue::try_from(val))
                {
                    let int_val = env.builder.build_ptr_to_int(ptr_val, env.ptr_int_type, "").unwrap();
                    let zero = env.ptr_int_type.const_int(0, true);
                    let int_val = env.builder.build_int_compare(inkwell::IntPredicate::NE, int_val, zero, "").unwrap();
                    env.builder.build_conditional_branch(int_val, *then_block, else_block).unwrap();
                    env.builder.position_at_end(else_block);
                }
                else
                {
                    panic_error!("error: tried to branch on non-integer expression");
                }
            }
            "sizeof" =>
            {
                let type_ = parse_type(env.types, node.child(0).unwrap()).unwrap();
                let backend_type = get_backend_type(env.backend_types, env.types, &type_);
                let size = backend_type.size_of().unwrap_or_else(|| panic_error!("error: type `{}` is not sized", type_.name));
                // FIXME cast up to u64 if sizeof result is not u64 large
                env.stack.push((env.types.get("u64").unwrap().clone(), size.into()));
            }
            "bitcast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack.pop().unwrap();
                
                let right_type = parse_type(env.types, node.child(1).unwrap()).unwrap();
                let right_basic_type = get_backend_type_sized(env.backend_types, env.types, &right_type);
                
                let (left_size, right_size) = (store_size_of_type(env.target_data, env.backend_types, env.types, &left_type), store_size_of_type(env.target_data, env.backend_types, env.types, &right_type));
                
                // cast as own type (replace type, aka do nothing)
                if left_type.name == right_type.name
                {
                    env.stack.push((right_type, left_val));
                }
                // cast from pointer to pointer (replace type)
                else if left_type.is_pointer_or_fpointer() && right_type.is_pointer_or_fpointer()
                {
                    env.stack.push((right_type, left_val));
                }
                // pointer-to-int cast
                else if left_type.is_pointer_or_fpointer() && right_type.is_int_unsigned() && right_type.size() as u32 * 8 == env.ptr_int_type.get_bit_width()
                {
                    let ret = env.builder.build_ptr_to_int(left_val.into_pointer_value(), env.ptr_int_type, "").unwrap().into();
                    env.stack.push((right_type, ret));
                }
                // int-to-pointer cast
                else if left_type.is_int_unsigned() && right_type.is_pointer_or_fpointer() && left_type.size() as u32 * 8 == env.ptr_int_type.get_bit_width()
                {
                    let ptr_type = env.context.ptr_type(inkwell::AddressSpace::default());
                    let ret = env.builder.build_int_to_ptr(left_val.into_int_value(), ptr_type, "").unwrap().into();
                    env.stack.push((right_type, ret));
                }
                // both primitive (non-pointer)
                else if left_size == right_size && right_type.is_pointer_or_fpointer() == left_type.is_pointer_or_fpointer() && !right_type.is_composite() && !left_type.is_composite()
                {
                    let ret = env.builder.build_bit_cast(left_val, right_basic_type, "").unwrap();
                    env.stack.push((right_type, ret));
                }
                // both composite
                else if left_size == right_size && right_type.is_composite() && left_type.is_composite()
                {
                    env.stack.push((right_type, left_val));
                }
                // composite to primitive
                else if left_size == right_size && left_type.is_composite() && !right_type.is_composite() && !right_type.is_pointer_or_fpointer()
                {
                    let slot = emit_alloca!(right_basic_type, "");
                    let len = right_basic_type.size_of().unwrap().into();
                    build_memcpy!(env.builder, env.module, slot, left_val, len, false).unwrap();
                    let res = env.builder.build_load(right_basic_type, slot, "").unwrap();
                    env.stack.push((right_type, res.into()));
                }
                // primitive to composite
                else if left_size == right_size && right_type.is_composite() && !left_type.is_composite() && !left_type.is_pointer_or_fpointer()
                {
                    let slot = emit_alloca!(right_basic_type, "");
                    env.builder.build_store(slot, left_val).unwrap();
                    env.stack.push((right_type, slot.into()));
                }
                else
                {
                    panic_error!("error: unsupported bitcast from type {} to type {}\n(types must have the same size and be sized to be bitcasted, and ptrs can only be casted to the target's ptr-sized int type, which is: {:?} - compare {:?})", left_type.to_string(), right_type.to_string(), env.ptr_int_type, right_basic_type);
                }
            }
            "unsafe_cast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack.pop().unwrap();
                let _left_basic_type : BasicTypeEnum = get_backend_type(env.backend_types, env.types, &left_type).try_into().unwrap();
                
                let right_type = parse_type(env.types, node.child(1).unwrap()).unwrap();
                let right_backend_type = get_backend_type(env.backend_types, env.types, &right_type);
                let _right_basic_type : BasicTypeEnum = right_backend_type.try_into().unwrap();
                
                
                if left_type.name == right_type.name
                {
                    panic_error!("unsupported unsafe cast from type {} to same type (unsafe casts are only for type pairs with expensive standard casts)", left_type.to_string());
                }
                // cast from float to int (must be int, not pointer)
                else if left_type.is_float() && right_type.is_int_unsigned()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_float_to_unsigned_int(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: float-to-uint cast internal and backend type mismatch");
                    }
                }
                else if left_type.is_float() && right_type.is_int_signed()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_float_to_signed_int(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: float-to-sint cast internal and backend type mismatch");
                    }
                }
                else
                {
                    panic_error!("unsupported unsafe cast from type {} to type {} (unsafe casts are only for type pairs with expensive standard casts)", left_type.to_string(), right_type.to_string());
                }
            }
            "cast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack.pop().unwrap();
                let left_basic_type : BasicTypeEnum = get_backend_type(env.backend_types, env.types, &left_type).try_into().unwrap();
                
                let right_type = parse_type(env.types, node.child(1).unwrap()).unwrap();
                let right_backend_type = get_backend_type(env.backend_types, env.types, &right_type);
                
                // cast as own type (replace type, aka do nothing)
                if left_type.name == right_type.name
                {
                    env.stack.push((right_type, left_val));
                }
                // cast from pointer to pointer (replace type)
                else if left_type.is_pointer_or_fpointer() && right_type.is_pointer_or_fpointer()
                {
                    env.stack.push((right_type, left_val));
                }
                // cast between ints of same size; replace type
                else if left_type.is_int() && right_type.is_int() && left_type.size() == right_type.size()
                {
                    env.stack.push((right_type, left_val));
                }
                // cast between different float types
                else if (left_type.name == "f32" && right_type.name == "f64") || (left_type.name == "f64" && right_type.name == "f32")
                {
                    if let (Ok(left_val), Ok(float_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::FloatType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_float_cast(left_val, float_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: float cast internal and backend type mismatch");
                    }
                }
                // cast from int to float (must be int, not pointer)
                else if left_type.is_int_unsigned() && right_type.is_float()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::IntValue::try_from(left_val), inkwell::types::FloatType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_unsigned_int_to_float(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: uint-to-float cast internal and backend type mismatch");
                    }
                }
                else if left_type.is_int_signed() && right_type.is_float()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::IntValue::try_from(left_val), inkwell::types::FloatType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_signed_int_to_float(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: sint-to-float cast internal and backend type mismatch");
                    }
                }
                // cast from float to int (must be int, not pointer)
                else if left_type.is_float() && right_type.is_int_unsigned()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        if left_val.is_const()
                        {
                            let (constant, _) = left_val.get_constant().unwrap();
                            
                            let min = 0.0;
                            let max_int = target_type.const_all_ones().get_zero_extended_constant().unwrap();
                            let max = max_int as f64;
                            
                            if constant.is_nan() || constant <= min
                            {
                                env.stack.push((right_type, target_type.const_int(0, true).into()));
                            }
                            else if constant >= max
                            {
                                env.stack.push((right_type, target_type.const_int(max_int, true).into()));
                            }
                            else
                            {
                                let ret = env.builder.build_float_to_signed_int(left_val, target_type, "").unwrap();
                                env.stack.push((right_type, ret.into()));
                            }
                        }
                        else
                        {
                            let intrinsic = inkwell::intrinsics::Intrinsic::find("llvm.fptoui.sat").unwrap();
                            let function = intrinsic.get_declaration(env.module, &[target_type.into(), left_basic_type]).unwrap();
                            
                            let callval = env.builder.build_direct_call(function, &[left_val.into()], "").unwrap();
                            let result = callval.try_as_basic_value().left().unwrap();
                            env.stack.push((right_type.clone(), result));
                        }
                    }
                    else
                    {
                        panic_error!("internal error: float-to-uint cast internal and backend type mismatch");
                    }
                }
                else if left_type.is_float() && right_type.is_int_signed()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::FloatValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        if left_val.is_const()
                        {
                            let (constant, _) = left_val.get_constant().unwrap();
                            
                            let ones_int : u64 = target_type.const_all_ones().get_zero_extended_constant().unwrap();
                            let min_int = -((ones_int >> 1) as i64) - 1;
                            let min = min_int as f64;
                            let max_int = ones_int >> 1;
                            let max = max_int as f64;
                            
                            if constant.is_nan()
                            {
                                env.stack.push((right_type, target_type.const_int(0, true).into()));
                            }
                            else if constant <= min
                            {
                                env.stack.push((right_type, target_type.const_int(min_int as u64, true).into()));
                            }
                            else if constant >= max
                            {
                                env.stack.push((right_type, target_type.const_int(max_int, true).into()));
                            }
                            else
                            {
                                let ret = env.builder.build_float_to_signed_int(left_val, target_type, "").unwrap();
                                env.stack.push((right_type, ret.into()));
                            }
                        }
                        else
                        {
                            let intrinsic = inkwell::intrinsics::Intrinsic::find("llvm.fptosi.sat").unwrap();
                            let function = intrinsic.get_declaration(env.module, &[target_type.into(), left_basic_type]).unwrap();
                            
                            let callval = env.builder.build_direct_call(function, &[left_val.into()], "").unwrap();
                            let result = callval.try_as_basic_value().left().unwrap();
                            env.stack.push((right_type.clone(), result));
                        }
                    }
                    else
                    {
                        panic_error!("internal error: float-to-sint cast internal and backend type mismatch");
                    }
                }
                // cast to larger int type, signed
                else if left_type.is_int_signed() && right_type.is_int_signed() && left_type.size() < right_type.size()
                {
                    
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::IntValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_int_s_extend(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: signed int upcast internal and backend type mismatch");
                    }
                }
                // cast to larger int type, unsigned
                else if left_type.is_int_unsigned() && right_type.is_int_unsigned() && left_type.size() < right_type.size()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::IntValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_int_z_extend(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: unsigned int upcast internal and backend type mismatch");
                    }
                }
                // cast to smaller int type
                else if left_type.is_int() && right_type.is_int() && left_type.size() > right_type.size()
                {
                    if let (Ok(left_val), Ok(target_type)) = (inkwell::values::IntValue::try_from(left_val), inkwell::types::IntType::try_from(right_backend_type))
                    {
                        let ret = env.builder.build_int_truncate(left_val, target_type, "").unwrap();
                        env.stack.push((right_type, ret.into()));
                    }
                    else
                    {
                        panic_error!("internal error: int downcast internal and backend type mismatch");
                    }
                }
                else
                {
                    panic_error!("unsupported cast from type {} to type {}", left_type.to_string(), right_type.to_string());
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
                    
                    if left_type.is_pointer() && right_type.is_int()
                    {
                        let right_backend_type = get_backend_type(env.backend_types, env.types, &right_type);
                        let right_basic_type : BasicTypeEnum = right_backend_type.try_into().unwrap();
                        
                        match op.as_str()
                        {
                            "&" =>
                            {
                                if !right_type.is_int_unsigned() || right_type.size() as u32 * 8 != env.ptr_int_type.get_bit_width()
                                {
                                    panic_error!("ptr mask (&) operation is only supported with a right-hand operand of the target's pointer-sized int type (usually u64 or u32) {:?}", right_basic_type);
                                }
                                if val_is_const(true, left_val) && val_is_const(true, right_val)
                                {
                                    let intptr : inkwell::values::IntValue = env.builder.build_ptr_to_int(left_val.into_pointer_value(), env.ptr_int_type, "").unwrap();
                                    let masked = env.builder.build_and(intptr, right_val.into_int_value(), "").unwrap();
                                    let diff = env.builder.build_int_sub(masked, intptr, "").unwrap();
                                    let offset_addr = unsafe { env.builder.build_gep(u8_type, left_val.into_pointer_value(), &[diff], "").unwrap() };
                                    env.stack.push((left_type.clone(), offset_addr.into()));
                                }
                                else
                                {
                                    let intrinsic = inkwell::intrinsics::Intrinsic::find("llvm.ptrmask").unwrap();
                                    let function = intrinsic.get_declaration(env.module, &[ptr_type.into(), env.ptr_int_type.into()]).unwrap();
                                    
                                    let callval = env.builder.build_direct_call(function, &[left_val.into(), right_val.into()], "").unwrap();
                                    let result = callval.try_as_basic_value().left().unwrap();
                                    env.stack.push((left_type.clone(), result));
                                }
                            }
                            "+" | "-" =>
                            {
                                let mut offset_val = right_val;
                                if op == "-"
                                {
                                    offset_val = env.builder.build_int_neg(offset_val.into_int_value(), "").unwrap().into();
                                }
                                let offset_addr = unsafe { env.builder.build_gep(u8_type, left_val.into_pointer_value(), &[offset_val.try_into().unwrap()], "").unwrap() };
                                
                                env.stack.push((left_type.clone(), offset_addr.into()));
                            }
                            _ => panic_error!("the only binary operators allowed for pointers are &, +, and -"),
                        }
                    }
                    else
                    {
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
                                            "+" => env.builder.build_float_add(left_val, right_val, "").unwrap(),
                                            "-" => env.builder.build_float_sub(left_val, right_val, "").unwrap(),
                                            "*" => env.builder.build_float_mul(left_val, right_val, "").unwrap(),
                                            "/" => env.builder.build_float_div(left_val, right_val, "").unwrap(),
                                            "%" => env.builder.build_float_rem(left_val, right_val, "").unwrap(),
                                            _ => panic_error!("internal error: operator mismatch")
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
                                            _ => panic_error!("internal error: operator mismatch")
                                        };
                                        let res = env.builder.build_float_compare(op, left_val, right_val, "").unwrap();
                                        let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "").unwrap();
                                        env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                    }
                                    _ => panic_error!("operator {} not supported on type pair {}, {}", op, left_type.name, right_type.name)
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
                                        let left_bool  = env.builder.build_int_compare(inkwell::IntPredicate::NE, left_val , zero, "").unwrap();
                                        let left_bool  = env.builder.build_int_cast_sign_flag(left_bool , u8_type, false, "").unwrap();
                                        let right_bool = env.builder.build_int_compare(inkwell::IntPredicate::NE, right_val, zero, "").unwrap();
                                        let right_bool = env.builder.build_int_cast_sign_flag(right_bool, u8_type, false, "").unwrap();
                                        
                                        let res = match op.as_str()
                                        {
                                            "||" | "or"  => env.builder.build_or (left_bool, right_bool, ""),
                                            "&&" | "and" => env.builder.build_and(left_bool, right_bool, ""),
                                            _ => panic_error!("internal error: operator mismatch")
                                        }.unwrap();
                                        env.stack.push((u8_type_frontend.clone(), res.into()));
                                    }
                                    "|" | "&" | "^" =>
                                    {
                                        let res = match op.as_str()
                                        {
                                            "|" => env.builder.build_or (left_val, right_val, ""),
                                            "&" => env.builder.build_and(left_val, right_val, ""),
                                            "^" => env.builder.build_xor(left_val, right_val, ""),
                                            _ => panic_error!("internal error: operator mismatch")
                                        }.unwrap();
                                        env.stack.push((left_type.clone(), res.into()));
                                    }
                                    "+" | "-" | "*" | "div_unsafe" | "rem_unsafe" =>
                                    {
                                        let res = match op.as_str()
                                        {
                                            "+" => env.builder.build_int_add(left_val, right_val, ""),
                                            "-" => env.builder.build_int_sub(left_val, right_val, ""),
                                            "*" => env.builder.build_int_mul(left_val, right_val, ""),
                                            "div_unsafe" => match is_u
                                            {
                                                true => env.builder.build_int_unsigned_div(left_val, right_val, ""),
                                                false => env.builder.build_int_signed_div(left_val, right_val, ""),
                                            }
                                            "rem_unsafe" => match is_u
                                            {
                                                true => env.builder.build_int_unsigned_rem(left_val, right_val, ""),
                                                false => env.builder.build_int_signed_rem(left_val, right_val, ""),
                                            }
                                            _ => panic_error!("internal error: operator mismatch")
                                        }.unwrap();
                                        env.stack.push((left_type.clone(), res.into()));
                                    }
                                    "/" | "%" =>
                                    {
                                        let zero = left_val.get_type().const_int(0, true);
                                        let comp_val = env.builder.build_int_compare(inkwell::IntPredicate::EQ, right_val, zero, "").unwrap();
                                        
                                        let op_res = match (op.as_str(), is_u)
                                        {
                                            ("/",  true) => env.builder.build_int_unsigned_div(left_val, right_val, ""),
                                            ("/", false) => env.builder.build_int_signed_div(left_val, right_val, ""),
                                            ("%",  true) => env.builder.build_int_unsigned_rem(left_val, right_val, ""),
                                            ("%", false) => env.builder.build_int_signed_rem(left_val, right_val, ""),
                                            _ => panic_error!("internal error: div/rem operator mismatch"),
                                        }.unwrap();
                                        
                                        let res = env.builder.build_select(comp_val, zero, op_res, "").unwrap();
                                        env.stack.push((left_type.clone(), res));
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
                                            _ => panic_error!("internal error: operator mismatch")
                                        };
                                        let res = env.builder.build_int_compare(op, left_val, right_val, "").unwrap();
                                        let res = env.builder.build_int_cast_sign_flag(res, u8_type, false, "").unwrap();
                                        env.stack.push((env.types.get("u8").unwrap().clone(), res.into()));
                                    }
                                    _ => panic_error!("operator {} not supported on type {}", op, left_type.name)
                                }
                            }
                            _ => panic_error!("unhandled type pair `{}`, `{}`", left_type.name, right_type.name)
                        }
                    }
                }
                else
                {
                    panic_error!("unhandled AST node {}", text);
                }
            }
        }
    }
    else
    {
        panic_error!("unhandled variable access");
    }
}

const VERBOSE : bool = false;
const PRINT_COMP_TIME : bool = true;
const DEBUG_FIRST_MODULE : bool = true;

fn run_program(modules : Vec<String>, _args : Vec<String>, settings : HashMap<&'static str, String>)
{
    if VERBOSE
    {
        println!("startup...");
    }
    let skip_jit = settings.get("asm_triple").is_some();
    
    let true_start = std::time::Instant::now();
    
    let start = std::time::Instant::now();
    let context = inkwell::context::Context::create();
    
    let ptr_type = context.ptr_type(inkwell::AddressSpace::default());
    if VERBOSE
    {
        println!("pointer type... {:?}", inkwell::AddressSpace::default());
    }
    
    let type_table = [
        ("void".to_string(), Type { name : "void".to_string(), data : TypeData::Void }, context.void_type().into()),
        
        // used in some intrinsics
        ("i1" .to_string(), Type { name : "i1" .to_string(), data : TypeData::Primitive }, context.bool_type().into()),
        
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
    backend_types.insert("ptr".to_string(), ptr_type.into());
    // backend types for functions
    let mut function_types = BTreeMap::new();
    
    let ir_grammar = include_str!("parser/irgrammar.txt");
    let mut parser = parser::Parser::new_from_grammar(ir_grammar).unwrap();
    
    //let opt_level = inkwell::OptimizationLevel::None;
    let opt_level = inkwell::OptimizationLevel::Aggressive;
    
    let mut imports : BTreeMap<String, (*const u8, FunctionSig)> = BTreeMap::new();
    fn import_function<T>(types: &BTreeMap<String, Type>, parser : &mut parser::Parser, imports : &mut BTreeMap<String, (*const u8, FunctionSig)>, name : &str, _pointer : T, pointer_usize : usize, type_string : &str)
    {
        let type_lines = vec!(type_string.to_string());
        let type_tokens = parser.tokenize(&type_lines, true).unwrap();
        let type_ast = parser.parse_with_root_node_type(&type_tokens, &type_lines, true, "type").unwrap().unwrap();
        let type_ = parse_type(types, &type_ast).unwrap();
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
    
    let mut env_options = HashMap::new();
    
    if settings.contains_key("simple_aggregates")
    {
        env_options.insert("bypass_agg_memcpy", true);
    }
    
    let env_options = &env_options;
    
    // format specifiers:
    // %X - u64    uppercase hex
    // %x - u64    lowercase hex
    // %u - i64    unsigned integer
    // %i - i64    signed integer
    // %F - f64    64-bit float
    // %f - f32    32-bit float
    // %s - u8...  null-terminated utf-8 string (forbidden codepoints are dropped)
    // %c - u32    unicode codepoint (forbidden codepoints are dropped)
    //
    // escape codes:
    // \\ - backslash
    // \% - % character
    // \n, \r, \t - LF, CR, and horizontal tab characters
    //
    // vars is allowed to be null if no format specifiers are used
    // vars is a pointer to a list of pointers. these pointers are reinterpreted as pointers to the correct type
    // optionally, the last pointer in the list of pointers can be null, to signal that there are no more vars
    unsafe extern "C" fn print_fmt(cstring_bytes : *mut u8, mut vars : *mut *mut u8)
    {
        unsafe
        {
            let mut strlen = 0;
            while *cstring_bytes.add(strlen) != 0
            {
                strlen += 1;
            }
            let orig_string = String::from_utf8_lossy(std::slice::from_raw_parts(cstring_bytes, strlen));
            
            let mut s = "".to_string();
            
            let mut state = ' '; // ' ' - normal, '%' - in format specifier
            for c in orig_string.chars()
            {
                match state
                {
                    ' ' =>
                        match c
                        {
                            '%' => state = '%',
                            _ => s.push(c),
                        }
                    '%' =>
                    {
                        state = ' ';
                        if !(*vars).is_null()
                        {
                            match c
                            {
                                'X' => s.push_str(&format!("{:X}", *((*vars) as *mut u64))),
                                'x' => s.push_str(&format!("{:x}", *((*vars) as *mut u64))),
                                'u' => s.push_str(&format!("{}", *((*vars) as *mut u64))),
                                'i' => s.push_str(&format!("{}", *((*vars) as *mut i64))),
                                'F' => s.push_str(&format!("{}", *((*vars) as *mut f64))),
                                'f' => s.push_str(&format!("{}", *((*vars) as *mut f32))),
                                's' =>
                                {
                                    let mut strlen = 0;
                                    let cstring_bytes = *vars;
                                    while *cstring_bytes.add(strlen) != 0
                                    {
                                        strlen += 1;
                                    }
                                    let orig_string = String::from_utf8_lossy(std::slice::from_raw_parts(cstring_bytes, strlen));
                                    s.push_str(&orig_string);
                                }
                                'c' =>
                                    if let Some(c) = char::from_u32(*((*vars) as *mut u32))
                                    {
                                        s.push(c);
                                    }
                                _ =>
                                {
                                    s.push('%');
                                    s.push(c);
                                }
                            }
                            vars = vars.offset(1);
                        }
                    }
                    _ => panic!(),
                }
            }
            print!("{}", s);
        }
    }
    import_function::<unsafe extern "C" fn(*mut u8, *mut *mut u8)>(&types, &mut parser, &mut imports, "print_fmt", print_fmt, print_fmt as usize, "funcptr(void, (ptr(u8), ptr(ptr(u8))))");
    
    unsafe extern "C" fn print_str(cstring_bytes : *mut u8)
    {
        unsafe
        {
            let mut strlen = 0;
            while *cstring_bytes.add(strlen) != 0
            {
                strlen += 1;
            }
            let orig_string = String::from_utf8_lossy(std::slice::from_raw_parts(cstring_bytes, strlen));
            print!("{}", orig_string);
        }
    }
    import_function::<unsafe extern "C" fn(*mut u8)>(&types, &mut parser, &mut imports, "print_str", print_str, print_str as usize, "funcptr(void, (ptr(u8)))");
    
    unsafe extern "C" fn print_bytes(bytes : *mut u8, count : u64)
    {
        unsafe
        {
            for i in 0..count
            {
                print!("{:02X} ", *bytes.add(i as usize));
            }
            println!();
        }
    }
    import_function::<unsafe extern "C" fn(*mut u8, u64)>(&types, &mut parser, &mut imports, "print_bytes", print_bytes, print_bytes as usize, "funcptr(void, (ptr(u8), u64))");
    
    unsafe extern "C" fn print_float(a : f64)
    {
        println!("{}", a);
    }
    import_function::<unsafe extern "C" fn(f64)>(&types, &mut parser, &mut imports, "print_float", print_float, print_float as usize, "funcptr(void, (f64))");
    
    if VERBOSE
    {
        println!("startup done! time: {}", start.elapsed().as_secs_f64());
    }
    
    let mut executor = None;
    
    let mut func_decs = BTreeMap::new();
    let mut global_decs = BTreeMap::new();
    let mut constants = BTreeMap::new();
    
    let mut parse_time = 0.0f64;
    
    let config = inkwell::targets::InitializationConfig::default();
    
    let mut _target_data : Option<TargetData> = None;
    let mut cpu = settings.get("cpu").cloned().unwrap_or("".to_string());
    let mut features = "".to_string();
    if cpu == "native"
    {
        cpu = TargetMachine::get_host_cpu_name().to_string();
        features = TargetMachine::get_host_cpu_features().to_string();
    }
    
    let (triple, mut target_data, machine) = if let Some(triple_string) = settings.get("asm_triple")
    {
        inkwell::targets::Target::initialize_all(&config);
        let mut triple = TargetTriple::create(&triple_string);
        if triple_string == "native"
        {
            if settings.get("cpu").is_none()
            {
                cpu = TargetMachine::get_host_cpu_name().to_string();
                features = TargetMachine::get_host_cpu_features().to_string();
            }
            triple = TargetMachine::get_default_triple();
        }
        let target = Target::from_triple(&triple).unwrap();
        let machine = target.create_target_machine(&triple, &cpu, &features, opt_level, RelocMode::Default, CodeModel::Default).unwrap();
        _target_data = Some(machine.get_target_data());
        (triple, _target_data.as_ref().unwrap(), machine)
    }
    else
    {
        if settings.get("cpu").is_none()
        {
            cpu = TargetMachine::get_host_cpu_name().to_string();
            features = TargetMachine::get_host_cpu_features().to_string();
            inkwell::targets::Target::initialize_native(&config).unwrap();
        }
        else
        {
            inkwell::targets::Target::initialize_all(&config);
        }
        let triple = if let Some(triple_string) = settings.get("triple")
        {
            TargetTriple::create(&triple_string)
        }
        else
        {
            TargetMachine::get_default_triple()
        };
        let target = Target::from_triple(&triple).unwrap();
        let machine = target.create_target_machine(&triple, &cpu, &features, opt_level, RelocMode::Default, CodeModel::Default).unwrap();
        _target_data = Some(machine.get_target_data());
        (triple, _target_data.as_ref().unwrap(), machine)
    };
    if VERBOSE
    {
        println!("using: '{}' '{}' {:?}", cpu, features, triple);
    }
    
    let mut first_module = true;
    
    macro_rules! load_module
    {
        ($fname:expr) =>
        {{
            use std::fs;
            if VERBOSE
            {
                println!("loading {}...", $fname);
            }
            let program_text = fs::read_to_string($fname).unwrap();
            let program_lines : Vec<String> = program_text.lines().map(|x| x.to_string()).collect();
            
            let parse_start = std::time::Instant::now();
            
            let tokens = parser.tokenize(&program_lines, true).unwrap();
            let ast = parser.parse_program(&tokens, &program_lines, true).unwrap().unwrap();
            
            parse_time += parse_start.elapsed().as_secs_f64();
            
            let start = std::time::Instant::now();
            if VERBOSE
            {
                println!("compiling {}...", $fname);
            }
            
            let module = context.create_module("main");
            module.set_triple(&triple);
            let program = Program::new(&mut types, &ast).unwrap();
            
            if first_module
            {
                first_module = false;
                for (f_name, (_, funcsig)) in &imports
                {
                    let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                    let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::External));
                    func_val.as_global_value().set_dll_storage_class(inkwell::DLLStorageClass::Import);
                    func_decs.insert(f_name.clone(), (func_val, funcsig.clone()));
                }
            }
            
            if !skip_jit
            {
                if executor.is_none()
                {
                    let _executor = module.create_jit_execution_engine(opt_level).unwrap();
                    for (f_name, (pointer, _)) in &imports
                    {
                        _executor.add_global_mapping(&func_decs.get(f_name).unwrap().0, *pointer as usize);
                    }
                    executor = Some(_executor);
                    target_data = executor.as_ref().unwrap().get_target_data();
                }
                else
                {
                    executor.as_ref().unwrap().add_module(&module).unwrap();
                }
            }
            let ptr_int_type = context.ptr_sized_int_type(&target_data, None);
            
            module.set_data_layout(&target_data.get_data_layout());
            
            let intrinsic_imports = [
                ("sqrt", "llvm.sqrt", "funcptr(f64, (f64))", vec!["f64"]),
                ("sqrt_f32", "llvm.sqrt", "funcptr(f32, (f32))", vec!["f32"]),
                ("memset"    , "llvm.memset", "funcptr(void, (ptr(u8), u8, u64))", vec!["ptr", "i64"]),
                ("memset_vol", "llvm.memset", "funcptr(void, (ptr(u8), u8, u64))", vec!["ptr", "i64"]),
                ("memcpy"    , "llvm.memcpy", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
                ("memcpy_vol", "llvm.memcpy", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
                ("memmove"    , "llvm.memmove", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
                ("memmove_vol", "llvm.memmove", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
            ];
            
            let mut intrinsic_decs = BTreeMap::new();
            
            for (name, llvm_name, type_name, overloaded_args) in intrinsic_imports
            {
                let type_lines = vec!(type_name.to_string());
                let tokens = parser.tokenize(&type_lines, true).unwrap();
                let type_ast = parser.parse_with_root_node_type(&tokens, &type_lines, true, "type").unwrap().unwrap();
                let type_ = parse_type(&types, &type_ast).unwrap();
                if let TypeData::FuncPointer(funcsig) = type_.data
                {
                    let intrinsic = inkwell::intrinsics::Intrinsic::find(llvm_name).unwrap();
                    
                    let mut arg_types = Vec::new();
                    for arg_type in &overloaded_args
                    {
                        arg_types.push(backend_types.get(arg_type.clone()).unwrap().clone().try_into().unwrap());
                    }
                    let function = intrinsic.get_declaration(&module, &arg_types).unwrap();
                    
                    intrinsic_decs.insert(name.to_string(), (function, *funcsig.clone()));
                }
            }
            
            if VERBOSE
            {
                println!("individual module compile time: {}", start.elapsed().as_secs_f64());
                println!("adding global mappings...");
            }
            
            for (f_name, (function, visibility)) in &program.funcs
            {
                let linkage = match *visibility
                {
                    // expose as much as possible
                    // exact semantics are implementation-defined; may be a dll export!
                    Visibility::Export => inkwell::module::Linkage::External,
                    // expose to other modules and objects
                    Visibility::Local => inkwell::module::Linkage::External,
                    // do not expose to other modules
                    Visibility::Private => inkwell::module::Linkage::Internal,
                    _ =>
                        panic!("internal error, invalid visibility class for function definition"),
                };
                
                let storage_class = match *visibility
                {
                    Visibility::Import => panic!("internal error, invalid visibility class for function definition"),
                    // expose as much as possible
                    // exact semantics are implementation-defined; may be a dll export!
                    Visibility::Export => inkwell::DLLStorageClass::Export,
                    _ => inkwell::DLLStorageClass::Default,
                };
                
                let funcsig = function.to_sig();
                let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                let func_val = module.add_function(&f_name, func_type, Some(linkage));
                func_val.as_global_value().set_dll_storage_class(storage_class);
                func_decs.insert(f_name.clone(), (func_val, funcsig.clone()));
            }
            for (f_name, (funcsig, visibility)) in &program.func_imports
            {
                let linkage = match *visibility
                {
                    // get from anywhere possible
                    // exact semantics are implementation-defined; may be a dll import!
                    Visibility::Import => inkwell::module::Linkage::External,
                    // get from external module or object
                    Visibility::ImportLocal => inkwell::module::Linkage::External,
                    _ => panic!("internal error, invalid visibility class for function import"),
                };
                
                let storage_class = match *visibility
                {
                    // get from anywhere possible
                    // exact semantics are implementation-defined; may be a dll import!
                    Visibility::Import => inkwell::DLLStorageClass::Import,
                    Visibility::Export => panic!("internal error, invalid visibility class for function import"),
                    _ => inkwell::DLLStorageClass::Default,
                };
                let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                let func_val = module.add_function(&f_name, func_type, Some(linkage));
                func_val.as_global_value().set_dll_storage_class(storage_class);
                func_decs.insert(f_name.clone(), (func_val, funcsig.clone()));
            }
            
            for g_name in &program.globals_order
            {
                if let Some((g_type, g_init, visibility)) = program.globals.get(g_name)
                {
                    let backend_type = get_backend_type(&mut backend_types, &types, &g_type);
                    let basic_type = inkwell::types::BasicTypeEnum::try_from(backend_type)
                        .unwrap_or_else(|()| panic!("internal error: tried to make global with unsized type"));
                    let linkage = match *visibility
                    {
                        // get from anywhere possible
                        // exact semantics are implementation-defined; may be a dll import!
                        Visibility::Import => inkwell::module::Linkage::External,
                        // get from external module or object
                        Visibility::ImportLocal => inkwell::module::Linkage::External,
                        // expose as much as possible
                        // exact semantics are implementation-defined; may be a dll export!
                        Visibility::Export => inkwell::module::Linkage::External,
                        // expose to other modules and objects
                        Visibility::Local => inkwell::module::Linkage::External,
                        // do not expose to other modules
                        Visibility::Private => inkwell::module::Linkage::Internal,
                    };
                    
                    let storage_class = match *visibility
                    {
                        // get from anywhere possible
                        // exact semantics are implementation-defined; may be a dll import!
                        Visibility::Import => inkwell::DLLStorageClass::Import,
                        // expose as much as possible
                        // exact semantics are implementation-defined; may be a dll export!
                        Visibility::Export => inkwell::DLLStorageClass::Export,
                        _ => inkwell::DLLStorageClass::Default,
                    };
                    
                    if let Some(node) = g_init
                    {
                        let f_name = format!("__init_global_{}_asdf1g0q", g_name);
                        let funcsig = FunctionSig { return_type : type_table[0].1.clone(), args : Vec::new() };
                        let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                        let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::Internal));
                        
                        let entry_block = context.append_basic_block(func_val, "entry");
                        let builder = context.create_builder();
                        builder.position_at_end(entry_block);
                        
                        let body_block = context.append_basic_block(func_val, "body");
                        builder.position_at_end(body_block);
                        
                        let stack = Vec::new();
                        let blocks = HashMap::new();
                        let mut env = Environment { source_text : &program_lines, module : &module, context : &context, stack, variables : BTreeMap::new(), constants : constants.clone(), builder : &builder, func_decs : &func_decs, global_decs : &global_decs, intrinsic_decs : &intrinsic_decs, types : &types, backend_types : &mut backend_types, function_types : &mut function_types, func_val, blocks, entry_block, ptr_int_type, target_data, anon_globals : HashMap::new(), return_type : None, hoisted_return : None, just_returned : false, options : env_options };
                        
                        compile(&mut env, &node, WantPointer::None);
                        let (type_val, val) = env.stack.pop().unwrap();
                        assert!(type_val == *g_type);
                        
                        if val_is_const(true, val)
                        {
                            let global = module.add_global(basic_type, None, g_name);
                            if g_type.is_composite() && val.as_instruction_value().is_none()
                            {
                                let val = *env.anon_globals.get(&val.into_pointer_value()).unwrap();
                                global.set_initializer(&val);
                            }
                            else if g_type.is_composite()
                            {
                                panic!("INTERNAL ERROR THIS SHOULD BE UNREACHABLE PLEASE REPORT 28753489");
                            }
                            else
                            {
                                global.set_initializer(&val);
                            }
                            global.set_linkage(linkage);
                            global.set_dll_storage_class(storage_class);
                            env.builder.build_return(None).unwrap();
                            
                            global_decs.insert(g_name.clone(), (g_type.clone(), global, None));
                        }
                        else
                        {
                            let global = module.add_global(basic_type, None, g_name);
                            global.set_initializer(&basic_type.as_basic_type_enum().const_zero());
                            global.set_linkage(linkage);
                            global.set_dll_storage_class(storage_class);
                            env.builder.build_store(global.as_pointer_value().into(), val).unwrap();
                            env.builder.build_return(None).unwrap();
                            
                            global_decs.insert(g_name.clone(), (g_type.clone(), global, Some(func_val)));
                        }
                        
                        builder.position_at_end(entry_block);
                        builder.build_unconditional_branch(body_block).unwrap();
                    }
                    else
                    {
                        let global = module.add_global(basic_type, None, g_name);
                        global.set_initializer(&basic_type.as_basic_type_enum().const_zero());
                        global.set_linkage(linkage);
                        global.set_dll_storage_class(storage_class);
                        global_decs.insert(g_name.clone(), (g_type.clone(), global, None));
                    }
                }
                else if let Some((g_type, node)) = program.constants.get(g_name)
                {
                    let backend_type = get_backend_type(&mut backend_types, &types, &g_type);
                    inkwell::types::BasicTypeEnum::try_from(backend_type).unwrap_or_else(|()| panic!("internal error: tried to make global constexpr with unsized type"));
                    
                    let f_name = format!("__init_const_{}_asdf1g0q", g_name);
                    let funcsig = FunctionSig { return_type : type_table[0].1.clone(), args : Vec::new() };
                    let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                    let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::Internal));
                    
                    let entry_block = context.append_basic_block(func_val, "entry");
                    let builder = context.create_builder();
                    builder.position_at_end(entry_block);
                    
                    let body_block = context.append_basic_block(func_val, "body");
                    builder.position_at_end(body_block);
                    
                    let stack = Vec::new();
                    let blocks = HashMap::new();
                    let mut env = Environment { source_text : &program_lines, module : &module, context : &context, stack, variables : BTreeMap::new(), constants : constants.clone(), builder : &builder, func_decs : &func_decs, global_decs : &global_decs, intrinsic_decs : &intrinsic_decs, types : &types, backend_types : &mut backend_types, function_types : &mut function_types, func_val, blocks, entry_block, ptr_int_type, target_data, anon_globals : HashMap::new(), return_type : None, hoisted_return : None, just_returned : false, options : env_options };
                    
                    compile(&mut env, &node, WantPointer::None);
                    let (type_val, val) = env.stack.pop().unwrap();
                    assert!(type_val == *g_type);
                    
                    if !val_is_const(true, val)
                    {
                        panic!("error: tried to make global constexpr with non-constexpr expression")
                    }
                    env.builder.build_return(None).unwrap();
                    constants.insert(g_name.clone(), (g_type.clone(), val));
                    
                    builder.position_at_end(entry_block);
                    builder.build_unconditional_branch(body_block).unwrap();
                }
            }
            
            if global_decs.len() > 0
            {
                let f_name = format!("__init_allglobal_asdf3f6g");
                let funcsig = FunctionSig { return_type : type_table[0].1.clone(), args : Vec::new() };
                let func_type = get_function_type(&mut function_types, &mut backend_types, &types, &funcsig, &env_options);
                let func_val = module.add_function(&f_name, func_type, Some(inkwell::module::Linkage::Internal));
                
                let block = context.append_basic_block(func_val, "entry");
                let builder = context.create_builder();
                builder.position_at_end(block);
                
                for (_, (_, _, f)) in &global_decs
                {
                    if let Some(f) = f
                    {
                        builder.build_direct_call(*f, &Vec::new(), "").unwrap();
                    }
                }
                builder.build_return(None).unwrap();
                
                let s_type = context.struct_type(&[context.i32_type().into(), ptr_type.into(), ptr_type.into()], false);
                let a_type = s_type.array_type(1);
                let ctors = module.add_global(a_type, Some(inkwell::AddressSpace::default()), "llvm.global_ctors");
                ctors.set_linkage(inkwell::module::Linkage::Appending);
                let s_val = s_type.const_named_struct(&[context.i32_type().const_int(65535, false).into(), func_val.as_global_value().as_pointer_value().into(), ptr_type.const_null().into()]);
                let a_val = s_type.const_array(&[s_val]);
                ctors.set_initializer(&a_val);
                // @llvm.global_ctors = appending global [1 x { i32, ptr, ptr }] [{ i32, ptr, ptr } { i32 65535, ptr @__init_allglobal_asdf3f6g, ptr null }

                //"llvm.global_ctors"
            }
            
            for f in &program.funcs
            {
                let f_name = f.0;
                let function = &f.1.0;
                
                let (func_val, _) = func_decs.get(f_name).unwrap().clone();
                
                let entry_block = context.append_basic_block(func_val, "entry");
                let builder = context.create_builder();
                builder.position_at_end(entry_block);
                
                let mut variables = BTreeMap::new();
                
                // declare and define arguments
                let mut i = 0;
                for (j, param) in function.args.iter().enumerate()
                {
                    let var_type = param.0.clone();
                    let var_name = param.1.clone();
                    
                    let backend_type = get_backend_type(&mut backend_types, &types, &var_type);
                    if let Ok(basic_type) = inkwell::types::BasicTypeEnum::try_from(backend_type)
                    {
                        if var_type.is_composite() && !env_options.contains_key("bypass_agg_memcpy")
                        {
                            let slot = builder.build_alloca(basic_type, &var_name).unwrap();
                            let len = basic_type.size_of().unwrap().into();
                            
                            let val = func_val.get_nth_param(j as u32).unwrap();
                            build_memcpy!(builder, module, slot, val, len, false).unwrap();
                            
                            if variables.contains_key(&var_name)
                            {
                                panic!("error: parameter {} redeclared", var_name);
                            }
                            variables.insert(var_name, (var_type, slot));
                        }
                        else
                        {
                            let slot = builder.build_alloca(basic_type, &var_name).unwrap();
                            
                            let val = func_val.get_nth_param(j as u32).unwrap();
                            builder.build_store(slot, val).unwrap();
                            
                            if variables.contains_key(&var_name)
                            {
                                panic!("error: parameter {} redeclared", var_name);
                            }
                            variables.insert(var_name, (var_type, slot));
                        }
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
                        
                        let basic_type = get_backend_type_sized(&mut backend_types, &types, &var_type);
                        
                        let slot = if let TypeData::Array(inner_type, size) = &var_type.data
                        {
                            let size = get_backend_type_sized(&mut backend_types, &types, types.get("u64").unwrap()).into_int_type().const_int(*size as u64, false);
                            let inner_basic_type = get_backend_type_sized(&mut backend_types, &types, inner_type);
                            builder.build_array_alloca(inner_basic_type, size, &var_name)
                        }
                        else
                        {
                            builder.build_alloca(basic_type, &var_name)
                        }.unwrap();
                        
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
                function.body.visit(&mut |node : &ASTNode|
                {
                    if node.is_parent() && node.text == "label"
                    {
                        let name = &node.child(0).unwrap().child(0).unwrap().text;
                        if !blocks.contains_key(name)
                        {
                            blocks.insert(name.clone(), context.append_basic_block(func_val, name));
                        }
                        else
                        {
                            panic!("error: redeclared block {}", name);
                        }
                    }
                    false
                });
                
                let mut hoisted_return = None;
                if function.return_type.is_composite() && !env_options.contains_key("bypass_agg_memcpy")
                {
                    let val = func_val.get_last_param().unwrap();
                    hoisted_return = Some((function.return_type.clone(), val.into_pointer_value()));
                }
                
                let stack = Vec::new();
                let mut env = Environment { source_text : &program_lines, module : &module, context : &context, stack, variables, constants : constants.clone(), builder : &builder, func_decs : &func_decs, global_decs : &global_decs, intrinsic_decs : &intrinsic_decs, types : &types, backend_types : &mut backend_types, function_types : &mut function_types, func_val, blocks, entry_block, ptr_int_type, target_data, anon_globals : HashMap::new(), return_type : Some(function.return_type.clone()), hoisted_return, just_returned : false, options : env_options };
                
                //println!("\n\ncompiling function {}...", function.name);
                //println!("{}", function.body.pretty_debug());
                
                let body_block = context.append_basic_block(func_val, "body");
                builder.position_at_end(body_block);
                
                compile(&mut env, &function.body, WantPointer::None);
                
                builder.position_at_end(entry_block);
                builder.build_unconditional_branch(body_block).unwrap();
            }
            
            if let Err(err) = module.verify()
            {
                println!("Internal compiler error:\n{}", err.to_string());
                //panic!();
            }
            
            module
        }};
    }
    
    let mut loaded_modules = Vec::new();
    for arg in &modules
    {
        loaded_modules.push(load_module!(arg));
    }
    
    if DEBUG_FIRST_MODULE
    {
        loaded_modules[0].print_to_file("out_unopt.ll").unwrap();
    }
    
    if VERBOSE
    {
        let comptime = start.elapsed().as_secs_f64();
        println!("compilation time: {}", comptime);
    }
    
    let start = std::time::Instant::now();
    if VERBOSE
    {
        println!("doing IR optimizations...");
    }
    for module in &loaded_modules
    {
        let pass_options = PassBuilderOptions::create();
        pass_options.set_verify_each(true);
        pass_options.set_loop_interleaving(true);
        pass_options.set_loop_vectorization(true);
        pass_options.set_loop_slp_vectorization(true);
        pass_options.set_loop_unrolling(true);
        pass_options.set_forget_all_scev_in_loop_unroll(true);
        pass_options.set_licm_mssa_opt_cap(1);
        pass_options.set_licm_mssa_no_acc_for_promotion_cap(10);
        pass_options.set_call_graph_profile(true);
        pass_options.set_merge_functions(true);
        
        module.run_passes("default<O3>", &machine, pass_options).unwrap();
    }
    
    if VERBOSE
    {
        println!("done doing IR optimizations. time: {}", start.elapsed().as_secs_f64());
    }
    
    if DEBUG_FIRST_MODULE
    {
        loaded_modules[0].print_to_file("out.ll").unwrap();
    }
    
    if VERBOSE || PRINT_COMP_TIME
    {
        let comptime = true_start.elapsed().as_secs_f64();
        println!("full compilation time: {}ms", (comptime) * 1000.0);
        println!("parse time (included in comp time): {}ms", (parse_time) * 1000.0);
        println!("compilation time without parsing: {}ms", (comptime - parse_time) * 1000.0);
    }
    
    if !skip_jit
    {
        let executor = executor.unwrap();
        
        macro_rules! get_func
        {
            ($name:expr, $T:ty) =>
            {{
                let dec = func_decs.get(&$name.to_string());
                if dec.is_none()
                {
                    panic!("error: no `{}` function", $name);
                }
                let dec = dec.unwrap();
                let type_string = dec.1.to_string_rusttype();
                
                let want_type_string = std::any::type_name::<$T>(); // FIXME: not guaranteed to be stable across rust versions
                assert!(want_type_string == type_string, "types do not match:\n{}\n{}\n", want_type_string, type_string);
                assert!(want_type_string.starts_with("unsafe "), "function pointer type must be unsafe");
                
                executor.get_function::<$T>(&$name).unwrap()
            }}
        }
        
        executor.run_static_constructors();
        
        unsafe
        {
            let name = "main";
            let f = get_func!(name, unsafe extern "C" fn());
            
            let start = std::time::Instant::now();
            if VERBOSE
            {
                println!("running {}...", name);
            }
            let out = f.call();
            let elapsed_time = start.elapsed();
            
            if VERBOSE
            {
                println!("{}() = {:?}", name, out);
                println!("time: {}", elapsed_time.as_secs_f64());
            }
        }
        
        executor.run_static_destructors();
    }

    if skip_jit
    {
        use inkwell::targets::*;
        machine.write_to_file(&loaded_modules[0], FileType::Assembly, "out.asm".as_ref()).unwrap();
    }
    if VERBOSE
    {
        println!("Finished gracefully.");
    }
    
}

use std::env;
fn main()
{
    let mut modules = Vec::new();
    let mut args = Vec::new();
    let mut settings = HashMap::new();
    let mut mode = "";
    for arg in env::args().skip(1)
    {
        if mode == "-i" || (mode == "" && !arg.starts_with("-"))
        {
            modules.push(arg);
            mode = "";
        }
        else if mode == "--"
        {
            args.push(arg);
        }
        else if mode == "-oat"
        {
            settings.insert("asm_triple", arg);
            mode = "";
        }
        else if mode == "-cpu"
        {
            settings.insert("cpu", arg);
            mode = "";
        }
        else if mode == "-ft"
        {
            settings.insert("triple", arg);
            mode = ""
        }
        else if mode == "-sag"
        {
            settings.insert("simple_aggregates", arg);
            mode = "";
        }
        else
        {
            match arg.as_str()
            {
                "-i" => mode = "-i",
                "--" => mode = "--",
                "-oat" => mode = "-oat",
                "-cpu" => mode = "-cpu",
                "--output-assembly-triple" => mode = "-oat",
                "-ft" => mode = "-ft",
                "--force-triple" => mode = "-ft",
                "-sag" => mode = "-sag",
                "--simple-aggregates" => mode = "-sag",
                _ => panic!("unknown argument `{}`", arg),
            }
        }
    }
    if modules.len() == 0
    {
        println!("Usage:");
        println!("konoran <source_file> -- <arguments>");
    }
    else
    {
        run_program(modules, args, settings);
    }
}
