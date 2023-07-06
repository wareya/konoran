extern crate alloc;

use alloc::collections::{BTreeMap, BTreeSet};

use inkwell::context::Context;
use inkwell::execution_engine::JitFunction;
use inkwell::OptimizationLevel;

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
    fn to_signature(&self) -> Signature
    {
        let mut signature = inkwell::types::FunctionType.make_signature();
        
        for var_type in &self.args
        {
            if var_type.is_void()
            {
                panic!("error: void function arguments are not allowed");
            }
            let var_abi = var_type.to_abi();
            if var_abi.is_none()
            {
                let name = &var_type.name;
                panic!("error: non-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", name, name);
            }
            signature.params.push(var_abi.unwrap().clone());
        }
        
        if !self.return_type.is_void()
        {
            let return_abi = self.return_type.to_abi();
            if return_abi.is_none()
            {
                let name = &self.return_type.name;
                panic!("error: funcsig-primitive type {} can't be used in function arguments or return types. use a `ptr({})` instead", name, name);
            }
            signature.returns.push(return_abi.unwrap().clone());
        }
        //println!("made func sig {:?}", signature);
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

struct Environment<'a, 'b, 'c, 'e>
{
    stack      : &'a mut Vec<(Type, Option<Value>, Option<PointerValue<'c>>)>,
    variables  : &'a mut BTreeMap<String, (Type, PointerValue<'c>, Option<Value>)>,
    builder    : &'b mut inkwell::builder::Builder<'c>,
    module     : &'e mut inkwell::module::Module,
    funcrefs   : &'a BTreeMap<String, (FunctionSig, FuncRef)>,
    types      : &'a BTreeMap<String, (Type, Box<inkwell::types::AnyType>)>,
    blocks     : &'a BTreeMap<String, Block>,
    next_block : &'a BTreeMap<Block, Block>,
    const_vars : BTreeMap<String, (Type, Option<Value>)>,
}

impl<'a, 'b, 'c, 'e> Environment<'a, 'b, 'c, 'e>
{
    fn stack_push(&mut self, stuff : (Type, Value))
    {
        self.stack.push((stuff.0, Some(stuff.1), None))
    }
    fn stack_pop(&mut self) -> Option<(Type, Value)>
    {
        let stuff = self.stack.pop().unwrap();
        Some((stuff.0, stuff.1.unwrap()))
    }
}


#[derive(Clone, Debug, Copy, PartialEq)]
enum WantPointer {
    None,
    Real,
    Virtual,
}

fn compile<'a, 'b>(env : &'a mut Environment, node : &'b ASTNode, want_pointer : WantPointer)
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
            /*
            "intrinsic_memcmp" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let (  len_type,   len_val) = env.stack_pop().unwrap();
                let (right_type, right_val) = env.stack_pop().unwrap();
                let ( left_type,  left_val) = env.stack_pop().unwrap();
                
                if left_type.is_pointer() && right_type.is_pointer() && len_type.name == "u64"
                {
                    let config = env.module.target_config();
                    let value = env.builder.call_memcmp(config, left_val, right_val, len_val);
                    env.stack_push((env.types.get("i32").unwrap().clone(), value));
                }
                else
                {
                    panic!("incompatible types for intrinsic_memcpy (must be ptr, ptr, u64)");
                }
            }
            "intrinsic_memset" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let (  val_type,   val_val) = env.stack_pop().unwrap();
                let (right_type, right_val) = env.stack_pop().unwrap();
                let ( left_type,  left_val) = env.stack_pop().unwrap();
                
                if left_type.is_pointer() && right_type.is_pointer() && val_type.name == "u8"
                {
                    let config = env.module.target_config();
                    env.builder.call_memset(config, left_val, right_val, val_val);
                }
                else
                {
                    panic!("incompatible types for intrinsic_memset (must be ptr, ptr, u8)");
                }
            }
            "call_memcpy" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let ( size_type,  size_val) = env.stack_pop().unwrap();
                let (right_type, right_val) = env.stack_pop().unwrap();
                let ( left_type,  left_val) = env.stack_pop().unwrap();
                
                if left_type.is_pointer() && right_type.is_pointer() && size_type.name == "u64"
                {
                    let config = env.module.target_config();
                    env.builder.call_memcpy(config, left_val, right_val, size_val);
                }
                else
                {
                    panic!("incompatible types for call_memcpy (must be ptr, ptr, u64)");
                }
            }
            "call_memmove" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let ( size_type,  size_val) = env.stack_pop().unwrap();
                let (right_type, right_val) = env.stack_pop().unwrap();
                let ( left_type,  left_val) = env.stack_pop().unwrap();
                
                if left_type.is_pointer() && right_type.is_pointer() && size_type.name == "u64"
                {
                    let config = env.module.target_config();
                    env.builder.call_memmove(config, left_val, right_val, size_val);
                }
                else
                {
                    panic!("incompatible types for call_memmove (must be ptr, ptr, u64)");
                }
            }
            */
            "return" =>
            {
                let mut returns = Vec::new();
                // FIXME: check types
                for child in node.get_children().unwrap()
                {
                    compile(env, child, WantPointer::None);
                    returns.push(env.stack_pop().unwrap().1);
                }
                env.builder.ins().return_(&returns);
            }
            /*
            "declaration" =>
            {
                return;
            }
            "fulldeclaration" =>
            {
                let name = &node.child(1).unwrap().child(0).unwrap().text;
                if env.variables.contains_key(name)
                {
                    let (type_var, slot, _) = env.variables[name].clone();
                    
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let (type_val, val) = env.stack_pop().unwrap();
                    
                    assert!(type_val == type_var);
                    
                    if !type_var.is_composite()
                    {
                        env.builder.ins().stack_store(val, slot, 0);
                    }
                    else
                    {
                        let slot_addr = env.builder.ins().stack_addr(types::I64, slot, 0);
                        let config = env.module.target_config();
                        //println!("uh... {:?} {}", type_val, type_val.size());
                        let size = env.builder.ins().iconst(types::I64, type_val.size() as i64);
                        env.builder.call_memcpy(config, slot_addr, val, size);
                        //let align = type_val.array_to_inner().align() as u8;
                        //env.builder.emit_small_memory_copy(config, slot_addr, val, type_val.size() as u64, align, align, true, MemFlags::trusted());
                    }
                }
                else
                {
                    panic!("internal error: failed to find variable in full declaration");
                }
            }
            "constdeclaration" =>
            {
                let name = &node.child(1).unwrap().child(0).unwrap().text;
                if env.const_vars.contains_key(name)
                {
                    let (type_var, _) = env.const_vars[name].clone();
                    if type_var.is_struct() || type_var.is_array()
                    {
                        panic!("error: cannot declare arrays or structs as const; use a normal non-const variable");
                    }
                    
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let (type_val, val) = env.stack_pop().unwrap();
                    
                    assert!(type_val == type_var);
                    
                    // store-cycle the value in an anonymous/dummy stack slot to prevent cranelift from recalculating it on every use
                    let slot = env.builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size : type_var.size() });
                    env.builder.ins().stack_store(val, slot, 0);
                    let val = env.builder.ins().stack_load(type_var.to_cranetype().unwrap(), slot, 0);
                    
                    env.const_vars.get_mut(name).as_mut().unwrap().1 = Some(val);
                }
                else
                {
                    panic!("internal error: failed to find variable in const declaration");
                }
            }
            "binstate" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(2).unwrap(), WantPointer::None);
                
                let (type_val, val) = env.stack_pop().unwrap();
                let (type_left_incomplete, left_addr, left_slot) = env.stack.pop().unwrap();
                
                for name in env.variables.keys()
                {
                    let slot = env.variables[name].1;
                    if left_slot.is_some() && slot == left_slot.unwrap()
                    {
                        if env.const_vars.contains_key(name)
                        {
                            panic!("error: can't reassign to const variables");
                        }
                        break;
                    }
                }
                
                if left_addr.is_some() && type_left_incomplete.is_virtual_pointer()
                {
                    println!("storing to virtual pointer!!!");
                    let type_left = type_left_incomplete.deref_vptr();
                    assert!(type_val == type_left);
                    env.builder.ins().store(MemFlags::trusted(), val, left_addr.unwrap(), 0);
                }
                else if left_addr.is_some() && type_left_incomplete.is_composite()
                {
                    println!("storing to composite!!!");
                    let type_left = type_left_incomplete;
                    assert!(type_val == type_left);
                    
                    let config = env.module.target_config();
                    let size = env.builder.ins().iconst(types::I64, type_left.size() as i64);
                    env.builder.call_memcpy(config, left_addr.unwrap(), val, size);
                }
                else if left_addr.is_none() && left_slot.is_some()
                {
                    println!("storing direct to stack!!!");
                    let type_left = type_left_incomplete;
                    assert!(type_val == type_left);
                    env.builder.ins().stack_store(val, left_slot.unwrap(), 0);
                }
                else
                {
                    panic!("tried to assign to fully evaluated expression (not a variable or pointer) {:?}", type_left_incomplete);
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
                    let type_ = env.variables[name].0.clone();
                    let will_want_addr =
                        want_pointer == WantPointer::Real ||
                        (want_pointer == WantPointer::Virtual && (type_.is_struct() || type_.is_array())
                        ) ||
                        type_.is_struct() || type_.is_array()
                    ;
                    if will_want_addr && env.variables[name].2.is_none()
                    {
                        let addr = env.builder.ins().stack_addr(types::I64, env.variables[name].1, 0);
                        env.variables.get_mut(name).as_mut().unwrap().2 = Some(addr);
                    }
                    let (_, slot, addr) = &env.variables[name];
                    
                    if want_pointer == WantPointer::Real
                    {
                        env.stack_push((type_.to_ptr(), addr.unwrap()));
                    }
                    else if type_.is_struct() || type_.is_array()
                    {
                        env.stack_push((type_.clone(), addr.unwrap()));
                    }
                    else
                    {
                        println!("slot!!!!!");
                        env.stack.push((type_.clone(), None, Some(*slot)));
                    }
                }
                else if env.const_vars.contains_key(name)
                {
                    panic!("error: can't reassign to constant variable `{}`", name);
                }
                else
                {
                    panic!("error: unrecognized variable `{}`", name);
                }
            }
            "rvarname" =>
            {
                let name = &node.child(0).unwrap().text;
                //println!("{}", name);
                if env.variables.contains_key(name)
                {
                    let type_ = env.variables[name].0.clone();
                    let will_want_addr =
                        want_pointer == WantPointer::Real ||
                        want_pointer == WantPointer::Virtual ||
                        type_.is_struct() || type_.is_array()
                    ;
                    if will_want_addr && env.variables[name].2.is_none()
                    {
                        let addr = env.builder.ins().stack_addr(types::I64, env.variables[name].1, 0);
                        env.variables.get_mut(name).as_mut().unwrap().2 = Some(addr);
                    }
                    let (_, slot, addr) = &env.variables[name];
                    
                    // FIXME: make this universal somehow (currently semi duplicated with indirection_head)
                    if want_pointer == WantPointer::Real
                    {
                        env.stack_push((type_.to_ptr(), addr.unwrap()));
                    }
                    else if want_pointer == WantPointer::Virtual
                    {
                        env.stack_push((type_.to_vptr(), addr.unwrap()));
                    }
                    else if type_.is_struct() || type_.is_array()
                    {
                        env.stack_push((type_.clone(), addr.unwrap()));
                    }
                    else
                    {
                        let val = env.builder.ins().stack_load(type_.to_cranetype().unwrap(), *slot, 0);
                        env.stack_push((type_.clone(), val));
                    }
                }
                else if env.const_vars.contains_key(name)
                {
                    let (type_, val) = &env.const_vars[name];
                    if want_pointer != WantPointer::None
                    {
                        panic!("error: tried to access a const variable in a way that involves its address; use a non-const variable");
                    }
                    else if val.is_some()
                    {
                        env.stack_push((type_.clone(), val.unwrap()));
                    }
                    else
                    {
                        panic!("error: tried to read from const variable before it has been assigned");
                    }
                }
                else if env.funcrefs.contains_key(name)
                {
                    let (funcsig, funcref) = env.funcrefs.get(name).unwrap();
                    let funcaddr = env.builder.ins().func_addr(types::I64, *funcref);
                    env.stack_push((Type::from_functionsig(funcsig), funcaddr));
                }
                else
                {
                    panic!("unrecognized identifier {}", name);
                }
            }
            "arrayindex_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                compile(env, node.child(1).unwrap(), WantPointer::None);
                let (offset_type, offset_val) = env.stack_pop().unwrap();
                let (base_type, base_addr) = env.stack_pop().unwrap();
                
                if offset_type.name == "i64"
                {
                    // TODO: do multi-level accesses in a single load operation instead of several
                    // FIXME: double check that nested types work properly
                    let inner_type = base_type.array_to_inner();
                    let inner_size = inner_type.aligned_size();
                    let inner_offset = env.builder.ins().imul_imm(offset_val, inner_size as i64);
                    let inner_addr = env.builder.ins().iadd(base_addr, inner_offset);
                    
                    // FIXME: make this universal somehow (currently semi duplicated with indirection_head)
                    if want_pointer == WantPointer::Real
                    {
                        env.stack_push((inner_type.to_ptr(), inner_addr));
                    }
                    else if want_pointer == WantPointer::Virtual
                    {
                        env.stack_push((inner_type.to_vptr(), inner_addr));
                    }
                    else if inner_type.is_struct() || inner_type.is_array()
                    {
                        env.stack_push((inner_type.clone(), inner_addr));
                    }
                    else
                    {
                        let val = env.builder.ins().load(inner_type.to_cranetype().unwrap(), MemFlags::trusted(), inner_addr, 0);
                        env.stack_push((inner_type.clone(), val));
                    }
                }
                else
                {
                    panic!("error: can't offset into arrays except with type i64 (used type `{}`)", offset_type.name)
                }
            }
            "indirection_head" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (struct_type, struct_addr) = env.stack_pop().unwrap();
                
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
                    if want_pointer == WantPointer::Real
                    {
                        let offset = env.builder.ins().iconst(types::I64, found.2 as i64);
                        let inner_addr = env.builder.ins().iadd(struct_addr, offset);
                        env.stack_push((inner_type.to_ptr(), inner_addr));
                    }
                    else if want_pointer == WantPointer::Virtual
                    {
                        let offset = env.builder.ins().iconst(types::I64, found.2 as i64);
                        let inner_addr = env.builder.ins().iadd(struct_addr, offset);
                        env.stack_push((inner_type.to_vptr(), inner_addr));
                    }
                    else if inner_type.is_struct() || inner_type.is_array()
                    {
                        let offset = env.builder.ins().iconst(types::I64, found.2 as i64);
                        let inner_addr = env.builder.ins().iadd(struct_addr, offset);
                        env.stack_push((inner_type.clone(), inner_addr));
                    }
                    else
                    {
                        let val = env.builder.ins().load(inner_type.to_cranetype().unwrap(), MemFlags::trusted(), struct_addr, found.2 as i32);
                        env.stack_push((inner_type.clone(), val));
                    }
                }
                else
                {
                    panic!("error: no such property {} in struct type {}", right_name, struct_type.name);
                }
            }
            "funcargs_head" =>
            {
                //println!("compiling func call");
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, funcaddr) = env.stack_pop().unwrap();
                match type_.data
                {
                    TypeData::FuncPointer(funcsig) =>
                    {
                        let sig = funcsig.to_signature(&env.module);
                        let sigref = env.builder.import_signature(sig);
                        
                        compile(env, node.child(1).unwrap(), WantPointer::None);
                        let num_args = node.child(1).unwrap().child_count().unwrap();
                        
                        let mut args = Vec::new();
                        for (i, arg_type) in funcsig.args.iter().rev().enumerate()
                        {
                            let (type_, val) = env.stack_pop().unwrap();
                            if type_ != *arg_type
                            {
                                panic!("mismatched types for parameter {} in call to function: expected `{}`, got `{}`", i+1, arg_type.to_string(), type_.to_string());
                            }
                            args.push(val);
                        }
                        
                        args.reverse();
                        
                        //println!("calling func with sigref {} and sig {}", sigref, funcsig.to_string());
                        let inst = env.builder.ins().call_indirect(sigref, funcaddr, &args);
                        let results = env.builder.inst_results(inst);
                        //println!("number of results {}", results.len());
                        for (result, type_) in results.iter().zip([funcsig.return_type])
                        {
                            env.stack.push((type_.clone(), Some(*result), None));
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
                println!("array length: {}", array_length);
                let mut vals = Vec::new();
                let mut element_type = None;
                
                for _ in 0..array_length
                {
                    let (type_, val) = env.stack_pop().unwrap();
                    if element_type.is_none()
                    {
                        element_type = Some(type_.clone());
                    }
                    if Some(type_.clone()) != element_type
                    {
                        panic!("error: array literals must entirely be of a single type");
                    }
                    vals.insert(0, val);
                }
                if element_type.is_some()
                {
                    let element_type = element_type.unwrap();
                    let array_type = element_type.to_array(array_length);
                    let size = array_type.size();
                    let slot = env.builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size });
                    let mut offset = 0;
                    for val in vals
                    {
                        // FIXME: this is unsustainable
                        if element_type.is_composite()
                        {
                            let config = env.module.target_config();
                            let size = env.builder.ins().iconst(types::I64, element_type.size() as i64);
                            let slot_addr = env.builder.ins().stack_addr(types::I64, slot, offset);
                            env.builder.call_memcpy(config, slot_addr, val, size);
                        }
                        else
                        {
                            env.builder.ins().stack_store(val, slot, offset);
                        }
                        offset += element_type.aligned_size() as i32;
                    }
                    
                    let addr = env.builder.ins().stack_addr(types::I64, slot, 0);
                    env.stack_push((array_type, addr));
                }
                else
                {
                    panic!("error: zero-length array literals are not allowed");
                }
            }*/
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
                        env.stack_push((env.types.get("f32").unwrap().clone(), res));
                    }
                    "f64" =>
                    {
                        let val : f64 = text.parse().unwrap();
                        let res = env.builder.ins().f64const(val);
                        env.stack_push((env.types.get("f64").unwrap().clone(), res));
                    }
                    _ => panic!("unknown float suffix pattern {}", parts.1)
                }
            }
            /*
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
                let res = match parts.1
                {
                    // FIXME: check if we need to use sign extension (probably don't)
                    "u8"  => env.builder.ins().iconst(types::I8 , text.parse::<u8>() .unwrap() as i64),
                    "u16" => env.builder.ins().iconst(types::I16, text.parse::<u16>().unwrap() as i64),
                    "u32" => env.builder.ins().iconst(types::I32, text.parse::<u32>().unwrap() as i64),
                    "u64" => env.builder.ins().iconst(types::I64, text.parse::<u64>().unwrap() as i64),
                    "i8"  => env.builder.ins().iconst(types::I8 , text.parse::<i8>() .unwrap() as i64),
                    "i16" => env.builder.ins().iconst(types::I16, text.parse::<i16>().unwrap() as i64),
                    "i32" => env.builder.ins().iconst(types::I32, text.parse::<i32>().unwrap() as i64),
                    "i64" => env.builder.ins().iconst(types::I64, text.parse::<i64>().unwrap() as i64),
                    _ => panic!("unknown float suffix pattern {}", parts.1)
                };
                env.stack_push((env.types.get(parts.1).unwrap().clone(), res));
            }
            "unary" =>
            {
                let op = &node.child(0).unwrap().child(0).unwrap().text;
                //println!("---- compiling unary operator `{}`", op);
                if op.as_str() == "&"
                {
                    compile(env, node.child(1).unwrap(), WantPointer::Real);
                    let (type_, val) = env.stack_pop().unwrap();
                    if !type_.is_pointer()
                    {
                        panic!("error: tried to get address of non-variable");
                    }
                    env.stack_push((type_, val));
                }
                else
                {
                    compile(env, node.child(1).unwrap(), WantPointer::None);
                    let (type_, val) = env.stack_pop().unwrap();
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
                                env.stack_push((type_.deref_ptr().to_vptr(), val));
                                //println!("---- * operator is virtual");
                            }
                            else
                            {
                                //println!("---- * operator is real");
                                let inner_type = type_.deref_ptr();
                                if inner_type.is_void()
                                {
                                    panic!("can't dereference void pointers");
                                }
                                let res = match op.as_str()
                                {
                                    "*" => env.builder.ins().load(inner_type.to_cranetype().unwrap(), MemFlags::trusted(), val, 0),
                                    _ => panic!("error: can't use operator `{}` on type `{}`", op, type_.name)
                                };
                                env.stack_push((inner_type, res));
                            }
                        }
                        "f32" | "f64" =>
                        {
                            let res = match op.as_str()
                            {
                                "+" => val,
                                "-" => env.builder.ins().fneg(val),
                                _ => panic!("error: can't use operator `{}` on type `{}`", op, type_.name)
                            };
                            env.stack_push((type_.clone(), res));
                        }
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
                            env.stack_push((type_.clone(), res));
                        }
                        _ => panic!("error: type `{}` is not supported by unary operators", type_.name)
                    }
                }
            }
            */
            /*
            "label" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                if let Some(block) = env.blocks.get(label)
                {
                    env.builder.ins().jump(*block, &[]);
                    env.builder.switch_to_block(*block);
                }
                else
                {
                    panic!("error: no such block {}", label);
                }
            }
            "ifcondition" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (type_, val)  = env.stack_pop().unwrap();
                let label = &node.child(1).unwrap().child(0).unwrap().text;
                // anonymous block for "else" case
                let else_block = env.builder.create_block();
                if let Some(then_block) = env.blocks.get(label)
                {
                    env.builder.ins().brif(val, *then_block, &[], else_block, &[]);
                    env.builder.switch_to_block(else_block);
                }
                else
                {
                    panic!("error: no such label {}", label);
                }
            }
            "goto" =>
            {
                let label = &node.child(0).unwrap().child(0).unwrap().text;
                // anonymous block for any dead code between here and the next label
                let dead_block = env.builder.create_block();
                if let Some(block) = env.blocks.get(label)
                {
                    env.builder.ins().jump(*block, &[]);
                    env.builder.switch_to_block(dead_block);
                }
                else
                {
                    panic!("error: no such label {}", label);
                }
            }
            "bitcast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val)  = env.stack_pop().unwrap();
                
                let right_type = parse_type(&env.types, &node.child(1).unwrap()).unwrap();
                
                let target_cranetype = right_type.to_cranetype().unwrap();
                
                // FIXME: platform-specific pointer size
                if left_type.size() == right_type.size() || (right_type.size() == 8 && left_type.is_composite())
                {
                    let ret = env.builder.ins().bitcast(target_cranetype, MemFlags::new(), left_val);
                    env.stack_push((right_type, ret));
                }
                else
                {
                    panic!("error: unsupported bitcast from type {} to type {} (types must have the same size to be bitcasted)", left_type.to_string(), right_type.to_string());
                }
            }
            "cast" =>
            {
                compile(env, node.child(0).unwrap(), WantPointer::None);
                let (left_type, left_val) = env.stack_pop().unwrap();
                
                let right_type = parse_type(&env.types, &node.child(1).unwrap()).unwrap();
                
                let target_cranetype = right_type.to_cranetype().unwrap();
                // cast as own type (do nothing)
                if left_type.name == right_type.name
                {
                    env.stack_push((right_type, left_val));
                }
                // cast from pointer to pointer
                else if left_type.is_pointer() && right_type.is_pointer()
                {
                    env.stack_push((right_type, left_val));
                }
                // cast between float types"
                else if left_type.name == "f32" && right_type.name == "f64"
                {
                    let ret = env.builder.ins().fpromote(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
                else if left_type.name == "f64" && right_type.name == "f32"
                {
                    let ret = env.builder.ins().fdemote(target_cranetype, left_val);
                    env.stack_push((right_type, ret));
                }
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
                else
                {
                    panic!("unsupported cast from type {} to type {}", left_type.to_string(), right_type.to_string());
                }
            }
            */
            text => 
            {
                /*
                if text.starts_with("binexpr")
                {
                    compile(env, node.child(0).unwrap(), WantPointer::None);
                    compile(env, node.child(2).unwrap(), WantPointer::None);
                    let op = &node.child(1).unwrap().child(0).unwrap().text;
                    let (right_type, right_val)  = env.stack_pop().unwrap();
                    let (left_type , left_val )  = env.stack_pop().unwrap();
                    
                    let is_u = left_type.name.starts_with("u");
                    
                    match (left_type.name.as_str(), right_type.name.as_str())
                    {
                        ("f32", "f32") | ("f64", "f64") =>
                        {
                            match op.as_str()
                            {
                                "+" | "-" | "*" | "/" | "%" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "+" => env.builder.ins().fadd(left_val, right_val),
                                        "-" => env.builder.ins().fsub(left_val, right_val),
                                        "*" => env.builder.ins().fmul(left_val, right_val),
                                        "/" => env.builder.ins().fdiv(left_val, right_val),
                                        "%" =>
                                        {
                                            let times = env.builder.ins().fdiv(left_val, right_val);
                                            let floored = env.builder.ins().floor(times);
                                            let n = env.builder.ins().fmul(floored, right_val);
                                            env.builder.ins().fsub(left_val, n)
                                        }
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack_push((left_type.clone(), res));
                                }
                                
                                ">" | "<" | ">=" | "<=" | "==" | "!=" =>
                                {
                                    let cond = match op.as_str()
                                    {
                                        "==" => FloatCC::Equal,
                                        "!=" => FloatCC::NotEqual,
                                        "<"  => FloatCC::LessThan,
                                        "<=" => FloatCC::LessThanOrEqual,
                                        ">"  => FloatCC::GreaterThan,
                                        ">=" => FloatCC::GreaterThanOrEqual,
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    let res = env.builder.ins().fcmp(cond, left_val, right_val);
                                    env.stack_push((env.types.get("u8").unwrap().clone(), res));
                                }
                                
                                _ => panic!("operator {} not supported on type {}", op, left_type.name)
                            }
                        }
                        ("i8", "i8") | ("i16", "i16") | ("i32", "i32") | ("i64", "i64") |
                        ("u8", "u8") | ("u16", "u16") | ("u32", "u32") | ("u64", "u64") =>
                        {
                            match op.as_str()
                            {
                                "&&" | "||" | "and" | "or" =>
                                {
                                    let left_bool  = env.builder.ins().icmp_imm(IntCC::NotEqual, left_val , 0);
                                    let right_bool = env.builder.ins().icmp_imm(IntCC::NotEqual, right_val, 0);
                                    
                                    let res = match op.as_str()
                                    {
                                        "||" | "or"  => env.builder.ins().bor (left_bool, right_bool),
                                        "&&" | "and" => env.builder.ins().band(left_bool, right_bool),
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack_push((env.types.get("u8").unwrap().clone(), res));
                                }
                                "|" | "&" | "^" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "|" => env.builder.ins().bor(left_val , right_val),
                                        "&" => env.builder.ins().band(left_val, right_val),
                                        "^" => env.builder.ins().bxor(left_val, right_val),
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack_push((left_type.clone(), res));
                                }
                                "+" | "-" | "*" | "/" | "%" =>
                                {
                                    let res = match op.as_str()
                                    {
                                        "+" => env.builder.ins().iadd(left_val, right_val),
                                        "-" => env.builder.ins().isub(left_val, right_val),
                                        "*" => env.builder.ins().imul(left_val, right_val),
                                        "/" => if is_u
                                        {
                                            env.builder.ins().udiv(left_val, right_val)
                                        }
                                        else
                                        {
                                            env.builder.ins().sdiv(left_val, right_val)
                                        },
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
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    env.stack_push((left_type.clone(), res));
                                }
                                
                                ">" | "<" | ">=" | "<=" | "==" | "!=" =>
                                {
                                    let cond = match op.as_str()
                                    {
                                        "==" => IntCC::Equal,
                                        "!=" => IntCC::NotEqual,
                                        "<"  => IntCC::SignedLessThan,
                                        "<=" => IntCC::SignedLessThanOrEqual,
                                        ">"  => IntCC::SignedGreaterThan,
                                        ">=" => IntCC::SignedGreaterThanOrEqual,
                                        _ => panic!("internal error: operator mismatch")
                                    };
                                    let res = env.builder.ins().icmp(cond, left_val, right_val);
                                    env.stack_push((env.types.get("u8").unwrap().clone(), res));
                                }
                                
                                _ => panic!("operator {} not supported on type {}", op, left_type.name)
                            }
                        }
                        _ => panic!("unhandled type pair `{}`, `{}`", left_type.name, right_type.name)
                    }
                }
                else
                */
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

fn main()
{
    let mut context = inkwell::context::Context::new();
    
    // only holds primitives and structs, not pointers or arrays, those are constructed dynamically
    let mut types = BTreeMap::new();
    
    types.insert("void".to_string(), (Type { name : "void".to_string(), data : TypeData::Void }, context.void_type()));
    
    types.insert("u8" .to_string(), (Type { name : "u8" .to_string(), data : TypeData::Primitive }, context.i8_type()));
    types.insert("u16".to_string(), (Type { name : "u16".to_string(), data : TypeData::Primitive }, context.i16_type()));
    types.insert("u32".to_string(), (Type { name : "u32".to_string(), data : TypeData::Primitive }, context.i32_type()));
    types.insert("u64".to_string(), (Type { name : "u64".to_string(), data : TypeData::Primitive }, context.i64_type()));
    
    types.insert("i8" .to_string(), (Type { name : "i8" .to_string(), data : TypeData::Primitive }, context.i8_type()));
    types.insert("i16".to_string(), (Type { name : "i16".to_string(), data : TypeData::Primitive }, context.i16_type()));
    types.insert("i32".to_string(), (Type { name : "i32".to_string(), data : TypeData::Primitive }, context.i32_type()));
    types.insert("i64".to_string(), (Type { name : "i64".to_string(), data : TypeData::Primitive }, context.i64_type()));
    
    types.insert("f32".to_string(), (Type { name : "f32".to_string(), data : TypeData::Primitive }, context.f32_type()));
    types.insert("f64".to_string(), (Type { name : "f64".to_string(), data : TypeData::Primitive }, context.f64_type()));
    
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
    import_function::<unsafe extern "C" fn(*mut u8, u64) -> ()>(&types, &mut parser, &mut imports, "print_bytes", print_bytes, print_bytes as usize, "funcptr(void, (ptr(u8), u64))");
    
    let context = Context::create();
    let module = context.create_module("main");
    
    //let mut builder = JITBuilder::with_flags(&settings, cranelift_module::default_libcall_names()).unwrap();
    
    for (f_name, (pointer, funcsig)) in &imports
    {
        builder.symbol(f_name, *pointer);
    }
    
    let mut module = JITModule::new(builder);
    
    let mut builder_context = FunctionBuilderContext::new();
    let mut ctx = module.make_context();
    
    let mut func_decs = BTreeMap::new();
    let mut func_sizes = BTreeMap::new();
    let mut func_disasm = BTreeMap::new();
    
    for (f_name, (pointer, funcsig)) in &imports
    {
        let signature = funcsig.to_signature(&module);
        let id = module.declare_function(&f_name, Linkage::Import, &signature).unwrap();
        func_decs.insert(f_name.clone(), (id, funcsig.clone()));
    }
    
    for (f_name, function) in &program.funcs
    {
        let funcsig = function.to_sig();
        let signature = funcsig.to_signature(&module);
        let id = module.declare_function(&f_name, Linkage::Export, &signature).unwrap();
        func_decs.insert(f_name.clone(), (id, funcsig));
    }
    
    for f in &program.funcs
    {
        ctx.want_disasm = true;
        
        let f_name = f.0;
        let function = f.1;
        
        let (id, funcsig) = func_decs.get(f_name).unwrap().clone();
        let signature = funcsig.to_signature(&module);
        
        let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_context);
        builder.func.signature = signature.clone();
        let block = builder.create_block();
        builder.append_block_params_for_function_params(block);
        builder.switch_to_block(block);
        
        let mut variables = BTreeMap::new();
        let mut const_vars = BTreeMap::new();
        
        // declare arguments
        let mut i = 0;
        for (j, param) in function.args.iter().enumerate()
        {
            let var_type = param.0.clone();
            let var_name = param.1.clone();
            let slot = builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size : var_type.size() });
            
            let tmp = builder.block_params(block)[j];
            builder.ins().stack_store(tmp, slot, 0);
            
            if variables.contains_key(&var_name) || func_decs.contains_key(&var_name) || const_vars.contains_key(&var_name)
            {
                panic!("error: parameter {} redeclared", var_name);
            }
            if var_type.is_void()
            {
                panic!("error: void variables are not allowed");
            }
            
            variables.insert(var_name, (var_type, slot, None));
            
            i += 1;
        }
        
        // declare variables
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && (node.text == "declaration" || node.text == "fulldeclaration" || node.text == "constdeclaration")
            {
                let var_type = parse_type(&types, &node.child(0).unwrap()).unwrap();
                let var_name = node.child(1).unwrap().child(0).unwrap().text.clone();
                let slot = builder.create_sized_stack_slot(StackSlotData { kind : StackSlotKind::ExplicitSlot, size : var_type.size() });
                
                if variables.contains_key(&var_name) || func_decs.contains_key(&var_name) || const_vars.contains_key(&var_name)
                {
                    panic!("error: variable or function {} shadowed or redeclared", var_name)
                }
                if var_type.is_void()
                {
                    panic!("error: void variables are not allowed");
                }
                
                if node.text == "constdeclaration"
                {
                    const_vars.insert(var_name, (var_type, None));
                }
                else
                {
                    variables.insert(var_name.clone(), (var_type, slot, None));
                }
                
                i += 1;
            }
            false
        });
        
        // collect referred function signatures
        let mut funcrefs = BTreeMap::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && node.text == "rvarname"
            {
                let name = &node.child(0).unwrap().text;
                if !variables.contains_key(name) && func_decs.contains_key(name)
                {
                    let (id, funcsig) = func_decs.get(name).unwrap();
                    let funcref = module.declare_func_in_func(*id, builder.func);
                    funcrefs.insert(name.clone(), (funcsig.clone(), funcref));
                }
            }
            false
        });
        
        // collect labels (blocks)
        let mut blocks = BTreeMap::new();
        let mut blocks_vec = Vec::new();
        let mut next_block = BTreeMap::new();
        function.body.visit(&mut |node : &ASTNode|
        {
            if node.is_parent() && node.text == "label"
            {
                let name = &node.child(0).unwrap().child(0).unwrap().text;
                if !blocks.contains_key(name)
                {
                    let block = builder.create_block();
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
        
        let mut stack = Vec::new();
        
        let mut env = Environment { stack : &mut stack, variables : &mut variables, builder : &mut builder, module : &mut module, funcrefs : &funcrefs, types : &types, blocks : &blocks, next_block : &next_block, const_vars };
        
        println!("compiling function {}...", function.name);
        compile(&mut env, &function.body, WantPointer::None);
        
        builder.seal_all_blocks();
        builder.finalize();
        
        //println!("{}", ctx.func.display());
        
        module.define_function(id, &mut ctx).unwrap();
        
        let mut b = settings::builder();
        for p in &settings
        {
            b.set(p.0, p.1);
        }
        let flags = settings::Flags::new(b);
        let res = verify_function(&ctx.func, &flags);
        
        println!("{}", ctx.func.display());
        
        if let Err(errors) = res
        {
            panic!("{}", errors);
        }
        println!("function {} compiled with no errors!", function.name);
        
        let bytes = ctx.compiled_code().unwrap().code_buffer().clone();
        let s = ctx.compiled_code().unwrap().code_info().total_size;
        func_sizes.insert(f_name.clone(), s);
        
        let mut cs = module.isa().clone().to_capstone().unwrap();
        cs.set_syntax(capstone::Syntax::Intel);
        //let dis = ctx.compiled_code().unwrap().disassemble(None, &cs).unwrap().clone();
        let dis = ctx.compiled_code().unwrap().disassemble(None, &cs).unwrap().clone();
        func_disasm.insert(f_name.clone(), dis);
        
        println!("size... {}", s);
        
        for c in bytes
        {
            print!("{:02X}", c);
        }
        println!("");
        
        module.clear_context(&mut ctx);
    }
    module.finalize_definitions().unwrap();
    
    let mut func_pointers = BTreeMap::new();
    
    for (f_name, (id, _)) in func_decs.iter()
    {
        if !imports.contains_key(f_name)
        {
            let code = module.get_finalized_function(*id);
            func_pointers.insert(f_name, code);
        }
    }
    
    macro_rules! get_funcptr {
        ($name:expr, $T:ty) =>
        {
            unsafe
            {
                let dec = func_decs.get(&$name.to_string()).unwrap();
                let type_string = dec.1.to_string_rusttype();
                
                let want_type_string = std::any::type_name::<$T>(); // FIXME: not guaranteed to be stable across rust versions
                assert!(want_type_string == type_string, "types do not match:\n{}\n{}\n", want_type_string, type_string);
                assert!(want_type_string.starts_with("unsafe "), "function pointer type must be unsafe");
                
                core::mem::transmute::<_, $T>(*func_pointers.get(&$name.to_string()).unwrap())
            }
        }
    }
    
    let _ = get_funcptr!("returns_void", unsafe extern "C" fn(f32) -> ());
    let _ = get_funcptr!("void_ptr_arg", unsafe extern "C" fn(*mut core::ffi::c_void) -> ());
    
    let gravity = get_funcptr!("func_gravity", unsafe extern "C" fn() -> f32);
    
    let print_garbage = get_funcptr!("print_garbage", unsafe extern "C" fn() -> ());
    
    
    let array_literal = get_funcptr!("array_literal", unsafe extern "C" fn() -> ());
    
    unsafe
    {
        println!("array_literal...");
        array_literal();
        println!("");
        println!("{}", func_disasm.get("array_literal").unwrap());
    }
    
    unsafe
    {
        println!("printing garbage...");
        print_garbage();
    }
    
    unsafe
    {
        let start = std::time::Instant::now();
        println!("running func_gravity...");
        let out = gravity();
        println!("func_gravity() = {}", out);
        let elapsed_time = start.elapsed();
        println!("time: {}", elapsed_time.as_secs_f64());
        
        let mut ptr : *mut u8 = core::mem::transmute::<_, _>(gravity);
        let size = *func_sizes.get("func_gravity").unwrap();
        let bytes = std::slice::from_raw_parts(ptr, size as usize);
        for c in bytes
        {
            print!("{:02X}", c);
        }
        println!("");
        println!("{}", func_disasm.get("func_gravity").unwrap());
    }
}
