use inkwell::types::*;
use inkwell::values::{BasicValueEnum, AsValueRef};

pub (crate) fn get_any_type_context(type_ : BasicTypeEnum) -> inkwell::context::ContextRef
{
    match type_
    {
        BasicTypeEnum::ArrayType(inner_type) => inner_type.get_context(),
        BasicTypeEnum::FloatType(inner_type) => inner_type.get_context(),
        BasicTypeEnum::IntType(inner_type) => inner_type.get_context(),
        BasicTypeEnum::PointerType(inner_type) => inner_type.get_context(),
        BasicTypeEnum::StructType(inner_type) => inner_type.get_context(),
        BasicTypeEnum::VectorType(inner_type) => inner_type.get_context(),
    }
}
pub (crate) fn get_vec_type(type_ : BasicTypeEnum, len : u32) -> inkwell::types::VectorType
{
    match type_
    {
        BasicTypeEnum::FloatType(inner_type) => inner_type.vec_type(len),
        BasicTypeEnum::IntType(inner_type) => inner_type.vec_type(len),
        _ => panic!("inernal error: tried to convert non-float/int type to backend vec type"),
    }
}
pub (crate) fn get_any_type_poison(type_ : BasicTypeEnum) -> inkwell::values::BasicValueEnum
{
    match type_
    {
        BasicTypeEnum::ArrayType(inner_type) => inner_type.get_poison().into(),
        BasicTypeEnum::FloatType(inner_type) => inner_type.get_poison().into(),
        BasicTypeEnum::IntType(inner_type) => inner_type.get_poison().into(),
        BasicTypeEnum::PointerType(inner_type) => inner_type.get_poison().into(),
        BasicTypeEnum::StructType(inner_type) => inner_type.get_poison().into(),
        BasicTypeEnum::VectorType(inner_type) => inner_type.get_poison().into(),
    }
}
pub (crate) fn val_is_const(mut is_const : bool, val : BasicValueEnum) -> bool
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
pub (crate) fn build_freeze<'ctx>(builder : &inkwell::builder::Builder<'ctx>, val : inkwell::values::BasicValueEnum<'ctx>) -> inkwell::values::BasicValueEnum<'ctx>
{
    unsafe{ BasicValueEnum::new(llvm_sys::core::LLVMBuildFreeze(builder.as_mut_ptr(), val.as_value_ref(), c"".as_ptr())) }
}
pub (crate) fn basic_const_array<'ctx>(type_ : BasicTypeEnum<'ctx>, vals : &[inkwell::values::BasicValueEnum<'ctx>]) -> inkwell::values::ArrayValue<'ctx>
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
