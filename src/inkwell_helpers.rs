use inkwell::types::*;
use inkwell::values::{BasicValueEnum, AsValueRef};

pub (crate) fn get_any_type_context(sdkawuidsguisagugarewudsga : BasicTypeEnum) -> inkwell::context::ContextRef
{
    match sdkawuidsguisagugarewudsga
    {
        BasicTypeEnum::ArrayType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        BasicTypeEnum::FloatType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        BasicTypeEnum::IntType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        BasicTypeEnum::PointerType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        BasicTypeEnum::StructType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
        BasicTypeEnum::VectorType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_context(),
    }
}
pub (crate) fn get_any_type_poison(sdkawuidsguisagugarewudsga : BasicTypeEnum) -> inkwell::values::BasicValueEnum
{
    match sdkawuidsguisagugarewudsga
    {
        BasicTypeEnum::ArrayType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        BasicTypeEnum::FloatType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        BasicTypeEnum::IntType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        BasicTypeEnum::PointerType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        BasicTypeEnum::StructType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
        BasicTypeEnum::VectorType(fdaguij34ihu34g789wafgjre) => fdaguij34ihu34g789wafgjre.get_poison().into(),
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
