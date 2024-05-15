pub (crate) fn get_vector_intrinsic_info(typename : &str, len : u32, funcname : &str) -> (String, String, Vec<String>, bool)
{
    let vtypename = format!("array({}, {})", typename, len);
    if matches!(typename, "f32" | "f64")
    {
        [
            ("fadd_reduce"     , "llvm.vp.reduce.fadd", "funcptr(ST, (ST, VT))", vec!["VT"], true),
            
            ("fmul"            , "llvm.vp.fmul",        "funcptr(VT, (VT, VT))", vec!["VT"], true),
            ("fadd"            , "llvm.vp.fadd",        "funcptr(VT, (VT, VT))", vec!["VT"], true),
            ("fsub"            , "llvm.vp.fsub",        "funcptr(VT, (VT, VT))", vec!["VT"], true),
            ("fdiv"            , "llvm.vp.fdiv",        "funcptr(VT, (VT, VT))", vec!["VT"], true),
            ("frem"            , "llvm.vp.frem",        "funcptr(VT, (VT, VT))", vec!["VT"], true),
            
            ("fneg"            , "llvm.vp.fneg",        "funcptr(VT, (VT))", vec!["VT"], true),
            ("fabs"            , "llvm.vp.fabs",        "funcptr(VT, (VT))", vec!["VT"], true),
            ("sqrt"            , "llvm.vp.sqrt",        "funcptr(VT, (VT))", vec!["VT"], true),
            
            ("fma"             , "llvm.vp.fma",         "funcptr(VT, (VT, VT, VT))", vec!["VT"], true),
            ("fmuladd"         , "llvm.vp.fmuladd",     "funcptr(VT, (VT, VT, VT))", vec!["VT"], true),
        ]
    }
    else if matches!(typename, "i8" | "u8" | "i16" | "u16" | "i32" | "u32" | "i64" | "u64")
    {
        todo!()
    }
    else
    {
        panic!("error: unsupported vector intrinsic type");
    }.into_iter()
    .filter(|(name, _, _, _, _)| *name == funcname)
    .map(|(_, llvmname, funcsig, overloads, hasmask)|
       (llvmname.replace("ST", typename).replace("VT", &vtypename).to_string(),
        funcsig .replace("ST", typename).replace("VT", &vtypename).to_string(),
        overloads.iter().map(|x| x.replace("ST", typename).replace("VT", &vtypename)).collect::<Vec<_>>(),
        hasmask
       )).next().unwrap()
}
pub (crate) fn get_basic_intrinsics() -> Vec<(String, String, String, Vec<&'static str>)>
{
    let mut intrinsic_imports = [
        ("pow"         , "llvm.pow",     "funcptr(f64, (f64, f64))", vec!["f64"]),
        ("pow_f32"     , "llvm.pow",     "funcptr(f32, (f32, f32))", vec!["f32"]),
        ("fmuladd"     , "llvm.fmuladd", "funcptr(f64, (f64, f64, f64))", vec!["f64"]),
        ("fmuladd_f32" , "llvm.fmuladd", "funcptr(f32, (f32, f32, f32))", vec!["f32"]),
        ("powi"        , "llvm.powi",    "funcptr(f64, (f64, i32))", vec!["f64", "i32"]),
        ("powi_f32"    , "llvm.powi",    "funcptr(f32, (f32, i32))", vec!["f32", "i32"]),
        
        ("rotl"        , "llvm.fshl",    "funcptr(u64, (u64, u64))", vec!["u64"]),
        ("rotl_u32"    , "llvm.fshl",    "funcptr(u32, (u32, u32))", vec!["u32"]),
        ("rotl_u16"    , "llvm.fshl",    "funcptr(u16, (u16, u16))", vec!["u16"]),
        ("rotl_u8"     , "llvm.fshl",    "funcptr(u8, (u8, u8))", vec!["u8"]),
        
        ("rotr"        , "llvm.fshr",    "funcptr(u64, (u64, u64))", vec!["u64"]),
        ("rotr_u32"    , "llvm.fshr",    "funcptr(u32, (u32, u32))", vec!["u32"]),
        ("rotr_u16"    , "llvm.fshr",    "funcptr(u16, (u16, u16))", vec!["u16"]),
        ("rotr_u8"     , "llvm.fshr",    "funcptr(u8, (u8, u8))", vec!["u8"]),
        
        ("sign"       , "llvm.copysign", "funcptr(f64, (f64))", vec!["f64"]),
        ("sign_f32"   , "llvm.copysign", "funcptr(f32, (f32))", vec!["f32"]),
        
        ("memset"     , "llvm.memset", "funcptr(void, (ptr(u8), u8, u64))", vec!["ptr", "i64"]),
        ("memset_vol" , "llvm.memset", "funcptr(void, (ptr(u8), u8, u64))", vec!["ptr", "i64"]),
        ("memcpy"     , "llvm.memcpy", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
        ("memcpy_vol" , "llvm.memcpy", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
        ("memmove"    , "llvm.memmove", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
        ("memmove_vol", "llvm.memmove", "funcptr(void, (ptr(u8), ptr(u8), u64))", vec!["ptr", "ptr", "i64"]),
    ].into_iter().map(|(a, b, c, d)| (a.to_string(), b.to_string(), c.to_string(), d)).collect::<Vec<_>>();
    
    // signed-in unsigned-out multi-width intrinsics
    for op in &["abs"]
    {
        for (i, (type_out, type_in)) in [("u64", "i64"), ("u32", "i32"), ("u16", "i16"), ("u8", "i8")].iter().enumerate()
        {
            let a = if i == 0 { op.to_string() } else { format!("{}_{}", op, type_in) };
            let b = format!("llvm.{}", op);
            let c = format!("funcptr({}, ({}))", type_out, type_in);
            intrinsic_imports.push((a, b, c, vec!(type_in)));
        }
    }
    
    // int-in same-int-out multi-width intrinsics
    for op in &["bitreverse", "bswap", "ctpop", "ctlz", "cttz", "ctlz"]
    {
        for (i, type_) in ["u64", "i64", "u32", "i32", "u16", "i16", "u8", "i8"].iter().enumerate()
        {
            let a = if i == 0 { op.to_string() } else { format!("{}_{}", op, type_) };
            let b = format!("llvm.{}", op);
            let c = format!("funcptr({}, ({}))", type_, type_);
            intrinsic_imports.push((a, b, c, vec!(type_)));
        }
    }
    
    // float-in float-out multi-width intrinsics
    for op in &["sqrt", "sin", "cos", /*"tan",*/
                "exp", "exp2", /*"exp10",*/ "log", "log2", "log10",
                "fabs", "floor", "ceil", "trunc", "round"]
    {
        for (i, type_) in ["f64", "f32"].iter().enumerate()
        {
            let a = if i == 0 { op.to_string() } else { format!("{}_{}", op, type_) };
            let b = format!("llvm.{}", op);
            let c = format!("funcptr({}, ({}))", type_, type_);
            intrinsic_imports.push((a, b, c, vec!(type_)));
        }
    }
    
    intrinsic_imports
}