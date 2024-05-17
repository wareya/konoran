
use std::collections::BTreeMap;
use crate::compiler::*;
use crate::parser;

pub (crate) fn import_function<T>(types: &BTreeMap<String, Type>, parser : &mut parser::Parser, imports : &mut BTreeMap<String, (*const u8, FunctionSig)>, name : &str, _pointer : T, pointer_usize : usize, type_string : &str)
{
    let type_lines = vec!(type_string.to_string());
    let type_tokens = parser.tokenize(&mut type_lines.clone().into_iter(), true).unwrap();
    let type_ast = parser.parse_with_root_node_type(&type_tokens, &mut type_lines.clone().into_iter(), true, "type").unwrap().unwrap();
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

pub (crate) fn import_stdlib(types: &BTreeMap<String, Type>, parser : &mut parser::Parser, imports : &mut BTreeMap<String, (*const u8, FunctionSig)>)
{
    import_function::<unsafe extern "C" fn(*mut u8, *mut *mut u8)>
        (types, parser, imports, "print_fmt", print_fmt, print_fmt as usize, "funcptr(void, (ptr(u8), ptr(ptr(u8))))");
    
    import_function::<unsafe extern "C" fn(*mut u8)>
        (types, parser, imports, "print_str", print_str, print_str as usize, "funcptr(void, (ptr(u8)))");
    
    import_function::<unsafe extern "C" fn(*mut u8, u64)>
        (types, parser, imports, "print_bytes", print_bytes, print_bytes as usize, "funcptr(void, (ptr(u8), u64))");
    
    import_function::<unsafe extern "C" fn(f64)>
        (types, parser, imports, "print_float", print_float, print_float as usize, "funcptr(void, (f64))");
}

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
pub (crate) unsafe extern "C" fn print_fmt(cstring_bytes : *mut u8, mut vars : *mut *mut u8)
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
pub (crate) unsafe extern "C" fn print_str(cstring_bytes : *mut u8)
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
pub (crate) unsafe extern "C" fn print_bytes(bytes : *mut u8, count : u64)
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
pub (crate) unsafe extern "C" fn print_float(a : f64)
{
    println!("{}", a);
}