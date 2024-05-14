
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