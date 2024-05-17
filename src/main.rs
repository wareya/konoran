use std::collections::HashMap;

use konoran::compiler::*;

use std::env;
fn main()
{
    let mut modules = Vec::new();
    let mut args = Vec::new();
    let mut settings = HashMap::new();
    let mut mode = "";
    let in_args = env::args();
    for arg in in_args.skip(1)
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
        else if mode == "-obj"
        {
            settings.insert("objfile", arg);
            mode = ""
        }
        else
        {
            match arg.as_str()
            {
                "-O0" | "-Od" | "-O1" | "-O2" | "-O3" | "-Os" | "-Oz" =>
                {
                    let which : String = arg.chars().skip(2).take(1).collect();
                    settings.insert("optlevel", which);
                }
                "-i" => mode = "-i",
                "--" => mode = "--",
                "-oat" | "--output-assembly-triple" => mode = "-oat",
                "-cpu" | "--force-target-cpu" => mode = "-cpu",
                "-ft"  | "--force-triple" => mode = "-ft",
                "-obj" | "--generate-obj" => mode = "-obj",
                "-sag"  | "--simple-aggregates" => { settings.insert("simple_aggregates", arg); }
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
        use std::fs::File;
        use std::io::{self, BufRead};
        use std::path::Path;

        fn read_lines<P : AsRef<Path> + ToString + Clone>(filename: P) -> io::Lines<io::BufReader<File>>
        {
            let file = File::open(filename.clone()).unwrap_or_else(|_| panic!("failed to open file {}", filename.to_string()));
            io::BufReader::new(file).lines()
        }
        
        let mut iter = modules.into_iter().map(|fname| (
            {
                let fname = fname.clone();
                read_lines(fname.clone()).map(move |x| x.unwrap_or_else(|_| panic!("IO error while reading input module {}", fname.clone())))
            },
            fname.clone()
        ));
        let output = process_program(&mut iter, settings.clone());
        
        let machine = output.machine;
        let loaded_modules = output.modules;
        
        let skip_jit = settings.get("asm_triple").is_some() | settings.get("objfile").is_some();
        
        if !skip_jit
        {
            let executor = output.executor;
            let exports = output.visible_function_signatures;
            
            println!("running code...");
            
            let executor = executor.as_ref().unwrap();
            
            macro_rules! get_func { ($name:expr, $T:ty) =>
            {{
                if let Some((type_string, _)) = exports.get(&$name.to_string())
                {
                    let want_type_string = std::any::type_name::<$T>(); // FIXME: not guaranteed to be stable across rust versions
                    assert!(want_type_string == type_string, "types do not match:\n{}\n{}\n", want_type_string, type_string);
                    assert!(want_type_string.starts_with("unsafe "), "function pointer type must be unsafe");
                    
                    executor.get_function::<$T>(&$name).unwrap()
                }
                else
                {
                    panic!("error: no `{}` function", $name);
                }
            }} }
            
            let start = std::time::Instant::now();
            executor.run_static_constructors();
            println!("time to get func: {}", start.elapsed().as_secs_f64());
            
            unsafe
            {
                // check for errors
                use core::ffi::c_char;
                let mut msg : *mut c_char = 0 as *mut c_char;
                let val = llvm_sys::execution_engine::LLVMExecutionEngineGetErrMsg(executor.as_mut_ptr(), &mut msg as *mut *mut c_char);
                if val != 0
                {
                    if !msg.is_null()
                    {
                        panic!("linker error:\n{:?}", core::ffi::CStr::from_ptr(msg).to_string_lossy());
                    }
                    panic!("unknown linker error");
                }
                
                // run main
                let name = "main".to_string();
                let start;
                
                macro_rules! time_start { () =>
                {{ 
                    start = std::time::Instant::now();
                    if VERBOSE
                    {
                        println!("running {}...", name);
                    }
                }} }
                macro_rules! time_end { ($out:expr) =>
                {{ 
                    if VERBOSE
                    {
                        println!("{}() = {:?}", name, $out);
                        println!("time: {}", start.elapsed().as_secs_f64());
                    }
                }} }
                
                use std::ffi::CString;
                let mut args2 = args;
                args2.insert(0, "konoran_jit".to_string());
                let mut args_c_vec = args2.into_iter().map(|x| CString::new(x).unwrap().into_raw()).collect::<Vec<_>>();
                let argc = args_c_vec.len() as i32;
                let argv = args_c_vec.as_mut_ptr() as *mut *mut u8;
                
                match exports.get(&name).map(|(x, y)| (x.as_str(), y))
                {
                    Some((r#"unsafe extern "C" fn()"#, _)) =>
                    {
                        let f = get_func!(name, unsafe extern "C" fn());
                        time_start!();
                        let out = f.call();
                        time_end!(out);
                    }
                    Some((r#"unsafe extern "C" fn(i32, *mut *mut u8)"#, _)) =>
                    {
                        let f = get_func!(name, unsafe extern "C" fn(i32, *mut *mut u8));
                        time_start!();
                        let out = f.call(argc, argv);
                        time_end!(out);
                    }
                    Some((r#"unsafe extern "C" fn(i32, *mut *mut u8) -> i32"#, _)) =>
                    {
                        let f = get_func!(name, unsafe extern "C" fn(i32, *mut *mut u8) -> i32);
                        time_start!();
                        let out = f.call(argc, argv);
                        time_end!(out);
                    }
                    _ => panic!("sdkgr"),
                }
            }
            
            executor.run_static_destructors();
        }

        if skip_jit
        {
            use inkwell::targets::*;
            if let Some(fname) = settings.get("objfile")
            {
                machine.write_to_file(&loaded_modules[0], FileType::Object, fname.as_ref()).unwrap();
            }
            else
            {
                machine.write_to_file(&loaded_modules[0], FileType::Assembly, "out.asm".as_ref()).unwrap();
            }
        }
        if VERBOSE
        {
            println!("Finished gracefully.");
        }
        
    }
}
