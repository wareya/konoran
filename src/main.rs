use std::collections::HashMap;

use konoran::compiler::*;
use konoran::filelines::*;

use std::env;
fn main()
{
    let mut module_fnames = Vec::new();
    let mut args = Vec::new();
    let mut settings = HashMap::new();
    let mut mode = "";
    let in_args = env::args();
    for arg in in_args.skip(1)
    {
        if mode == "-i" || (mode == "" && !arg.starts_with("-"))
        {
            module_fnames.push(arg);
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
                "--timeinfo" => { settings.insert("semiverbose", arg); }
                "--verbose" => { settings.insert("verbose", arg); }
                "--no-opt-no-verify" => { settings.insert("early_exit", arg); }
                _ => panic!("unknown argument `{}`", arg),
            }
        }
    }
    if module_fnames.len() == 0
    {
        println!("Usage:");
        println!("konoran <source_files> <options> -- <arguments>");
        println!("");
        println!("Options:");
        println!("");
        println!("-oat");
        println!("--output-assembly-triple");
        println!("    Specify a triple to output assembly for, e.g. x86_64-pc-windows");
        println!("");
        println!("-ft");
        println!(" --force-triple");
        println!("    Force a target triple, e.g. x86_64-pc-windows");
        println!("");
        println!("-cpu <fname>");
        println!("--force-target-cpu <fname>");
        println!("    Force a particular target CPU, e.g. athlon64, skylake, native");
        println!("");
        println!("-obj <fname>");
        println!("--generate-obj <fname>");
        println!("    Output an object file (e.g. main.o) for linking with an external toolchain.");
        println!("");
        println!("-sag");
        println!("--simple-aggregates");
        println!("    Uses a different strategy for compiling structs/arrays that usually results in worse machine code, except on certain niche architectures it results in better machine code instead.");
        println!("");
        println!("-O0 -O1 -O2 -O3 -Os -Oz -Od");
        println!("    Specify an optimization level. O0 is the least optimized, O3 is the most. Os and Oz optimize for size. Od optimizes for developer throughput time and sits somewhere between O0 and O1 in terms of compile-time speed without being horribly slow at runtime like O0 is. (On math-heavy code, Od is usually faster than O1.) Note that the first character is a capital o, not a zero.");
        println!("");
        println!("--verbose");
        println!("    Print verbose mid-compilation output.");
        println!("");
        println!("--timeinfo");
        println!("    Print output compilation timing info after compilation.");
        println!("");
        println!("--no-opt-no-verify");
        println!("    Do not verify or optimize LLVM IR before finishing compilation. Only useful for debugging.");
        
    }
    else
    {
        let verbose = settings.contains_key("verbose");
        
        use std::fs::File;
        use std::path::Path;
        
        fn read_lines<P : AsRef<Path> + ToString + Clone>(filename: P) -> FileLines
        {
            let file = File::open(filename.clone()).unwrap_or_else(|_| panic!("failed to open file {}", filename.to_string()));
            FileLines::from_seekable(file)
        }
        
        let mut iter = module_fnames.into_iter().map(|fname|
        (
            {
                let fname = fname.clone();
                read_lines(fname.clone())
            },
            fname.clone()
        ));
        
        let process_output = process_program(&mut iter, settings.clone());
        
        let machine = process_output.machine;
        let loaded_modules = process_output.modules;
        
        let skip_jit = settings.get("asm_triple").is_some() | settings.get("objfile").is_some();
        
        if !skip_jit
        {
            let executor = process_output.executor;
            let exports = process_output.visible_function_signatures;
            
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
                    if verbose
                    {
                        println!("running {}...", name);
                    }
                }} }
                macro_rules! time_end { ($out:expr) =>
                {{ 
                    if verbose
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
                    _ => panic!("Failed to find function `{}` to run; maybe the file failed to parse?", name),
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
            // TODO: module.print_to_file("out_unopt.ll").unwrap();
        }
        if verbose
        {
            println!("Finished gracefully.");
        }
        
    }
}
