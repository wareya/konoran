use std::collections::HashMap;

mod parser;
mod stdlib;
mod inkwell_helpers;
mod intrinsics_lists;
mod compiler;
use compiler::*;

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
        run_program(modules, args, settings);
    }
}
