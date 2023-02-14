extern crate alloc;

use alloc::collections::BTreeMap;

use cranelift::prelude::*;

mod parser;

struct Function {
    
}

struct Struct {
    vars : BTreeMap<String, String>,
}

struct Program {
    funcs : BTreeMap<String, Function>,
}

impl Program {
    fn new(ast : &parser::ast::ASTNode) -> Program
    {
        let mut funcs = BTreeMap::new();
        Program { funcs }
    }
}

fn main()
{
    let ir_grammar = include_str!("parser/irgrammar.txt");
    let program_text = include_str!("parser/irexample.txt");
    let mut parser = parser::Parser::new_from_grammar(&ir_grammar).unwrap();
    let program_lines : Vec<String> = program_text.lines().map(|x| x.to_string()).collect();
    let tokens = parser.tokenize(&program_lines, false).unwrap();
    let ast = parser.parse_program(&tokens, &program_lines, false).unwrap().unwrap();
    
    let program = Program::new(&ast);
    
    
    print!("{:#?}", ast);
}
