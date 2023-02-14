
use cranelift::prelude::*;

mod parser;

fn main()
{
    let ir_grammar = include_str!("parser/irgrammar.txt");
    let program_text = include_str!("parser/irexample.txt");
    let mut parser = parser::Parser::new_from_grammar(&ir_grammar).unwrap();
    let program_lines : Vec<String> = program_text.lines().map(|x| x.to_string()).collect();
    let tokens = parser.tokenize(&program_lines, false).unwrap();
    let ast = parser.parse_program(&tokens, &program_lines, false).unwrap();
    print!("{:#?}", ast);
}
