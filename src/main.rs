use anyhow::Context;
use clap::Parser as _;
use interpreter::Interpreter;
use lexer::Lexer;
use parser::Parser;
use std::path::PathBuf;

mod interpreter;
mod lexer;
mod parser;

#[derive(clap::Parser)]
struct Cli {
    #[arg()]
    source: PathBuf,

    #[arg(short, long)]
    ast: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Cli::parse();
    let source_code = std::fs::read_to_string(args.source).context("Error reading file")?;

    let lexer = Lexer::new(source_code.as_str());
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    if args.ast {
        println!("{:#?}", program);
    }

    let mut interpreter = Interpreter::new();
    interpreter
        .interpret(program)
        .context("Error interpreting program")
}
