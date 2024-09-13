use anyhow::{anyhow, Context};
use clap::Parser as _;
use std::collections::HashMap;
use std::fmt::Display;
use std::iter::Peekable;
use std::path::PathBuf;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Let,
    Fn,
    If,
    Else,
    Return,
    Identifier(String),
    Number(f64),
    StringLiteral(String),
    Bool(bool),
    Plus,
    Minus,
    Asterisk,
    Slash,
    Assign,
    Equal,
    NotEqual,
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Semicolon,
    Eof,
}

struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(input_str: &'a str) -> Self {
        Lexer {
            input: input_str.chars().peekable(),
        }
    }

    fn next_token(&mut self) -> Token {
        while let Some(&ch) = self.input.peek() {
            if ch.is_whitespace() {
                self.input.next();
            } else {
                break;
            }
        }

        if let Some(ch) = self.input.next() {
            match ch {
                '+' => Token::Plus,
                '-' => Token::Minus,
                '*' => Token::Asterisk,
                '/' => Token::Slash,
                '=' => {
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        Token::Equal
                    } else {
                        Token::Assign
                    }
                }
                '!' => {
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        Token::NotEqual
                    } else {
                        Token::Eof
                    }
                }
                '<' => {
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        Token::LessEqual
                    } else {
                        Token::Less
                    }
                }
                '>' => {
                    if self.input.peek() == Some(&'=') {
                        self.input.next();
                        Token::GreaterEqual
                    } else {
                        Token::Greater
                    }
                }
                '(' => Token::LParen,
                ')' => Token::RParen,
                '{' => Token::LBrace,
                '}' => Token::RBrace,
                ',' => Token::Comma,
                ';' => Token::Semicolon,
                '"' => {
                    let mut str = String::new();
                    while let Some(&next_ch) = self.input.peek() {
                        if next_ch != '"' {
                            str.push(self.input.next().unwrap());
                        } else {
                            self.input.next();
                            break;
                        }
                    }
                    Token::StringLiteral(str)
                }
                ch if ch.is_alphabetic() => {
                    let mut ident = ch.to_string();
                    while let Some(&next_ch) = self.input.peek() {
                        if next_ch.is_alphanumeric() || next_ch == '_' {
                            ident.push(self.input.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    match ident.as_str() {
                        "let" => Token::Let,
                        "fn" => Token::Fn,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "return" => Token::Return,
                        "true" => Token::Bool(true),
                        "false" => Token::Bool(false),
                        _ => Token::Identifier(ident),
                    }
                }
                ch if ch.is_ascii_digit() => {
                    let mut number = ch.to_string();
                    while let Some(&next_ch) = self.input.peek() {
                        if next_ch.is_ascii_digit() || next_ch == '.' {
                            number.push(self.input.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    Token::Number(number.parse().unwrap())
                }
                _ => Token::Eof,
            }
        } else {
            Token::Eof
        }
    }
}

#[derive(Debug, Clone)]
enum Expr {
    Number(f64),
    String(String),
    Bool(bool),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Stmt {
    Variable(String, Expr),
    Expression(Expr),
    Return(Expr),
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    Function {
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    fn new(mut lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn parse_program(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        while self.current_token != Token::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            } else {
                break;
            }
        }
        statements
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_token.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Fn => self.parse_function(),
            Token::If => self.parse_if_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> Option<Stmt> {
        self.next_token();
        if let Token::Identifier(name) = self.current_token.clone() {
            self.next_token();
            if self.current_token == Token::Assign {
                self.next_token();
                let expr = self.parse_expression();
                if self.current_token == Token::Semicolon {
                    self.next_token();
                    return Some(Stmt::Variable(name, expr));
                }
            }
        }
        None
    }

    fn parse_function(&mut self) -> Option<Stmt> {
        self.next_token();
        if let Token::Identifier(name) = self.current_token.clone() {
            self.next_token();
            if self.current_token == Token::LParen {
                self.next_token();
                let params = self.parse_parameters();
                if self.current_token == Token::LBrace {
                    let body = self.parse_block();
                    return Some(Stmt::Function { name, params, body });
                }
            }
        }
        None
    }

    fn parse_parameters(&mut self) -> Vec<String> {
        let mut params = Vec::new();
        if self.current_token != Token::RParen {
            while let Token::Identifier(param_name) = self.current_token.clone() {
                params.push(param_name);
                self.next_token();
                if self.current_token == Token::Comma {
                    self.next_token();
                } else {
                    break;
                }
            }
        }
        self.next_token();
        params
    }

    fn parse_block(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        if self.current_token == Token::LBrace {
            self.next_token();
            while self.current_token != Token::RBrace && self.current_token != Token::Eof {
                if let Some(stmt) = self.parse_statement() {
                    statements.push(stmt);
                } else {
                    break;
                }
            }
            self.next_token();
        }
        statements
    }

    fn parse_if_statement(&mut self) -> Option<Stmt> {
        self.next_token();
        let condition = self.parse_expression();
        let then_branch = if self.current_token == Token::LBrace {
            self.parse_block()
        } else {
            Vec::new()
        };
        let else_branch = if self.current_token == Token::Else {
            self.next_token();
            if self.current_token == Token::LBrace {
                Some(self.parse_block())
            } else {
                None
            }
        } else {
            None
        };
        Some(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Stmt> {
        self.next_token();
        let expr = self.parse_expression();
        if self.current_token == Token::Semicolon {
            self.next_token();
            Some(Stmt::Return(expr))
        } else {
            None
        }
    }

    fn parse_expression_statement(&mut self) -> Option<Stmt> {
        let expr = self.parse_expression();
        if self.current_token == Token::Semicolon {
            self.next_token();
            Some(Stmt::Expression(expr))
        } else {
            None
        }
    }

    fn parse_expression(&mut self) -> Expr {
        self.parse_equality()
    }

    fn parse_equality(&mut self) -> Expr {
        let mut expr = self.parse_comparison();
        while self.current_token == Token::Equal || self.current_token == Token::NotEqual {
            let op = self.current_token.clone();
            self.next_token(); // Consume operator
            let right = self.parse_comparison();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_comparison(&mut self) -> Expr {
        let mut expr = self.parse_term();
        while self.current_token == Token::Greater
            || self.current_token == Token::GreaterEqual
            || self.current_token == Token::Less
            || self.current_token == Token::LessEqual
        {
            let op = self.current_token.clone();
            self.next_token(); // Consume operator
            let right = self.parse_term();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();
        while self.current_token == Token::Plus || self.current_token == Token::Minus {
            let op = self.current_token.clone();
            self.next_token(); // Consume operator
            let right = self.parse_factor();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_factor(&mut self) -> Expr {
        let mut expr = self.parse_unary();
        while self.current_token == Token::Asterisk || self.current_token == Token::Slash {
            let op = self.current_token.clone();
            self.next_token(); // Consume operator
            let right = self.parse_unary();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_unary(&mut self) -> Expr {
        if self.current_token == Token::Minus {
            let op = self.current_token.clone();
            self.next_token();
            let right = self.parse_unary();
            Expr::BinaryOp(Box::new(Expr::Number(0.0)), op, Box::new(right))
        } else {
            self.parse_primary()
        }
    }

    fn parse_primary(&mut self) -> Expr {
        match self.current_token.clone() {
            Token::Number(value) => {
                self.next_token();
                Expr::Number(value)
            }
            Token::Identifier(name) => {
                self.next_token();
                if self.current_token == Token::LParen {
                    self.next_token();
                    let args = self.parse_arguments();
                    Expr::Call(name, args)
                } else {
                    Expr::Identifier(name)
                }
            }
            Token::LParen => {
                self.next_token();
                let expr = self.parse_expression();
                if self.current_token == Token::RParen {
                    self.next_token();
                }
                expr
            }
            Token::StringLiteral(value) => {
                self.next_token();
                Expr::String(value)
            }
            Token::Bool(value) => {
                self.next_token();
                Expr::Bool(value)
            }
            _ => Expr::Number(0.0),
        }
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        let mut args = Vec::new();
        if self.current_token != Token::RParen {
            loop {
                let expr = self.parse_expression();
                args.push(expr);
                if self.current_token == Token::Comma {
                    self.next_token();
                } else {
                    break;
                }
            }
        }
        if self.current_token == Token::RParen {
            self.next_token();
        }
        args
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
        }
    }
}

struct Interpreter {
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Vec<String>, Vec<Stmt>)>,
}

impl Interpreter {
    fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn interpret(&mut self, statements: Vec<Stmt>) -> anyhow::Result<()> {
        for stmt in statements {
            self.execute_statement(stmt)
                .context("Error executing statement")?;
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: Stmt) -> anyhow::Result<Option<Value>> {
        match stmt {
            Stmt::Variable(name, expr) => {
                let value = self
                    .evaluate_expression(expr)
                    .context("Invalid expression")?;
                self.variables.insert(name, value);

                Ok(None)
            }
            Stmt::Expression(expr) => self
                .evaluate_expression(expr)
                .context("Invalid expression")
                .map(Some),
            Stmt::Return(expr) => {
                let value = self
                    .evaluate_expression(expr)
                    .context("Invalid expression")?;
                Ok(Some(value))
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate_expression(condition);
                match cond_value {
                    Ok(Value::Bool(true)) => {
                        for stmt in then_branch {
                            if let Ok(Some(ret_val)) = self.execute_statement(stmt) {
                                return Ok(Some(ret_val));
                            }
                        }
                    }
                    _ => {
                        if let Some(else_stmts) = else_branch {
                            for stmt in else_stmts {
                                if let Ok(Some(ret_val)) = self.execute_statement(stmt) {
                                    return Ok(Some(ret_val));
                                }
                            }
                        }
                    }
                };
                Ok(None)
            }
            Stmt::Function { name, params, body } => {
                self.functions.insert(name, (params, body));
                Ok(None)
            }
        }
    }

    fn evaluate_expression(&mut self, expr: Expr) -> anyhow::Result<Value> {
        match expr {
            Expr::Number(value) => Ok(Value::Number(value)),
            Expr::String(value) => Ok(Value::String(value)),
            Expr::Bool(value) => Ok(Value::Bool(value)),
            Expr::Identifier(name) => self
                .variables
                .get(&name)
                .cloned()
                .context("Invalid identifier"),
            Expr::BinaryOp(left, op, right) => {
                let left_val = self
                    .evaluate_expression(*left)
                    .context("Invalid left operand")?;
                let right_val = self
                    .evaluate_expression(*right)
                    .context("Invalid right operand")?;

                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => match op {
                        Token::Plus => Ok(Value::Number(l + r)),
                        Token::Minus => Ok(Value::Number(l - r)),
                        Token::Asterisk => Ok(Value::Number(l * r)),
                        Token::Slash => Ok(Value::Number(l / r)),
                        Token::Greater => Ok(Value::Bool(l > r)),
                        Token::Less => Ok(Value::Bool(l < r)),
                        Token::Equal => Ok(Value::Bool(l == r)),
                        Token::NotEqual => Ok(Value::Bool(l != r)),
                        Token::GreaterEqual => Ok(Value::Bool(l >= r)),
                        Token::LessEqual => Ok(Value::Bool(l <= r)),
                        _ => Err(anyhow!("Unsupported operator for numbers")),
                    },
                    (Value::String(l), Value::String(r)) => {
                        if let Token::Plus = op {
                            Ok(Value::String(l + &r))
                        } else {
                            Err(anyhow!("Unsupported operation on strings"))
                        }
                    }
                    (Value::Bool(l), Value::Bool(r)) => match op {
                        Token::Equal => Ok(Value::Bool(l == r)),
                        Token::NotEqual => Ok(Value::Bool(l != r)),
                        _ => Err(anyhow!("Unsupported operator for booleans")),
                    },
                    _ => Err(anyhow!("Type mismatch in binary operation")),
                }
            }
            Expr::Call(name, args) => {
                if name == "print" {
                    for (i, arg) in args.iter().enumerate() {
                        let val = self
                            .evaluate_expression(arg.clone())
                            .context("Invalid argument")?;
                        print!("{val}");

                        if i == args.len() - 1 {
                            println!();
                        } else {
                            print!(" ");
                        }
                    }
                    Ok(Value::Number(0.0))
                } else if let Some((params, body)) = self.functions.get(&name).cloned() {
                    // Save current state
                    let saved_vars = self.variables.clone();
                    // Map arguments to parameters
                    for (param, arg) in params.iter().zip(args.iter()) {
                        let arg_val = self
                            .evaluate_expression(arg.clone())
                            .context("Invalid argument")?;
                        self.variables.insert(param.clone(), arg_val);
                    }
                    // Execute function body
                    let mut return_value: Option<Value> = None;
                    for stmt in body {
                        if let Ok(Some(ret_val)) = self.execute_statement(stmt.clone()) {
                            return_value = Some(ret_val);
                            break;
                        }
                    }
                    // Restore state
                    self.variables = saved_vars;
                    return_value.context("No return value")
                } else {
                    Err(anyhow!("Bad call"))
                }
            }
        }
    }
}

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
