use std::collections::HashMap;
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    Let,
    Fn,
    If,
    Else,
    Return,

    Identifier(String),
    Number(i64),
    StringLiteral(String),

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
                        _ => Token::Identifier(ident),
                    }
                }
                ch if ch.is_ascii_digit() => {
                    let mut number = ch.to_string();
                    while let Some(&next_ch) = self.input.peek() {
                        if next_ch.is_ascii_digit() {
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
    Number(i64),
    String(String),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
enum Stmt {
    Let(String, Expr),
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
                    return Some(Stmt::Let(name, expr));
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
                } else if self.current_token == Token::RParen {
                    break;
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
        } else {
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
            self.next_token();
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
            self.next_token();
            let right = self.parse_term();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_term(&mut self) -> Expr {
        let mut expr = self.parse_factor();
        while self.current_token == Token::Plus || self.current_token == Token::Minus {
            let op = self.current_token.clone();
            self.next_token();
            let right = self.parse_factor();
            expr = Expr::BinaryOp(Box::new(expr), op, Box::new(right));
        }
        expr
    }

    fn parse_factor(&mut self) -> Expr {
        let mut expr = self.parse_unary();
        while self.current_token == Token::Asterisk || self.current_token == Token::Slash {
            let op = self.current_token.clone();
            self.next_token();
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

            Expr::BinaryOp(Box::new(Expr::Number(0)), op, Box::new(right))
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
                } else {
                }
                expr
            }
            Token::StringLiteral(value) => {
                self.next_token();
                Expr::String(value)
            }
            _ => Expr::Number(0),
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
        } else {
        }
        args
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(i64),
    String(String),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
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

    fn interpret(&mut self, statements: Vec<Stmt>) {
        for stmt in statements {
            self.execute_statement(stmt);
        }
    }

    fn execute_statement(&mut self, stmt: Stmt) -> Option<Value> {
        match stmt {
            Stmt::Let(name, expr) => {
                let value = self.evaluate_expression(expr);
                self.variables.insert(name, value);
                None
            }
            Stmt::Expression(expr) => {
                self.evaluate_expression(expr);
                None
            }
            Stmt::Return(expr) => {
                let value = self.evaluate_expression(expr);
                Some(value)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.evaluate_expression(condition);
                match cond_value {
                    Value::Number(1) => {
                        for stmt in then_branch {
                            if let Some(ret_val) = self.execute_statement(stmt) {
                                return Some(ret_val);
                            }
                        }
                    }
                    _ => {
                        if let Some(else_stmts) = else_branch {
                            for stmt in else_stmts {
                                if let Some(ret_val) = self.execute_statement(stmt) {
                                    return Some(ret_val);
                                }
                            }
                        }
                    }
                };
                None
            }
            Stmt::Function { name, params, body } => {
                self.functions.insert(name, (params, body));
                None
            }
        }
    }

    fn evaluate_expression(&mut self, expr: Expr) -> Value {
        match expr {
            Expr::Number(value) => Value::Number(value),
            Expr::String(value) => Value::String(value),
            Expr::Identifier(name) => self
                .variables
                .get(&name)
                .cloned()
                .expect("Invalid identifier"),
            Expr::BinaryOp(left, op, right) => {
                let left_val = self.evaluate_expression(*left);
                let right_val = self.evaluate_expression(*right);
                match (left_val, right_val) {
                    (Value::Number(l), Value::Number(r)) => {
                        let result = match op {
                            Token::Plus => l + r,
                            Token::Minus => l - r,
                            Token::Asterisk => l * r,
                            Token::Slash => l / r,
                            Token::Greater => (l > r) as i64,
                            Token::Less => (l < r) as i64,
                            Token::Equal => (l == r) as i64,
                            Token::NotEqual => (l != r) as i64,
                            Token::GreaterEqual => (l >= r) as i64,
                            Token::LessEqual => (l <= r) as i64,
                            _ => 0,
                        };
                        Value::Number(result)
                    }
                    (Value::String(l), Value::String(r)) => {
                        if let Token::Plus = op {
                            Value::String(l + &r)
                        } else {
                            panic!("Unsupported operation on strings")
                        }
                    }
                    _ => panic!("Type mismatch in binary operation"),
                }
            }
            Expr::Call(name, args) => {
                if name == "print" {
                    for (i, arg) in args.iter().enumerate() {
                        let val = self.evaluate_expression(arg.clone());
                        print!("{val}");

                        if i == args.len() - 1 {
                            println!();
                        } else {
                            print!(" ");
                        }
                    }
                    Value::Number(0)
                } else if let Some((params, body)) = self.functions.get(&name).cloned() {
                    let saved_vars = self.variables.clone();

                    for (param, arg) in params.iter().zip(args.iter()) {
                        let arg_val = self.evaluate_expression(arg.clone());
                        self.variables.insert(param.clone(), arg_val);
                    }

                    let mut return_value: Option<Value> = None;
                    for stmt in body {
                        if let Some(ret_val) = self.execute_statement(stmt.clone()) {
                            return_value = Some(ret_val);
                            break;
                        }
                    }

                    self.variables = saved_vars;
                    return_value.unwrap()
                } else {
                    panic!("Undefined function: {}", name);
                }
            }
        }
    }
}

fn main() {
    let source_code = r#"
    let x = 10;
    let y = 20;

    fn add(a, b) {
        return a + b;
    }

    let result = add(x, y);

    if result > 20 {
        print(result);
    }

    print("Hello", "There");
    print("Something");
    "#;

    let lexer = Lexer::new(source_code);
    let mut parser = Parser::new(lexer);
    let program = parser.parse_program();

    let mut interpreter = Interpreter::new();
    interpreter.interpret(program);
}
