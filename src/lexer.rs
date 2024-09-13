use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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
    Multiply,
    Divide,
    Modulo,
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

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input_str: &'a str) -> Self {
        Lexer {
            input: input_str.chars().peekable(),
        }
    }

    pub fn next_token(&mut self) -> Token {
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
                '*' => Token::Multiply,
                '/' => Token::Divide,
                '%' => Token::Modulo,
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
