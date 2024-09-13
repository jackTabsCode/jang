use crate::lexer::{Lexer, Token};

#[derive(Debug, Clone)]
pub enum Expr {
    Number(f64),
    String(String),
    Bool(bool),
    Identifier(String),
    BinaryOp(Box<Expr>, Token, Box<Expr>),
    Call(String, Vec<Expr>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Variable(String, Expr),
    Assignment(String, Expr),
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

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let current_token = lexer.next_token();
        Parser {
            lexer,
            current_token,
        }
    }

    pub fn parse_program(&mut self) -> Vec<Stmt> {
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

    fn next_token(&mut self) {
        self.current_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Option<Stmt> {
        match self.current_token.clone() {
            Token::Let => self.parse_let_statement(),
            Token::Fn => self.parse_function(),
            Token::If => self.parse_if_statement(),
            Token::Return => self.parse_return_statement(),
            Token::Identifier(_) => self.parse_assignment_statement(),
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

    fn parse_assignment_statement(&mut self) -> Option<Stmt> {
        if let Token::Identifier(name) = self.current_token.clone() {
            self.next_token();
            if self.current_token == Token::Assign {
                self.next_token();
                let expr = self.parse_expression();
                if self.current_token == Token::Semicolon {
                    self.next_token();
                    return Some(Stmt::Assignment(name, expr));
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
