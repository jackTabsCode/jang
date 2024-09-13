use anyhow::{anyhow, Context};

use crate::{
    lexer::Token,
    parser::{Expr, Stmt},
};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone)]
enum Value {
    Number(f64),
    String(String),
    Bool(bool),
    Void,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Void => write!(f, "void"),
        }
    }
}

pub struct Interpreter {
    variables: HashMap<String, Value>,
    functions: HashMap<String, (Vec<String>, Vec<Stmt>)>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> anyhow::Result<()> {
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

                if self.variables.contains_key(&name) {
                    return Err(anyhow!("Variable already defined"));
                }

                self.variables.insert(name, value);

                Ok(None)
            }
            Stmt::Assignment(name, expr) => {
                let value = self
                    .evaluate_expression(expr)
                    .context("Invalid expression")?;

                if !self.variables.contains_key(&name) {
                    return Err(anyhow!("Variable not defined"));
                }

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

                    Ok(Value::Void)
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
                        if let Stmt::Return(expr) = stmt {
                            match self.evaluate_expression(expr) {
                                Ok(value) => {
                                    return_value = Some(value);
                                    break;
                                }
                                Err(err) => return Err(err),
                            }
                        } else {
                            self.execute_statement(stmt)
                                .context("Error executing statement")?;
                        }
                    }
                    // Restore state
                    self.variables = saved_vars;

                    Ok(return_value.unwrap_or(Value::Void))
                } else {
                    Err(anyhow!("Bad call"))
                }
            }
        }
    }
}
