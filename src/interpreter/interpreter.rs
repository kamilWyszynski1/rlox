use crate::ast::ast::{Expr, LiteralValue, Stmt};
use crate::interpreter::environment::Environment;
use crate::interpreter::native::ClockCaller;
use crate::representation::token::{Token, TokenType};
use anyhow::{anyhow, bail, Context};
use std::fmt::{Debug, Display};
use std::rc::Rc;

pub trait LoxCallable: Debug {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> anyhow::Result<RuntimeValue>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
pub enum RuntimeValue {
    Bool(bool),
    Null,
    Number(f64),
    String(String),
    Callable(Rc<dyn LoxCallable>),
}

impl RuntimeValue {
    /// Lox follows Ruby’s simple rule: false and nil are falsey, and everything else is truthy.
    fn is_truthy(&self) -> bool {
        match self {
            RuntimeValue::Bool(true)
            | RuntimeValue::Number(_)
            | RuntimeValue::String(_)
            | RuntimeValue::Callable(_) => true,
            RuntimeValue::Bool(false) | RuntimeValue::Null => false,
        }
    }
}

impl PartialEq for RuntimeValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (RuntimeValue::Bool(lhs), RuntimeValue::Bool(rhs)) => lhs == rhs,
            (RuntimeValue::Null, RuntimeValue::Null) => true,
            (RuntimeValue::Number(lhs), RuntimeValue::Number(rhs)) => lhs == rhs,
            (RuntimeValue::String(lhs), RuntimeValue::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            RuntimeValue::Bool(b) => b.to_string(),
            RuntimeValue::Null => "null".to_string(),
            RuntimeValue::Number(n) => n.to_string(),
            RuntimeValue::String(s) => s.clone(),
            RuntimeValue::Callable(_) => "callable".to_string(),
        };
        write!(f, "{}", str)
    }
}

impl From<bool> for RuntimeValue {
    fn from(value: bool) -> Self {
        RuntimeValue::Bool(value)
    }
}

impl From<f64> for RuntimeValue {
    fn from(value: f64) -> Self {
        RuntimeValue::Number(value)
    }
}

impl TryInto<f64> for RuntimeValue {
    type Error = anyhow::Error;

    fn try_into(self) -> Result<f64, Self::Error> {
        match self {
            RuntimeValue::Bool(_) => bail!("Cannot convert runtime value (Bool) to number"),
            RuntimeValue::Null => bail!("Cannot convert runtime value (Null) to number"),
            RuntimeValue::Number(number) => Ok(number),
            RuntimeValue::String(string) => string
                .parse()
                .map_err(|_| anyhow!("Cannot convert runtime value (String, {string}) to number")),
            RuntimeValue::Callable(_) => bail!("Cannot convert runtime value (Callable) to number"),
        }
    }
}

impl From<String> for RuntimeValue {
    fn from(value: String) -> Self {
        RuntimeValue::String(value)
    }
}

impl From<LiteralValue> for RuntimeValue {
    fn from(value: LiteralValue) -> Self {
        Self::from(&value)
    }
}

impl From<&LiteralValue> for RuntimeValue {
    fn from(value: &LiteralValue) -> Self {
        match value {
            LiteralValue::True => RuntimeValue::Bool(true),
            LiteralValue::False => RuntimeValue::Bool(false),
            LiteralValue::Null => RuntimeValue::Null,
            LiteralValue::Number(n) => RuntimeValue::Number(*n),
            LiteralValue::String(s) => RuntimeValue::String(s.clone()),
        }
    }
}

#[derive(Debug, Clone)]
struct CallableObject<'a> {
    parameters: Vec<Token<'a>>,
    body: Vec<Stmt<'a>>,
}

impl<'a> CallableObject<'a> {
    fn new(parameters: Vec<Token<'a>>, body: Vec<Stmt<'a>>) -> Self {
        Self { parameters, body }
    }
}

impl<'a> LoxCallable for CallableObject<'a> {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<RuntimeValue>,
    ) -> anyhow::Result<RuntimeValue> {
        interpreter.environment.new_scope();

        for (param, arg) in self.parameters.iter().zip(arguments.iter()) {
            interpreter
                .environment
                .define(param.lexeme.to_string(), arg.clone());
        }

        interpreter.interpret(self.body.clone())?;

        interpreter.environment.drop_scope();

        Ok(RuntimeValue::Null)
    }

    fn arity(&self) -> usize {
        self.parameters.len()
    }
}

/// Adds more context to the statement execution.
///
#[derive(Debug, Clone)]
struct ExecutionInfo {
    loop_break: bool,
}

pub struct Interpreter {
    globals: Environment,
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Self {
        let mut globals = Environment::new();
        globals.define(
            "clock".to_string(),
            RuntimeValue::Callable(Rc::new(ClockCaller::new())),
        );
        Self {
            environment: globals.clone(),
            globals,
        }
    }
    pub fn interpret(&mut self, statements: Vec<Stmt>) -> anyhow::Result<()> {
        // self.evaluate_expr(expr)
        for statement in statements {
            self.execute(statement)?;
        }
        Ok(())
    }

    fn execute<'a>(&mut self, statement: Stmt<'a>) -> anyhow::Result<Option<ExecutionInfo>> {
        match statement {
            Stmt::Expression { expression } => {
                self.evaluate_expr(&expression)?;
                Ok(None)
            }
            Stmt::Print { expression } => {
                println!("{}", self.evaluate_expr(&expression)?);
                Ok(None)
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate_expr(&expr)?,
                    None => RuntimeValue::Null,
                };
                self.environment.define(name.lexeme, value);
                Ok(None)
            }

            Stmt::Block { statements } => {
                self.environment.new_scope();
                for stmt in statements {
                    if let Some(exec_info) = self.execute(stmt)? {
                        if exec_info.loop_break {
                            return Ok(Some(exec_info));
                        }
                    }
                }
                self.environment.drop_scope();
                Ok(None)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let evaluation = self.evaluate_expr(&condition)?;
                if evaluation.is_truthy() {
                    self.execute(*then_branch)
                } else if let Some(else_stmt) = else_branch {
                    self.execute(*else_stmt)
                } else {
                    Ok(None)
                }
            }
            Stmt::While {
                condition,
                statement,
            } => {
                while self.evaluate_expr(&condition)?.is_truthy() {
                    if let Some(exec_info) = self.execute(statement.as_ref().clone())? {
                        if exec_info.loop_break {
                            break;
                        }
                    }
                }
                Ok(None)
            }
            Stmt::Break => Ok(Some(ExecutionInfo { loop_break: true })),
            Stmt::Function { name, params, body } => {
                let callable = Rc::new(CallableObject::new(
                    params.iter().map(|x| x.clone()).collect(),
                    vec![],
                ));
                self.environment
                    .define(name.lexeme.clone(), RuntimeValue::Callable(callable));
                Ok(None)
            }
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> anyhow::Result<RuntimeValue> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate_expr(left)?;
                let right_value = self.evaluate_expr(right)?;

                match operator.token_type {
                    TokenType::Minus => Ok(RuntimeValue::Number(
                        TryInto::<f64>::try_into(left_value)?
                            - TryInto::<f64>::try_into(right_value)?,
                    )),
                    TokenType::Slash => Ok(RuntimeValue::Number(
                        TryInto::<f64>::try_into(left_value)?
                            / TryInto::<f64>::try_into(right_value)?,
                    )),
                    TokenType::Star => Ok(RuntimeValue::Number(
                        TryInto::<f64>::try_into(left_value)?
                            * TryInto::<f64>::try_into(right_value)?,
                    )),

                    TokenType::Greater => Ok(RuntimeValue::Bool(
                        TryInto::<f64>::try_into(left_value)?
                            > TryInto::<f64>::try_into(right_value)?,
                    )),
                    TokenType::GreaterEqual => Ok(RuntimeValue::Bool(
                        TryInto::<f64>::try_into(left_value)?
                            >= TryInto::<f64>::try_into(right_value)?,
                    )),
                    TokenType::Less => Ok(RuntimeValue::Bool(
                        TryInto::<f64>::try_into(left_value)?
                            < TryInto::<f64>::try_into(right_value)?,
                    )),
                    TokenType::LessEqual => Ok(RuntimeValue::Bool(
                        TryInto::<f64>::try_into(left_value)?
                            <= TryInto::<f64>::try_into(right_value)?,
                    )),

                    TokenType::BangEqual => Ok(RuntimeValue::Bool(left_value != right_value)),
                    TokenType::EqualEqual => Ok(RuntimeValue::Bool(left_value == right_value)),

                    TokenType::Plus => match (left_value.clone(), right_value.clone()) {
                        (RuntimeValue::Number(left_num), RuntimeValue::Number(right_num)) => {
                            Ok(RuntimeValue::Number(left_num + right_num))
                        }
                        // string concatenation
                        (RuntimeValue::String(left_str), RuntimeValue::String(right_str)) => {
                            Ok(RuntimeValue::String(left_str + &right_str))
                        }
                        _ => {
                            bail!(
                                "Operands of '+' must be two numbers or two strings, left: {:?}, right: {:?}",
                                left_value, right_value,
                            )
                        }
                    },

                    _ => {
                        bail!("Invalid token type {:?}", operator);
                    }
                }
            }
            Expr::Unary { operator, right } => {
                let sub_expr = self.evaluate_expr(right)?;

                match (operator.token_type, &sub_expr) {
                    (TokenType::Minus, RuntimeValue::Number(number)) => {
                        Ok(RuntimeValue::Number(-number))
                    }
                    (TokenType::Minus, RuntimeValue::String(s)) => {
                        let number = s
                            .parse::<f64>()
                            .context("Could not cast string to number")?;
                        Ok(RuntimeValue::Number(-number))
                    }
                    (TokenType::Minus, _) => {
                        bail!("Operand must be a number: {:?}", operator)
                    }
                    (TokenType::Bang, _) => Ok(RuntimeValue::from(!sub_expr.is_truthy())),
                    _ => {
                        bail!(
                            "Unsupported unary operator, {:?} with subexpr: {:?}",
                            operator,
                            sub_expr
                        );
                    }
                }
            }
            Expr::Literal(value) => Ok(RuntimeValue::from(value)),
            // To evaluate the grouping expression itself, we recursively evaluate that subexpression and return it.
            Expr::Grouping { expression } => self.evaluate_expr(expression),
            Expr::Variable { name } => self
                .environment
                .get(&name.lexeme)
                .context(format!("Undefined variable '{}'.", name.lexeme))
                .cloned(),
            Expr::Assign { name, value } => {
                // dbg!(&name, &initializer);
                let value = self.evaluate_expr(value)?;

                let variable = self
                    .environment
                    .get_mut(&name.lexeme)
                    .context(format!("Undefined variable '{}'", name.lexeme))?;
                *variable = value.clone();

                Ok(value)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate_expr(left)?;
                match operator.token_type {
                    TokenType::And if !left_value.is_truthy() => Ok(left_value),
                    TokenType::And => self.evaluate_expr(right),
                    TokenType::Or if left_value.is_truthy() => Ok(left_value),
                    TokenType::Or => self.evaluate_expr(right),
                    _ => bail!("invalid operator {:?}", operator),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee)?;

                let lox_arguments = arguments
                    .iter()
                    .map(|arg| self.evaluate_expr(arg))
                    .collect::<Result<Vec<_>, _>>()?;

                if let RuntimeValue::Callable(c) = callee {
                    if c.arity() != arguments.len() {
                        bail!("function must be called with {} arguments", c.arity())
                    }
                    c.call(self, lox_arguments)
                } else {
                    bail!("only callable can be invoked")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::ast::LiteralValue::Number;
    use crate::ast::ast::*;
    use crate::representation::token::Token;

    fn literal_number<'a>(value: f64) -> Expr<'a> {
        Expr::Literal(LiteralValue::Number(value))
    }

    fn literal_string<'a>(value: &str) -> Expr<'a> {
        Expr::Literal(LiteralValue::String(value.to_string()))
    }

    fn literal_bool<'a>(value: bool) -> Expr<'a> {
        if value {
            Expr::Literal(LiteralValue::True)
        } else {
            Expr::Literal(LiteralValue::False)
        }
    }

    fn literal_null<'a>() -> Expr<'a> {
        Expr::Literal(LiteralValue::Null)
    }

    fn binary<'a>(left: Expr<'a>, operator: TokenType<'a>, right: Expr<'a>) -> Expr<'a> {
        Expr::Binary {
            left: Box::new(left),
            operator: Token {
                token_type: operator,
                lexeme: "".to_string(),
                line: 1,
            },
            right: Box::new(right),
        }
    }

    fn unary<'a>(operator: TokenType<'a>, right: Expr<'a>) -> Expr<'a> {
        Expr::Unary {
            operator: Token {
                token_type: operator,
                lexeme: "".to_string(),
                line: 1,
            },
            right: Box::new(right),
        }
    }

    #[test]
    fn test_literal_number() {
        let mut interpreter = Interpreter::new();
        let expr = literal_number(42.0);
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Number(42.0));
    }

    #[test]
    fn test_literal_string() {
        let mut interpreter = Interpreter::new();
        let expr = literal_string("hello");
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::String("hello".to_string()));
    }

    #[test]
    fn test_literal_bool() {
        let mut interpreter = Interpreter::new();
        let expr = literal_bool(true);
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Bool(true));
    }

    #[test]
    fn test_literal_null() {
        let mut interpreter = Interpreter::new();
        let expr = literal_null();
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Null);
    }

    #[test]
    fn test_binary_addition_numbers() {
        let mut interpreter = Interpreter::new();
        let expr = binary(literal_number(1.0), TokenType::Plus, literal_number(2.0));
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Number(3.0));
    }

    #[test]
    fn test_binary_addition_strings() {
        let mut interpreter = Interpreter::new();
        let expr = binary(
            literal_string("foo"),
            TokenType::Plus,
            literal_string("bar"),
        );
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::String("foobar".to_string()));
    }

    #[test]
    fn test_unary_minus_number() {
        let mut interpreter = Interpreter::new();
        let expr = unary(TokenType::Minus, literal_number(42.0));
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Number(-42.0));
    }

    #[test]
    fn test_binary_greater() {
        let mut interpreter = Interpreter::new();
        let expr = binary(literal_number(3.0), TokenType::Greater, literal_number(1.0));
        let result = interpreter.evaluate_expr(&expr).unwrap();
        assert_eq!(result, RuntimeValue::Bool(true));
    }

    #[test]
    fn test_grouped_operations() {
        let mut interpreter = Interpreter::new();
        let expr = Expr::Binary {
            left: Box::new(Expr::Literal(Number(1.))),
            operator: Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_string(),
                line: 1,
            },
            right: Box::new(Expr::Grouping {
                expression: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Number(2.))),
                    operator: Token {
                        token_type: TokenType::Star,
                        lexeme: "*".to_string(),
                        line: 1,
                    },
                    right: Box::new(Expr::Literal(Number(3.))),
                }),
            }),
        };

        assert_eq!(
            interpreter.evaluate_expr(&expr).unwrap(),
            RuntimeValue::Number(7.0)
        );
    }
}
