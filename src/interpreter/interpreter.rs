use crate::ast::ast::{Expr, Stmt};
use crate::interpreter::class::{LoxClass, LoxInstance};
use crate::interpreter::environment::Environment;
use crate::interpreter::native::ClockCaller;
use crate::interpreter::runtime::RuntimeValue;
use crate::representation::token::{Token, TokenType};
use anyhow::{bail, Context};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Debug, Display};
use std::rc::Rc;

pub trait LoxCallable: Debug + Display {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Rc<RefCell<RuntimeValue>>>,
    ) -> anyhow::Result<Rc<RefCell<RuntimeValue>>>;
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
pub struct CallableObject {
    closure: Rc<RefCell<Environment>>,
    parameters: Vec<Token>,
    body: Vec<Stmt>,
}

impl CallableObject {
    fn new(parameters: Vec<Token>, body: Vec<Stmt>, closure: Rc<RefCell<Environment>>) -> Self {
        Self {
            parameters,
            body,
            closure,
        }
    }

    pub fn bind(&self, instance: LoxInstance) -> Self {
        let mut env = Environment::new(self.closure.clone());
        env.define(
            "this".to_string(),
            Rc::new(RefCell::new(RuntimeValue::Instance(instance))),
        );
        Self {
            closure: Rc::new(RefCell::new(env)),
            ..self.clone()
        }
    }
}

impl Display for CallableObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "callable")
    }
}

impl LoxCallable for CallableObject {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<Rc<RefCell<RuntimeValue>>>,
    ) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        let copied = interpreter.environment.clone();

        // take function's saved environment and make copy of this, we don't want to
        // make another reference because it would change original environment.
        interpreter.environment = Rc::new(RefCell::new(Environment::new(self.closure.clone())));

        for (param, arg) in self.parameters.iter().zip(arguments.iter()) {
            interpreter
                .environment
                .try_borrow_mut()?
                .define(param.lexeme.to_string(), arg.clone());
        }

        let value = interpreter
            ._interpret(self.body.clone())
            .context("error during function call")?
            .and_then(|exec_info| exec_info.function_return)
            .unwrap_or(Rc::new(RefCell::new(RuntimeValue::Null)));

        // update closure with changes that were made during function execution
        // *self.closure.borrow_mut() = interpreter.environment.borrow().clone();
        // revert environment change
        interpreter.environment = copied;
        Ok(value)
    }

    fn arity(&self) -> usize {
        self.parameters.len()
    }
}

/// Adds more context to the statement execution.
///
#[derive(Debug, Clone)]
pub struct ExecutionInfo {
    loop_break: bool,
    function_return: Option<Rc<RefCell<RuntimeValue>>>,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct ExprNameWithLine {
    name: String,
    line: usize,
    column: usize,
}

impl Debug for ExprNameWithLine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.name)
    }
}

pub struct Interpreter {
    globals: Rc<RefCell<Environment>>,
    environment: Rc<RefCell<Environment>>,
    locals: HashMap<ExprNameWithLine, usize>, // maps variable name to how nested it's in Environment
}

impl Interpreter {
    pub fn new() -> Self {
        let globals = Rc::new(RefCell::from(Environment::new_empty()));
        globals.borrow_mut().define(
            "clock".to_string(),
            Rc::new(RefCell::new(RuntimeValue::Callable(Rc::new(
                ClockCaller::new(),
            )))),
        );
        Self {
            environment: globals.clone(),
            globals,
            locals: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> anyhow::Result<Option<ExecutionInfo>> {
        self._interpret(statements)
    }

    fn _interpret(&mut self, statements: Vec<Stmt>) -> anyhow::Result<Option<ExecutionInfo>> {
        for statement in statements {
            if let Some(exec_info) = self.execute(statement)? {
                return Ok(Some(exec_info));
            }
        }
        Ok(None)
    }

    fn execute(&mut self, statement: Stmt) -> anyhow::Result<Option<ExecutionInfo>> {
        match statement {
            Stmt::Expression { expression } => {
                self.evaluate_expr(&expression)?;
                Ok(None)
            }
            Stmt::Print { expression } => {
                println!("{}", self.evaluate_expr(&expression)?.borrow());
                Ok(None)
            }
            Stmt::Var { name, initializer } => {
                let value = match initializer {
                    Some(expr) => self.evaluate_expr(&expr)?,
                    None => Rc::new(RefCell::new(RuntimeValue::Null)),
                };
                self.environment
                    .try_borrow_mut()?
                    .define(name.lexeme, value);
                Ok(None)
            }

            Stmt::Block { statements } => {
                let previous = self.environment.clone();

                self.environment =
                    Rc::new(RefCell::new(Environment::new(self.environment.clone())));
                for stmt in statements {
                    if let Some(exec_info) = self.execute(stmt)? {
                        if exec_info.loop_break || exec_info.function_return.is_some() {
                            self.environment = previous;
                            return Ok(Some(exec_info));
                        }
                    }
                }
                self.environment = previous;
                Ok(None)
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let evaluation = self.evaluate_expr(&condition)?;
                if evaluation.try_borrow()?.is_truthy() {
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
                while self.evaluate_expr(&condition)?.try_borrow()?.is_truthy() {
                    if let Some(exec_info) = self.execute(statement.as_ref().clone())? {
                        if exec_info.loop_break {
                            break;
                        }
                        if exec_info.function_return.is_some() {
                            return Ok(Some(exec_info));
                        }
                    }
                }
                Ok(None)
            }
            Stmt::Break => Ok(Some(ExecutionInfo {
                loop_break: true,
                function_return: None,
            })),
            Stmt::Function { name, params, body } => {
                let function_env = Rc::new(RefCell::new(self.environment.borrow().clone()));
                let callable = Rc::new(CallableObject::new(params, body, function_env));
                self.environment.try_borrow_mut()?.define(
                    name.lexeme.clone(),
                    Rc::new(RefCell::new(RuntimeValue::Callable(callable))),
                );
                Ok(None)
            }
            Stmt::Return { expr, keyword } => {
                let function_return = if let Some(expr) = expr {
                    Some(self.evaluate_expr(&expr)?)
                } else {
                    None
                };
                Ok(Some(ExecutionInfo {
                    loop_break: false,
                    function_return,
                }))
            }
            Stmt::Class { name, methods } => {
                self.environment.try_borrow_mut()?.define(
                    name.lexeme.clone(),
                    Rc::new(RefCell::new(RuntimeValue::Null)),
                );

                let mut class_methods: HashMap<String, CallableObject> = HashMap::new();
                for method in methods {
                    let method_env = Rc::new(RefCell::new(self.environment.borrow().clone()));
                    if let Stmt::Function { name, params, body } = method {
                        let function = CallableObject::new(params, body, method_env);
                        class_methods.insert(name.lexeme, function);
                    } else {
                        bail!("Interpreter: Class' method should be a function type")
                    }
                }

                let klass = LoxClass::new(name.lexeme.clone(), class_methods);
                self.environment.try_borrow_mut()?.assign(
                    &name.lexeme,
                    Rc::new(RefCell::new(RuntimeValue::Callable(Rc::new(klass)))),
                )?;
                Ok(None)
            }
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        match expr {
            Expr::Binary {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate_expr(left)?.borrow().clone();
                let right_value = self.evaluate_expr(right)?.borrow().clone();

                let v: anyhow::Result<RuntimeValue> = match operator.token_type {
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
                };
                Ok(Rc::new(RefCell::new(v?)))
            }
            Expr::Unary { operator, right } => {
                let sub_expr = self.evaluate_expr(right)?.borrow().clone();

                let v: anyhow::Result<RuntimeValue> = match (&operator.token_type, &sub_expr) {
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
                };
                Ok(Rc::new(RefCell::new(v?)))
            }
            Expr::Literal(value) => Ok(Rc::new(RefCell::new(RuntimeValue::from(value)))),
            // To evaluate the grouping expression itself, we recursively evaluate that subexpression and return it.
            Expr::Grouping { expression } => self.evaluate_expr(expression),
            Expr::Variable { name } => self.visit_expr_var(name).context(format!(
                "cannot visit variable {}, line {}",
                name.lexeme.clone(),
                name.line
            )),
            Expr::Assign { name, value } => {
                let value = self.evaluate_expr(value)?;

                let key = ExprNameWithLine {
                    name: name.lexeme.clone(),
                    line: name.line,
                    column: name.column,
                };
                match self.locals.get(&key) {
                    Some(depth) => self.assign_at(name, value.clone(), *depth)?,
                    None => self
                        .globals
                        .borrow_mut()
                        .assign(&name.lexeme, value.clone())?,
                }

                Ok(value)
            }
            Expr::Logical {
                left,
                operator,
                right,
            } => {
                let left_value = self.evaluate_expr(left)?;
                match operator.token_type {
                    TokenType::And if !left_value.try_borrow()?.is_truthy() => Ok(left_value),
                    TokenType::And => self.evaluate_expr(right),
                    TokenType::Or if left_value.try_borrow()?.is_truthy() => Ok(left_value),
                    TokenType::Or => self.evaluate_expr(right),
                    _ => bail!("invalid operator {:?}", operator),
                }
            }
            Expr::Call {
                callee,
                paren,
                arguments,
            } => {
                let callee = self.evaluate_expr(callee)?.try_borrow()?.clone();

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
            Expr::Get { object, name } => {
                let evaluated = self.evaluate_expr(object)?;
                let x = if let RuntimeValue::Instance(instance) = evaluated.try_borrow()?.clone() {
                    let v = instance
                        .get(&name.lexeme)
                        .context(format!("Undefined property '{}'.", name.lexeme))?
                        .clone();
                    Ok(Rc::new(RefCell::new(v)))
                } else {
                    bail!("Only instances have properties.")
                };
                x
            }

            Expr::Set {
                object,
                name,
                value,
            } => {
                let evaluated = self.evaluate_expr(object)?;
                let mut borrowed = evaluated.try_borrow_mut()?;
                let x = if let RuntimeValue::Instance(mut instance) = borrowed.clone() {
                    let value = self.evaluate_expr(value)?;

                    instance.set(name.lexeme.clone(), value.try_borrow()?.clone());
                    *borrowed = RuntimeValue::Instance(instance);

                    Ok(value)
                } else {
                    bail!("Only instances have properties.")
                };
                x
            }

            Expr::This { keyword } => self.visit_expr_var(keyword),
        }
    }

    fn visit_expr_var(&mut self, name: &Token) -> anyhow::Result<Rc<RefCell<RuntimeValue>>> {
        let key = ExprNameWithLine {
            name: name.lexeme.clone(),
            line: name.line,
            column: name.column,
        };
        match self.locals.get(&key) {
            Some(depth) => {
                let env = if *depth == 0 {
                    self.environment.clone()
                } else {
                    match self.environment.borrow().ancestor(*depth) {
                        Some(env) => env,
                        None => bail!("cannot get environment with {} depth", depth),
                    }
                };
                let v = env
                    .borrow()
                    .get(&name.lexeme)
                    .context(format!("Undefined variable '{}'.", name.lexeme));
                v
            }
            None => self
                .globals
                .borrow_mut()
                .get(&name.lexeme)
                .context(format!("Undefined variable in globals '{}'.", name.lexeme)),
        }
    }

    fn assign_at(
        &mut self,
        name: &Token,
        value: Rc<RefCell<RuntimeValue>>,
        depth: usize,
    ) -> anyhow::Result<()> {
        let env = if depth == 0 {
            self.environment.clone()
        } else {
            match self.environment.borrow().ancestor(depth) {
                Some(env) => env,
                None => bail!("cannot get environment with {} depth", depth),
            }
        };
        env.borrow_mut().assign(&name.lexeme, value)?;
        Ok(())
    }

    pub fn resolve(&mut self, name: String, line: usize, column: usize, depth: usize) {
        self.locals
            .insert(ExprNameWithLine { name, line, column }, depth);
    }
}
