use crate::ast::ast::{Expr, Stmt};
use crate::error::error::{ErrorType, RLoxError};
use crate::interpreter::interpreter::Interpreter;
use crate::representation::token::{Token, TokenType};
use anyhow::{anyhow, bail, Context};
use std::cmp::PartialEq;
use std::collections::{HashMap, HashSet, VecDeque};
use std::hash::Hash;

#[derive(Debug, Clone)]
struct VariableInfo {
    is_defined: bool,
    is_used: bool,
    is_function_param: bool,
    token: Token,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
enum FunctionType {
    Function,
    Method,
    StaticMethod,
    Initializer,
    None,
}

impl Default for FunctionType {
    fn default() -> Self {
        Self::None
    }
}

impl FunctionType {
    fn is_function_like(&self) -> bool {
        matches!(self, Self::Function | Self::Method)
    }
}

#[derive(Debug, Clone, Copy)]
enum ClassType {
    None,
    Class,
    SubClass,
}

impl Default for ClassType {
    fn default() -> Self {
        Self::None
    }
}

pub struct Resolver {
    interpreter: Interpreter,
    scopes: VecDeque<HashMap<String, VariableInfo>>,

    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            scopes: VecDeque::new(),
            current_function: FunctionType::default(),
            current_class: ClassType::default(),
        }
    }

    pub fn resolve(mut self, statements: &[Stmt]) -> anyhow::Result<Interpreter> {
        self._resolve(statements)?;
        Ok(self.interpreter)
    }

    fn _resolve(&mut self, statements: &[Stmt]) -> anyhow::Result<()> {
        for statement in statements {
            self.resolve_stmt(statement)?
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> anyhow::Result<()> {
        match stmt {
            Stmt::Expression { expression } => self.resolve_expr(&expression)?,
            Stmt::Function {
                name,
                params,
                body,
                is_static_method,
            } => {
                self.declare(&name, false)?;
                self.define(&name, false)?;

                self.resolve_function(params, body, FunctionType::Function)?;
            }
            Stmt::Print { expression } => self.resolve_expr(&expression)?,
            Stmt::Var { name, initializer } => {
                if name.lexeme.eq("super") {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(
                            "Cannot use 'super' keyword as variable name".to_string()
                        ),
                        line: name.line,
                        column: name.column,
                        start: name.start,
                        end: name.end,
                    }));
                }

                self.declare(&name, false)?; // mark as declared but not defined
                if let Some(initializer) = initializer {
                    self.resolve_expr(&initializer)?;
                }
                self.define(&name, false)?; // define after initialization
            }
            Stmt::Block { statements } => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_stmt(statement)?;
                }
                self.end_scope()?;
            }
            Stmt::While {
                condition,
                statement,
            } => {
                self.resolve_expr(&condition)?;
                self.resolve_stmt(statement)?;
            }
            Stmt::Break => {}
            Stmt::Return { expr, keyword } => {
                match self.current_function {
                    FunctionType::Initializer => {
                        if expr.is_some() {
                            return Err(anyhow!(RLoxError {
                                error: ErrorType::Resolve(
                                    "Can't return from init method".to_string()
                                ),
                                line: keyword.line,
                                column: keyword.column,
                                start: keyword.start,
                                end: keyword.end,
                            }));
                        }
                    }
                    FunctionType::None => {
                        return Err(anyhow!(RLoxError {
                            error: ErrorType::Resolve(
                                "Can't return from top-level code.".to_string()
                            ),
                            line: keyword.line,
                            column: keyword.column,
                            start: keyword.start,
                            end: keyword.end,
                        }))
                    }
                    _ => {}
                }
                if let Some(expr) = expr {
                    self.resolve_expr(&expr)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(&condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::Class {
                name,
                methods,
                static_fields,
                superclass,
            } => {
                let enclosing = self.current_class;
                self.current_class = ClassType::Class;

                self.declare(&name, false)?;
                self.define(&name, false)?;

                // resolve static fields
                has_unique_elements(static_fields)?;

                if let Some(superclass) = superclass {
                    self.current_class = ClassType::SubClass;

                    if let Expr::Variable {
                        name: superclass_token,
                    } = superclass
                    {
                        if name.lexeme == superclass_token.lexeme {
                            return Err(anyhow!(RLoxError {
                                error: ErrorType::Resolve(
                                    "A class can't inherit from itself.".to_string()
                                ),
                                line: superclass_token.line,
                                column: superclass_token.column,
                                start: superclass_token.start,
                                end: superclass_token.end,
                            }));
                        }
                    } else {
                        unreachable!()
                    }

                    self.resolve_expr(superclass)?;

                    self.begin_scope();
                    self.scopes.front_mut().context("no front context")?.insert(
                        "super".to_string(),
                        VariableInfo {
                            is_defined: true,
                            is_used: true,
                            is_function_param: false,
                            token: Token::new(TokenType::Super, "this".to_string(), 0, 0, 0, 0),
                        },
                    );
                }

                self.begin_scope();
                self.scopes.front_mut().unwrap().insert(
                    "this".to_string(),
                    VariableInfo {
                        token: Token::new(TokenType::This, "this".to_string(), 0, 0, 0, 0),
                        is_used: true,
                        is_defined: true,
                        is_function_param: false,
                    },
                );
                for method in methods {
                    if let Stmt::Function {
                        name,
                        params,
                        body,
                        is_static_method,
                    } = method
                    {
                        let fn_type = if name.lexeme.eq("init") {
                            if *is_static_method {
                                return Err(anyhow!(RLoxError {
                                    error: ErrorType::Resolve(
                                        "Init method cannot be static".to_string()
                                    ),
                                    line: name.line,
                                    column: name.column,
                                    start: name.start,
                                    end: name.end,
                                }));
                            }
                            FunctionType::Initializer
                        } else if *is_static_method {
                            FunctionType::StaticMethod
                        } else {
                            FunctionType::Method
                        };
                        self.resolve_function(params, body, fn_type)?;
                    } else {
                        bail!("Resolver: Class' method should be a function type")
                    }
                }
                self.end_scope()?;
                if superclass.is_some() {
                    self.end_scope()?;
                }
                self.current_class = enclosing;
            }

            Stmt::Enum { name, variants } => {
                self.declare(&name, false)?;
                self.define(&name, false)?;
            }

            Stmt::Match {
                variable_name,
                operations,
            } => {
                self.resolve_local(&variable_name)?;

                for (_, op) in operations {
                    self.resolve_stmt(op)?
                }
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        function_type: FunctionType,
    ) -> anyhow::Result<()> {
        let enclosing_function = self.current_function;
        self.current_function = function_type;

        self.begin_scope();
        for param in params {
            self.declare(&param, true)?;
            self.define(&param, true)?;
        }
        self._resolve(body)?;
        self.end_scope()?;

        self.current_function = enclosing_function;
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> anyhow::Result<()> {
        match expr {
            Expr::Binary {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Unary { right, operator: _ } => self.resolve_expr(right)?,
            Expr::Literal(_) => {}
            Expr::Grouping { expression } => {
                self.resolve_expr(expression)?;
            }
            Expr::Variable { name } => {
                if !self.scopes.is_empty() {
                    if let Some(scope) = self.scopes.front() {
                        if let Some(info) = scope.get(&name.lexeme) {
                            if !info.is_defined {
                                return Err(anyhow!(RLoxError {
                                    error: ErrorType::Resolve(
                                        "Can't read local variable in its own initializer."
                                            .to_string()
                                    ),
                                    line: name.line,
                                    column: name.column,
                                    start: name.start,
                                    end: name.end,
                                }));
                            }
                        }
                    }
                }
                self.resolve_local(name)?;
            }
            Expr::Assign { name, value } => {
                self.resolve_expr(&value)?;
                self.resolve_local(name)?;
            }
            Expr::Logical {
                left,
                operator: _,
                right,
            } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call {
                callee,
                paren: _,
                arguments,
            } => {
                self.resolve_expr(callee)?;
                for arg in arguments {
                    self.resolve_expr(arg)?;
                }
            }

            Expr::Get { object, name } => {
                // Since properties are looked up dynamically, they don’t get resolved.
                // During resolution, we recurse only into the expression to the left of the dot. The actual property access happens in the interpreter.
                self.resolve_expr(object)?;
            }

            Expr::Set {
                object,
                name: _,
                value,
            } => {
                // Again, like Expr::Get, the property itself is dynamically evaluated, so there’s nothing to resolve there.
                // All we need to do is recurse into the two subexpressions of Expr::Set, the object whose property is being set, and the value it’s being set to.
                self.resolve_expr(object)?;
                self.resolve_expr(value)?;
            }

            Expr::This { keyword } => {
                if !matches!(self.current_class, ClassType::Class) {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(
                            "Can't use 'this' outside of a class.".to_string()
                        ),
                        line: keyword.line,
                        column: keyword.column,
                        start: keyword.start,
                        end: keyword.end,
                    }));
                }

                if self.current_function == FunctionType::StaticMethod {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(
                            "Can't access 'this' keyword in static method.".to_string()
                        ),
                        line: keyword.line,
                        column: keyword.column,
                        start: keyword.start,
                        end: keyword.end,
                    }));
                }

                self.resolve_local(keyword)?;
            }

            Expr::Super { keyword, method } => {
                if matches!(self.current_class, ClassType::None) {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(
                            "Can't use 'this' outside of a class.".to_string()
                        ),
                        line: keyword.line,
                        column: keyword.column,
                        start: keyword.start,
                        end: keyword.end,
                    }));
                }
                if !matches!(self.current_class, ClassType::SubClass) {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(
                            "Can't use 'super' in a class with no superclass.".to_string()
                        ),
                        line: keyword.line,
                        column: keyword.column,
                        start: keyword.start,
                        end: keyword.end,
                    }));
                }
                self.resolve_local(keyword)?;
            }

            Expr::EnumVariant {
                enum_name,
                variant_name,
            } => {
                self.resolve_local(enum_name)?;
            }
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &Token) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        for i in 0..self.scopes.len() {
            let scope = self.scopes.get_mut(i).context("cannot get scope")?;
            if scope.contains_key(&name.lexeme) {
                scope.get_mut(&name.lexeme).unwrap().is_used = true;
                self.interpreter
                    .resolve(name.lexeme.clone(), name.line, name.column, i);
                return Ok(());
            }
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push_front(HashMap::new());
    }

    fn end_scope(&mut self) -> anyhow::Result<()> {
        if let Some(scope) = self.scopes.pop_front() {
            // Check whether all variables are used, if not report an error.
            // For now, it will only work for function's local variables.
            for (name, info) in scope {
                if !info.is_used && !info.is_function_param && !name.starts_with('_') {
                    return Err(anyhow!(RLoxError {
                        error: ErrorType::Resolve(format!("Variable {name} is never used")),
                        line: info.token.line,
                        column: info.token.column,
                        start: info.token.start,
                        end: info.token.end,
                    }));
                }
            }
        }
        Ok(())
    }

    fn declare(&mut self, name: &Token, is_function_param: bool) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self.scopes.front_mut().context("no front scope")?;
        if scope.contains_key(&name.lexeme) {
            return Err(anyhow!(RLoxError {
                error: ErrorType::Resolve(
                    "Already a variable with this name in this scope.".to_string()
                ),
                line: name.line,
                column: name.column,
                start: name.start,
                end: name.end,
            }));
        }
        scope.insert(
            name.lexeme.clone(),
            VariableInfo {
                is_used: false,
                is_defined: false,
                is_function_param,
                token: name.clone(),
            },
        );
        Ok(())
    }

    fn define(&mut self, name: &Token, is_function_param: bool) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.scopes.front_mut().context("no front scope")?.insert(
            name.lexeme.clone(),
            VariableInfo {
                is_defined: true,
                is_used: false,
                is_function_param,
                token: name.clone(),
            },
        );
        Ok(())
    }
}

fn has_unique_elements(iter: &[(Token, Expr)]) -> anyhow::Result<()> {
    let mut set = HashSet::new();
    for (name, _) in iter {
        if !set.insert(name.lexeme.clone()) {
            return Err(anyhow!(RLoxError {
                error: ErrorType::Resolve("Static field names must be unique.".to_string()),
                line: name.line,
                column: name.column,
                start: name.start,
                end: name.end,
            }));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::parser;
    use crate::representation::lexer::Lexer;

    #[test]
    fn test_resolving() -> anyhow::Result<()> {
        let input = r#"
        fun counter(n) {
  fun nested_addition(x, y) {
    return x + y;
  }

  while (n < 100) {
    if (n == 3) return n; // <--
    print n;
    n = nested_addition(n, 1);
  }
}

print counter(1);"#;

        let mut lexer = Lexer::new(&input);
        let tokens = lexer.scan_tokens()?;
        let expressions = parser::Parser::new(tokens).parse()?;

        let resolver: Resolver = Resolver::new(Interpreter::new());
        let _interpreter = resolver.resolve(&expressions)?;
        Ok(())
    }
}
