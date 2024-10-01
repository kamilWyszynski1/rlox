use crate::ast::ast::{Expr, Stmt};
use crate::interpreter::interpreter::Interpreter;
use crate::representation::token::Token;
use anyhow::{bail, Context};
use std::collections::{HashMap, VecDeque};

pub struct Resolver {
    interpreter: Interpreter,
    scopes: VecDeque<HashMap<String, bool>>,
}

impl Resolver {
    pub fn new(interpreter: Interpreter) -> Self {
        Self {
            interpreter,
            scopes: VecDeque::new(),
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
            Stmt::Function { name, params, body } => {
                self.declare(&name)?;
                self.define(&name)?;

                self.resolve_function(params, body)?;
            }
            Stmt::Print { expression } => self.resolve_expr(&expression)?,
            Stmt::Var { name, initializer } => {
                self.declare(&name)?; // mark as declared but not defined
                if let Some(initializer) = initializer {
                    self.resolve_expr(&initializer)?;
                }
                self.define(&name)?; // define after initialization
            }
            Stmt::Block { statements } => {
                self.begin_scope();
                for statement in statements {
                    self.resolve_stmt(statement)?;
                }
                self.end_scope();
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
        }
        Ok(())
    }

    fn resolve_function(&mut self, params: &[Token], body: &[Stmt]) -> anyhow::Result<()> {
        self.begin_scope();
        for param in params {
            self.declare(&param)?;
            self.define(&param)?;
        }
        self._resolve(body)?;
        self.end_scope();
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
                        if let Some(value) = scope.get(&name.lexeme) {
                            if !*value {
                                bail!("Can't read local variable in its own initializer.")
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
        }
        Ok(())
    }

    fn resolve_local(&mut self, name: &Token) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        for i in 0..self.scopes.len() {
            if self
                .scopes
                .get(i)
                .context("cannot get scope")?
                .contains_key(&name.lexeme)
            {
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

    fn end_scope(&mut self) {
        self.scopes.pop_front();
    }

    fn declare(&mut self, name: &Token) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        let scope = self.scopes.front_mut().context("no front scope")?;
        if scope.contains_key(&name.lexeme) {
            bail!("Already a variable with this name in this scope.")
        }
        scope.insert(name.lexeme.clone(), false);
        Ok(())
    }

    fn define(&mut self, name: &Token) -> anyhow::Result<()> {
        if self.scopes.is_empty() {
            return Ok(());
        }
        self.scopes
            .front_mut()
            .context("no front scope")?
            .insert(name.lexeme.clone(), true);
        Ok(())
    }
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
