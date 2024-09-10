use crate::ast::ast::Expr::{Assign, Literal, Variable};
use crate::ast::ast::{Expr, LiteralValue, Stmt};
use crate::representation::token::TokenType::{
    And, Else, Equal, Identifier, If, LeftBrace, LeftParen, Or, Print, RightBrace, RightParen,
    Semicolon, Var,
};
use crate::representation::token::{Token, TokenType};
use anyhow::{bail, Context};

// Grammar:
//
// program        → declaration* EOF ;
//
// declaration    → varDecl
//                | statement ;
//
// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement      → exprStmt
//                | ifStmt
//                | printStmt
//                | block ;
//
// exprStmt       → expression ";" ;
//
// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;
//
// printStmt      → "print" expression ";" ;
//
// block          → "{" declaration* "}" ;
//
// expression     → assignment ;
// assignment     → IDENTIFIER "=" assignment
//                | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → "true" | "false" | "nil"
//                | NUMBER | STRING
//                | "(" expression ")"
//                | IDENTIFIER ;
pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,

    /// Points to the next token waiting to be parsed.
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token<'a>>) -> Self {
        Self { tokens, current: 0 }
    }

    /// Starts parsing process.
    pub fn parse(&mut self) -> anyhow::Result<Vec<Stmt<'a>>> {
        // self.expression()
        let mut statements = Vec::new();
        while !self.is_at_end() {
            let stmt = match self.declaration() {
                Ok(stmt) => stmt,
                Err(err) => {
                    self.synchronize().context("cannot synchronize")?;
                    eprintln!("parser error: {}", err);
                    continue;
                }
            };
            statements.push(stmt);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> anyhow::Result<Stmt<'a>> {
        match self.match_token_types(&[Var]) {
            // TODO: synchronize?
            Some(_) => self.var_declaration(),
            None => self.statement(),
        }
    }

    fn var_declaration(&mut self) -> anyhow::Result<Stmt<'a>> {
        let name = self.consume(Identifier).context("Expect variable name")?;
        let initializer = match self.match_token_types(&[Equal]) {
            Some(_) => Some(self.expression()?),
            None => {
                // no initializer
                None
            }
        };
        self.consume(Semicolon)
            .context("Expect ';' after variable declaration")?;

        Ok(Stmt::Var { initializer, name })
    }

    fn statement(&mut self) -> anyhow::Result<Stmt<'a>> {
        match self.match_token_types(&[Print, LeftBrace, If]) {
            Some(token) => match token.token_type {
                Print => self.print_statement(),
                LeftBrace => self.block(),
                If => self.if_statement(),
                _ => bail!("unsupported token type in statement method"),
            },
            None => self.expression_statement(),
        }
    }

    fn if_statement(&mut self) -> anyhow::Result<Stmt<'a>> {
        self.consume(LeftParen).context("Expect '(' after 'if'")?;
        let condition = self.expression()?;
        self.consume(RightParen)
            .context("Expect ')' after 'if' condition")?;
        let then_branch = Box::new(self.statement()?);

        let else_branch = if let Some(_) = self.match_token_types(&[Else]) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn block(&mut self) -> anyhow::Result<Stmt<'a>> {
        let mut declarations = Vec::new();
        while self
            .peek()
            .map_or(false, |token| token.token_type != RightBrace)
        {
            declarations.push(self.declaration()?);
        }
        self.consume(RightBrace).context("Expect '}' after block")?;
        Ok(Stmt::Block {
            statements: declarations,
        })
    }

    fn print_statement(&mut self) -> anyhow::Result<Stmt<'a>> {
        let value = self.expression()?;
        self.consume(Semicolon)
            .context("Expected ';' after print statement")?;
        Ok(Stmt::Print { expression: value })
    }

    fn expression_statement(&mut self) -> anyhow::Result<Stmt<'a>> {
        let expression = self.expression()?;
        self.consume(Semicolon).context("Expected ';'")?;
        Ok(Stmt::Expression { expression })
    }

    fn expression(&mut self) -> anyhow::Result<Expr<'a>> {
        self.assignment()
    }

    fn assignment(&mut self) -> anyhow::Result<Expr<'a>> {
        let expr = self.logic_or()?;

        match self.match_token_types(&[Equal]) {
            Some(_operator) => {
                let equals = self
                    .previous()
                    .cloned()
                    .context("cannot find previous token after '='")?;
                let value = self.assignment()?;

                if let Variable { name } = expr {
                    Ok(Assign {
                        name,
                        value: Box::new(value),
                    })
                } else {
                    bail!("Invalid assignment target {:?}", &equals);
                }
            }
            None => Ok(expr),
        }
    }

    fn logic_or(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.logic_and()?;

        if let Some(token) = self.match_token_types(&[Or]) {
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn logic_and(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.equality()?;

        if let Some(token) = self.match_token_types(&[And]) {
            let right = self.equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: token,
                right: Box::new(right),
            }
        }
        Ok(expr)
    }

    fn equality(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.comparison()?;

        while let Some(operator) =
            self.match_token_types(&[TokenType::BangEqual, TokenType::EqualEqual])
        {
            let operator = operator.clone();
            let right = self.comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.term()?;

        while let Some(operator) = self.match_token_types(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = operator.clone();
            let right = self.term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn term(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.factor()?;

        while let Some(operator) = self.match_token_types(&[TokenType::Minus, TokenType::Plus]) {
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn factor(&mut self) -> anyhow::Result<Expr<'a>> {
        let mut expr = self.unary()?;

        while let Some(operator) = self.match_token_types(&[TokenType::Slash, TokenType::Star]) {
            let operator = operator.clone();
            let right = self.factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn unary(&mut self) -> anyhow::Result<Expr<'a>> {
        if let Some(operator) = self.match_token_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = operator.clone();
            let right = self.factor()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.primary()
    }

    fn primary(&mut self) -> anyhow::Result<Expr<'a>> {
        let expr = match self.peek() {
            None => bail!("unexpected end of file"),
            Some(token) => match token.token_type {
                TokenType::True => Literal(LiteralValue::True),
                TokenType::False => Literal(LiteralValue::False),
                TokenType::Nil => Literal(LiteralValue::Null),
                TokenType::String(string) => Literal(LiteralValue::String(string.to_string())),
                TokenType::Number(number) => Literal(LiteralValue::Number(number)),
                TokenType::Identifier => Variable {
                    name: token.clone(),
                },
                TokenType::LeftParen => {
                    self.current += 1; // consume
                    let expression = self.expression()?;
                    self.consume(TokenType::RightParen)
                        .context("expected ')' after expression")?;
                    return Ok(Expr::Grouping {
                        expression: Box::new(expression),
                    });
                }
                _ => bail!("expression expected, {:?}", token),
            },
        };
        self.current += 1; // consume
        Ok(expr)
    }

    fn match_token_types(&mut self, token_types: &[TokenType<'_>]) -> Option<Token<'a>> {
        for tk in token_types {
            let token = match self.peek() {
                None => continue,
                Some(token) => {
                    if token.token_type == *tk {
                        token.clone()
                    } else {
                        continue;
                    }
                }
            };

            self.current += 1; // consume
            return Some(token);
        }
        None
    }

    fn peek(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Token<'a>> {
        self.tokens.get(self.current - 1)
    }

    fn consume(&mut self, check_type: TokenType) -> anyhow::Result<Token<'a>> {
        if let Some(token) = self.match_token_types(&[check_type]) {
            return Ok(token);
        }

        let token = self.peek();
        bail!("unexpected token {:?}, expected: {:?}", token, check_type);
    }

    fn is_at_end(&mut self) -> bool {
        self.current >= self.tokens.len()
    }

    /// Helps the parser realign to a good state and resume parsing useful input.
    /// It should be called after catching and error to "reset" Parser to state where
    /// we could continue to parse tokens.
    fn synchronize(&mut self) -> anyhow::Result<()> {
        self.current += 1;

        while let Some(token) = self.peek() {
            if self.previous().map_or(false, |previous| {
                previous.token_type == TokenType::Semicolon
            }) {
                return Ok(());
            }
            match token.token_type {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => return Ok(()),
                _ => (),
            }
            self.current += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse() {
        // Define some tokens for the expression: 1 + (2 * 3)
        let tokens = vec![
            Token::new(TokenType::Number(1.0), "1".to_string(), 1),
            Token::new(TokenType::Plus, "+".to_string(), 1),
            Token::new(TokenType::LeftParen, "(".to_string(), 1),
            Token::new(TokenType::Number(2.0), "2".to_string(), 1),
            Token::new(TokenType::Star, "*".to_string(), 1),
            Token::new(TokenType::Number(3.0), "3".to_string(), 1),
            Token::new(TokenType::RightParen, ")".to_string(), 1),
            Token::new(TokenType::Semicolon, ";".to_string(), 1),
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();
        assert_eq!(
            "(+ 1 (group (* 2 3)))".to_string(),
            result.unwrap()[0].to_string()
        );

        let tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), 0),
            Token::new(TokenType::Number(1.), "1".to_string(), 0),
            Token::new(TokenType::Plus, "+".to_string(), 0),
            Token::new(TokenType::Number(2.), "2".to_string(), 0),
            Token::new(TokenType::RightParen, ")".to_string(), 0),
            Token::new(TokenType::Star, "*".to_string(), 0),
            Token::new(TokenType::Number(3.), "3".to_string(), 0),
            Token::new(TokenType::Semicolon, ";".to_string(), 0),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse().unwrap();
        assert_eq!("(* (group (+ 1 2)) 3)".to_string(), result[0].to_string());
    }
}
