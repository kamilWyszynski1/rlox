use crate::ast::ast::Expr::Literal;
use crate::ast::ast::{Expr, LiteralValue};
use crate::representation::token::{Token, TokenType};
use anyhow::{bail, Context};

// Grammar:
//
// expression     → equality ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary
//                | primary ;
// primary        → NUMBER | STRING | "true" | "false" | "nil"
//                | "(" expression ")" ;
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
    pub fn parse(&mut self) -> anyhow::Result<Expr<'a>> {
        self.expression()
    }

    fn expression(&mut self) -> anyhow::Result<Expr<'a>> {
        self.equality()
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

    fn consume(&mut self, check_type: TokenType) -> anyhow::Result<()> {
        if let Some(_) = self.match_token_types(&[check_type]) {
            return Ok(());
        }

        let token = self.peek();
        bail!("unexpected token {:?}", token);
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
            Token {
                token_type: TokenType::Number(1.0),
                lexeme: "1".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::LeftParen,
                lexeme: "(".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number(2.0),
                lexeme: "2".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::Star,
                lexeme: "*".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::Number(3.0),
                lexeme: "3".to_string(),
                line: 1,
            },
            Token {
                token_type: TokenType::RightParen,
                lexeme: ")".to_string(),
                line: 1,
            },
        ];

        let mut parser = Parser::new(tokens);
        let result = parser.parse();

        assert!(
            result.is_ok(),
            "Parser should successfully parse the expression"
        );
        assert_eq!(
            "(+ 1 (group (* 2 3)))".to_string(),
            result.unwrap().to_string()
        );

        let tokens = vec![
            Token::new(TokenType::LeftParen, "(".to_string(), 0),
            Token::new(TokenType::Number(1.), "1".to_string(), 0),
            Token::new(TokenType::Plus, "+".to_string(), 0),
            Token::new(TokenType::Number(2.), "2".to_string(), 0),
            Token::new(TokenType::RightParen, ")".to_string(), 0),
            Token::new(TokenType::Star, "*".to_string(), 0),
            Token::new(TokenType::Number(3.), "3".to_string(), 0),
        ];
        let mut parser = Parser::new(tokens);
        let result = parser.parse().unwrap();
        assert_eq!("(* (group (+ 1 2)) 3)".to_string(), result.to_string());
    }
}
