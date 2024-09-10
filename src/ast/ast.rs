use crate::representation::token::Token;

pub enum Stmt<'a> {
    Expression { expression: Expr<'a> },
    Print { expression: Expr<'a> },
}

impl<'a> std::fmt::Display for Stmt<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format_to_string())
    }
}

impl Stmt<'_> {
    fn format_to_string(&self) -> String {
        match self {
            Stmt::Expression { expression } => expression.format_to_string(),
            Stmt::Print { expression } => {
                format!("print {}", expression)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    True,
    False,
    Null,
    Number(f64),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },
    Unary {
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },
    Literal(LiteralValue),
    Grouping {
        expression: Box<Expr<'a>>,
    },
}

impl<'a> std::fmt::Display for Expr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format_to_string())
    }
}

impl<'a> Expr<'_> {
    fn format_to_string(&self) -> String {
        match self {
            Expr::Binary {
                ref left,
                operator,
                ref right,
            } => {
                format!("({} {} {})", operator.lexeme, left, right)
            }
            Expr::Unary { operator, right } => {
                format!("({} {})", operator.lexeme, right)
            }
            Expr::Literal(value) => match value {
                LiteralValue::True => "true".to_string(),
                LiteralValue::False => "false".to_string(),
                LiteralValue::Null => "null".to_string(),
                LiteralValue::String(s) => s.clone(),
                LiteralValue::Number(number) => number.to_string(),
            },
            Expr::Grouping { expression } => {
                format!("(group {})", expression)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::ast::LiteralValue::String;
    use crate::representation::token::TokenType;

    #[test]
    fn test_format_to_string() {
        let expr = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Token::new(TokenType::Minus, "-".to_string(), 1),
                right: Box::new(Expr::Literal(String("123".to_string()))),
            }),
            operator: Token::new(TokenType::Star, "*".to_string(), 1),
            right: Box::new(Expr::Grouping {
                expression: Box::new(Expr::Literal(String("45.67".to_string()))),
            }),
        };
        assert_eq!(expr.format_to_string(), "(* (- 123) (group 45.67))");

        let expr = Expr::Binary {
            left: Box::new(Expr::Literal(String("1".to_string()))),
            operator: Token {
                token_type: TokenType::Plus,
                lexeme: "+".to_string(),
                line: 1,
            },
            right: Box::new(Expr::Grouping {
                expression: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(String("2".to_string()))),
                    operator: Token {
                        token_type: TokenType::Star,
                        lexeme: "*".to_string(),
                        line: 1,
                    },
                    right: Box::new(Expr::Literal(String("3".to_string()))),
                }),
            }),
        };
        assert_eq!(expr.format_to_string(), "(+ 1 (group (* 2 3)))");
    }
}
