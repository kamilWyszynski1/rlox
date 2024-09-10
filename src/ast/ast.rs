use crate::interpreter::token::Token;
enum Expr<'a> {
    Binary {
        left: Box<Expr<'a>>,
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },
    Unary {
        operator: Token<'a>,
        right: Box<Expr<'a>>,
    },
    Literal {
        value: String,
    },
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
            Expr::Literal { value } => value.clone(),
            Expr::Grouping { expression } => {
                format!("({})", expression)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::token::TokenType;

    #[test]
    fn test_format_to_string() {
        let expr = Expr::Binary {
            left: Box::new(Expr::Unary {
                operator: Token::new(TokenType::Minus, "-".to_string(), 1),
                right: Box::new(Expr::Literal {
                    value: "123".to_string(),
                }),
            }),
            operator: Token::new(TokenType::Star, "*".to_string(), 1),
            right: Box::new(Expr::Grouping {
                expression: Box::new(Expr::Literal {
                    value: "45.67".to_string(),
                }),
            }),
        };
        assert_eq!(expr.format_to_string(), "(* (- 123) (45.67))");
    }
}
