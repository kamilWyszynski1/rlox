use crate::representation::token::Token;

#[derive(Debug, Clone)]
pub enum Stmt {
    Expression {
        expression: Expr,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Vec<Stmt>,
    },
    Print {
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Option<Expr>,
    },
    Block {
        statements: Vec<Stmt>,
    },
    Class {
        name: Token,
        methods: Vec<Stmt>, // list of functions
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    While {
        condition: Expr,
        statement: Box<Stmt>,
    },
    Break,
    Return {
        keyword: Token,
        expr: Option<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    True,
    False,
    Null,
    Number(f64),
    String(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal(LiteralValue),
    Grouping {
        expression: Box<Expr>,
    },
    Variable {
        name: Token,
    },
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.format_to_string())
    }
}

impl Expr {
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
            Expr::Variable { name } => {
                format!("{:?}", name)
            }
            Expr::Assign { name, value } => {
                format!("{:?} = ...", name)
            }
            Expr::Logical { .. } => "logical".to_string(),
            Expr::Call {
                callee,
                paren,
                arguments,
            } => format!("call {}", callee.format_to_string()),
        }
    }
}
