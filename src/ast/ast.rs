struct Token {}
enum Expr {
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Literal {
        value: String,
    },
    Grouping {
        expression: Box<Expr>,
    },
}

// impl Expr {
//     fn to_string(&self) -> String {
//         match self {
//             Expr::Binary {
//                 left,
//                 operator,
//                 right,
//             } => {
//                 format!(
//                     "({} {} {})",
//                     operator.to_string(),
//                     left.to_string(),
//                     right.to_string()
//                 )
//             }
//             Expr::Unary { operator, right } => {
//                 format!("({} {})", operator.to_string(), right.to_string())
//             }
//             Expr::Literal { value } => value.clone(),
//             Expr::Grouping { expression } => {
//                 format!("({})", expression.to_string())
//             }
//         }
//     }
// }
