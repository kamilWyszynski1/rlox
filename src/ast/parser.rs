use crate::ast::ast::Expr::{Assign, Literal, Variable};
use crate::ast::ast::{Expr, LiteralValue, Stmt};
use crate::representation::token::TokenType::{
    And, Break, Class, Else, Equal, For, Fun, Identifier, If, LeftBrace, LeftParen, Or, Print,
    Return, RightBrace, RightParen, Semicolon, Var, While,
};
use crate::representation::token::{Token, TokenType};
use anyhow::{bail, Context};

// Grammar:
//
// program        → declaration* EOF ;
//
// declaration    → classDecl
//                | funDecl
//                | varDecl
//                | statement ;
//
// classDecl      → "class" IDENTIFIER "{" function* "}" ;
//
// funDecl        → "fun" function ;
// function       → IDENTIFIER "(" parameters? ")" block ;
// parameters     → IDENTIFIER ( "," IDENTIFIER )* ;
//
// varDecl        → "var" IDENTIFIER ( "=" expression )? ";" ;
//
// statement      → exprStmt
//                | forStmt
//                | ifStmt
//                | printStmt
//                | returnStmt
//                | whileStmt
//                | block ;
//
// returnStmt     → "return" expression? ";" ;
//
// forStmt        → "for" "(" ( varDecl | exprStmt | ";" )
//                  expression? ";"
//                  expression? ")" statement ;
//
// exprStmt       → expression ";" ;
//
// whileStmt      → "while" "(" expression ")" statement ;
//
// ifStmt         → "if" "(" expression ")" statement
//                ( "else" statement )? ;
//
// printStmt      → "print" expression ";" ;
//
// block          → "{" declaration* "}" ;
//
// expression     → assignment ;
// assignment     → ( call "." )? IDENTIFIER "=" assignment
//                | logic_or ;
// logic_or       → logic_and ( "or" logic_and )* ;
// logic_and      → equality ( "and" equality )* ;
// equality       → comparison ( ( "!=" | "==" ) comparison )* ;
// comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
// term           → factor ( ( "-" | "+" ) factor )* ;
// factor         → unary ( ( "/" | "*" ) unary )* ;
// unary          → ( "!" | "-" ) unary | call ;
// call           → primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
// arguments      → expression ( "," expression )* ;
// primary        → "true" | "false" | "nil"
//                | NUMBER | STRING
//                | "(" expression ")"
//                | IDENTIFIER | BREAK ;
pub struct Parser {
    tokens: Vec<Token>,

    /// Points to the next token waiting to be parsed.
    current: usize,

    is_loop_open: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            current: 0,
            is_loop_open: false,
        }
    }

    /// Starts parsing process.
    pub fn parse(&mut self) -> anyhow::Result<Vec<Stmt>> {
        // self.expression()
        let mut statements = Vec::new();
        while !self.is_at_end() {
            let stmt = match self.declaration() {
                Ok(stmt) => stmt,
                Err(err) => {
                    self.synchronize().context("cannot synchronize")?;
                    println!("parser error: {:?}", err);
                    continue;
                }
            };
            statements.push(stmt);
        }
        Ok(statements)
    }

    fn declaration(&mut self) -> anyhow::Result<Stmt> {
        match self.match_token_types(&[Var, Fun, Class]) {
            // TODO: synchronize?
            Some(token) => match token.token_type {
                Var => self.var_declaration(),
                Fun => self.fun(),
                Class => self.class_declaration(),
                _ => bail!("unsupported token type in declaration method"),
            },
            None => self.statement(),
        }
    }

    fn class_declaration(&mut self) -> anyhow::Result<Stmt> {
        let name = self.consume(Identifier).context("Expect class name")?;
        self.consume(LeftBrace).context("Expect '{'")?;

        let mut methods = vec![];
        loop {
            match self.fun() {
                Ok(method) => methods.push(method),
                Err(err) => {
                    if err.to_string().contains("Expect function name") {
                        break;
                    }
                    bail!(err);
                }
            }
        }
        self.consume(RightBrace).context("Expect '}'")?;
        Ok(Stmt::Class { name, methods })
    }

    fn fun(&mut self) -> anyhow::Result<Stmt> {
        let name = self.consume(Identifier).context("Expect function name")?;
        self.consume(LeftParen)
            .context("Expect '(' after function name")?;

        let mut params = Vec::new();

        loop {
            match self.match_token_types(&[Identifier]) {
                None => break,
                Some(token) => {
                    params.push(token);
                    if let Some(peeked) = self.peek() {
                        if TokenType::Comma == peeked.token_type {
                            self.current += 1;
                            continue;
                        }
                    }
                    break;
                }
            }
        }
        self.consume(RightParen)
            .context("Expect ')' after parameters")?;
        self.consume(LeftBrace)
            .context("Expect '{' before body function")?;
        if let Stmt::Block { statements } = self.block()? {
            Ok(Stmt::Function {
                name,
                params,
                body: statements,
            })
        } else {
            bail!("expected block")
        }
    }

    fn var_declaration(&mut self) -> anyhow::Result<Stmt> {
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

    fn statement(&mut self) -> anyhow::Result<Stmt> {
        match self.match_token_types(&[Print, LeftBrace, If, While, For, Break, Return]) {
            Some(token) => match token.token_type {
                Print => self.print_statement(),
                LeftBrace => self.block(),
                If => self.if_statement(),
                While => self.while_statement(),
                For => self.for_loop_statement(),
                Break => self.break_statement(),
                Return => self.return_statement(token),
                _ => bail!("unsupported token type in statement method"),
            },
            None => self.expression_statement(),
        }
    }

    fn return_statement(&mut self, keyword: Token) -> anyhow::Result<Stmt> {
        let expr = if let Ok(expr) = self.expression() {
            Some(expr)
        } else {
            None
        };
        self.consume(Semicolon)
            .context("Expect ';' after return statement")?;
        Ok(Stmt::Return { keyword, expr })
    }

    fn break_statement(&mut self) -> anyhow::Result<Stmt> {
        self.consume(Semicolon)
            .context("Expect ';' after break statement")?;
        if self.is_loop_open {
            Ok(Stmt::Break)
        } else {
            bail!("break keyword cannot be used outside of loop")
        }
    }

    fn for_loop_statement(&mut self) -> anyhow::Result<Stmt> {
        self.consume(LeftParen).context("Expect '(' after 'for'")?;

        let initializer = match self.match_token_types(&[Semicolon, Var]) {
            Some(token) => match token.token_type {
                Semicolon => None,
                Var => Some(self.var_declaration()?),
                _ => bail!("unsupported token type as for loop initializer"),
            },
            None => Some(self.expression_statement()?),
        };

        let condition = match self.match_token_types(&[Semicolon]) {
            None => {
                let expr = Some(self.expression()?);
                self.consume(Semicolon)
                    .context("Expect ';' after loop condition")?;
                expr
            }
            Some(_) => None,
        };

        let increment = match self.match_token_types(&[RightParen]) {
            None => {
                let expr = Some(self.expression()?);
                self.consume(RightParen)
                    .context("Expect ')' after for loop increment")?;
                expr
            }
            Some(_) => None,
        };

        self.is_loop_open = true; // allow 'break' keyword to be parsed
        let mut body = self.statement()?;
        self.is_loop_open = true; // allow 'break' keyword to be parsed

        if let Some(increment) = increment {
            // the increment, if there is one, executes after the body in each iteration of the loop
            body = Stmt::Block {
                statements: vec![
                    body,
                    Stmt::Expression {
                        expression: increment,
                    },
                ],
            }
        }

        // We take the condition and the body and build the loop using a primitive while loop.
        // If the condition is omitted, we jam in true to make an infinite loop.
        body = Stmt::While {
            condition: condition.unwrap_or(Literal(LiteralValue::True)),
            statement: Box::new(body),
        };

        if let Some(initializer) = initializer {
            // if there is an initializer, it runs once before the entire loop
            body = Stmt::Block {
                statements: vec![initializer, body],
            };
        }

        Ok(body)
    }

    fn while_statement(&mut self) -> anyhow::Result<Stmt> {
        self.consume(LeftParen)
            .context("Expect '(' after 'while'")?;
        let condition = self.expression()?;
        self.consume(RightParen)
            .context("Expect ')' after 'while' condition")?;

        self.is_loop_open = true; // allow 'break' keyword to be parsed
        let statement = self.statement()?;
        self.is_loop_open = false;

        Ok(Stmt::While {
            condition,
            statement: Box::new(statement),
        })
    }

    fn if_statement(&mut self) -> anyhow::Result<Stmt> {
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

    fn block(&mut self) -> anyhow::Result<Stmt> {
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

    fn print_statement(&mut self) -> anyhow::Result<Stmt> {
        let value = self.expression()?;
        self.consume(Semicolon)
            .context("Expected ';' after print statement")?;
        Ok(Stmt::Print { expression: value })
    }

    fn expression_statement(&mut self) -> anyhow::Result<Stmt> {
        let expression = self.expression()?;
        self.consume(Semicolon).context("Expected ';'")?;
        Ok(Stmt::Expression { expression })
    }

    fn expression(&mut self) -> anyhow::Result<Expr> {
        self.assignment()
    }

    fn assignment(&mut self) -> anyhow::Result<Expr> {
        let expr = self.logic_or()?;

        match self.match_token_types(&[Equal]) {
            Some(_operator) => {
                let equals = self
                    .previous()
                    .cloned()
                    .context("cannot find previous token after '='")?;
                let value = self.assignment()?;

                match expr {
                    Expr::Variable { name } => Ok(Assign {
                        name,
                        value: Box::new(value),
                    }),
                    Expr::Get { object, name } => Ok(Expr::Set {
                        object,
                        name,
                        value: Box::new(value),
                    }),
                    _ => bail!("Invalid assignment target {:?}", &equals),
                }
            }
            None => Ok(expr),
        }
    }

    fn logic_or(&mut self) -> anyhow::Result<Expr> {
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

    fn logic_and(&mut self) -> anyhow::Result<Expr> {
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

    fn equality(&mut self) -> anyhow::Result<Expr> {
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

    fn comparison(&mut self) -> anyhow::Result<Expr> {
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

    fn term(&mut self) -> anyhow::Result<Expr> {
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

    fn factor(&mut self) -> anyhow::Result<Expr> {
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

    fn unary(&mut self) -> anyhow::Result<Expr> {
        if let Some(operator) = self.match_token_types(&[TokenType::Bang, TokenType::Minus]) {
            let operator = operator.clone();
            let right = self.factor()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.call()
    }

    /// Dot syntax should support something like that "egg.scramble(3).with(cheddar)".
    fn call(&mut self) -> anyhow::Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            match self.match_token_types(&[TokenType::LeftParen, TokenType::Dot]) {
                None => break,
                Some(token) => match token.token_type {
                    TokenType::LeftParen => expr = self.finish_call(expr)?,
                    TokenType::Dot => {
                        let name = self
                            .consume(Identifier)
                            .context("Expect property name after '.'.")?;
                        expr = Expr::Get {
                            object: Box::new(expr.clone()),
                            name,
                        };
                    }
                    _ => unreachable!(),
                },
            }
        }
        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> anyhow::Result<Expr> {
        let mut arguments = vec![];
        let paren = match self.match_token_types(&[RightParen]) {
            None => {
                arguments.push(self.expression()?); // take first argument before first comma
                while let Some(_) = self.match_token_types(&[TokenType::Comma]) {
                    // take rest of arguments
                    if arguments.len() >= 255 {
                        eprintln!("Can't have more than 255 arguments");
                    }
                    arguments.push(self.expression()?);
                }
                self.consume(RightParen)
                    .context("Expect ')' after arguments")?
            }
            Some(token) => token,
        };

        Ok(Expr::Call {
            arguments,
            callee: Box::new(callee),
            paren,
        })
    }

    fn primary(&mut self) -> anyhow::Result<Expr> {
        let expr = match self.peek() {
            None => bail!("unexpected end of file"),
            Some(token) => match &token.token_type {
                TokenType::True => Literal(LiteralValue::True),
                TokenType::False => Literal(LiteralValue::False),
                TokenType::Nil => Literal(LiteralValue::Null),
                TokenType::String(string) => Literal(LiteralValue::String(string.to_string())),
                TokenType::Number(number) => Literal(LiteralValue::Number(*number)),
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

    fn match_token_types(&mut self, token_types: &[TokenType]) -> Option<Token> {
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

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn previous(&self) -> Option<&Token> {
        self.tokens.get(self.current - 1)
    }

    fn consume(&mut self, check_type: TokenType) -> anyhow::Result<Token> {
        if let Some(token) = self.match_token_types(&[check_type.clone()]) {
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

    // #[test]
    // fn test_parse() {
    //     // Define some tokens for the expression: 1 + (2 * 3)
    //     let tokens = vec![
    //         Token::new(TokenType::Number(1.0), "1".to_string(), 1),
    //         Token::new(TokenType::Plus, "+".to_string(), 1),
    //         Token::new(TokenType::LeftParen, "(".to_string(), 1),
    //         Token::new(TokenType::Number(2.0), "2".to_string(), 1),
    //         Token::new(TokenType::Star, "*".to_string(), 1),
    //         Token::new(TokenType::Number(3.0), "3".to_string(), 1),
    //         Token::new(TokenType::RightParen, ")".to_string(), 1),
    //         Token::new(TokenType::Semicolon, ";".to_string(), 1),
    //     ];
    //
    //     let mut parser = Parser::new(tokens);
    //     let result = parser.parse();
    //     assert_eq!(
    //         "(+ 1 (group (* 2 3)))".to_string(),
    //         result.unwrap()[0].to_string()
    //     );
    //
    //     let tokens = vec![
    //         Token::new(TokenType::LeftParen, "(".to_string(), 0),
    //         Token::new(TokenType::Number(1.), "1".to_string(), 0),
    //         Token::new(TokenType::Plus, "+".to_string(), 0),
    //         Token::new(TokenType::Number(2.), "2".to_string(), 0),
    //         Token::new(TokenType::RightParen, ")".to_string(), 0),
    //         Token::new(TokenType::Star, "*".to_string(), 0),
    //         Token::new(TokenType::Number(3.), "3".to_string(), 0),
    //         Token::new(TokenType::Semicolon, ";".to_string(), 0),
    //     ];
    //     let mut parser = Parser::new(tokens);
    //     let result = parser.parse().unwrap();
    //     assert_eq!("(* (group (+ 1 2)) 3)".to_string(), result[0].to_string());
    // }
}
