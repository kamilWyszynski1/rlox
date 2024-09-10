use crate::interpreter::token;
use crate::interpreter::token::Token;
use crate::interpreter::token::TokenType::*;
use anyhow::bail;

struct Lexer<'a> {
    source: &'a str,
    index: usize,
    line: usize,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            line: 0,
            index: 0,
        }
    }

    fn scan_tokens(&mut self) -> anyhow::Result<Vec<Token>> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            match self.source.chars().nth(self.index) {
                None => {
                    break;
                }
                Some(ch) => {
                    let start = self.index;
                    let mut end = self.index + 1;

                    let token_type = match ch {
                        '\n' => {
                            self.line += 1;
                            self.index += 1;
                            continue;
                        }
                        ' ' | '\t' => {
                            self.index += 1; // skip this character
                            continue;
                        } // ignore whitespaces
                        '(' => LeftParen,
                        ')' => RightParen,
                        '{' => LeftBrace,
                        '}' => RightBrace,
                        ',' => Comma,
                        '.' => Dot,
                        '+' => Plus,
                        '-' => Minus,
                        ';' => Semicolon,
                        '/' => {
                            // check for TokenType::Slash or comment
                            if self.match_character('/') {
                                while self.source.chars().nth(self.index) != Some('\n')
                                    && !self.is_end()
                                {
                                    self.index += 1;
                                }
                                continue;
                            } else {
                                Slash
                            }
                        }
                        '*' => Star,
                        '!' => {
                            if self.match_character('=') {
                                end += 1;
                                BangEqual
                            } else {
                                Bang
                            }
                        }
                        '=' => {
                            if self.match_character('=') {
                                end += 1;
                                EqualEqual
                            } else {
                                Equal
                            }
                        }
                        '>' => {
                            if self.match_character('=') {
                                end += 1;
                                GreaterEqual
                            } else {
                                Greater
                            }
                        }
                        '<' => {
                            if self.match_character('=') {
                                end += 1;
                                LessEqual
                            } else {
                                Less
                            }
                        }
                        '"' => {
                            self.index += 1; // skip opening "
                            let start = self.index;
                            // parse string literal
                            while self.source.chars().nth(self.index) != Some('"') && !self.is_end()
                            {
                                if self.source.chars().nth(self.index) == Some('\n') {
                                    self.line += 1;
                                }
                                self.index += 1;
                            }

                            if self.is_end() {
                                bail!("Unterminated string")
                            }

                            self.index += 1; // closing " character

                            let string = &self.source[start..self.index - 1];
                            tokens.push(Token::new(String(string), string.to_string(), self.line));
                            continue; // has to go further to omit self.index += 1 after match.
                        }
                        '0'..='9' => {
                            let start = self.index;
                            while is_digit(self.peek()) {
                                self.index += 1;
                            }

                            if self.peek() == Some('.') && is_digit(self.peek_next()) {
                                self.index += 1;
                                while is_digit(self.peek()) {
                                    self.index += 1;
                                }
                            }

                            let number = &self.source[start..self.index];
                            tokens.push(Token::new(
                                Number(number.parse()?),
                                number.to_string(),
                                self.line,
                            ));
                            continue;
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            let start = self.index;
                            while is_alphanumeric(self.peek()) {
                                self.index += 1;
                            }

                            let text = &self.source[start..self.index];
                            tokens.push(Token::new(
                                token::KEYWORDS.get(text).copied().unwrap_or(Identifier),
                                text.to_string(),
                                self.line,
                            ));
                            continue;
                        }
                        _ => {
                            unimplemented!()
                        }
                    };
                    tokens.push(Token::new(
                        token_type,
                        self.source[start..end].to_string(),
                        self.line,
                    ));
                }
            }
            self.index += 1;
        }
        Ok(tokens)
    }

    fn is_end(&self) -> bool {
        self.index >= self.source.len()
    }

    /// Returns current character without consuming
    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.index)
    }

    /// Returns current character without consuming
    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.index + 1)
    }

    fn is_digit(&self) -> bool {
        if let Some(ch) = self.source.chars().nth(self.index) {
            ch.is_ascii_digit()
        } else {
            false
        }
    }

    fn match_character(&mut self, ch: char) -> bool {
        if self.is_end() {
            return false;
        }

        if let Some(next) = self.source.chars().nth(self.index + 1) {
            if ch == next {
                self.index += 1;
                return true;
            }
        }
        false
    }
}

fn is_digit(ch: Option<char>) -> bool {
    if let Some(ch) = ch {
        ch.is_ascii_digit()
    } else {
        false
    }
}

fn is_alphanumeric(ch: Option<char>) -> bool {
    if let Some(ch) = ch {
        matches!(ch,  'a'..='z' | 'A'..='Z' | '_')
    } else {
        false
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::token::TokenType::{Greater, Less};

    #[test]
    fn test_scan_tokens() {
        let mut lexer = Lexer::new("()!=");
        let expected = vec![
            Token::new(LeftParen, "(".to_string(), 0),
            Token::new(RightParen, ")".to_string(), 0),
            Token::new(BangEqual, "!=".to_string(), 0),
        ];
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(expected, got);

        let input = "
        // this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators
";

        let expected = vec![
            Token::new(LeftParen, "(".to_string(), 2),
            Token::new(LeftParen, "(".to_string(), 2),
            Token::new(RightParen, ")".to_string(), 2),
            Token::new(RightParen, ")".to_string(), 2),
            Token::new(LeftBrace, "{".to_string(), 2),
            Token::new(RightBrace, "}".to_string(), 2),
            Token::new(Bang, "!".to_string(), 3),
            Token::new(Star, "*".to_string(), 3),
            Token::new(Plus, "+".to_string(), 3),
            Token::new(Minus, "-".to_string(), 3),
            Token::new(Slash, "/".to_string(), 3),
            Token::new(Equal, "=".to_string(), 3),
            Token::new(Less, "<".to_string(), 3),
            Token::new(Greater, ">".to_string(), 3),
            Token::new(LessEqual, "<=".to_string(), 3),
            Token::new(EqualEqual, "==".to_string(), 3),
        ];
        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(expected, got);
    }

    #[test]
    fn test_scan_tokens_string_literal() {
        let input = r#"()<>"hello world"!!!"#;

        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(
            vec![
                Token::new(LeftParen, "(".to_string(), 0),
                Token::new(RightParen, ")".to_string(), 0),
                Token::new(Less, "<".to_string(), 0),
                Token::new(Greater, ">".to_string(), 0),
                Token::new(String("hello world"), "hello world".to_string(), 0),
                Token::new(Bang, "!".to_string(), 0),
                Token::new(Bang, "!".to_string(), 0),
                Token::new(Bang, "!".to_string(), 0),
            ],
            got
        );

        let input = r#"()<>"hello
world"!!!"#;
        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(
            vec![
                Token::new(LeftParen, "(".to_string(), 0),
                Token::new(RightParen, ")".to_string(), 0),
                Token::new(Less, "<".to_string(), 0),
                Token::new(Greater, ">".to_string(), 0),
                Token::new(String("hello\nworld"), "hello\nworld".to_string(), 1),
                Token::new(Bang, "!".to_string(), 1),
                Token::new(Bang, "!".to_string(), 1),
                Token::new(Bang, "!".to_string(), 1),
            ],
            got
        );
    }

    #[test]
    fn test_scan_tokens_numbers() {
        let input = r#"123123!<>123.534"#;
        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(
            vec![
                Token::new(Number(123123.), "123123".to_string(), 0),
                Token::new(Bang, "!".to_string(), 0),
                Token::new(Less, "<".to_string(), 0),
                Token::new(Greater, ">".to_string(), 0),
                Token::new(Number(123.534), "123.534".to_string(), 0),
            ],
            got
        );
    }

    #[test]
    fn test_scan_tokens_keywords_and_identifiers() {
        let input =
            r#"if else for while fun return and class false nil or print super this true var"#;
        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(
            vec![
                Token::new(If, "if".to_string(), 0),
                Token::new(Else, "else".to_string(), 0),
                Token::new(For, "for".to_string(), 0),
                Token::new(While, "while".to_string(), 0),
                Token::new(Fun, "fun".to_string(), 0),
                Token::new(Return, "return".to_string(), 0),
                Token::new(And, "and".to_string(), 0),
                Token::new(Class, "class".to_string(), 0),
                Token::new(False, "false".to_string(), 0),
                Token::new(Nil, "nil".to_string(), 0),
                Token::new(Or, "or".to_string(), 0),
                Token::new(Print, "print".to_string(), 0),
                Token::new(Super, "super".to_string(), 0),
                Token::new(This, "this".to_string(), 0),
                Token::new(True, "true".to_string(), 0),
                Token::new(Var, "var".to_string(), 0)
            ],
            got
        );

        let input = r#"variableName variableName variable_name _variable_name"#;
        let mut lexer = Lexer::new(input);
        let got = lexer.scan_tokens().unwrap();
        assert_eq!(
            vec![
                Token::new(Identifier, "variableName".to_string(), 0),
                Token::new(Identifier, "variableName".to_string(), 0),
                Token::new(Identifier, "variable_name".to_string(), 0),
                Token::new(Identifier, "_variable_name".to_string(), 0),
            ],
            got
        );
    }
}
