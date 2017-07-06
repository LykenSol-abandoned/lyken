use unicode_xid::UnicodeXID;
use std::str;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    WhiteSpace(String),
    Punctuation(char),
    Identifier(String),
    IntegerLiteral(String),
    StringLiteral {
        contents: String,
        raw: bool,
        triple: bool,
        quote: char,
        interpolation_before: bool,
        interpolation_after: bool,
    },
}

impl Token {
    pub fn as_ident(&self) -> Option<&str> {
        if let Token::Identifier(ref ident) = *self {
            Some(ident)
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnterminatedStringLiteral,
    UnterminatedBlockComment { start: usize },
    UnhandledCharacter(char),
}

pub struct ErrorLocation {
    pub err: Error,
    pub line: usize,
}

pub struct Lexer<'a> {
    chars: str::Chars<'a>,
    c: char,
    tokens: Vec<Token>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Lexer {
            chars: src.chars(),
            c: '\n',
            tokens: vec![],
            line: 0,
        }
    }

    pub fn tokenize(mut self) -> Result<Vec<Token>, ErrorLocation> {
        struct InterpolationLevel {
            quote: char,
            triple: bool,
            brace_depth: usize,
        }
        let mut interpolation_levels: Vec<InterpolationLevel> = vec![];

        macro_rules! bump {
            (or $otherwise:stmt) => {
                match self.chars.next() {
                    Some(next) => {
                        if self.c == '\n' {
                            self.line += 1;
                        }
                        self.c = next
                    }
                    None => {
                        $otherwise;
                        return Ok(self.tokens);
                    }
                }
            };
            () => {
                bump!(or {})
            }
        }
        macro_rules! emit {
            ($e:expr) => (Err(ErrorLocation { err: $e, line: self.line })?)
        }
        bump!();
        loop {
            let mut buffer = String::new();
            let mut interpolation_before = false;
            if let Some(last) = interpolation_levels.last_mut() {
                if self.c == '{' {
                    last.brace_depth += 1;
                } else if self.c == '}' {
                    last.brace_depth -= 1;
                    if last.brace_depth == 0 {
                        interpolation_before = true;
                    }
                }
            }

            if self.c.is_numeric() {
                while self.c.is_alphanumeric() {
                    buffer.push(self.c);
                    bump!(or self.tokens.push(Token::IntegerLiteral(buffer)));
                }
                self.tokens.push(Token::IntegerLiteral(buffer));
            } else if self.c.is_whitespace() {
                while self.c.is_whitespace() {
                    buffer.push(self.c);
                    bump!(or self.tokens.push(Token::WhiteSpace(buffer)));
                }
                self.tokens.push(Token::WhiteSpace(buffer));
            } else if UnicodeXID::is_xid_start(self.c) || self.c == '$' || self.c == '_' {
                while UnicodeXID::is_xid_continue(self.c) || self.c == '$' {
                    buffer.push(self.c);
                    bump!(or self.tokens.push(Token::Identifier(buffer)));
                }
                self.tokens.push(Token::Identifier(buffer));
            } else if "{}[]()=+-*%&|.,:;!<>@~#?^".contains(self.c) && !interpolation_before {
                self.tokens.push(Token::Punctuation(self.c));
                bump!();
            } else if self.c == '/' {
                bump!(or self.tokens.push(Token::Punctuation (self.c)));
                if self.c == '/' {
                    buffer.push('/');
                    while self.c != '\n' {
                        buffer.push(self.c);
                        bump!(or self.tokens.push(Token::WhiteSpace(buffer)));
                    }
                    self.tokens.push(Token::WhiteSpace(buffer));
                } else if self.c == '*' {
                    let mut depth = 0;
                    let unterminated = Error::UnterminatedBlockComment { start: self.line };
                    buffer.push_str("/*");
                    bump!(or emit!(unterminated));
                    loop {
                        if self.c == '*' {
                            buffer.push(self.c);
                            bump!(or emit!(unterminated));
                            if self.c == '/' {
                                if depth == 0 {
                                    break;
                                }
                                depth -= 1;
                            } else {
                                continue;
                            }
                        } else if self.c == '/' {
                            buffer.push(self.c);
                            bump!(or emit!(unterminated));
                            if self.c == '*' {
                                depth += 1;
                            } else {
                                continue;
                            }
                        }

                        buffer.push(self.c);
                        bump!(or emit!(unterminated));
                    }
                    buffer.push('/');
                    self.tokens.push(Token::WhiteSpace(buffer));
                    bump!();

                } else {
                    self.tokens.push(Token::Punctuation('/'));
                }
            } else if self.c == '\'' || self.c == '"' || interpolation_before {

                let mut quote = self.c;
                let mut raw = false;
                let mut triple = false;
                let mut interpolation_after = false;
                match self.tokens.last() {
                    Some(&Token::Identifier(ref last)) => {
                        if *last == "r" {
                            raw = true;
                        }
                    }
                    Some(&Token::StringLiteral {
                        ref contents,
                        raw: prev_raw,
                        triple: false,
                        quote: prev_quote,
                        interpolation_after: false,
                        interpolation_before: false,
                    }) => {
                        if contents.is_empty() && prev_quote == quote {
                            triple = true;
                            raw = prev_raw;
                        }
                    }
                    _ => {}
                }
                if raw || triple {
                    self.tokens.pop();
                }
                if interpolation_before {
                    let original = interpolation_levels.pop().unwrap();
                    quote = original.quote;
                    triple = original.triple;
                }
                bump!(or emit!(Error::UnterminatedStringLiteral));
                loop {
                    if self.c == quote {
                        if !triple {
                            break;
                        }
                        bump!(or emit!(Error::UnterminatedStringLiteral));
                        if self.c == quote {
                            bump!(or emit!(Error::UnterminatedStringLiteral));
                            if self.c == quote {
                                break;
                            }
                            buffer.push(quote);
                        }
                        buffer.push(quote);
                    }
                    if self.c == '\n' && !triple {
                        emit!(Error::UnterminatedStringLiteral)
                    }
                    if self.c == '$' && !raw {
                        bump!(or emit!(Error::UnterminatedStringLiteral));
                        if self.c == '{' {
                            interpolation_levels.push(InterpolationLevel {
                                quote,
                                triple,
                                brace_depth: 1,
                            });
                            interpolation_after = true;
                            break;
                        }
                        buffer.push('$');
                        continue;
                    }
                    if self.c == '\\' && !raw {
                        buffer.push(self.c);
                        bump!(or emit!(Error::UnterminatedStringLiteral));
                    }
                    buffer.push(self.c);
                    bump!(or emit!(Error::UnterminatedStringLiteral));
                }
                self.tokens.push(Token::StringLiteral {
                    contents: buffer,
                    raw,
                    triple,
                    quote,
                    interpolation_after,
                    interpolation_before,
                });
                bump!();
            } else {
                emit!(Error::UnhandledCharacter(self.c))
            }
        }
    }
}

pub fn stringify(tokens: &[Token]) -> String {
    let mut text = String::new();
    for token in tokens {
        match *token {
            Token::WhiteSpace(ref s) |
            Token::IntegerLiteral(ref s) |
            Token::Identifier(ref s) => text.push_str(s),
            Token::Punctuation(c) => text.push(c),
            Token::StringLiteral {
                ref contents,
                raw,
                triple,
                quote,
                interpolation_before,
                interpolation_after,
            } => {
                if raw {
                    text.push('r');
                }
                if interpolation_before {
                    text.push('}');
                } else {
                    if triple {
                        text.push(quote);
                        text.push(quote);
                    }
                    text.push(quote);
                }
                text.push_str(&contents);
                if interpolation_after {
                    text.push_str("${");
                } else {
                    text.push(quote);
                    if triple {
                        text.push(quote);
                        text.push(quote);
                    }
                }
            }
        }
    }
    text
}
