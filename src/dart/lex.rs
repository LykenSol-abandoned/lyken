use unicode_xid::UnicodeXID;
use std::str;
use syntax::symbol::Symbol;
use std::fmt;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Token {
    WhiteSpace(Symbol),
    Punctuation(char),
    Identifier(Symbol),
    IntegerLiteral(Symbol),
    StringLiteral {
        contents: Symbol,
        raw: bool,
        triple: bool,
        quote: char,
        interpolation_before: bool,
        interpolation_after: bool,
    },
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::WhiteSpace(s) |
            Token::IntegerLiteral(s) |
            Token::Identifier(s) => write!(f, "{}", s),
            Token::Punctuation(c) => write!(f, "{}", c),
            Token::StringLiteral {
                contents,
                raw,
                triple,
                quote,
                interpolation_before,
                interpolation_after,
            } => {
                if raw {
                    write!(f, "r")?;
                }
                if interpolation_before {
                    write!(f, "}}")?;
                } else {
                    if triple {
                        write!(f, "{}{}", quote, quote)?;
                    }
                    write!(f, "{}", quote)?;
                }
                write!(f, "{}", &contents.as_str())?;
                if interpolation_after {
                    write!(f, "${{")?;
                } else {
                    write!(f, "{}", quote)?;
                    if triple {
                        write!(f, "{}{}", quote, quote)?;
                    }
                }
                Ok(())
            }
        }
    }
}

impl Token {
    pub fn as_ident(&self) -> Option<Symbol> {
        if let Token::Identifier(ident) = *self {
            Some(ident)
        } else {
            None
        }
    }

    pub fn is_whitespace(&self) -> bool {
        match *self {
            Token::WhiteSpace(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum Error {
    UnterminatedStringLiteral,
    UnterminatedBlockComment { start: usize },
    UnhandledCharacter(char),
}

#[derive(Debug)]
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
                    bump!(or self.tokens.push(Token::IntegerLiteral(Symbol::intern(&buffer))));
                }
                self.tokens.push(
                    Token::IntegerLiteral(Symbol::intern(&buffer)),
                );
            } else if self.c.is_whitespace() {
                while self.c.is_whitespace() {
                    buffer.push(self.c);
                    bump!(or self.tokens.push(Token::WhiteSpace(Symbol::intern(&buffer))));
                }
                self.tokens.push(Token::WhiteSpace(Symbol::intern(&buffer)));
            } else if UnicodeXID::is_xid_start(self.c) || self.c == '$' || self.c == '_' {
                while UnicodeXID::is_xid_continue(self.c) || self.c == '$' {
                    buffer.push(self.c);
                    bump!(or self.tokens.push(Token::Identifier(Symbol::intern(&buffer))));
                }
                self.tokens.push(Token::Identifier(Symbol::intern(&buffer)));
            } else if "{}[]()=+-*%&|.,:;!<>@~#?^".contains(self.c) && !interpolation_before {
                self.tokens.push(Token::Punctuation(self.c));
                bump!();
            } else if self.c == '/' {
                bump!(or self.tokens.push(Token::Punctuation (self.c)));
                if self.c == '/' {
                    buffer.push('/');
                    while self.c != '\n' {
                        buffer.push(self.c);
                        bump!(or self.tokens.push(Token::WhiteSpace(Symbol::intern(&buffer))));
                    }
                    self.tokens.push(Token::WhiteSpace(Symbol::intern(&buffer)));
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
                    self.tokens.push(Token::WhiteSpace(Symbol::intern(&buffer)));
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
                        if contents.as_str().is_empty() && prev_quote == quote {
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
                    contents: Symbol::intern(&buffer),
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
