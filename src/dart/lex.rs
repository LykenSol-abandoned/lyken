use unicode_xid::UnicodeXID;

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

pub fn tokenize(src: &str) -> Result<Vec<Token>, ErrorLocation> {
    let mut chars = src.chars();
    let mut c = '\n';
    let mut tokens = vec![];
    let mut line = 0;
    struct InterpolationLevel {
        quote: char,
        triple: bool,
        brace_depth: usize,
    }
    let mut interpolation_levels: Vec<InterpolationLevel> = vec![];
    macro_rules! bump {
    	(or $otherwise:stmt) => {
            match chars.next() {
                Some(next) => {
                    if c == '\n' {
                        line += 1;
                    }
                    c = next
                }
                None => {
                    $otherwise;
                    return Ok(tokens);
                }
            }
    	};
        () => {
            bump!(or {})
        }
    }
    macro_rules! emit {
        ($e:expr) => (Err(ErrorLocation { err: $e, line })?)
    }
    bump!();
    loop {
        let mut buffer = String::new();
        let mut interpolation_before = false;
        if let Some(last) = interpolation_levels.last_mut() {
            if c == '{' {
                last.brace_depth += 1;
            } else if c == '}' {
                last.brace_depth -= 1;
                if last.brace_depth == 0 {
                    interpolation_before = true;
                }
            }
        }

        if c.is_numeric() {
            while c.is_alphanumeric() {
                buffer.push(c);
                bump!(or tokens.push(Token::IntegerLiteral(buffer)));
            }
            tokens.push(Token::IntegerLiteral(buffer));
        } else if c.is_whitespace() {
            while c.is_whitespace() {
                buffer.push(c);
                bump!(or tokens.push(Token::WhiteSpace(buffer)));
            }
            tokens.push(Token::WhiteSpace(buffer));
        } else if UnicodeXID::is_xid_start(c) || c == '$' || c == '_' {
            while UnicodeXID::is_xid_continue(c) || c == '$' {
                buffer.push(c);
                bump!(or tokens.push(Token::Identifier(buffer)));
            }
            tokens.push(Token::Identifier(buffer));
        } else if "{}[]()=+-*%&|.,:;!<>@~#?^".contains(c) && !interpolation_before {
            tokens.push(Token::Punctuation(c));
            bump!();
        } else if c == '/' {
            bump!(or tokens.push(Token::Punctuation(c)));
            if c == '/' {
                buffer.push('/');
                while c != '\n' {
                    buffer.push(c);
                    bump!(or tokens.push(Token::WhiteSpace(buffer)));
                }
                tokens.push(Token::WhiteSpace(buffer));
            } else if c == '*' {
                let mut depth = 0;
                let unterminated = Error::UnterminatedBlockComment { start: line };
                buffer.push_str("/*");
                bump!(or emit!(unterminated));
                loop {
                    if c == '*' {
                        buffer.push(c);
                        bump!(or emit!(unterminated));
                        if c == '/' {
                            if depth == 0 {
                                break;
                            }
                            depth -= 1;
                        } else {
                            continue;
                        }
                    } else if c == '/' {
                        buffer.push(c);
                        bump!(or emit!(unterminated));
                        if c == '*' {
                            depth += 1;
                        } else {
                            continue;
                        }
                    }

                    buffer.push(c);
                    bump!(or emit!(unterminated));
                }
                buffer.push('/');
                tokens.push(Token::WhiteSpace(buffer));
                bump!();

            } else {
                tokens.push(Token::Punctuation('/'));
            }
        } else if c == '\'' || c == '"' || interpolation_before {

            let mut quote = c;
            let mut raw = false;
            let mut triple = false;
            let mut interpolation_after = false;
            match tokens.last() {
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
                tokens.pop();
            }
            if interpolation_before {
                let original = interpolation_levels.pop().unwrap();
                quote = original.quote;
                triple = original.triple;
            }
            bump!(or emit!(Error::UnterminatedStringLiteral));
            loop {
                if c == quote {
                    if !triple {
                        break;
                    }
                    bump!(or emit!(Error::UnterminatedStringLiteral));
                    if c == quote {
                        bump!(or emit!(Error::UnterminatedStringLiteral));
                        if c == quote {
                            break;
                        }
                        buffer.push(quote);
                    }
                    buffer.push(quote);
                }
                if c == '\n' && !triple {
                    emit!(Error::UnterminatedStringLiteral)
                }
                if c == '$' && !raw {
                    bump!(or emit!(Error::UnterminatedStringLiteral));
                    if c == '{' {
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
                if c == '\\' && !raw {
                    buffer.push(c);
                    bump!(or emit!(Error::UnterminatedStringLiteral));
                }
                buffer.push(c);
                bump!(or emit!(Error::UnterminatedStringLiteral));
            }
            tokens.push(Token::StringLiteral {
                contents: buffer,
                raw,
                triple,
                quote,
                interpolation_after,
                interpolation_before,
            });
            bump!();
        } else {
            emit!(Error::UnhandledCharacter(c))
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
