#![allow(unused_doc_comments)]

use unicode_xid::UnicodeXID;
use std::fmt;
use std::path::Path;
use std::rc::Rc;
use std::str;
use syntax::codemap::{BytePos, FileMap, FileName, Pos};
use syntax::symbol::Symbol;
use Span;

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Token {
    WhiteSpace(Span),
    Comment(Span),
    Punctuation(char),
    Identifier(Symbol),
    IntegerLiteral(Symbol),
    StringLiteral {
        contents: Span,
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
            Token::WhiteSpace(s) | Token::Comment(s) => {
                write!(f, "{}", ::codemap().span_to_snippet(s.to_span()).unwrap())
            }
            Token::IntegerLiteral(s) | Token::Identifier(s) => write!(f, "{}", s),
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
                write!(
                    f,
                    "{}",
                    ::codemap().span_to_snippet(contents.to_span()).unwrap()
                )?;
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
            Token::WhiteSpace(_) | Token::Comment(_) => true,
            _ => false,
        }
    }
}

error_chain! {
    types {
        Error, ErrorKind, LexResultExt, LexResult;
    }

    errors {
        At {
            msg: ErrorMsg,
            span: Span,
        } {
            display("{:?}: {:?}", msg, span)
        }
    }
}

#[derive(Debug)]
pub enum ErrorMsg {
    UnterminatedShebang,
    UnterminatedStringLiteral,
    UnterminatedBlockComment,
    UnhandledCharacter(char),
}

pub struct Lexer {
    filemap: Rc<FileMap>,
    pos: BytePos,
    next_pos: BytePos,
    end: BytePos,
    c: char,
    tokens: Vec<(Span, Token)>,
}

impl Lexer {
    pub fn new(span: Span) -> Self {
        assert!(span.lo <= span.hi);
        let begin = ::codemap().lookup_byte_offset(span.lo);
        let end = ::codemap().lookup_byte_offset(span.hi);
        assert_eq!(begin.fm.start_pos, end.fm.start_pos);

        Lexer {
            filemap: begin.fm,
            pos: span.lo,
            next_pos: span.lo,
            end: span.hi,
            c: ' ',
            tokens: vec![],
        }
    }

    pub fn from_file(path: &Path) -> ::std::io::Result<Lexer> {
        let codemap = ::codemap();
        let file = codemap
            .get_filemap(&FileName::Real(path.to_path_buf()))
            .ok_or(())
            .or_else(|_| codemap.load_file(path))?;
        Ok(Lexer::new(::mk_sp(file.start_pos, file.end_pos)))
    }

    fn bump(&mut self) {
        if self.next_pos >= self.end {
            return;
        }
        self.pos = self.next_pos;
        let i = (self.pos - self.filemap.start_pos).to_usize();
        let src = self.filemap.src.clone().unwrap();
        self.c = src[i..].chars().next().unwrap();
        self.next_pos = self.pos + Pos::from_usize(self.c.len_utf8());
    }

    pub fn tokenize(mut self) -> LexResult<Vec<(Span, Token)>> {
        struct InterpolationLevel {
            quote: char,
            triple: bool,
            brace_depth: usize,
        }
        let mut interpolation_levels: Vec<InterpolationLevel> = vec![];
        let mut span = ::mk_sp(self.pos, self.pos);
        macro_rules! bump {
            (or $otherwise:stmt) => {
                span.hi = self.next_pos;
                if self.next_pos >= self.end {
                    if true {
                        $otherwise;
                    }
                    return Ok(self.tokens);
                }
                self.bump();
            };
            () => {
                bump!(or {})
            }
        }
        macro_rules! put {
            ($t:expr) => (self.tokens.push((span, $t)))
        }
        macro_rules! emit {
            ($e:expr) => (bail!(ErrorKind::At { msg: $e, span }))
        }
        bump!();

        if self.c == '#' {
            span.lo = self.pos;
            span.hi = self.next_pos;
            bump!(or put!(Token::Punctuation('#')));
            if self.c != '!' {
                put!(Token::Punctuation('#'));
            } else {
                bump!(or emit!(ErrorMsg::UnterminatedShebang));
                while self.c != '\n' {
                    bump!(or put!(Token::Comment(span)));
                }
                put!(Token::Comment(span));
            }
        }

        loop {
            span.lo = self.pos;
            span.hi = self.next_pos;

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
                    bump!(or put!(Token::IntegerLiteral(Symbol::intern(&buffer))));
                }
                put!(Token::IntegerLiteral(Symbol::intern(&buffer)));
            } else if self.c.is_whitespace() {
                while self.c.is_whitespace() {
                    bump!(or put!(Token::WhiteSpace(span)));
                }
                put!(Token::WhiteSpace(span));
            } else if UnicodeXID::is_xid_start(self.c) || self.c == '$' || self.c == '_' {
                while UnicodeXID::is_xid_continue(self.c) || self.c == '$' {
                    buffer.push(self.c);
                    bump!(or put!(Token::Identifier(Symbol::intern(&buffer))));
                }
                put!(Token::Identifier(Symbol::intern(&buffer)));
            } else if "{}[]()=+-*%&|.,:;!<>@~#?^".contains(self.c) && !interpolation_before {
                put!(Token::Punctuation(self.c));
                bump!();
            } else if self.c == '/' {
                bump!(or put!(Token::Punctuation (self.c)));
                if self.c == '/' {
                    while self.c != '\n' {
                        bump!(or put!(Token::Comment(span)));
                    }
                    put!(Token::Comment(span));
                } else if self.c == '*' {
                    let mut depth = 0;
                    bump!(or emit!(ErrorMsg::UnterminatedBlockComment));
                    loop {
                        if self.c == '*' {
                            bump!(or emit!(ErrorMsg::UnterminatedBlockComment));
                            if self.c == '/' {
                                if depth == 0 {
                                    break;
                                }
                                depth -= 1;
                            } else {
                                continue;
                            }
                        } else if self.c == '/' {
                            bump!(or emit!(ErrorMsg::UnterminatedBlockComment));
                            if self.c == '*' {
                                depth += 1;
                            } else {
                                continue;
                            }
                        }

                        bump!(or emit!(ErrorMsg::UnterminatedBlockComment));
                    }
                    bump!(or put!(Token::Comment(span)));
                    put!(Token::Comment(span));
                } else {
                    put!(Token::Punctuation('/'));
                }
            } else if self.c == '\'' || self.c == '"' || interpolation_before {
                let mut quote = self.c;
                let mut raw = false;
                let mut triple = false;
                let mut interpolation_after = false;
                match self.tokens.last() {
                    Some(&(sp, Token::Identifier(ref last))) => if *last == "r" {
                        raw = true;
                        span.lo = sp.lo;
                    },
                    Some(&(
                        sp,
                        Token::StringLiteral {
                            contents,
                            raw: prev_raw,
                            triple: false,
                            quote: prev_quote,
                            interpolation_after: false,
                            interpolation_before: false,
                        },
                    )) => if ::codemap()
                        .span_to_snippet(contents.to_span())
                        .unwrap()
                        .is_empty() && prev_quote == quote
                    {
                        triple = true;
                        raw = prev_raw;
                        span.lo = sp.lo;
                    },
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
                bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                let mut contents = ::mk_sp(self.pos, self.pos);
                loop {
                    contents.hi = self.pos;
                    if self.c == quote {
                        if !triple {
                            break;
                        }
                        bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                        if self.c == quote {
                            bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                            if self.c == quote {
                                break;
                            }
                        }
                    }
                    if self.c == '\n' && !triple {
                        emit!(ErrorMsg::UnterminatedStringLiteral)
                    }
                    if self.c == '$' && !raw {
                        bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                        if self.c == '{' {
                            interpolation_levels.push(InterpolationLevel {
                                quote,
                                triple,
                                brace_depth: 1,
                            });
                            interpolation_after = true;
                            break;
                        }
                        continue;
                    }
                    if self.c == '\\' && !raw {
                        bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                    }
                    bump!(or emit!(ErrorMsg::UnterminatedStringLiteral));
                }
                let token = Token::StringLiteral {
                    contents,
                    raw,
                    triple,
                    quote,
                    interpolation_after,
                    interpolation_before,
                };
                bump!(or put!(token));
                put!(token);
            } else {
                emit!(ErrorMsg::UnhandledCharacter(self.c))
            }
        }
    }
}
