use move_compiler::diagnostics::Diagnostic;
use move_compiler::parser::ast::Definition;
use move_compiler::parser::lexer::{Lexer, Tok};

#[derive(Clone, Copy)]
pub enum NestKind_ {
    /// ()
    ParentTheses,
    /// []  
    Bracket,
    /// {}
    Brace,
    /// type parameter like  A<B>
    Type,
    /// lambda like |a , b|
    Lambda,
}

#[derive(Clone, Copy)]
pub struct NestKind {
    kind: NestKind_,
    start_pos: u32,
    end_pos: u32,
}

impl NestKind_ {
    pub(crate) fn is_nest_start(tok: Tok) -> Option<Self> {
        unimplemented!()
    }
    pub(crate) fn start_tok(self) -> Tok {
        unimplemented!()
    }
    pub(crate) fn end_tok(self) -> Tok {
        unimplemented!()
    }
}

pub enum TokenTree {
    SimpleToken {
        content: String,
        pos: u32,
    },
    Nested {
        elements: Vec<TokenTree>,
        kind: NestKind,
        delimiter: Option<Delimiter>,
    },
}

pub enum Delimiter {
    /// a `,`
    Comma,
    /// a `;`
    Semicolon,
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    defs: Vec<Definition>,
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a>, defs: Vec<Definition>) -> Self {
        Self { lexer, defs }
    }
}

impl<'a> Parser<'a> {
    fn parse_tokens(&mut self) -> Result<Vec<TokenTree>, Box<Diagnostic>> {
        let mut ret = vec![];
        while self.lexer.peek() != Tok::EOF {
            if let Some(kind) = NestKind_::is_nest_start(self.lexer.peek()) {
                ret.push(self.parse_nest(kind)?);
                continue;
            }
            ret.push(TokenTree::SimpleToken {
                content: self.lexer.content().to_string(),
                pos: self.lexer.start_loc() as u32,
            });
        }
        Ok(ret)
    }

    fn is_nest_start(&self) -> Option<NestKind_> {
        unimplemented!()
    }

    fn parse_nest(&mut self, kind: NestKind_) -> Result<TokenTree, Box<Diagnostic>> {
        debug_assert!(self.lexer.peek() == kind.start_tok());
        self.lexer.advance()?;
        let mut ret = vec![];
        while self.lexer.peek() != kind.end_tok() && self.lexer.peek() != Tok::EOF {
            ret.extend(self.parse_tokens()?.into_iter());
        }
        self.lexer.advance()?;
        Ok(TokenTree::Nested {
            elements: ret,
            kind: unimplemented!(),
            delimiter: None,
        })
    }
}
