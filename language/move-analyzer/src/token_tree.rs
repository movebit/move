use std::cmp::Ordering;

use move_compiler::parser::ast::Definition;
use move_compiler::parser::ast::*;
use move_compiler::parser::lexer::{Lexer, Tok};

#[derive(Clone, Copy, PartialEq, Eq)]
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
    pub(crate) kind: NestKind_,
    pub(crate) start_pos: u32,
    pub(crate) end_pos: u32,
}

impl NestKind_ {
    pub(crate) fn is_nest_start(tok: Tok) -> Option<NestKind_> {
        match tok {
            Tok::LParen => Some(Self::ParentTheses),
            Tok::LBracket => Some(Self::Bracket),
            Tok::LBrace => Some(Self::Brace),
            Tok::Less => Some(Self::Type),
            Tok::Pipe => Some(Self::Lambda),
            _ => None,
        }
    }

    pub(crate) const fn start_tok(self) -> Tok {
        match self {
            NestKind_::ParentTheses => Tok::LParen,
            NestKind_::Bracket => Tok::LBracket,
            NestKind_::Brace => Tok::LBrace,
            NestKind_::Type => Tok::Less,
            NestKind_::Lambda => Tok::Pipe,
        }
    }
    pub(crate) fn end_tok(self) -> Tok {
        match self {
            NestKind_::ParentTheses => Tok::RParen,
            NestKind_::Bracket => Tok::RBracket,
            NestKind_::Brace => Tok::RBrace,
            NestKind_::Type => Tok::Greater,
            NestKind_::Lambda => Tok::Pipe,
        }
    }
}

pub enum TokenTree {
    SimpleToken {
        content: String,
        pos: u32, // start offset in file buffer.
        tok: Tok,
    },
    Nested {
        elements: Vec<TokenTree>,
        kind: NestKind,
    },
}

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    defs: &'a Vec<Definition>,
    type_lambda_pair: Vec<(u32, u32)>,
    type_lambda_pair_index: usize,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(lexer: Lexer<'a>, defs: &'a Vec<Definition>) -> Self {
        let mut x = Self {
            lexer,
            defs,
            type_lambda_pair: Default::default(),
            type_lambda_pair_index: 0,
        };
        x.collect_type_and_lambda_pair();
        x
    }
}

impl<'a> Parser<'a> {
    pub(crate) fn parse_tokens(&mut self) -> Vec<TokenTree> {
        let mut ret = vec![];
        self.lexer.advance().unwrap();
        while self.lexer.peek() != Tok::EOF {
            if let Some(kind) = self.is_nest_start() {
                ret.push(self.parse_nested(kind));
                continue;
            }
            ret.push(TokenTree::SimpleToken {
                content: self.lexer.content().to_string(),
                pos: self.lexer.start_loc() as u32,
                tok: self.lexer.peek(),
            });
            self.lexer.advance().unwrap();
        }
        ret
    }

    fn is_nest_start(&mut self) -> Option<NestKind_> {
        let t = match NestKind_::is_nest_start(self.lexer.peek()) {
            Some(x) => x,
            None => return None,
        };
        match t {
            NestKind_::Type | NestKind_::Lambda => {
                let pos = self.lexer.start_loc() as u32;
                // try drop
                for (start, _end) in &self.type_lambda_pair[self.type_lambda_pair_index..] {
                    if (*start) > pos {
                        //
                        self.type_lambda_pair_index = self.type_lambda_pair_index + 1;
                    } else {
                        break;
                    }
                }
                for (start, end) in &self.type_lambda_pair[self.type_lambda_pair_index..] {
                    if pos >= *start && pos <= *end {
                        return Some(t);
                    } else {
                        return None;
                    }
                }
                return None;
            }
            _ => return Some(t),
        }
    }

    fn parse_nested(&mut self, kind: NestKind_) -> TokenTree {
        debug_assert!(self.lexer.peek() == kind.start_tok());
        let start = self.lexer.start_loc();
        self.lexer.advance().unwrap();
        let mut ret = vec![];
        while self.lexer.peek() != Tok::EOF {
            if let Some(kind) = self.is_nest_start() {
                ret.push(self.parse_nested(kind));
                continue;
            }
            if self.lexer.peek() == kind.end_tok() {
                break;
            }
            if kind == NestKind_::Type && self.lexer.peek() == Tok::GreaterGreater {
                self.adjust_token(Tok::Greater);
                break;
            }
            ret.push(TokenTree::SimpleToken {
                content: self.lexer.content().to_string(),
                pos: self.lexer.start_loc() as u32,
                tok: self.lexer.peek(),
            });
            self.lexer.advance().unwrap();
        }
        debug_assert_eq!(self.lexer.peek(), kind.end_tok());
        let end = self.lexer.start_loc();
        self.lexer.advance().unwrap();
        TokenTree::Nested {
            elements: ret,
            kind: NestKind {
                kind,
                start_pos: start as u32,
                end_pos: end as u32,
            },
        }
    }
}

impl<'a> Parser<'a> {
    // While parsing a list and expecting a ">" token to mark the end, replace
    // a ">>" token with the expected ">". This handles the situation where there
    // are nested type parameters that result in two adjacent ">" tokens, e.g.,
    // "A<B<C>>".
    fn adjust_token(&mut self, end_token: Tok) {
        if self.lexer.peek() == Tok::GreaterGreater && end_token == Tok::Greater {
            self.lexer.replace_token(Tok::Greater, 1);
        }
    }
}
impl<'a> Parser<'a> {
    fn collect_type_and_lambda_pair(&mut self) {
        for d in self.defs.iter() {
            collect_definition(self, d);
        }

        self.type_lambda_pair.sort_by(|x, y| {
            debug_assert!(x.0.cmp(&y.0) != Ordering::Equal, "{:?}?{:?}", x, y);
            if x.0.cmp(&y.0) == Ordering::Greater {
                Ordering::Greater
            } else {
                x.1.cmp(&y.1)
            }
        });

        //// all collector functions.

        fn collect_definition(p: &mut Parser, d: &Definition) {
            match d {
                Definition::Module(x) => collect_module(p, x),
                Definition::Address(x) => {
                    for x in x.modules.iter() {
                        collect_module(p, x);
                    }
                }
                Definition::Script(x) => collect_script(p, x),
            }
        }

        fn collect_script(p: &mut Parser, d: &Script) {
            collect_function(p, &d.function);
            for s in d.specs.iter() {
                collect_spec(p, s);
            }
        }

        fn collect_module(p: &mut Parser, d: &ModuleDefinition) {
            for m in d.members.iter() {
                match &m {
                    ModuleMember::Function(x) => collect_function(p, x),
                    ModuleMember::Struct(x) => collect_struct(p, x),
                    ModuleMember::Use(_) => {}
                    ModuleMember::Friend(_) => {}
                    ModuleMember::Constant(_) => {}
                    ModuleMember::Spec(s) => collect_spec(p, s),
                }
            }
        }

        fn collect_struct(p: &mut Parser, s: &StructDefinition) {
            match &s.fields {
                StructFields::Defined(fs) => {
                    for f in fs.iter() {
                        collect_ty(p, &f.1);
                    }
                }
                StructFields::Native(_) => {}
            }
        }
        fn collect_seq_item(p: &mut Parser, s: &SequenceItem) {
            match &s.value {
                SequenceItem_::Seq(e) => collect_expr(p, &e),
                SequenceItem_::Declare(_, ty) => {
                    if let Some(ty) = ty {
                        collect_ty(p, ty);
                    }
                }
                SequenceItem_::Bind(_, ty, e) => {
                    if let Some(ty) = ty {
                        collect_ty(p, ty);
                    }
                    collect_expr(p, &e);
                }
            }
        }
        fn collect_seq(p: &mut Parser, s: &Sequence) {
            for s in s.1.iter() {
                collect_seq_item(p, s);
            }
            if let Some(t) = s.3.as_ref() {
                collect_expr(p, t);
            }
        }

        fn collect_expr(p: &mut Parser, e: &Exp) {
            match &e.value {
                Exp_::Value(_) => {}
                Exp_::Move(_) => {}
                Exp_::Copy(_) => {}
                Exp_::Name(_, tys) => {
                    if let Some(tys) = tys {
                        tys.iter().for_each(|ty| collect_ty(p, ty));
                    }
                }
                Exp_::Call(_, _, tys, es) => {
                    if let Some(tys) = tys {
                        for ty in tys.iter() {
                            collect_ty(p, ty);
                        }
                    };
                    es.value.iter().for_each(|e| collect_expr(p, e));
                }
                Exp_::Pack(_, tys, es) => {
                    if let Some(tys) = tys {
                        for ty in tys.iter() {
                            collect_ty(p, ty);
                        }
                    };
                    es.iter().for_each(|e| collect_expr(p, &e.1));
                }
                Exp_::Vector(_, tys, es) => {
                    if let Some(tys) = tys {
                        for ty in tys.iter() {
                            collect_ty(p, ty);
                        }
                    };
                    es.value.iter().for_each(|e| collect_expr(p, e));
                }
                Exp_::IfElse(c, then_, eles_) => {
                    collect_expr(p, c.as_ref());
                    collect_expr(p, then_.as_ref());
                    if let Some(else_) = eles_ {
                        collect_expr(p, else_.as_ref());
                    }
                }
                Exp_::While(e, then_) => {
                    collect_expr(p, e.as_ref());
                    collect_expr(p, then_.as_ref());
                }
                Exp_::Loop(b) => {
                    collect_expr(p, b.as_ref());
                }
                Exp_::Block(b) => collect_seq(p, b),

                Exp_::Lambda(b, e) => {
                    p.type_lambda_pair.push((b.loc.start(), b.loc.end()));
                    collect_expr(p, e.as_ref());
                }
                Exp_::Quant(_, _, es, e1, e2) => {
                    es.iter().for_each(|e| {
                        for e in e.iter() {
                            collect_expr(p, e)
                        }
                    });
                    if let Some(t) = e1 {
                        collect_expr(p, t.as_ref());
                    }
                    collect_expr(p, e2.as_ref());
                }
                Exp_::ExpList(es) => {
                    es.iter().for_each(|e| collect_expr(p, e));
                }
                Exp_::Unit => {}
                Exp_::Assign(l, r) => {
                    collect_expr(p, l.as_ref());
                    collect_expr(p, r.as_ref());
                }
                Exp_::Return(e) => {
                    if let Some(t) = e {
                        collect_expr(p, t.as_ref());
                    }
                }
                Exp_::Abort(e) => {
                    collect_expr(p, e.as_ref());
                }
                Exp_::Break => {}
                Exp_::Continue => {}
                Exp_::Dereference(e) => {
                    collect_expr(p, e.as_ref());
                }
                Exp_::UnaryExp(_, e) => {
                    collect_expr(p, e.as_ref());
                }
                Exp_::BinopExp(l, _, r) => {
                    collect_expr(p, l.as_ref());
                    collect_expr(p, r.as_ref());
                }
                Exp_::Borrow(_, e) => {
                    collect_expr(p, e.as_ref());
                }
                Exp_::Dot(e, _) => {
                    collect_expr(p, e.as_ref());
                }
                Exp_::Index(e, i) => {
                    collect_expr(p, e.as_ref());
                    collect_expr(p, i.as_ref());
                }
                Exp_::Cast(e, ty) => {
                    collect_expr(p, e.as_ref());
                    collect_ty(p, ty);
                }
                Exp_::Annotate(e, ty) => {
                    collect_expr(p, e.as_ref());
                    collect_ty(p, ty);
                }
                Exp_::Spec(s) => collect_spec(p, s),
                Exp_::UnresolvedError => {
                    unreachable!()
                }
            }
        }

        fn collect_spec(p: &mut Parser, spec_block: &SpecBlock) {
            p.type_lambda_pair.push((
                spec_block.value.target.loc.start(),
                spec_block.value.target.loc.end(),
            ));
            for m in spec_block.value.members.iter() {
                match &m.value {
                    SpecBlockMember_::Condition {
                        kind,
                        properties: _,
                        exp,
                        additional_exps,
                    } => {
                        p.type_lambda_pair.push((kind.loc.start(), kind.loc.end()));
                        collect_expr(p, exp);
                        additional_exps.iter().for_each(|e| collect_expr(p, e));
                    }
                    SpecBlockMember_::Function {
                        uninterpreted: _,
                        name,
                        signature,
                        body,
                    } => {
                        p.type_lambda_pair
                            .push((name.0.loc.start(), signature.return_type.loc.end()));
                        match &body.value {
                            FunctionBody_::Defined(s) => collect_seq(p, s),
                            FunctionBody_::Native => {}
                        }
                    }
                    SpecBlockMember_::Variable {
                        is_global: _,
                        name: _,
                        type_parameters: _,
                        type_,
                        init,
                    } => {
                        if let Some(init) = init {
                            p.type_lambda_pair
                                .push((type_.loc.start(), init.loc.start() - 1))
                        } else {
                            p.type_lambda_pair
                                .push((type_.loc.start(), spec_block.loc.end()));
                        }
                    }

                    SpecBlockMember_::Let {
                        name: _,
                        post_state: _,
                        def,
                    } => collect_expr(p, def),
                    SpecBlockMember_::Update { lhs, rhs } => {
                        collect_expr(p, lhs);
                        collect_expr(p, rhs);
                    }
                    SpecBlockMember_::Include { properties: _, exp } => {
                        collect_expr(p, exp);
                    }
                    SpecBlockMember_::Apply {
                        exp: _,
                        patterns: _,
                        exclusion_patterns: _,
                    } => p
                        .type_lambda_pair
                        .push((spec_block.loc.start(), spec_block.loc.end())),
                    SpecBlockMember_::Pragma { properties: _ } => {}
                }
            }
        }

        fn collect_function(p: &mut Parser, d: &Function) {
            p.type_lambda_pair
                .push((d.name.0.loc.start(), d.signature.return_type.loc.end()));
            match &d.body.value {
                FunctionBody_::Defined(s) => collect_seq(p, s),
                FunctionBody_::Native => {}
            }
        }

        fn collect_ty(p: &mut Parser, ty: &Type) {
            p.type_lambda_pair.push((ty.loc.start(), ty.loc.end()))
        }
    }
}

#[derive(Default, Debug)]
pub struct CommentExtrator {
    comments: Vec<Comment>,
}

#[derive(Debug)]
pub struct Comment {
    pub(crate) line: u32,
    #[allow(dead_code)]
    pub(crate) col: u32,
    pub(crate) content: String,
}

impl CommentExtrator {
    pub(crate) fn new(content: &str) -> Self {
        if content.len() == 0 {
            return Self::default();
        }
        enum State {
            Init,
            OneSlash,
            InlineComment,
        }
        impl Default for State {
            fn default() -> Self {
                Self::Init
            }
        }
        let mut line = 0;
        let mut col = 0;
        let mut state = State::default();
        const NEW_LINE: u8 = 10;
        const SLASH: u8 = 47;
        let mut comments = Vec::new();
        let mut comment = Vec::new();
        let last_index = content.as_bytes().len() - 1;
        for (index, c) in content.as_bytes().iter().enumerate() {
            match state {
                State::Init => match *c {
                    NEW_LINE => {
                        line += 1;
                        col = 0;
                    }
                    SLASH => {
                        state = State::OneSlash;
                        col += 1;
                    }
                    _ => {
                        col += 1;
                    }
                },
                State::OneSlash => {
                    if *c == SLASH {
                        state = State::InlineComment;
                        comment.push(SLASH);
                        comment.push(SLASH);
                    } else {
                        state = State::Init;
                    }
                    col += 1;
                }
                State::InlineComment => {
                    if *c == NEW_LINE || index == last_index {
                        if *c != NEW_LINE {
                            comment.push(*c);
                        }
                        // ending
                        let col_ = col - (comment.len() as u32);
                        comments.push(Comment {
                            line,
                            col: col_,
                            content: String::from_utf8(comment.clone()).unwrap(),
                        });
                        line += 1;
                        col = 0;
                        comment = Vec::new();
                        state = State::Init;
                    } else {
                        comment.push(*c);
                        col += 1;
                    }
                }
            };
        }
        assert!(state == State::Init);
        Self { comments }
    }
}

#[test]
fn test_comment_extrator() {
    let x = CommentExtrator::new(
        r#"
        // 111
        // 222
        fdfdf
        // bb
        
    "#,
    );
    eprintln!("c:{:?}", x);
}
