#![allow(dead_code)]
use std::cell::RefCell;

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use std::result::Result::*;

use move_command_line_common::files::FileHash;
use move_compiler::diagnostics::Diagnostics;
use move_compiler::parser::lexer::{Lexer, Tok};
use move_compiler::parser::syntax::parse_file_string;
use move_compiler::shared::CompilationEnv;
use move_compiler::Flags;
use std::cell::Cell;

use crate::token_tree::{
    Comment, CommentExtrator, CommentKind, Delimiter, NestKind_, ParseResult, TokenTree,
};
use crate::utils::FileLineMapping;
struct Format {
    config: FormatConfig,
    depth: Cell<usize>,
    token_tree: Vec<TokenTree>,
    comments: Vec<Comment>,
    line_mapping: FileLineMapping,

    comments_index: Cell<usize>,
    ret: RefCell<String>,
    cur_line: Cell<u32>,
    struct_definitions: Vec<(u32, u32)>,
    fun_body: HashSet<(u32, u32)>,
    bin_op: HashSet<u32>,
}

pub struct FormatConfig {
    pub indent_size: usize,
}

impl Format {
    fn new(
        config: FormatConfig,
        comments: CommentExtrator,
        line_mapping: FileLineMapping,
        p: ParseResult,
    ) -> Self {
        let ParseResult {
            token_tree,
            struct_definitions,
            bin_op,
            fun_body,
        } = p;
        Self {
            comments_index: Default::default(),
            config,
            depth: Default::default(),
            token_tree,
            comments: comments.comments,
            line_mapping,
            ret: Default::default(),
            cur_line: Default::default(),
            struct_definitions,
            fun_body,
            bin_op,
        }
    }

    pub fn format_token_trees(self) -> String {
        let length = self.token_tree.len();
        let mut index = 0;
        let mut pound_sign = None;
        while index < length {
            let t = self.token_tree.get(index).unwrap();
            if t.is_pound() {
                pound_sign = Some(index);
            }
            self.format_token_trees_(t, self.token_tree.get(index + 1));
            if pound_sign.map(|x| (x + 1) == index).unwrap_or_default() {
                self.new_line(Some(t.end_pos()));
                pound_sign = None;
            }
            // top level
            match t {
                TokenTree::SimpleToken {
                    content: _,
                    pos: _,
                    tok: _,
                } => {}
                TokenTree::Nested { elements: _, kind } => {
                    if kind.kind == NestKind_::Brace {
                        self.new_line(Some(t.end_pos()));
                    }
                }
            }
            index += 1;
        }
        self.ret.into_inner()
    }

    fn need_new_line(
        kind: NestKind_,
        delimiter: Option<Delimiter>,
        _has_colon: bool,
        current: &TokenTree,
        next: Option<&TokenTree>,
    ) -> bool {
        //
        if next.map(|x| x.simple_str()).flatten() == delimiter.map(|x| x.to_static_str()) {
            return false;
        }
        let next_tok = next.map(|x| match x {
            TokenTree::SimpleToken {
                content: _,
                pos: _,
                tok,
            } => tok.clone(),
            TokenTree::Nested { elements: _, kind } => kind.kind.start_tok(),
        });

        let next_content = next.map(|x| match x {
            TokenTree::SimpleToken {
                content,
                pos: _,
                tok: _,
            } => content.clone(),
            TokenTree::Nested { elements: _, kind } => kind.kind.start_tok().to_string(),
        });

        // special case for `}}`
        if match current {
            TokenTree::SimpleToken {
                content: _,
                pos: _,
                tok: _,
            } => false,
            TokenTree::Nested { elements: _, kind } => kind.kind == NestKind_::Brace,
        } && kind == NestKind_::Brace
            && match next_tok {
                Some(x) => match x {
                    Tok::Friend
                    | Tok::Const
                    | Tok::Fun
                    | Tok::While
                    | Tok::Use
                    | Tok::Struct
                    | Tok::Spec
                    | Tok::Return
                    | Tok::Public
                    | Tok::Native
                    | Tok::Move
                    | Tok::Module
                    | Tok::Loop
                    | Tok::Let
                    | Tok::Invariant
                    | Tok::If
                    | Tok::Continue
                    | Tok::Break
                    | Tok::Abort => true,
                    Tok::Identifier
                        if next_content
                            .map(|x| x.as_str() == "entry")
                            .unwrap_or_default() =>
                    {
                        true
                    }
                    _ => false,
                },
                None => true,
            }
        {
            return true;
        }
        false
    }

    fn format_token_trees_(&self, token: &TokenTree, next_token: Option<&TokenTree>) {
        match token {
            TokenTree::Nested { elements, kind } => {
                const MAX: usize = 30;
                let length = self.analyze_token_tree_length(elements, MAX);
                let (delimiter, has_colon) = Self::analyze_token_tree_delimiter(elements);
                let mut new_line_mode = {
                    // more rules.
                    let nested_in_struct_definition = self
                        .struct_definitions
                        .iter()
                        .any(|x| kind.start_pos >= x.0 && kind.end_pos <= x.1)
                        && kind.kind == NestKind_::Brace;

                    let fun_body = self.fun_body.contains(&(kind.start_pos, kind.end_pos))
                        && kind.kind == NestKind_::Brace;

                    length > MAX
                        || delimiter
                            .map(|x| x == Delimiter::Semicolon)
                            .unwrap_or_default()
                        || nested_in_struct_definition
                        || fun_body
                };
                match kind.kind {
                    NestKind_::ParentTheses
                    | NestKind_::Bracket
                    | NestKind_::Type
                    | NestKind_::Lambda => {
                        if delimiter.is_none() {
                            new_line_mode = false;
                        }
                    }
                    NestKind_::Brace => {}
                }
                self.format_token_trees_(&kind.start_token_tree(), None);
                self.inc_depth();
                if new_line_mode {
                    self.new_line(Some(kind.start_pos));
                }
                let mut pound_sign = None;
                let len = elements.len();
                for index in 0..len {
                    let t = elements.get(index).unwrap();
                    if t.is_pound() {
                        pound_sign = Some(index)
                    }
                    let next_t = elements.get(index + 1);
                    self.format_token_trees_(t, elements.get(index + 1));
                    if pound_sign.map(|x| (x + 1) == index).unwrap_or_default() {
                        self.new_line(Some(t.end_pos()));
                        pound_sign = None;
                        continue;
                    }

                    // need new line.
                    if new_line_mode {
                        let d = delimiter.map(|x| x.to_static_str());
                        let t_str = t.simple_str();
                        if (Self::need_new_line(kind.kind, delimiter, has_colon, t, next_t)
                            || d == t_str)
                            && index != len - 1
                        {
                            self.new_line(Some(t.end_pos()));
                        }
                    }
                }
                self.add_comments(kind.end_pos);
                self.dec_depth();
                if new_line_mode {
                    self.new_line(Some(kind.end_pos));
                }
                self.format_token_trees_(&kind.end_token_tree(), None);
                if need_space_nested(
                    kind.kind,
                    match next_token {
                        Some(x) => match x {
                            TokenTree::SimpleToken {
                                content: _,
                                pos: _,
                                tok,
                            } => Some(*tok),
                            TokenTree::Nested {
                                elements: _,
                                kind: _,
                            } => None,
                        },
                        None => None,
                    },
                ) {
                    self.push_str(" ");
                }
            }

            //Add to string
            TokenTree::SimpleToken { content, pos, tok } => {
                self.add_comments(*pos);
                if (self.translate_line(*pos) - self.cur_line.get()) > 1 {
                    self.new_line(None);
                }
                self.push_str(&content.as_str());
                self.cur_line.set(self.translate_line(*pos));
                if need_space_simpletoken(
                    *tok,
                    match next_token {
                        Some(x) => match x {
                            TokenTree::SimpleToken {
                                content: _,
                                pos: _,
                                tok,
                            } => Some(*tok),
                            TokenTree::Nested {
                                elements: _,
                                kind: _,
                            } => None,
                        },
                        None => None,
                    },
                ) {
                    self.push_str(" ");
                }
            }
        }
    }

    fn add_comments(&self, pos: u32) {
        for c in &self.comments[self.comments_index.get()..] {
            if c.start_offset < pos {
                if (self.translate_line(c.start_offset) - self.cur_line.get()) > 1 {
                    self.new_line(None);
                }
                //TODO: If the comment is in the same line with the latest token
                //1 don't change line
                //2 if find \n move it after the comment
                self.push_str(c.content.as_str());
                let kind = c.comment_kind();
                match kind {
                    CommentKind::DocComment => {
                        self.new_line(None);
                    }
                    CommentKind::BlockComment => {
                        let end = c.start_offset + (c.content.len() as u32);
                        let line_start = self.translate_line(c.start_offset);
                        let line_end = self.translate_line(end);
                        if line_start != line_end {
                            self.new_line(None);
                        }
                    }
                    CommentKind::InlineComment => {}
                }
                self.comments_index.set(self.comments_index.get() + 1);
                self.cur_line
                    .set(self.translate_line(c.start_offset + (c.content.len() as u32) - 1));
            } else {
                break;
            }
        }
    }
}

impl Format {
    /// if a bin operation is actual a binary operation.
    /// like `&`,`*`.
    fn bin_is_bin(&self, pos: u32) -> bool {
        self.bin_op.contains(&pos)
    }
    fn inc_depth(&self) {
        let old = self.depth.get();
        self.depth.set(old + 1);
    }
    fn dec_depth(&self) {
        let old = self.depth.get();
        self.depth.set(old - 1);
    }
    fn push_str(&self, s: &str) {
        self.ret.borrow_mut().push_str(s);
    }
    fn push_string(&self, s: &String) {
        self.push_str(s.as_str());
    }

    /// 缩进
    fn indent(&self) {
        self.push_str(
            " ".to_string()
                .repeat(self.depth.get() * self.config.indent_size)
                .as_str(),
        );
    }

    fn translate_line(&self, pos: u32) -> u32 {
        let p: PathBuf = Path::new(".").to_path_buf();
        self.line_mapping
            .translate(&p, pos, pos)
            .unwrap()
            .line_start
    }

    /// analyzer a `Nested` token tree.
    fn analyze_token_tree_delimiter(
        token_tree: &Vec<TokenTree>,
    ) -> (
        Option<Delimiter>, // if this is a `Delimiter::Semicolon` we can know this is a function body or etc.
        bool,              // has a `:`
    ) {
        let mut d = None;
        let mut has_colon = false;
        for t in token_tree.iter() {
            match t {
                TokenTree::SimpleToken {
                    content,
                    pos: _,
                    tok: _,
                } => match content.as_str() {
                    ";" => {
                        d = Some(Delimiter::Semicolon);
                    }
                    "," => {
                        if d.is_none() {
                            // Somehow `;` has high priority.
                            d = Some(Delimiter::Comma);
                        }
                    }
                    ":" => {
                        has_colon = true;
                    }
                    _ => {}
                },
                TokenTree::Nested { .. } => {}
            }
        }
        return (d, has_colon);
    }

    /// analyzer How long is list of token_tree
    fn analyze_token_tree_length(&self, token_tree: &Vec<TokenTree>, max: usize) -> usize {
        let mut ret = usize::default();
        fn analyzer_token_tree_length_(ret: &mut usize, token_tree: &TokenTree, max: usize) {
            match token_tree {
                TokenTree::SimpleToken { content, .. } => {
                    *ret = *ret + content.len();
                }
                TokenTree::Nested { elements, .. } => {
                    for t in elements.iter() {
                        analyzer_token_tree_length_(ret, t, max);
                        if *ret > max {
                            return;
                        }
                    }
                    *ret = *ret + 2; // for delimiter.
                }
            }
        }
        for t in token_tree.iter() {
            analyzer_token_tree_length_(&mut ret, t, max);
            if ret > max {
                return ret;
            }
        }
        ret
    }

    fn new_line(&self, add_line_comment: Option<u32>) {
        if let Some(add_line_comment) = add_line_comment {
            // emit same line comments.
            let cur_line = self.cur_line.get();
            let mut call_new_line = false;
            for c in &self.comments[self.comments_index.get()..] {
                if self.translate_line(add_line_comment) == self.translate_line(c.start_offset)
                    && c.start_offset < add_line_comment
                {
                    if (self.translate_line(c.start_offset) - self.cur_line.get()) > 1 {
                        self.new_line(None);
                    }
                    self.push_str(c.content.as_str());
                    let kind = c.comment_kind();
                    match kind {
                        CommentKind::DocComment => {
                            self.new_line(None);
                            call_new_line = true;
                        }
                        CommentKind::BlockComment => {
                            let end = c.start_offset + (c.content.len() as u32);
                            let line_start = self.translate_line(c.start_offset);
                            let line_end = self.translate_line(end);
                            if line_start != line_end {
                                self.new_line(None);
                                call_new_line = true;
                            }
                        }
                        CommentKind::InlineComment => {}
                    }
                    self.comments_index.set(self.comments_index.get() + 1);
                    self.cur_line
                        .set(self.translate_line(c.start_offset + (c.content.len() as u32) - 1));
                } else {
                    break;
                }
            }
            if cur_line != self.cur_line.get() || call_new_line {
                return;
            }
        }

        self.push_str("\n");
        self.indent();
    }
}

pub fn format(content: impl AsRef<str>, config: FormatConfig) -> Result<String, Diagnostics> {
    let content = content.as_ref();
    let mut env = CompilationEnv::new(Flags::testing());
    let filehash = FileHash::empty();
    let (defs, _) = parse_file_string(&mut env, filehash, &content)?;
    let lexer = Lexer::new(&content, filehash);
    let parse = super::token_tree::Parser::new(lexer, &defs);
    let parse_result = parse.parse_tokens();
    let ce = CommentExtrator::new(content).unwrap();
    let mut t = FileLineMapping::default();
    t.update(Path::new(".").to_path_buf(), &content);
    let f = Format::new(config, ce, t, parse_result);
    Ok(f.format_token_trees())
}

pub enum TokType {
    /// abc like token,
    Alphabet,
    /// + - ...
    MathSign,
    ///
    Sign,
    // specials no need space at all.
    NoNeedSpace,
    /// numbers 0x1 ...
    Number,
    /// b"hello world"
    String,
    /// &
    Amp,
    /// &mut
    AmpMut,
    ///
    Semicolon,
    ///:
    Colon,
}

impl From<Tok> for TokType {
    fn from(value: Tok) -> Self {
        match value {
            Tok::EOF => unreachable!(), // EOF not in `TokenTree`.
            Tok::NumValue => TokType::Number,
            Tok::NumTypedValue => TokType::Number,
            Tok::ByteStringValue => TokType::String,
            Tok::Exclaim => TokType::Sign,
            Tok::ExclaimEqual => TokType::MathSign,
            Tok::Percent => TokType::MathSign,
            Tok::Amp => TokType::Amp,
            Tok::AmpAmp => TokType::MathSign,
            Tok::LParen => TokType::Sign,
            Tok::RParen => TokType::Sign,
            Tok::LBracket => TokType::Sign,
            Tok::RBracket => TokType::Sign,
            Tok::Star => TokType::MathSign,
            Tok::Plus => TokType::MathSign,
            Tok::Comma => TokType::Sign,
            Tok::Minus => TokType::Sign,
            Tok::Period => TokType::NoNeedSpace,
            Tok::PeriodPeriod => TokType::NoNeedSpace,
            Tok::Slash => TokType::Sign,
            Tok::Colon => TokType::Colon,
            Tok::ColonColon => TokType::NoNeedSpace,
            Tok::Semicolon => TokType::Semicolon,
            Tok::Less => TokType::MathSign,
            Tok::LessEqual => TokType::MathSign,
            Tok::LessLess => TokType::MathSign,
            Tok::Equal => TokType::MathSign,
            Tok::EqualEqual => TokType::MathSign,
            Tok::EqualEqualGreater => TokType::MathSign,
            Tok::LessEqualEqualGreater => TokType::MathSign,
            Tok::Greater => TokType::MathSign,
            Tok::GreaterEqual => TokType::MathSign,
            Tok::GreaterGreater => TokType::MathSign,
            Tok::LBrace => TokType::Sign,
            Tok::Pipe => TokType::Sign,
            Tok::PipePipe => TokType::Sign,
            Tok::RBrace => TokType::Sign,
            Tok::NumSign => TokType::Sign,
            Tok::AtSign => TokType::Sign,
            Tok::AmpMut => TokType::Amp,
            _ => TokType::Alphabet,
        }
    }
}

pub(crate) fn need_space_simpletoken(current: Tok, next: Option<Tok>) -> bool {
    if next.is_none() {
        return false;
    }
    return match (TokType::from(current), TokType::from(next.unwrap())) {
        (TokType::Alphabet, TokType::Alphabet) => true,
        (TokType::MathSign, _) => true,
        (TokType::Sign, TokType::Alphabet) => true,
        (_, TokType::MathSign) => true,
        (_, TokType::Amp) => true,
        (_, TokType::AmpMut) => true,
        (TokType::Colon, _) => true,
        (TokType::Alphabet, TokType::Number) => true,
        _ => false,
    };
}

pub(crate) fn need_space_nested(current: NestKind_, next: Option<Tok>) -> bool {
    if next.is_none() {
        return false;
    }

    return match (NestKind_::from(current), TokType::from(next.unwrap())) {
        (_, TokType::MathSign) => true,
        (NestKind_::Type, TokType::Alphabet) => true,
        _ => false,
    };
}
