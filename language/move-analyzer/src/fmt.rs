#![allow(dead_code)]
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::result::Result::*;

use move_command_line_common::files::FileHash;
use move_compiler::diagnostics::Diagnostics;
use move_compiler::parser::lexer::{Lexer, Tok};
use move_compiler::parser::syntax::parse_file_string;
use move_compiler::shared::CompilationEnv;
use move_compiler::{Flags, MatchedFileCommentMap};
use std::cell::Cell;

use crate::move_generate_spec::indent;
use crate::token_tree::{Comment, CommentExtrator, NestKind_, TokenTree};
use crate::utils::FileLineMapping;
struct Format {
    config: FormatConfig,
    depth: Rc<RefCell<usize>>,
    token_tree: Vec<TokenTree>,
    comments: Vec<Comment>,
    line_mapping: FileLineMapping,
    path: PathBuf,
    comment_index: Cell<usize>,
}

pub enum TokType {
    Alphabet,
    MathSign,
    Sign,
    Specical,
    Value,
}

impl From<Tok> for TokType {
    fn from(value: Tok) -> Self {
        match value {
            Tok::EOF => unreachable!(), // EOF not in `TokenTree`.
            Tok::NumValue => TokType::Specical,
            Tok::NumTypedValue => TokType::Value,
            Tok::ByteStringValue => TokType::Value,
            Tok::Exclaim => TokType::Sign,
            Tok::ExclaimEqual => TokType::MathSign,
            Tok::Percent => TokType::MathSign,
            Tok::Amp => TokType::Sign,
            Tok::AmpAmp => TokType::MathSign,
            Tok::LParen => TokType::Sign,
            Tok::RParen => TokType::Sign,
            Tok::LBracket => TokType::Sign,
            Tok::RBracket => TokType::Sign,
            Tok::Star => TokType::MathSign,
            Tok::Plus => TokType::MathSign,
            Tok::Comma => TokType::Sign,
            Tok::Minus => TokType::Sign,
            Tok::Period => TokType::Sign,
            Tok::PeriodPeriod => TokType::Sign,
            Tok::Slash => TokType::Sign,
            Tok::Colon => TokType::Sign,
            Tok::ColonColon => TokType::Sign,
            Tok::Semicolon => TokType::Sign,
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
            _ => TokType::Alphabet,
        }
    }
}

pub struct FormatConfig {
    pub indent_size: u32,
}

impl Format {
    fn new(
        config: FormatConfig,
        token_tree: Vec<TokenTree>,
        comments: CommentExtrator,
        line_mapping: FileLineMapping,
        path: PathBuf,
    ) -> Self {
        Self {
            comment_index: Default::default(),
            config,
            depth: Default::default(),
            token_tree,
            comments: comments.comments,
            line_mapping,
            path,
        }
    }

    #[must_use]
    fn increment_depth(&self) -> DepthGuard {
        let old = *self.depth.as_ref().borrow();
        *self.depth.as_ref().borrow_mut() = old + 1;
        DepthGuard(self.depth.clone())
    }

    pub fn format_token_trees(self) -> String {
        let mut ret = String::new();
        let length = self.token_tree.len();
        let mut index = 0;
        while index < length {
            self.format_token_trees_(
                &mut ret,
                self.token_tree.get(index).unwrap(),
                self.token_tree.get(index + 1),
            );
            index += 1;
        }
        ret
    }

    /// analyzer a `Nested` token tree.
    fn analyzer_token_tree_delimiter(
        token_tree: &Vec<TokenTree>,
    ) -> (
        Option<Delimiter>, // if this is a `Delimiter::Semicolon` we can know this is a function body or etc.
        bool,              // has a `:`
    ) {
        let mut d = None;
        let mut has_colon = false;
        for t in token_tree.iter() {
            match t {
                TokenTree::SimpleToken { content, pos, tok } => match content.as_str() {
                    ";" => {
                        d = Some(Delimiter::Semicolon);
                    }
                    "," => {
                        d = Some(Delimiter::Comma);
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
    fn analyzer_token_tree_length(token_tree: &Vec<TokenTree>) -> usize {
        let mut ret = usize::default();
        fn analyzer_token_tree_length_(ret: &mut usize, token_tree: &TokenTree) {
            match token_tree {
                TokenTree::SimpleToken { content, .. } => {
                    *ret = *ret + content.len();
                }
                TokenTree::Nested { elements, .. } => {
                    for t in elements.iter() {
                        analyzer_token_tree_length_(ret, t);
                    }
                    *ret = *ret + 2; // for delimiter.
                }
            }
        }
        for t in token_tree.iter() {
            analyzer_token_tree_length_(&mut ret, t);
        }
        ret
    }

    fn format_token_trees_(
        &self,
        ret: & /* 1 */ mut String,
        token: &TokenTree,
        next_token: Option<&TokenTree>,
    ) {
        match token {
            //Iter Nested
            TokenTree::Nested { elements, kind } => {
                //Add comment
                let _gurard = self.increment_depth();
                for temp_comment in &self.comments[self.comment_index.get()..] {
                    if (temp_comment.start_offset < kind.start_pos) {
                        ret.push_str(temp_comment.content.as_str());
                        //TODO: Change line in different system
                        ret.push_str("\n");
                        self.comment_index.set(self.comment_index.get() + 1);
                    } else {
                        break;
                    }
                }

                let length = Self::analyzer_token_tree_length(elements);
                let (delimiter, has_colon) = Self::analyzer_token_tree_delimiter(elements);

                //If brace, change line?
                match kind.kind {
                    NestKind_::Brace => {
                        if (ret.chars().last().unwrap() == ':'
                            && format!("{}", kind.kind.start_tok()).as_str() == "{")
                        {
                            ret.push_str(format!("{}", kind.kind.start_tok()).as_str());
                        } else {
                            ret.push_str(format!("{}", kind.kind.start_tok()).as_str());
                            ret.push_str("\n");
                            ret.push_str(&indent(*self.depth.as_ref().borrow()));
                        }
                    }
                    NestKind_::Lambda => {
                        ret.push_str("|");
                    }
                    NestKind_::Type => {
                        ret.push_str("<");
                    }
                    NestKind_::ParentTheses => {
                        ret.push_str("(");
                    }
                    NestKind_::Bracket => {
                        ret.push_str("[");
                    }
                }

                //Add signer

                for i in 0..elements.len() {
                    self.format_token_trees_(ret, elements.get(i).unwrap(), elements.get(i + 1));
                }
                match kind.kind {
                    NestKind_::Brace => {
                        match next_token {
                            None => {
                                ret.push_str("\n");
                                ret.push_str(&indent(*self.depth.as_ref().borrow()));
                            }
                            Some(temp_token) => match temp_token {
                                TokenTree::SimpleToken { content, pos, tok } => {
                                    if (!content.as_str().contains(";")
                                        || ret.chars().last().unwrap() == ',')
                                    {
                                        ret.push_str("\n");
                                        ret.push_str(&indent(*self.depth.as_ref().borrow()));
                                    }
                                }
                                _ => {}
                            },
                        }
                        ret.push_str("}");
                        if (*self.depth.as_ref().borrow() <= 1) {
                            ret.push_str("\n");
                        }
                    }
                    NestKind_::Lambda => {
                        ret.push_str("|");
                    }
                    NestKind_::Type => {
                        ret.push_str(">");
                    }
                    NestKind_::ParentTheses => {
                        ret.push_str(")");
                    }
                    NestKind_::Bracket => {
                        ret.push_str("]");
                    }
                }
                //Add signer
            }
            //Add to string
            TokenTree::SimpleToken { content, pos, tok } => {
                //Add comment
                for temp_comment in &self.comments[self.comment_index.get()..] {
                    if (temp_comment.start_offset < *pos) {
                        match next_token {
                            None => {}
                            Some(temp_token) => match temp_token {
                                TokenTree::SimpleToken {
                                    content: (_),
                                    pos: (_),
                                    tok: (temp_tok),
                                } => {
                                    if (comment_need_changeline(*tok, temp_tok.clone())) {
                                        ret.push_str("\n");
                                    }
                                }
                                _ => {
                                    ret.push_str("\n");
                                }
                            },
                        }

                        ret.push_str(temp_comment.content.as_str());
                        //TODO: Change line in different system
                        ret.push_str("\n");
                        ret.push_str(&indent(*self.depth.as_ref().borrow()));
                        self.comment_index.set(self.comment_index.get() + 1);
                    } else {
                        break;
                    }
                }
                // Check Token Type and React
                //ret.push_str(" ");
                match tok {
                    move_compiler::parser::lexer::Tok::EOF => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::NumValue => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::NumTypedValue => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::ByteStringValue => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::Identifier => match content as &str {
                        "has" => {
                            ret.push_str(" ");
                        }
                        _ => {
                            ret.push_str("");
                        }
                    },
                    move_compiler::parser::lexer::Tok::Exclaim => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::ExclaimEqual => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Percent => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Amp => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::AmpAmp => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::AmpMut => {
                        //ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LParen => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::RParen => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LBracket => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::RBracket => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Star => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Plus => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Comma => {
                        //ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Minus => {
                        ret.push_str(" ");
                    }

                    move_compiler::parser::lexer::Tok::PeriodPeriod => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Slash => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Colon => {
                        //ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::ColonColon => {
                        ret.push_str("");
                    }
                    move_compiler::parser::lexer::Tok::Semicolon => {}
                    move_compiler::parser::lexer::Tok::Less => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LessEqual => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LessLess => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Equal => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::EqualEqual => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::EqualEqualGreater => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LessEqualEqualGreater => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Greater => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::GreaterEqual => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::GreaterGreater => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Caret => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Abort => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Acquires => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::As => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Break => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Continue => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Copy => {}
                    move_compiler::parser::lexer::Tok::Else => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::False => {
                        ret.push_str(" ");
                    }

                    move_compiler::parser::lexer::Tok::Public => {
                        ret.push_str("\n");
                        ret.push_str(&indent(*self.depth.as_ref().borrow()));
                    }
                    move_compiler::parser::lexer::Tok::Struct => {
                        ret.push_str("\n");
                        ret.push_str(&indent(*self.depth.as_ref().borrow()));
                    }
                    move_compiler::parser::lexer::Tok::True => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Use => {
                        //ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::While => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::LBrace => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::Pipe => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::PipePipe => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::RBrace => {
                        ret.push_str(" ");
                    }
                    move_compiler::parser::lexer::Tok::NumSign => {
                        ret.push_str("\n");
                        ret.push_str("\n");
                        ret.push_str(&indent(*self.depth.as_ref().borrow()));
                    }
                    _ => {
                        ret.push_str("");
                    }
                }
                //Push simpletoken
                ret.push_str(&content.as_str());
                // if content.as_str().contains(";") {

                // } else {
                //     //ret.push_str(" ");
                // }
                //Back push
                match next_token {
                    None => {}
                    Some(temp_token) => match tok {
                        move_compiler::parser::lexer::Tok::EOF => {
                            unimplemented!()
                        }
                        move_compiler::parser::lexer::Tok::Semicolon => {
                            ret.push_str("\n");
                            ret.push_str(&indent(*self.depth.as_ref().borrow()));
                        }
                        _ => match temp_token {
                            TokenTree::SimpleToken {
                                content: (_),
                                pos: (_),
                                tok: (temp_tok),
                            } => {
                                if (need_space(*tok, temp_tok.clone())) {
                                    ret.push_str(" ");
                                }
                            }
                            _ => {}
                        },
                    },
                }
            }
        }
    }

    /// 缩进
    fn indent(&mut self, ret: &mut String) {
        ret.push_str(&indent(*self.depth.as_ref().borrow()));
    }

    fn translate_line(&self, pos: u32) -> u32 {
        self.line_mapping
            .translate(&self.path, pos, pos)
            .unwrap()
            .line_start
    }
}

/// A RAII type  
#[must_use]
struct DepthGuard(Rc<RefCell<usize>>);

impl Drop for DepthGuard {
    fn drop(&mut self) {
        let old = *self.0.as_ref().borrow();
        *self.0.as_ref().borrow_mut() = old - 1;
    }
}

pub fn format(p: impl AsRef<Path>, config: FormatConfig) -> Result<String, Diagnostics> {
    let p = p.as_ref();
    let content = std::fs::read_to_string(p).unwrap();
    let mut env = CompilationEnv::new(Flags::testing());
    let filehash = FileHash::empty();
    let (defs, _) = parse_file_string(&mut env, filehash, &content)?;
    let lexer = Lexer::new(&content, filehash);
    let mut parse = super::token_tree::Parser::new(lexer, &defs);
    let token_tree = parse.parse_tokens();
    let ce = CommentExtrator::new(content.as_str()).unwrap();
    let mut t = FileLineMapping::default();
    t.update(p.to_path_buf(), &content);
    let f = Format::new(config, token_tree, ce, t, p.to_path_buf());
    Ok(f.format_token_trees())
}

pub(crate) fn need_space(current: Tok, next: Tok) -> bool {
    match (TokType::from(current), TokType::from(next)) {
        (TokType::Alphabet, TokType::Alphabet) => true,
        (TokType::MathSign, _) => true,
        _ => false,
    }
}

pub(crate) fn comment_need_changeline(current: Tok, next: Tok) -> bool {
    match (TokType::from(current), TokType::from(next)) {
        (TokType::Sign, TokType::Alphabet) => true,
        _ => false,
    }
}

// pub(crate) fn no_need_space_str(current: &str, next: &str) -> bool {
//     if (current.as_bytes().get(0).unwrap().is_ascii_alphabetic()) {
//         return true;
//     }
//     false
// }
