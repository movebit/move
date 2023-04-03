#![allow(dead_code)]
use std::cell::RefCell;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::result::Result::*;
use std::string;

use move_command_line_common::files::FileHash;
use move_compiler::diagnostics::Diagnostics;
use move_compiler::parser::lexer::Lexer;
use move_compiler::parser::syntax::parse_file_string;
use move_compiler::shared::CompilationEnv;
use move_compiler::{Flags, MatchedFileCommentMap};
use stderrlog::new;

use crate::move_generate_spec::indent;
use crate::token_tree::TokenTree;
use crate::utils::FileLineMapping;
struct Format {
    config: FormatConfig,
    depth: Rc<RefCell<usize>>,
    token_tree: Vec<TokenTree>,
    comments: Vec<(u32, String)>,
    line_mapping: FileLineMapping,
    path: PathBuf,
}

pub struct FormatConfig {
    pub ident_size: u32,
}

impl Format {
    fn new(
        config: FormatConfig,
        token_tree: Vec<TokenTree>,
        comments: MatchedFileCommentMap,
        line_mapping: FileLineMapping,
        path: PathBuf,
    ) -> Self {
        Self {
            config,
            depth: Default::default(),
            token_tree,
            comments: comments.into_iter().map(|(k, v)| (k, v)).collect(),
            line_mapping,
            path,
        }
    }
    fn increment_depth(&self) -> DepthGuard {
        let old = *self.depth.as_ref().borrow();
        *self.depth.as_ref().borrow_mut() = old + 1;
        DepthGuard(self.depth.clone())
    }

    pub fn format_token_trees(mut self) -> String {
        let mut ret = String::new();
        let length = self.token_tree.len();
        let mut index = 0;
        while index < length {
            self.format_token_trees_(&mut ret, self.token_tree.get(index).unwrap());
            index += 1;
        }
        ret
    }

    fn format_token_trees_(&self, ret: &mut String, token: &TokenTree) {
        match token {
            TokenTree::Nested { elements, kind } => {
                //Iter
            }
            TokenTree::SimpleToken { content, pos } => {
                //Add to string
                ret.push_str(&content.as_str());
            }
        }
    }
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
    let (defs, comments) = parse_file_string(&mut env, filehash, &content)?;
    let lexer = Lexer::new(&content, filehash);
    let mut parse = super::token_tree::Parser::new(lexer, &defs);
    let token_tree = parse.parse_tokens();
    let mut t = FileLineMapping::default();
    t.update(p.to_path_buf(), &content);
    let f = Format::new(config, token_tree, comments, t, p.to_path_buf());
    Ok(f.format_token_trees())
}
