#![allow(dead_code)]
use std::cell::RefCell;
use std::rc::Rc;

use crate::token_tree::TokenTree;
struct Format {
    config: FormatConfig,
    depth: Rc<RefCell<usize>>,
    token_tree: Vec<TokenTree>,
}

pub struct FormatConfig {
    pub ident_size: usize,
}

impl Format {
    fn new(config: FormatConfig, token_tree: Vec<TokenTree>) -> Self {
        Self {
            config,
            depth: Default::default(),
            token_tree,
        }
    }
}

impl Format {
    fn increment_depth(&self) -> DepthGuard {
        let old = *self.depth.as_ref().borrow();
        *self.depth.as_ref().borrow_mut() = old + 1;
        DepthGuard(self.depth.clone())
    }
    pub fn format_token_trees(&mut self) -> String {
        unimplemented!()
    }
}

struct DepthGuard(Rc<RefCell<usize>>);

impl Drop for DepthGuard {
    fn drop(&mut self) {
        let old = *self.0.as_ref().borrow();
        *self.0.as_ref().borrow_mut() = old - 1;
    }
}
