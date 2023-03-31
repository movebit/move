use super::fmt2::NestKind;

pub enum TokenTree {
    SimpleToken {
        content: String,
        pos: u32,
    },
    Nest {
        elements: Vec<TokenTree>,
        kind: NestKind,
    },
}

pub fn dump_token_tree(tree: &Vec<TokenTree>) -> String {
    unimplemented!()
}
