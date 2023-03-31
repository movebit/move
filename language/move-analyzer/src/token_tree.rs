use super::fmt2::NestKind;

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
    Simicolon,
}

pub fn dump_token_tree(tree: &Vec<TokenTree>) -> String {
    unimplemented!()
}
