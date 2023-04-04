use crate::fmt::FormatConfig;

use super::utils::FileLineMapping;
use move_command_line_common::files::FileHash;
use move_compiler::parser::lexer::{Lexer, Tok};
use std::path::Path;

#[test]
fn xxx() {
    for x in walkdir::WalkDir::new(Path::new(
        "/Users/yuyang/projects/sui/sui_programmability/examples",
    )) {
        let x = match x {
            Ok(x) => x,
            Err(_) => todo!(),
        };
        if x.file_type().is_file() && x.file_name().to_str().unwrap().ends_with(".move") {
            let p = x.into_path();
            let content = std::fs::read_to_string(&p).unwrap();
            let t1 = extract_tokens(content.as_str());
            let conten2 = super::fmt::format(p.as_path(), FormatConfig { ident_size: 2 }).unwrap();
            let t2 = extract_tokens(conten2.as_str());
            // TODO fix >> may make multi line after format.
            for (t1, t2) in t1.iter().zip(t2.iter()) {
                assert_eq!(
                    t1,
                    t2,
                    "format wrong file:{:?} line:{} col:{}",
                    p.as_path(),
                    t1.line,
                    t2.col
                );
            }
            assert_eq!(t1.len(), t2.len(), "token length should equal");
            eprintln!("{:?} format ok.", p);
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct ExtractToken {
    content: String,
    line: u32,
    col: u32,
}

fn extract_tokens(content: &str) -> Vec<ExtractToken> {
    let p = Path::new(".").to_path_buf();
    let mut t = FileLineMapping::default();
    t.update(p.clone(), &content);
    let filehash = FileHash::empty();
    let mut lexer = Lexer::new(&content, filehash);

    let mut ret = Vec::new();
    lexer.advance().unwrap();

    while (lexer.peek() != Tok::EOF) {
        let loc = t
            .translate(&p, lexer.start_loc() as u32, lexer.start_loc() as u32)
            .unwrap();

        ret.push(ExtractToken {
            content: lexer.content().to_string(),
            line: loc.line_start,
            col: loc.col_start,
        });
        lexer.advance().unwrap();
    }

    ret
}
