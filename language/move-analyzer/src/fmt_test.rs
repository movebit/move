use crate::{fmt::FormatConfig, token_tree::TokenTree};

use super::utils::FileLineMapping;

use move_command_line_common::files::FileHash;
use move_compiler::{
    diagnostics::{Diagnostic, Diagnostics},
    parser::{
        lexer::{Lexer, Tok},
        syntax::parse_file_string,
    },
    shared::CompilationEnv,
    Flags,
};
use std::fmt::format;
use std::path::Path;

#[test]
fn scan_dir() {
    for x in walkdir::WalkDir::new(match std::env::var("MOVE_FMT_TEST_DIR") {
        Ok(x) => x,
        Err(_) => {
            eprintln!("MOVE_FMT_TEST_DIR env var not set this test skipped.");
            return;
        }
    }) {
        let x = match x {
            Ok(x) => x,
            Err(_) => todo!(),
        };
        if x.file_type().is_file() && x.file_name().to_str().unwrap().ends_with(".move") {
            let p = x.into_path();
            eprintln!("try format:{:?}", p);
            let content = std::fs::read_to_string(&p).unwrap();
            {
                let mut env = CompilationEnv::new(Flags::testing());
                match parse_file_string(&mut env, FileHash::empty(), &content) {
                    Ok(_) => {}
                    Err(_) => {
                        eprintln!("file '{:?}' skipped because of parse not ok", p.as_path());
                        continue;
                    }
                }
            }
            let t1 = extract_tokens(content.as_str())
                .expect("test file should be about to lexer,err:{:?}");
            let conten2 = super::fmt::format(p.as_path(), FormatConfig { indent_size: 2 }).unwrap();
            let t2 = match extract_tokens(conten2.as_str()) {
                Ok(x) => x,
                Err(err) => {
                    unreachable!(
                        "should be able to parse after format:err{:?} , content2:\n{}\n",
                        err, conten2
                    );
                }
            };

            // TODO fix >> may make multi line after format.
            for (t1, t2) in t1.iter().zip(t2.iter()) {
                assert_eq!(
                    t1.content,
                    t2.content,
                    "format not ok,file:{:?} line:{} col:{}",
                    p.as_path(),
                    // +1 in vscode UI line and col start with 1
                    t1.line + 1,
                    t2.col + 1
                );
            }
            assert_eq!(t1.len(), t2.len(), "{:?} token length should equal", p);
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

fn extract_tokens(content: &str) -> Result<Vec<ExtractToken>, Box<Diagnostics>> {
    let p = Path::new(".").to_path_buf();
    let mut line_mapping = FileLineMapping::default();
    line_mapping.update(p.clone(), &content);
    let filehash = FileHash::empty();
    let mut env = CompilationEnv::new(Flags::testing());
    let (defs, comments) = parse_file_string(&mut env, filehash, content)?;
    let mut lexer = Lexer::new(&content, filehash);
    let mut ret = Vec::new();
    let mut parse = super::token_tree::Parser::new(lexer, &defs);
    let token_tree = parse.parse_tokens();
    let mut line_mapping = FileLineMapping::default();
    line_mapping.update(p.to_path_buf(), &content);
    fn collect_token_tree(ret: &mut Vec<ExtractToken>, m: &FileLineMapping, t: &TokenTree) {
        match t {
            TokenTree::SimpleToken {
                content,
                pos,
                tok: _tok,
            } => {
                let loc = m
                    .translate(&Path::new(".").to_path_buf(), *pos, *pos)
                    .unwrap();

                ret.push(ExtractToken {
                    content: content.clone(),
                    line: loc.line_start,
                    col: loc.col_start,
                });
            }
            TokenTree::Nested { elements, kind } => {
                let start_loc = m
                    .translate(
                        &Path::new(".").to_path_buf(),
                        kind.start_pos,
                        kind.start_pos,
                    )
                    .unwrap();
                ret.push(ExtractToken {
                    content: format!("{:?}", kind.kind.start_tok()),
                    line: start_loc.line_start,
                    col: start_loc.col_start,
                });

                for token in elements.iter() {
                    collect_token_tree(ret, m, token);
                }
                let end_loc = m
                    .translate(&Path::new(".").to_path_buf(), kind.end_pos, kind.end_pos)
                    .unwrap();
                ret.push(ExtractToken {
                    content: format!("{:?}", kind.kind.end_tok()),
                    line: end_loc.line_start,
                    col: end_loc.col_start,
                });
            }
        }
    }
    for token in token_tree.iter() {
        collect_token_tree(&mut ret, &line_mapping, token);
    }

    Ok(ret)
}
