use crate::fmt::FormatConfig;

use super::utils::FileLineMapping;

use move_command_line_common::files::FileHash;
use move_compiler::{
    diagnostics::Diagnostic,
    parser::{
        lexer::{Lexer, Tok},
        syntax::parse_file_string,
    },
    shared::CompilationEnv,
    Flags,
};
use std::path::Path;

#[test]
fn scan_dir() {
    for x in walkdir::WalkDir::new(match std::env::var("MOVE_FMT_TEST_DIR") {
        Ok(x) => x,
        Err(_) => {
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
                        "should be able to lexer after lexer:err{:?} , content2:\n{}\n",
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

fn extract_tokens(content: &str) -> Result<Vec<ExtractToken>, Box<Diagnostic>> {
    let p = Path::new(".").to_path_buf();
    let mut line_mapping = FileLineMapping::default();
    line_mapping.update(p.clone(), &content);
    let filehash = FileHash::empty();
    let mut lexer = Lexer::new(&content, filehash);
    let mut ret = Vec::new();
    lexer.advance()?;
    while lexer.peek() != Tok::EOF {
        let loc = line_mapping
            .translate(&p, lexer.start_loc() as u32, lexer.start_loc() as u32)
            .unwrap();

        ret.push(ExtractToken {
            content: lexer.content().to_string(),
            line: loc.line_start,
            col: loc.col_start,
        });
        lexer.advance()?;
    }
    Ok(ret)
}
