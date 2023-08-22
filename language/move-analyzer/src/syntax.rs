// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::utils::path_concat;
use move_command_line_common::files::FileHash;
use move_compiler::{
    diagnostics::{Diagnostic, Diagnostics},
    parser::{ast::*, lexer::*},
    shared::*,
    MatchedFileCommentMap,
};

use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use move_compiler::shared::PackagePaths;
use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};
use std::{
    fs,
    path::Path,
    path::PathBuf,
};

fn parse_file(fpath: &Path) -> Result<Vec<Definition>, Box<Diagnostic>> {
    let mut defs = vec![];
    eprintln!("fpath = {:?}", fpath);
    let targets = vec![PackagePaths {
        name: None,
        paths: vec![fpath.to_str().unwrap().to_string()],
        named_address_map: std::collections::BTreeMap::<String, _>::new(),
    }];
    let env = run_model_builder_with_options(
        targets, vec![], ModelBuilderOptions {
                compile_via_model: false,
                ..Default::default()
            }
    );
    match env {
        Ok(env) => {
            eprintln!("env.get_module_count() = {:?}", &env.get_module_count());
            let diags = if env.diag_count(Severity::Warning) > 0 {
                let mut writer = Buffer::ansi();
                env.report_diag(&mut writer, Severity::Warning);
                String::from_utf8_lossy(&writer.into_inner()).to_string()
            } else {
                "All good, no errors!".to_string()
            };
            eprintln!("diags = {:?}", diags);
            if let Some(file_stem) = fpath.file_stem() {
                if let Some(file_stem_str) = file_stem.to_str() {
                    let output_file = format!("{}{}.txt", "./output_global_env-", file_stem_str);
                    // eprintln!("sava env '{:?}'", env);
                    let _ = fs::write(output_file, env.dump_env());
                }
            }
        
            for module in env.get_modules() {
                eprintln!("*module.get_name() = {:?}{:?}", module.get_name().addr(), module.get_name().name());
                for fun in module.get_functions() {
                    let id = fun.get_qualified_id();
                    eprintln!("func id = {:?}", id);
                    eprintln!("func get_full_name_str = {:?}", fun.get_full_name_str());                
                }
            }
        },
        Err(_) => {},
    }

    Ok(defs)
}

/// Parse the `input` string as a file of Move source code and return the
/// result as either a pair of FileDefinition and doc comments or some Diagnostics. The `file` name
/// is used to identify source locations in error messages.
pub fn parse_file_string(
    env: &mut CompilationEnv,
    file_hash: FileHash,
    input: &str,
) -> Result<(Vec<Definition>, MatchedFileCommentMap), Diagnostics> {
    let mut tokens = Lexer::new(input, file_hash);
    match tokens.advance() {
        Err(err) => Err(Diagnostics::from(vec![*err])),
        Ok(..) => Ok(()),
    }?;
    let fpath = path_concat(
        std::env::current_dir().unwrap().as_path(),
        PathBuf::from("tests/symbols/sources/conditions_ok.move").as_path(),
    );
    match parse_file(&fpath) {
        Err(err) => Err(Diagnostics::from(vec![*err])),
        Ok(def) => Ok((def, tokens.check_and_get_doc_comments(env))),
    }
}
