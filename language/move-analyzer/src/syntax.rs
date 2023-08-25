// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_compiler::{
    diagnostics::Diagnostics,
    parser::ast::*,
    MatchedFileCommentMap,
};
use move_package::{source_package::layout::SourcePackageLayout, BuildConfig, ModelConfig};
use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use move_model::{model::GlobalEnv, options::ModelBuilderOptions, run_model_builder_with_options};
use std::{
    path::Path,
    path::PathBuf,
};

pub fn reroot_path(path: Option<PathBuf>) -> anyhow::Result<PathBuf> {
    let path: PathBuf = path.unwrap_or_else(|| PathBuf::from("."));
    // find the root dir where Move.toml is located.
    let rooted_path = SourcePackageLayout::try_find_root(&path.canonicalize()?)?;
    Ok(rooted_path)
    // std::env::set_current_dir(&rooted_path).unwrap();
    // Ok(PathBuf::from("."))
}

pub fn parse_package(path: &Path) -> GlobalEnv {
    let config = BuildConfig {
        ..Default::default()
    };

    let package_path = reroot_path(Option::Some(path.to_path_buf())).unwrap();
    log::info!("lll >> package_path = {:?}", package_path);
    let env = config
        .move_model_for_package(
            &package_path,
            ModelConfig {
                all_files_as_targets: false,
                target_filter: None,
            },
        )
        .expect("Failed to create GlobalEnv!");

    if env.has_errors() {
        let mut error_writer = Buffer::no_color();
        env.report_diag(&mut error_writer, Severity::Error);
        eprintln!(
            "{}",
            String::from_utf8_lossy(&error_writer.into_inner()).to_string()
        );
    } else {
        eprintln!("env.get_module_count() = {:?}", &env.get_module_count());
        // for module in env.get_modules() {
        //     for fun in module.get_functions() {
        //         let id = fun.get_qualified_id();
        //     }
        // }
    }
    env
}

pub fn get_definition_in_global_env_by_move_file(env: &GlobalEnv, move_file_path: &Path)
 -> Result<(Vec<Definition>, MatchedFileCommentMap), Diagnostics> {
    let defs = vec![];
    for module in env.get_modules() {
        eprintln!("lll >> get_definition_in_global_env_by_move_file, env.get_function_count() = {:?}", module.get_function_count());
        for fun in module.get_functions() {
            // let id = fun.get_qualified_id();
            log::info!("lll >> get_definition_in_global_env_by_move_file, fun.get_def() = {:?}", fun.get_def());
            if let Some(exp) = fun.get_def() {
                log::info!("lll >> get_definition_in_global_env_by_move_file, fn body = {}", exp.display_for_fun(fun.clone()));
            }
        }
    }
    Ok((defs, MatchedFileCommentMap::new()))
}

use move_compiler::shared::PackagePaths;
pub fn parse_package_v2(fpath: &Path) -> GlobalEnv {
    eprintln!("fpath = {:?}", fpath);
    let targets = vec![PackagePaths {
        name: None,
        paths: vec![fpath.to_str().unwrap().to_string()],
        named_address_map: std::collections::BTreeMap::<String, _>::new(),
    }];
    let dependents = vec![PackagePaths {
        name: None,
        paths: vec![fpath.to_str().unwrap().to_string()],
        named_address_map: std::collections::BTreeMap::<String, _>::new(),
    }];
    let env = run_model_builder_with_options(
        targets, dependents, ModelBuilderOptions {
                compile_via_model: true,
                ..Default::default()
            }
    );
    match env {
        Ok(env) => {
            if !env.has_errors() {
                eprintln!("env.get_module_count() = {:?}", &env.get_module_count());
                // for module in env.get_target_modules() {
                //     for fun in module.get_functions() {
                //         let id = fun.get_qualified_id();
                //         eprintln!("func id = {:?}", id);
                //         eprintln!("func get_full_name_str = {:?}", fun.get_full_name_str());                
                //     }
                // }
            }
            env
        },
        Err(_) => {return GlobalEnv::new()},
    }
}


/// Parse the `input` string as a file of Move source code and return the
/// result as either a pair of FileDefinition and doc comments or some Diagnostics. The `file` name
/// is used to identify source locations in error messages.
pub fn parse_file_string(file_path: PathBuf) -> 
Result<(Vec<Definition>, MatchedFileCommentMap), Diagnostics> {
    get_definition_in_global_env_by_move_file(&parse_package(&file_path), file_path.as_path())
}
