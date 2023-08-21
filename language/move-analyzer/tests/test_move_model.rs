// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#[cfg(test)]
mod tests {
    use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
    use move_compiler::shared::PackagePaths;
    use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};
    use std::{
        fs,
        path::{Path, PathBuf},
    };
    use aptos_move_analyzer::utils::*;

    fn test_run_model_builder_with_options(fpath: &Path) -> datatest_stable::Result<()>  {
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
        )?;
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
                eprintln!("sava env '{:?}'", env);
                let _ = fs::write(output_file, env.dump_env());
            }
        }
        Ok(())
    }

    #[test]
    fn test_001() {
        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/conditions_ok.move").as_path(),
        );
        let _ = test_run_model_builder_with_options(&fpath);
    }

    #[test]
    fn test_002() {
        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/structs_ok.move").as_path(),
        );
        let _ = test_run_model_builder_with_options(&fpath);
    }
}
