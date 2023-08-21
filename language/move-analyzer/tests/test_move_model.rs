// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
#[cfg(test)]
mod tests {
    use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
    use move_command_line_common::testing::EXP_EXT;
    use move_compiler::shared::PackagePaths;
    use move_model::{options::ModelBuilderOptions, run_model_builder_with_options,
        model::EnvDisplay,
    };
    use move_prover_test_utils::baseline_test::verify_or_update_baseline;
    use std::{
        cell::RefCell, fs,
        path::{Path, PathBuf},
    };
    use aptos_move_analyzer::{
        context::{Context, FileDiags, MultiProject},
        goto_definition,
        utils::*,
    };

    #[test]
    fn test_run_model_builder_with_options_001() -> datatest_stable::Result<()>  {
        let current_dir = std::env::current_dir().unwrap();
        let fpath = aptos_move_analyzer::utils::path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/conditions_ok.move").as_path(),
        );
        eprintln!("fpath = {:?}", fpath);
        let targets = vec![move_compiler::shared::PackagePaths {
            name: None,
            paths: vec![fpath.to_str().unwrap().to_string()],
            named_address_map: std::collections::BTreeMap::<String, _>::new(),
        }];
        let env = move_model::run_model_builder_with_options(
            targets, vec![], move_model::options::ModelBuilderOptions {
                    compile_via_model: true,
                    ..Default::default()
                }
        )?;
        eprintln!("env.get_module_count() = {:?}", &env.get_module_count());
        let all_structs = &env.get_all_structs_with_conditions();
        eprintln!("all_structs.len = {:?}", all_structs.len()); 
        let diags = if env.diag_count(codespan_reporting::diagnostic::Severity::Warning) > 0 {
            let mut writer = codespan_reporting::term::termcolor::Buffer::ansi();
            env.report_diag(&mut writer, codespan_reporting::diagnostic::Severity::Warning);
            String::from_utf8_lossy(&writer.into_inner()).to_string()
        } else {
            "All good, no errors!".to_string()
        };
        eprintln!("diags = {:?}", diags);
    
        // Putting the generated test baseline into a Refcell to avoid problems with mut borrow
        // in closures.
        let test_output = std::cell::RefCell::new(String::new());
        
        if let Some(file_stem) = fpath.file_stem() {
            if let Some(file_stem_str) = file_stem.to_str() {
                let output_file = format!("{}{}.txt", "./output_global_env-", file_stem_str);
                let out = &mut test_output.borrow_mut();
                // out.push_str(&env.dump_env());
                for struct_type in all_structs {  
                    eprintln!("{:?}", struct_type);  
                }
                // eprintln!("sava env '{:?}'", env);  
    
                std::fs::write(output_file, env.dump_env());
    
                // fs::write(output_file, &content)?
                // eprintln!("sava env'{:?}' result =  '{:?}'", output_file.clone(), fs::write(output_file, env.dump_env()));
            }
        }
        Ok(())
    }
}
