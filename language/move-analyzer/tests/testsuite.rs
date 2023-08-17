// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

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
fn test_runner(path: &Path, options: ModelBuilderOptions) -> datatest_stable::Result<()> {
    let targets = vec![PackagePaths {
        name: None,
        paths: vec![path.to_str().unwrap().to_string()],
        named_address_map: std::collections::BTreeMap::<String, _>::new(),
    }];
    let env = run_model_builder_with_options(targets, vec![], options)?;
    let diags = if env.diag_count(Severity::Warning) > 0 {
        let mut writer = Buffer::ansi();
        env.report_diag(&mut writer, Severity::Warning);
        String::from_utf8_lossy(&writer.into_inner()).to_string()
    } else {
        "All good, no errors!".to_string()
    };
    
    // Putting the generated test baseline into a Refcell to avoid problems with mut borrow
    // in closures.
    let test_output = RefCell::new(String::new());
    
    if let Some(file_stem) = path.file_stem() {
        if let Some(file_stem_str) = file_stem.to_str() {
            // let output_file = format!("{}{}.txt", "./output_global_env-", file_stem_str);
            let out = &mut test_output.borrow_mut();
            out.push_str("// ---- Model Dump\n");
            out.push_str(&env.dump_env());
            eprintln!("sava env'{}'", out);

            // fs::write(output_file, &content)?
            // eprintln!("sava env'{:?}' result =  '{:?}'", output_file.clone(), fs::write(output_file, env.dump_env()));
        }
    }
    let baseline_path = path.with_extension(EXP_EXT);
    verify_or_update_baseline(baseline_path.as_path(), &test_output.borrow())?;
    Ok(())
}

fn runner(path: &Path) -> datatest_stable::Result<()> {
    test_runner(path, ModelBuilderOptions::default())
}

datatest_stable::harness!(runner, "tests/symbols/sources", r".*\.move");
