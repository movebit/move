// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use move_command_line_common::testing::EXP_EXT;
use move_compiler::shared::PackagePaths;
use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};
use move_prover_test_utils::baseline_test::verify_or_update_baseline;
use std::path::Path;

fn test_runner(path: &Path, options: ModelBuilderOptions) -> datatest_stable::Result<()> {
    eprintln!("lll >> test_runner");
    let targets = vec![PackagePaths {
        name: None,
        paths: vec![path.to_str().unwrap().to_string()],
        named_address_map: std::collections::BTreeMap::<String, _>::new(),
    }];
    let env = run_model_builder_with_options(targets, vec![], options)?;
    eprintln!("env.get_module_count() = {:?}", &env.get_module_count());
    let diags = if env.diag_count(Severity::Warning) > 0 {
        let mut writer = Buffer::ansi();
        env.report_diag(&mut writer, Severity::Warning);
        String::from_utf8_lossy(&writer.into_inner()).to_string()
    } else {
        "All good, no errors!".to_string()
    };
    eprintln!("diags = {:?}", diags);
    // eprintln!("sava env '{:?}'", env);
    let baseline_path = path.with_extension(EXP_EXT);
    verify_or_update_baseline(baseline_path.as_path(), &diags)?;
    Ok(())
}

fn runner(path: &Path) -> datatest_stable::Result<()> {
    eprintln!("lll >> runner");
    test_runner(path, ModelBuilderOptions::default())
}

datatest_stable::harness!(runner, "tests/symbols/sources", r".*\.move");
