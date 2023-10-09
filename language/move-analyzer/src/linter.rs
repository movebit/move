// Copyright (c) Mysten Labs, Inc.
// SPDX-License-Identifier: Apache-2.0

use std::{collections::BTreeMap, fs, path::Path};
use crate::utils::discover_manifest_and_kind;
use move_command_line_common::{
    testing::EXP_EXT,
    testing::{add_update_baseline_fix, format_diff, read_env_update_baseline},
};
use move_compiler::{
    cfgir::visitor::AbstractInterpreterVisitor,
    command_line::compiler::move_check_for_errors,
    diagnostics::codes::{self, WarningFilter},
    editions::Flavor,
    expansion::ast as E,
    shared::{NumericalAddress, PackageConfig},
    typing::visitor::TypingVisitor,
    Compiler, PASS_PARSER,
};

use sui_move_build::linters::{
    coin_field::CoinFieldVisitor, collection_equality::CollectionEqualityVisitor,
    custom_state_change::CustomStateChangeVerifier, freeze_wrapped::FreezeWrappedVisitor,
    known_filters, self_transfer::SelfTransferVerifier, share_owned::ShareOwnedVerifier,
    LINT_WARNING_PREFIX,
};

use std::{path::PathBuf, str::FromStr, str};

use super::context::*;
use lsp_server::*;
use serde::Deserialize;

#[derive(Clone, Deserialize)]
pub struct ReqParameters {
    fpath: String,
}
#[derive(Clone, serde::Serialize)]
pub struct Resp {
    result_msg: String,
}

pub fn on_run_linter(context: &Context, request: &Request) {
    log::info!("on_run_linter request = {:?}", request);
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = PathBuf::from_str(parameters.fpath.as_str()).unwrap();
    let send_err = |context: &Context, msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let project = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => return,
    };
    // let mut target = vec![];
    // if let Some((manifest_path, _)) = discover_manifest_and_kind(fpath.as_path()) {
    //     let d = Default::default();
    //     let b = project
    //         .modules
    //         .get(&manifest_path)
    //         .unwrap_or(&d)
    //         .as_ref()
    //         .borrow();
    //     target = b.clone().sources.into_iter()
    //                 .map(|(path_buf, _)| path_buf.to_string_lossy().to_string())
    //                 .collect::<Vec<_>>()
    //                 .clone();
    // };
    // let result_msg = run_project_linter(target, &project.dependents);

    let mut working_dir = fpath.clone();
    if let Some((manifest_path, _)) = discover_manifest_and_kind(fpath.as_path()) {
        working_dir = manifest_path;
        log::info!("linter -- working_dir = {:?}", working_dir);
    }
    let result_msg = run_sigle_file_linter(&working_dir, &fpath, &project.dependents);
    let r = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(Resp {
            result_msg: match result_msg.clone() {
                Some(result_msg) => result_msg,
                None => "null".to_string(),
            },
        })
        .unwrap(),
    );
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
    log::info!("result_msg = ----> \n{:?}", result_msg);
}

// fn default_addresses() -> BTreeMap<String, NumericalAddress> {
//     let mapping = [("std", "0x1"), ("sui", "0x2")];
//     mapping
//         .iter()
//         .map(|(name, addr)| (name.to_string(), NumericalAddress::parse_str(addr).unwrap()))
//         .collect()
// }

pub fn known_filters_for_linter() -> (E::AttributeName_, Vec<WarningFilter>) {
    let (filter_attr_name, mut filters) = known_filters();

    let unused_function_code_filter = WarningFilter::code(
        Some(LINT_WARNING_PREFIX),
        codes::Category::UnusedItem as u8,
        codes::UnusedItem::Function as u8,
        Some("code_suppression_should_not_work"),
    );
    let unused_function_category_filter = WarningFilter::category(
        Some(LINT_WARNING_PREFIX),
        codes::Category::UnusedItem as u8,
        Some("category_suppression_should_not_work"),
    );
    filters.push(unused_function_code_filter);
    filters.push(unused_function_category_filter);
    (filter_attr_name, filters)
}

fn run_sigle_file_linter(working_dir: &Path, path: &Path, deps: &Vec<std::string::String>) -> Option<String> {
    let targets: Vec<String> = vec![path.to_str().unwrap().to_owned()];
    let lint_visitors = vec![
        ShareOwnedVerifier.visitor(),
        SelfTransferVerifier.visitor(),
        CustomStateChangeVerifier.visitor(),
        CoinFieldVisitor.visitor(),
        FreezeWrappedVisitor.visitor(),
        CollectionEqualityVisitor.visitor(),
    ];

    use tempfile::tempdir;
    let build_config = move_package::BuildConfig {
        test_mode: true,
        install_dir: Some(tempdir().unwrap().path().to_path_buf()),
        skip_fetch_latest_git_deps: true,
        ..Default::default()
    };
    let resolution_graph =
        build_config.resolution_graph_for_package(&working_dir, &mut Vec::new()).ok()?;
    let named_address_mapping: Vec<_> = resolution_graph
        .extract_named_address_mapping()
        .map(|(name, addr)| format!("{}={}", name.as_str(), addr))
        .collect();
    // log::info!("named_address_mapping = {:?}", named_address_mapping);
    use move_model::parse_addresses_from_options;
    let addrs = parse_addresses_from_options(named_address_mapping.clone()).ok()?;

    // deps: vec![MOVE_STDLIB_PATH.to_string(), SUI_FRAMEWORK_PATH.to_string()],
    let (filter_attr_name, filters) = known_filters_for_linter();
    let (files, comments_and_compiler_res) = Compiler::from_files(
        targets,
        deps.clone(),
        addrs,
        // default_addresses(),
    )
    .add_visitors(lint_visitors)
    .set_default_config(PackageConfig {
        flavor: Flavor::Sui,
        ..PackageConfig::default()
    })
    .add_custom_known_filters(filters, filter_attr_name)
    .run::<PASS_PARSER>().ok()?;

    let diags = move_check_for_errors(comments_and_compiler_res);

    let has_diags = !diags.is_empty();
    let diag_buffer = if has_diags {
        move_compiler::diagnostics::report_diagnostics_to_buffer(&files, diags)
    } else {
        vec![]
    };

    let rendered_diags = std::str::from_utf8(&diag_buffer).ok()?;
    Some(rendered_diags.to_string())
}

// fn run_project_linter(targets: Vec<std::string::String>, deps: &Vec<std::string::String>) -> Option<String> {
//     let lint_visitors = vec![
//         ShareOwnedVerifier.visitor(),
//         SelfTransferVerifier.visitor(),
//         CustomStateChangeVerifier.visitor(),
//         CoinFieldVisitor.visitor(),
//         FreezeWrappedVisitor.visitor(),
//         CollectionEqualityVisitor.visitor(),
//     ];

//     // deps: vec![MOVE_STDLIB_PATH.to_string(), SUI_FRAMEWORK_PATH.to_string()],
//     let (filter_attr_name, filters) = known_filters_for_linter();
//     let (files, comments_and_compiler_res) = Compiler::from_files(
//         targets,
//         deps.clone(),
//         default_addresses(),
//     )
//     .add_visitors(lint_visitors)
//     .set_default_config(PackageConfig {
//         flavor: Flavor::Sui,
//         ..PackageConfig::default()
//     })
//     .add_custom_known_filters(filters, filter_attr_name)
//     .run::<PASS_PARSER>().ok()?;

//     let diags = move_check_for_errors(comments_and_compiler_res);

//     let has_diags = !diags.is_empty();
//     let diag_buffer = if has_diags {
//         move_compiler::diagnostics::report_diagnostics_to_buffer(&files, diags)
//     } else {
//         vec![]
//     };

//     let rendered_diags = std::str::from_utf8(&diag_buffer).ok()?;
//     Some(rendered_diags.to_string())
// }
