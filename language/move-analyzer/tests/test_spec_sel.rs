// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(test)]
mod tests {

    use aptos_move_analyzer::{
        analyzer_handler::*,
        context::*,
        utils::{path_concat, FileRange,discover_manifest_and_kind},
        multiproject::MultiProject,
        move_generate_spec_sel::{on_generate_spec_sel,Resp},
    };
    use lsp_server::*;
    use lsp_types::*;
    use move_model::{
        ast::{ExpData::*, Operation::*, Spec, SpecBlockTarget},
        model::{FunId, GlobalEnv, ModuleId, StructId},
    };
    use std::path::{Path, PathBuf};
    use serde_json::json;
    // use itertools::Itertools;

    fn prepare_project(context: &mut Context, fpath: PathBuf) {
        log::info!("fpath = {:?}", fpath.to_str());
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            },
        };
        match context.projects.get_project(&fpath) {
            Some(_) => {
                if let Ok(x) = std::fs::read_to_string(fpath.as_path()) {
                    // update_defs_on_changed(context, fpath.clone(), x);
                };
                return;
            },
            None => {
                eprintln!("project '{:?}' not found try load.", fpath.as_path());
            },
        };
        let p = match context.projects.load_projects(&context.connection, &mani) {
            anyhow::Result::Ok(x) => x,
            anyhow::Result::Err(e) => {
                log::error!("load project failed,err:{:?}", e);
                return;
            },
        };
        context.projects.insert_project(p);
    }

    /// Generate Spec for Selected function with assert!()
    #[test]
    fn test_generate_spec_request_001() {
        let (connection, _) = Connection::stdio();
        let mut context = Context {
            projects: MultiProject::new(),
            connection,
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/v1-core/LPResourceAccount/sources/resourceAccount.move").as_path(),
        );
        prepare_project(&mut context, fpath.clone());

        let _ = match context.projects.get_project(&fpath) {
            Some(x) => x,
            None => {
                log::error!("project '{:?}' not found.", fpath.as_path());
                return;
            }
        };

        let params_json = json!({
            "col": 69,
            "line": 10,
            "fpath": fpath.to_string_lossy().to_string()
        });
        let request = Request {
            id: "generate_spec_request_001".to_string().into(),
            method: String::from("move/generate/spec/sel"),
            params: params_json,
        };

        let actual_r = on_generate_spec_sel(&mut context, &request);
        let ex = Some(
            Resp {
                line: 11, 
                col: 4,
                content: String::from("    spec CapabilityStorage{\n\n    }\n")
            }
        );
        let expect_r = Response::new_ok(
            "generate_spec_request_001".to_string().into(),
            serde_json::to_value(ex).unwrap(),
        );
        // // std::thread::sleep(Duration::new(1, 0));
        // log::info!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", actual_r.result);
        eprintln!("\n\n\n");
        // log::trace!("expect_r = {:?}", expect_r);
        log::info!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }
    

    /// Generate Spec for Operators which may Overflow or Underflow in a Function.
    #[test]
    fn test_generate_spec_request_002() {
        let (connection, _) = Connection::stdio();
        let mut context = Context {
            projects: MultiProject::new(),
            connection,
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/v1-core/LPResourceAccount/sources/resourceAccount.move").as_path(),
        );
        prepare_project(&mut context, fpath.clone());

        let _ = match context.projects.get_project(&fpath) {
            Some(x) => x,
            None => {
                log::error!("project '{:?}' not found.", fpath.as_path());
                return;
            }
        };

        let params_json = json!({
            "col": 5,
            "line": 28,
            "fpath": fpath.to_string_lossy().to_string()
        });
        let request = Request {
            id: "generate_spec_request_002".to_string().into(),
            method: String::from("move/generate/spec/sel"),
            params: params_json,
        };

        let actual_r = on_generate_spec_sel(&mut context, &request);
        let ex = Some(
            Resp {
                line: 29, 
                col: 5,
                content: String::from("    spec initialize_lp_account(admin: &signer, lp_coin_metadata_serialized: vector<u8>, lp_coin_code: vector<u8>){\n\n        aborts_if signer::address_of(admin) != @SwapDeployer with ERR_FORBIDDEN;\n    }\n")
            }
        );
        let expect_r = Response::new_ok(
            "generate_spec_request_001".to_string().into(),
            serde_json::to_value(ex).unwrap(),
        );
        // // std::thread::sleep(Duration::new(1, 0));
        // log::info!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", actual_r.result);
        eprintln!("\n\n\n");
        // log::trace!("expect_r = {:?}", expect_r);
        log::info!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }

    
}