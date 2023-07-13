// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(test)]
mod tests {
    use lsp_server::{Connection, Request, Response};
    use move_analyzer::{
        context::{Context, FileDiags, MultiProject},
        symbols, inlay_hints, inlay_hints::*,
        utils::*,
        vfs::VirtualFileSystem,
    };
    use move_command_line_common::files::FileHash;
    use move_compiler::shared::*;
    use serde_json::json;
    use std::{
        path::PathBuf,
        sync::{Arc, Mutex},
        time::Duration,
    };
    pub use url::Url;

    fn update_defs(context: &mut Context, fpath: PathBuf, content: &str) {
        use move_analyzer::syntax::parse_file_string;
        let file_hash = FileHash::new(content);
        let mut env = CompilationEnv::new(Flags::testing());
        let defs = parse_file_string(&mut env, file_hash, content);
        let defs = match defs {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(d) => {
                log::error!("update file failed,err:{:?}", d);
                return;
            }
        };
        let (defs, _) = defs;
        context.projects.update_defs(fpath.clone(), defs);
        context.ref_caches.clear();
        context
            .projects
            .hash_file
            .as_ref()
            .borrow_mut()
            .update(fpath.clone(), file_hash);
        context
            .projects
            .file_line_mapping
            .as_ref()
            .borrow_mut()
            .update(fpath, content);
    }

    #[test]
    fn test_on_inlay_hints_001() {
        let (connection, _) = Connection::stdio();
        let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));
        let inlay_hints_config = InlayHintsConfig::default();

        let mut mock_ctx = Context {
            projects: MultiProject::new(),
            connection,
            files: VirtualFileSystem::default(),
            symbols,
            ref_caches: Default::default(),
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/symbols/sources/M2.move").as_path(),
        );

        eprintln!("fpath = {:?}", fpath.to_str());
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            }
        };
        match mock_ctx.projects.get_project(&fpath) {
            Some(_) => {
                if let Ok(x) = std::fs::read_to_string(fpath.as_path()) {
                    update_defs(&mut mock_ctx, fpath.clone(), x.as_str());
                };
                return;
            }
            None => {
                eprintln!("project '{:?}' not found try load.", fpath.as_path());
            }
        };
        let p = match mock_ctx.projects.load_project(&mock_ctx.connection, &mani) {
            anyhow::Result::Ok(x) => x,
            anyhow::Result::Err(e) => {
                log::error!("load project failed,err:{:?}", e);
                return;
            }
        };
        mock_ctx.projects.insert_project(p);

        let params_json = json!({
            "context": {
                "includeDeclaration": true
            },
            "range": [{
                "line": 0,
                "character": 0
            },
            {
                "line": 12,
                "character": 18
            }],
            "textDocument": {
                "uri": "file:///".to_string() + fpath.to_str().unwrap()
            },
        });
        let request: Request = Request {
            id: "inlay_hints_001".to_string().into(),
            method: String::from("textDocument/inlayHint"),
            params: params_json,
        };

        let expect_r: Response = Response::new_ok(
            "inlay_hints_001".to_string().into(),
            json!([{
                "kind": 1,
                "label": [
                     {
                          "value": ": "
                     },
                     {
                          "value": "u64"
                     }
                ],
                "paddingLeft": true,
                "paddingRight": true,
                "position": {
                     "character": 36,
                     "line": 7
                }
           }]),
        );

        let actual_r = inlay_hints::on_inlay_hints(&mock_ctx, &request, inlay_hints_config);
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", serde_json::to_string(&actual_r));
        eprintln!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }
}
