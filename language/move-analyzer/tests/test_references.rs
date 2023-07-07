// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(test)]
mod tests {
    use lsp_server::{Connection, Request, Response};
    use move_analyzer::{
        context::{Context, FileDiags, MultiProject},
        references, symbols,
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
    fn test_on_references_request_001() {
        let (connection, _) = Connection::stdio();
        let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));

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
            "position": {
                "line": 7,
                "character": 16
            },
            "textDocument": {
                "uri": "file:///".to_string() + fpath.to_str().unwrap()
            },
        });
        let request = Request {
            id: "references_request_001".to_string().into(),
            method: String::from("textDocument/references"),
            params: params_json,
        };

        let expect_r = Response::new_ok(
            "go_to_def_request_001".to_string().into(),
            json!([
                {
                    "range": {
                            "end": {
                                "character": 26,
                                "line": 2
                            },
                            "start": {
                                "character": 11,
                                "line": 2
                            }
                    },
                    
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M2.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 57,
                                "line": 6
                            },
                            "start": {
                                "character": 42,
                                "line": 6
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M2.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 23,
                                "line": 7
                            },
                            "start": {
                                "character": 8,
                                "line": 7
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M2.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 39,
                                "line": 122
                            },
                            "start": {
                                "character": 24,
                                "line": 122
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M1.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 57,
                                "line": 122
                            },
                            "start": {
                                "character": 42,
                                "line": 122
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M1.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 56,
                                "line": 24
                            },
                            "start": {
                                "character": 41,
                                "line": 24
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M1.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
                    "range": {
                            "end": {
                                "character": 44,
                                "line": 126
                            },
                            "start": {
                                "character": 29,
                                "line": 126
                            }
                    },
                    "uri": ("file:///".to_string() + path_concat(
                                    std::env::current_dir().unwrap().as_path(),
                                    PathBuf::from("tests/symbols/sources/M1.move").as_path()).to_str().unwrap()
                            ).replace('\\', "/")
                },
                {
               "range": {
                    "end": {
                         "character": 50,
                         "line": 30
                    },
                    "start": {
                         "character": 35,
                         "line": 30
                    }
               },
               "uri": ("file:///".to_string() + path_concat(
                            std::env::current_dir().unwrap().as_path(),
                            PathBuf::from("tests/symbols/sources/M1.move").as_path()).to_str().unwrap()
                       ).replace('\\', "/")
          }
            ]),
        );

        let actual_r = references::on_references_request(&mut mock_ctx, &request);
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", serde_json::to_string(&actual_r));
        eprintln!("\n------------------------------\n");
        if let Some(serde_json::Value::Array(actual_r_result)) = actual_r.result {
            assert_eq!(actual_r_result.len(), 8);
            for actual_r_value in actual_r_result {
                let mut found_same_item = false;
                if let Some(serde_json::Value::Array(expect_r_result)) = expect_r.result.clone() {
                    for expect_r_value in expect_r_result {
                        if actual_r_value.eq(&expect_r_value) {
                            found_same_item = true;
                            break;
                        }
                    }
                }
                assert_eq!(found_same_item, true);
            }
            return;
        } 
    }
}
