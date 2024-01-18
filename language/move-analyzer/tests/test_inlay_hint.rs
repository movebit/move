// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[cfg(test)]
mod tests {
    use lsp_server::{Connection, Request, Response};
    use aptos_move_analyzer::{
        context::{Context, FileDiags},
        multiproject::MultiProject,
        inlay_hints,
        utils::*,
    };

    use serde_json::json;
    use std::{
        path::PathBuf,
        time::Duration,
    };

    fn prepare_project(context: &mut Context, fpath: PathBuf) {
        eprintln!("<UT> -- prepare_project -- fpath = {:?}", fpath.to_str());
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            },
        };
        match context.projects.get_project(&fpath) {
            Some(_) => {
                if let Ok(_x) = std::fs::read_to_string(fpath.as_path()) {
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

    #[test]
    fn test_inlay_hint_001() {
        let (connection, _) = Connection::stdio();
        let mut context = Context {
            projects: MultiProject::new(),
            connection,
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/v1-core/Swap/sources/swap.move").as_path(),
        );
        prepare_project(&mut context, fpath.clone());

        let params_json = json!({
            "context": {
                "includeDeclaration": true
            },
            "range": [{
                "line": 272,
                "character": 0
            },
            {
                "line": 290,
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
                "label": [{
                    "command": {
                        "arguments": [{
                            "fpath": fpath.to_str().unwrap(),
                            "range": {
                                "end": {
                                    "character": 76,
                                    "line": 279
                                },
                                "start": {
                                    "character": 76,
                                    "line": 279
                                }
                            }
                        }],
                        "command": "aptos-move-analyzer.goto_definition",
                        "title": "Goto Definition"
                    },
                    "value": ": bool"
                }],
                "paddingLeft": true,
                "paddingRight": true,
                "position": {
                    "character": 76,
                    "line": 279
                }
            }, {
                "kind": 1,
                "label": [{
                    "command": {
                        "arguments": [{
                            "fpath": fpath.to_str().unwrap(),
                            "range": {
                                "end": {
                                    "character": 75,
                                    "line": 274
                                },
                                "start": {
                                    "character": 75,
                                    "line": 274
                                }
                            }
                        }],
                        "command": "aptos-move-analyzer.goto_definition",
                        "title": "Goto Definition"
                    },
                    "value": ": bool"
                }],
                "paddingLeft": true,
                "paddingRight": true,
                "position": {
                    "character": 75,
                    "line": 274
                }
            }, {
                "kind": 1,
                "label": [{
                    "command": {
                        "arguments": [{
                            "fpath": fpath.to_str().unwrap(),
                            "range": {
                                "end": {
                                    "character": 87,
                                    "line": 284
                                },
                                "start": {
                                    "character": 87,
                                    "line": 284
                                }
                            }
                        }],
                        "command": "aptos-move-analyzer.goto_definition",
                        "title": "Goto Definition"
                    },
                    "value": ": account::SignerCapability"
                }],
                "paddingLeft": true,
                "paddingRight": true,
                "position": {
                    "character": 87,
                    "line": 284
                }
            }, {
                "kind": 1,
                "label": [{
                    "command": {
                        "arguments": [{
                            "fpath": fpath.to_str().unwrap(),
                            "range": {
                                "end": {
                                    "character": 57,
                                    "line": 285
                                },
                                "start": {
                                    "character": 57,
                                    "line": 285
                                }
                            }
                        }],
                        "command": "aptos-move-analyzer.goto_definition",
                        "title": "Goto Definition"
                    },
                    "value": ": &account::SignerCapability"
                }],
                "paddingLeft": true,
                "paddingRight": true,
                "position": {
                    "character": 57,
                    "line": 285
                }
            }]),
        );

        let actual_r = inlay_hints::on_inlay_hints(&context, &request);
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", serde_json::to_string(&actual_r));
        eprintln!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }

    #[test]
    fn test_inlay_hint_002() {
        let (connection, _) = Connection::stdio();
        let mut context = Context {
            projects: MultiProject::new(),
            connection,
            diag_version: FileDiags::new(),
        };

        let fpath = path_concat(
            std::env::current_dir().unwrap().as_path(),
            PathBuf::from("tests/v1-core/TestCoin/sources/testCoins.move").as_path(),
        );
        prepare_project(&mut context, fpath.clone());

        let params_json = json!({
            "context": {
                "includeDeclaration": true
            },
            "range": [{
                "line": 21,
                "character": 0
            },
            {
                "line": 45,
                "character": 0
            }],
            "textDocument": {
                "uri": "file:///".to_string() + fpath.to_str().unwrap()
            },
        });
        let request: Request = Request {
            id: "inlay_hints_002".to_string().into(),
            method: String::from("textDocument/inlayHint"),
            params: params_json,
        };

        let expect_r: Response = Response::new_ok(
            "inlay_hints_002".to_string().into(),
            json!(
                [{
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 39,
                                        "line": 24
                                    },
                                    "start": {
                                        "character": 39,
                                        "line": 24
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 39,
                        "line": 24
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 40,
                                        "line": 27
                                    },
                                    "start": {
                                        "character": 40,
                                        "line": 27
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 40,
                        "line": 27
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 21,
                                        "line": 29
                                    },
                                    "start": {
                                        "character": 21,
                                        "line": 29
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 21,
                        "line": 29
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 46,
                                        "line": 29
                                    },
                                    "start": {
                                        "character": 46,
                                        "line": 29
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::MintCapability<TestCoinsV1::BTC>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 46,
                        "line": 29
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 61,
                                        "line": 29
                                    },
                                    "start": {
                                        "character": 61,
                                        "line": 29
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::FreezeCapability<TestCoinsV1::BTC>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 61,
                        "line": 29
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 74,
                                        "line": 29
                                    },
                                    "start": {
                                        "character": 74,
                                        "line": 29
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::BurnCapability<TestCoinsV1::BTC>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 74,
                        "line": 29
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 21,
                                        "line": 30
                                    },
                                    "start": {
                                        "character": 21,
                                        "line": 30
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 21,
                        "line": 30
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 48,
                                        "line": 30
                                    },
                                    "start": {
                                        "character": 48,
                                        "line": 30
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::MintCapability<TestCoinsV1::USDT>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 48,
                        "line": 30
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 64,
                                        "line": 30
                                    },
                                    "start": {
                                        "character": 64,
                                        "line": 30
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::FreezeCapability<TestCoinsV1::USDT>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 64,
                        "line": 30
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 78,
                                        "line": 30
                                    },
                                    "start": {
                                        "character": 78,
                                        "line": 30
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": coin::BurnCapability<TestCoinsV1::USDT>"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 78,
                        "line": 30
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 32,
                                        "line": 31
                                    },
                                    "start": {
                                        "character": 32,
                                        "line": 31
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 32,
                        "line": 31
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 53,
                                        "line": 36
                                    },
                                    "start": {
                                        "character": 53,
                                        "line": 36
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 53,
                        "line": 36
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 58,
                                        "line": 37
                                    },
                                    "start": {
                                        "character": 58,
                                        "line": 37
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": address"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 58,
                        "line": 37
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 39,
                                        "line": 38
                                    },
                                    "start": {
                                        "character": 39,
                                        "line": 38
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 39,
                        "line": 38
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 59,
                                        "line": 40
                                    },
                                    "start": {
                                        "character": 59,
                                        "line": 40
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": address"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 59,
                        "line": 40
                    }
                }, {
                    "kind": 1,
                    "label": [{
                        "command": {
                            "arguments": [{
                                "fpath": fpath.to_str().unwrap(),
                                "range": {
                                    "end": {
                                        "character": 40,
                                        "line": 41
                                    },
                                    "start": {
                                        "character": 40,
                                        "line": 41
                                    }
                                }
                            }],
                            "command": "aptos-move-analyzer.goto_definition",
                            "title": "Goto Definition"
                        },
                        "value": ": &signer"
                    }],
                    "paddingLeft": true,
                    "paddingRight": true,
                    "position": {
                        "character": 40,
                        "line": 41
                    }
                }]
            ),
        );

        let actual_r = inlay_hints::on_inlay_hints(&context, &request);
        std::thread::sleep(Duration::new(1, 0));
        eprintln!("\n------------------------------\n");
        eprintln!("actual_r = {:?}", serde_json::to_string(&actual_r));
        eprintln!("\n------------------------------\n");
        assert_eq!(actual_r.result, expect_r.result);
    }



}
