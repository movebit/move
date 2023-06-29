#[cfg(test)]
mod tests {
    use crossbeam::channel::select;
    use lsp_server::{Connection, Message, Request, Response};
    use move_command_line_common::files::FileHash;
    use move_compiler::shared::*;
    use std::{
        path::PathBuf,
        sync::{Arc, Mutex},
    };
    use move_analyzer::{
        context::{Context, FileDiags, MultiProject},
        goto_definition,
        utils::*,
        symbols,
        vfs::VirtualFileSystem,
    };
    use serde_json::json;

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
            .update(fpath.clone(), content);
    }

    #[test]
    fn test_on_go_to_def_request() {
        let (connection, _) = Connection::stdio();
        let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));
        
        let mut mock_ctx = Context {
            projects: MultiProject::new(),
            connection,
            files: VirtualFileSystem::default(),
            symbols: symbols.clone(),
            ref_caches: Default::default(),
            diag_version: FileDiags::new(),
        };

        let fpath = PathBuf::from("d:\\workspace\\private_code_with_custom\\suiswap-audit\\sources\\pool.move");
        let (mani, _) = match discover_manifest_and_kind(&fpath) {
            Some(x) => x,
            None => {
                log::error!("not move project.");
                return;
            }
        };
        match mock_ctx.projects.get_project(&fpath) {
            Some(_) => {
                match std::fs::read_to_string(fpath.as_path()) {
                    Ok(x) => {
                        update_defs(&mut mock_ctx, fpath.clone(), x.as_str());
                    }
                    Err(_) => {}
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
            "position": {
                "line": 1404,
                "character": 29
            },
            "textDocument": {
                "uri": "file:///d%3A/workspace/private_code_with_custom/suiswap-audit/sources/pool.move"
            },
        });
        let request = Request {
            id: "001".to_string().into(),
            method: String::from("textDocument/definition"),
            params: params_json,
        };

        goto_definition::on_go_to_def_request(&mut mock_ctx, &request);

        // 检查结果
        // loop {
        //     select! {
        //         recv(mock_ctx.connection.receiver) -> message => {
        //             match message {
        //                 Ok(Message::Response(response)) => {
        //                     eprintln!("IDE message response: {:?}", response);
        //                 },
        //                 Err(error) =>  {
        //                     eprintln!("IDE message error: {:?}", error);
        //                 },
        //                 _ => {}
        //             }
        //         }
        //     };
        // }
    
        // let expected_r = Response::new_ok(
        //     "001".to_string().into(),
        //     json!({
        //         "jsonrpc":"2.0",
        //         "id":"001",
        //         "result":[{
        //             "range":{
        //                 "end":{
        //                     "character":22,
        //                     "line":44
        //                 },
        //                 "start":{
        //                     "character":15,
        //                     "line":44
        //                 }
        //             },
        //             "uri":"file:///D:/workspace/private_code_with_custom/suiswap-audit/sources/vpt.move"
        //         }]
        //     }),
        // );

        // match mock_ctx.connection.receiver.try_recv() {
        //     Ok(message) => println!("Received message: {:?}", message),
        //     Err(_) => {
        //         println!("No message received");
        //     },
        // }
        // log::info!("actual_r = {:?}", actual_r);

        // eprintln!("\n------------------------------\n");
        // eprintln!("expected_r = {:?}", expected_r);
        // eprintln!("\n------------------------------\n");
        // assert_eq!(actual_r, Message::Response(expected_r));
    }
}
