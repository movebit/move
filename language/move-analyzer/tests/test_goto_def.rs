#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::{Position, TextDocumentIdentifier, Url};
    use serde_json::Value;

    #[test]
    fn test_on_go_to_def_request() {
        // 构造函数参数
        let uri = Url::from_directory_path("/path/to/file").unwrap();
        let doc_id = TextDocumentIdentifier::new(uri);
        let pos = Position::new(10, 5);
        let params = Value::from(GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: doc_id.clone(),
                position: pos.clone(),
            },
            work_done_progress_params: Default::default(),
        });

        // 构造 Mock
        let proj_path = std::env::current_dir().unwrap().join("/path/to");
        let mut mock_proj = Project::new(proj_path.clone());
        mock_proj.add_source_file("/path/to/file", "foo".to_string()).unwrap();
        let mut mock_ctx = Context::default();
        mock_ctx.projects.add_project(mock_proj).unwrap();

        // 调用被测函数
        on_go_to_def_request(&mock_ctx, &Request::new("42", "textDocument/definition", params));

        // 检查结果
        let expected_r = Response::new_ok(
            "42".to_string(),
            json!({
                "result": [
                    Location {
                        uri: uri.clone(),
                        range: Range {
                            start: pos.clone(),
                            end: pos.clone(),
                        },
                    },
                ],
                "jsonrpc": "2.0",
            }),
        );
        let actual_r = mock_ctx.connection.receiver.try_recv().unwrap();
        assert_eq!(actual_r, Message::Response(expected_r));
    }
}
