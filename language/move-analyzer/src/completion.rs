// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
use crate::{
    context::*,
    utils::path_concat,
};
use lsp_server::*;
use move_model::model::{ModuleId, GlobalEnv};
use std::vec;
use lsp_server::Request;
use lsp_types::{CompletionItem, CompletionItemKind, CompletionParams, Position};
use move_command_line_common::files::FileHash;
use move_compiler::parser::{
    keywords::{BUILTINS, CONTEXTUAL_KEYWORDS, KEYWORDS, PRIMITIVE_TYPES},
    lexer::{Lexer, Tok},
};
use std::{collections::HashSet, path::PathBuf};

/// Constructs an `lsp_types::CompletionItem` with the given `label` and `kind`.
fn completion_item(label: &str, kind: CompletionItemKind) -> CompletionItem {
    CompletionItem {
        label: label.to_owned(),
        kind: Some(kind),
        ..Default::default()
    }
}

/// Return a list of completion items corresponding to each one of Move's keywords.
///
/// Currently, this does not filter keywords out based on whether they are valid at the completion
/// request's cursor position, but in the future it ought to. For example, this function returns
/// all specification language keywords, but in the future it should be modified to only do so
/// within a spec block.
fn keywords() -> Vec<CompletionItem> {
    KEYWORDS
        .iter()
        .chain(CONTEXTUAL_KEYWORDS.iter())
        .chain(PRIMITIVE_TYPES.iter())
        .map(|label| {
            let kind = if label == &"copy" || label == &"move" {
                CompletionItemKind::OPERATOR
            } else {
                CompletionItemKind::KEYWORD
            };
            completion_item(label, kind)
        })
        .collect()
}

/// Return a list of completion items of Move's primitive types
fn primitive_types() -> Vec<CompletionItem> {
    PRIMITIVE_TYPES
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::KEYWORD))
        .collect()
}

/// Return a list of completion items corresponding to each one of Move's builtin functions.
fn builtins() -> Vec<CompletionItem> {
    BUILTINS
        .iter()
        .map(|label| completion_item(label, CompletionItemKind::FUNCTION))
        .collect()
}

/// Lexes the Move source file at the given path and returns a list of completion items
/// corresponding to the non-keyword identifiers therein.
///
/// Currently, this does not perform semantic analysis to determine whether the identifiers
/// returned are valid at the request's cursor position. However, this list of identifiers is akin
/// to what editors like Visual Studio Code would provide as completion items if this language
/// server did not initialize with a response indicating it's capable of providing completions. In
/// the future, the server should be modified to return semantically valid completion items, not
/// simple textual suggestions.
fn identifiers(buffer: &str, env: &GlobalEnv, move_file_path: &PathBuf) -> Vec<CompletionItem> {
    let mut lexer = Lexer::new(buffer, FileHash::new(buffer));
    if lexer.advance().is_err() {
        return vec![];
    }

    let mut ids = HashSet::new();
    while lexer.peek() != Tok::EOF {
        // Some tokens, such as "phantom", are contextual keywords that are only reserved in
        // certain contexts. Since for now this language server doesn't analyze semantic context,
        // tokens such as "phantom" are always present in keyword suggestions. To avoid displaying
        // these keywords to the user twice in the case that the token "phantom" is present in the
        // source program (once as a keyword, and once as an identifier), we filter out any
        // identifier token that has the same text as a keyword.
        if lexer.peek() == Tok::Identifier && !KEYWORDS.contains(&lexer.content()) {
            // The completion item kind "text" indicates the item is not based on any semantic
            // context of the request cursor's position.
            ids.insert(lexer.content());
        }
        if lexer.advance().is_err() {
            break;
        }
    }

    let mut target_module_id = ModuleId::new(0);
    if crate::utils::get_target_module(env, move_file_path, &mut target_module_id) {
        let target_module = env.get_module(target_module_id);
        // The completion item kind "text" indicates that the item is based on simple textual matching,
        // not any deeper semantic analysis.
        return ids.iter()
            .map(|label| {
                for fun in target_module.get_functions() {
                    let fun_name_str = fun.get_name_str();
                    if fun_name_str.contains(label) {
                        return completion_item(label, CompletionItemKind::FUNCTION);
                    }
                    // log::info!("lll >> func_start_pos = {:?}, func_end_pos = {:?}", func_start_pos, func_end_pos);
                }
                completion_item(label, CompletionItemKind::TEXT)
            })
            .collect::<Vec<_>>();
    }
    vec![]
}

/// Returns the token corresponding to the "trigger character" that precedes the user's cursor,
/// if it is one of `.`, `:`, or `::`. Otherwise, returns `None`.
fn get_cursor_token(buffer: &str, position: &Position) -> Option<Tok> {
    // If the cursor is at the start of a new line, it cannot be preceded by a trigger character.
    if position.character == 0 {
        return None;
    }

    let line = match buffer.lines().nth(position.line as usize) {
        Some(line) => line,
        None => return None, // Our buffer does not contain the line, and so must be out of date.
    };
    match line.chars().nth(position.character as usize - 1) {
        Some('.') => Some(Tok::Period),
        Some(':') => {
            if position.character > 1
                && line.chars().nth(position.character as usize - 2) == Some(':')
            {
                Some(Tok::ColonColon)
            } else {
                Some(Tok::Colon)
            }
        }
        _ => None,
    }
}

/// Handles on_completion_request of the language server.
pub fn on_completion_request(context: &Context, request: &Request) -> lsp_server::Response {
    log::info!("on_completion_request request = {:?}", request);
    let parameters = serde_json::from_value::<CompletionParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = parameters
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let loc = parameters.text_document_position.position;
    let line = loc.line;
    let col = loc.character;
    let fpath = path_concat(std::env::current_dir().unwrap().as_path(), fpath.as_path());
    eprintln!(
        "on_completion_request, fpath:{:?} line:{} col:{}",
        fpath.as_path(),
        line,
        col,
    );

    let current_project = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return Response {
                id: "".to_string().into(),
                result: Some(serde_json::json!({"msg": "No available project"})),
                error: None,
            };
        },
    };
   
    let buffer_str = current_project.current_modifing_file_content.as_str();
    let buffer = Some(buffer_str);
    // The completion items we provide depend upon where the user's cursor is positioned.
    let cursor =
    buffer.and_then(|buf| get_cursor_token(buf, &parameters.text_document_position.position));

    let mut items = vec![];
    match cursor {
        Some(Tok::Colon) => {
            items.extend_from_slice(&primitive_types());
        }
        Some(Tok::Period) | Some(Tok::ColonColon) => {
            // `.` or `::` must be followed by identifiers, which are added to the completion items
            // below.
        }
        _ => {
            // If the user's cursor is positioned anywhere other than following a `.`, `:`, or `::`,
            // offer them Move's keywords, operators, and builtins as completion items.
            items.extend_from_slice(&keywords());
            items.extend_from_slice(&builtins());
        }
    }

    if let Some(buffer) = &buffer {
        let identifiers = identifiers(buffer, &current_project.global_env, &fpath);
        log::info!("identifiers = {:?}", identifiers);
        items.extend_from_slice(&identifiers);
    }

    let result = serde_json::to_value(items).expect("could not serialize completion response");
    eprintln!("about to send completion response");
    let response = lsp_server::Response::new_ok(request.id.clone(), result);
    let ret_response = response.clone();
    // log::info!("on complete------------------------------------> \n{:?}", ret_response);
    if let Err(err) = context
        .connection
        .sender
        .send(lsp_server::Message::Response(response))
    {
        eprintln!("could not send completion response: {:?}", err);
    }
    log::info!("<------------------------------------ \n");
    return ret_response;
}
