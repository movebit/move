// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    analyzer_handler::*,
    context::*,
    item::*,
    goto_definition_move_model,
    utils::{path_concat, FileRange},
};
use lsp_server::*;
use lsp_types::*;
use move_ir_types::location::Loc;
use std::{collections::HashSet, path::*};
use move_model::{
    ast::{ExpData::*, Operation::*, Value, Value::*},
    model::{FunId, GlobalEnv, ModuleId, StructId},
};

pub fn on_references_request(context: &mut Context, request: &Request) {
    log::info!("on_references_request request = {:?}", request);
    let parameters = serde_json::from_value::<ReferenceParams>(request.params.clone())
        .expect("could not deserialize references request");
    let fpath = parameters
        .text_document_position
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let loc = parameters.text_document_position.position;
    let line = loc.line;
    let col = loc.character;
    let include_declaration = parameters.context.include_declaration;
    let fpath = path_concat(std::env::current_dir().unwrap().as_path(), fpath.as_path());
    // first find definition.
    let mut goto_definition = goto_definition_move_model::Handler::new(fpath.clone(), line, col);
    let modules = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => return,
    };
    let _ = modules.run_visitor_for_file(&mut goto_definition, &fpath);
    let send_err = || {
        let err = format!("{:?}:{}:{} not found definition.", fpath.clone(), line, col);
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, err);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    // let def_loc = match goto_definition.result_loc {
    //     Some(x) => x,
    //     None => {
    //         send_err();
    //         return Response {
    //             id: "".to_string().into(),
    //             result: Some(serde_json::json!({"msg": "No definition found"})),
    //             error: None,
    //         };
    //     }
    // };
    // if let Some(x) = context.ref_caches.get(&(include_declaration, def_loc)) {
    //     let r = Response::new_ok(
    //         request.id.clone(),
    //         serde_json::to_value(Some(x.clone())).unwrap(),
    //     );
    //     let ret_response = r.clone();
    //     context
    //         .connection
    //         .sender
    //         .send(Message::Response(r))
    //         .unwrap();
    //     return ret_response;
    // }
    // let def_loc_range = match modules.convert_loc_range(&def_loc) {
    //     Some(x) => x,
    //     None => {
    //         send_err();
    //         return Response {
    //             id: "".to_string().into(),
    //             result: Some(serde_json::json!({"msg": "No location found"})),
    //             error: None,
    //         };
    //     }
    // };
    // let is_local = goto_definition
    //     .result_item_or_access
    //     .as_ref()
    //     .map(|x| x.is_local())
    //     .unwrap_or(false);
    // let modules = match context.projects.get_project(&fpath) {
    //     Some(x) => x,
    //     None => return Response {
    //         id: "".to_string().into(),
    //         result: Some(serde_json::json!({"msg": "No available project"})),
    //         error: None,
    //     },
    // };
    // let mut handle = Handler::new(def_loc, def_loc_range, include_declaration, is_local);
    // if is_local {
    //     let _ = modules.run_visitor_for_file(&mut handle, &fpath, false);
    // } else {
    //     modules.run_full_visitor(&mut handle);
    // }
    // let locations = handle.to_locations(modules);
    // let loc = Some(locations.clone());
    // if !is_local {
    //     // We only cache global items.
    //     context
    //         .ref_caches
    //         .set((include_declaration, def_loc), locations);
    // }
    // let r = Response::new_ok(request.id.clone(), serde_json::to_value(loc).unwrap());
    // let ret_response = r.clone();
    // context
    //     .connection
    //     .sender
    //     .send(Message::Response(r))
    //     .unwrap();
    // ret_response
}

struct Handler {
    def_loc: Loc,
    def_loc_range: FileRange,
    include_declaration: bool,
    refs: HashSet<Loc>,
    is_local: bool,

    pub(crate) filepath: PathBuf,
    pub(crate) line: u32,
    pub(crate) col: u32,
    pub(crate) result: Option<FileRange>,

    pub(crate) mouse_span: codespan::Span,
    pub(crate) capture_items_span: Vec<codespan::Span>,
    pub(crate) result_candidates: Vec<FileRange>,
}

impl Handler {
    pub(crate) fn new(
        def_loc: Loc,
        def_loc_range: FileRange,
        include_declaration: bool,
        is_local: bool,
        filepath: impl Into<PathBuf>,
        line: u32,
        col: u32
    ) -> Self { 
        Self {
            def_loc,
            include_declaration,
            refs: Default::default(),
            def_loc_range,
            is_local,
            filepath: filepath.into(),
            line,
            col,
            result: None,
            mouse_span: Default::default(),
            capture_items_span: vec![],
            result_candidates: vec![],
        }
    }

    fn run_move_model_visitor_internal(
        &mut self,
        env: &GlobalEnv,
        move_file_path: &Path
    ) {

    }
}
impl ItemOrAccessHandler for Handler {
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        item_or_access: &ItemOrAccess,
    ) {
    }

    fn finished(&self) -> bool {
        false
    }

    fn handle_project_env(
        &mut self,
        _services: &dyn HandleItemService,
        env: &GlobalEnv,
        move_file_path: &Path,
    ) {
        self.run_move_model_visitor_internal(env, move_file_path);
    }
}

impl std::fmt::Display for Handler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "goto_definition,file:{:?} line:{} col:{}",
            self.filepath, self.line, self.col
        )
    }
}
