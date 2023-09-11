// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    analyzer_handler::*,
    context::*,
    item::*,
    utils::{path_concat, FileRange},
};
use codespan::ByteIndex;
use lsp_server::*;
use lsp_types::*;
use move_model::{
    ast::{ExpData::*, Operation::*},
    model::{FunId, GlobalEnv, ModuleId, StructId},
};
use std::path::{Path, PathBuf};

/// Handles on_references_request of the language server.
pub fn on_references_request(context: &Context, request: &Request) -> lsp_server::Response {
    log::info!("on_go_to_def_request request = {:?}", request);
    let parameters = serde_json::from_value::<GotoDefinitionParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = parameters
        .text_document_position_params
        .text_document
        .uri
        .to_file_path()
        .unwrap();
    let loc = parameters.text_document_position_params.position;
    let line = loc.line;
    let col = loc.character;
    let fpath = path_concat(std::env::current_dir().unwrap().as_path(), fpath.as_path());
    eprintln!(
        "request is goto definition,fpath:{:?}  line:{} col:{}",
        fpath.as_path(),
        line,
        col,
    );

    let mut handler = Handler::new(fpath.clone(), line, col);
    let _ = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return Response {
                id: "".to_string().into(),
                result: Some(serde_json::json!({"msg": "No available project"})),
                error: None,
            };
        },
    }
    .run_visitor_for_file(&mut handler, &fpath);
    let locations = handler.to_locations();
    let r = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(GotoDefinitionResponse::Array(locations)).unwrap(),
    );
    let ret_response = r.clone();
    log::info!("\n\n------------------------------------ret_response = {:?}", ret_response);
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
    ret_response
}

pub(crate) struct Handler {
    /// The file we are looking for.
    pub(crate) filepath: PathBuf,
    pub(crate) line: u32,
    pub(crate) col: u32,
    // pub(crate) result: Option<FileRange>,

    pub(crate) mouse_span: codespan::Span,
    pub(crate) capture_items_span: Vec<codespan::Span>,
    pub(crate) result_candidates: Vec<FileRange>,
    pub(crate) result_ref_candidates: Vec<Vec<FileRange>>,
}

impl Handler {
    pub(crate) fn new(filepath: impl Into<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            filepath: filepath.into(),
            line,
            col,
            // result: None,
            mouse_span: Default::default(),
            capture_items_span: vec![],
            result_candidates: vec![],
            result_ref_candidates: vec![],
        }
    }

    fn to_locations(&mut self) -> Vec<Location> {
        let mut most_clost_item_idx: usize = 0;
        if let Some(item_idx) = find_smallest_length_index(&self.capture_items_span) {
            most_clost_item_idx = item_idx;
        }

        if self.result_ref_candidates.len() > 0 && 
           most_clost_item_idx < self.result_ref_candidates.len() {
            let mut ret = Vec::with_capacity(self.result_ref_candidates.len());
            for item in &self.result_ref_candidates[most_clost_item_idx] {
                ret.push(item.mk_location());
            }
            self.capture_items_span.clear();
            self.result_candidates.clear();
            self.result_ref_candidates.clear();
            return ret;
        }
        self.capture_items_span.clear();
        self.result_candidates.clear();
        self.result_ref_candidates.clear();
        vec![]
    }

    fn get_mouse_loc(&mut self, env: &GlobalEnv, target_fn_or_struct_loc: &move_model::model::Loc) {
        let mut mouse_line_first_col = move_model::model::Loc::new(
            target_fn_or_struct_loc.file_id(),
            codespan::Span::new(
                target_fn_or_struct_loc.span().start() + codespan::ByteOffset(1),
                target_fn_or_struct_loc.span().start() + codespan::ByteOffset(2),
            ),
        );
        let mut mouse_loc = env.get_location(&mouse_line_first_col).unwrap();
        // locate to self.line first column
        while mouse_loc.line.0 < self.line {
            mouse_line_first_col = move_model::model::Loc::new(
                target_fn_or_struct_loc.file_id(),
                codespan::Span::new(
                    mouse_line_first_col.span().start() + codespan::ByteOffset(1),
                    target_fn_or_struct_loc.span().end(),
                ),
            );
            mouse_loc = env.get_location(&mouse_line_first_col).unwrap();
        }
        // locate to self.line last column
        let mut mouse_line_last_col = move_model::model::Loc::new(
            target_fn_or_struct_loc.file_id(),
            codespan::Span::new(
                mouse_line_first_col.span().start() + codespan::ByteOffset(1),
                mouse_line_first_col.span().start() + codespan::ByteOffset(2),
            ),
        );
    
        mouse_loc = env.get_location(&mouse_line_last_col).unwrap();
        // locate to self.line first column
        while mouse_loc.column.0 < self.col {
            mouse_line_last_col = move_model::model::Loc::new(
                target_fn_or_struct_loc.file_id(),
                codespan::Span::new(
                    mouse_line_last_col.span().start() + codespan::ByteOffset(1),
                    target_fn_or_struct_loc.span().end(),
                ),
            );
            mouse_loc = env.get_location(&mouse_line_last_col).unwrap();
            // log::info!("lll >> loop: mouse_loc = {:?}", mouse_loc);
        }
    
        let mouse_source = env.get_source(&move_model::model::Loc::new(
            target_fn_or_struct_loc.file_id(),
            codespan::Span::new(
                mouse_line_first_col.span().start(),
                mouse_line_last_col.span().start(),
            ),
        ));
        log::info!("lll >> mouse_source = {:?}", mouse_source);
    
        self.mouse_span = codespan::Span::new(mouse_line_first_col.span().start(), mouse_line_last_col.span().start());
    }
    
    fn process_func(&mut self, env: &GlobalEnv, move_file_path: &Path) {
        log::info!("lll >> process_func =======================================\n\n");
        let mut found_target_module = false;
        let mut found_target_fun = false;
        let mut target_module_id = ModuleId::new(0);
        let mut target_fun_id = FunId::new(env.symbol_pool().make("name"));

        let mut move_file_str: &str = "null_move_file";
        if let Some(file_stem) = move_file_path.file_stem() {
            if let Some(file_stem_str) = file_stem.to_str() {
                move_file_str = file_stem_str;
            }
        }
        for module in env.get_target_modules() {
            if module.matches_name(move_file_str) {
                target_module_id = module.get_id();
                found_target_module = true;
                // log::info!("lll >> module_file_name = {:?}", module.get_full_name_str());
                break;
            }
        }

        if found_target_module {
            let target_module = env.get_module(target_module_id);
            for fun in target_module.get_functions() {
                let this_fun_loc = fun.get_loc();
                let (_, func_start_pos) = env.get_file_and_location(&this_fun_loc).unwrap();
                let (_, func_end_pos) = env
                    .get_file_and_location(&move_model::model::Loc::new(
                        this_fun_loc.file_id(),
                        codespan::Span::new(this_fun_loc.span().end(), this_fun_loc.span().end()),
                    ))
                    .unwrap();
                if func_start_pos.line.0 < self.line && self.line < func_end_pos.line.0 {
                    target_fun_id = fun.get_id();
                    found_target_fun = true;
                    break;
                }
                // log::info!("lll >> func_start_pos = {:?}, func_end_pos = {:?}", func_start_pos, func_end_pos);
            }
        }

        if !found_target_fun {
            return;
        }

        let target_module = env.get_module(target_module_id);
        let target_fun = target_module.get_function(target_fun_id);
        let target_fun_loc = target_fun.get_loc();
        self.get_mouse_loc(env, &target_fun_loc);

        if let Some(exp) = target_fun.get_def() {
            self.process_expr(env, exp);
        }
    }

    fn process_struct(&mut self, env: &GlobalEnv, move_file_path: &Path) {
        log::info!("lll >> process_struct =======================================\n\n");
        let mut found_target_module = false;
        let mut found_target_struct = false;
        let mut target_module_id = ModuleId::new(0);
        let mut target_struct_id = StructId::new(env.symbol_pool().make("name"));
    
        let mut move_file_str: &str = "null_move_file";
        if let Some(file_stem) = move_file_path.file_stem() {
            if let Some(file_stem_str) = file_stem.to_str() {
                move_file_str = file_stem_str;
            }
        }
        for module in env.get_target_modules() {
            if module.matches_name(move_file_str) {
                target_module_id = module.get_id();
                found_target_module = true;
                // log::info!("lll >> module_file_name = {:?}", module.get_full_name_str());
                break;
            }
        }
    
        if found_target_module {
            let target_module = env.get_module(target_module_id);
            for struct_env in target_module.get_structs() {
                let struct_loc = struct_env.get_loc();
                let (_, struct_start_pos) = env.get_file_and_location(&struct_loc).unwrap();
                let (_, struct_end_pos) = env
                    .get_file_and_location(&move_model::model::Loc::new(
                        struct_loc.file_id(),
                        codespan::Span::new(struct_loc.span().end(), struct_loc.span().end()),
                    ))
                    .unwrap();
                if struct_start_pos.line.0 < self.line && self.line < struct_end_pos.line.0 {
                    target_struct_id = struct_env.get_id();
                    found_target_struct = true;
                    break;
                }
                // log::info!("lll >> struct_start_pos = {:?}, struct_end_pos = {:?}", struct_start_pos, struct_end_pos);
            }
        }
    
        if !found_target_struct {
            return;
        }
    
        let target_module = env.get_module(target_module_id);
        let target_struct = target_module.get_struct(target_struct_id);
        let target_struct_loc = target_struct.get_loc();
        self.get_mouse_loc(env, &target_struct_loc);
    
        for field_env in target_struct.get_fields() {
            let field_name = field_env.get_name();
            let field_name_str = field_name.display(env.symbol_pool());
            log::info!("lll >> field_name = {}", field_name_str);
            let struct_source = env.get_source(&target_struct_loc);
            if let Ok(struct_str) = struct_source {
                if let Some(index) = struct_str.find(field_name_str.to_string().as_str()) {
                    let field_len = field_name_str.to_string().len();
                    let field_start = target_struct_loc.span().start()
                        + codespan::ByteOffset((index + field_len).try_into().unwrap());
                    // Assuming a relatively large distance
                    let field_end = field_start + codespan::ByteOffset((128).try_into().unwrap());
                    let field_loc = move_model::model::Loc::new(
                        target_struct_loc.file_id(),
                        codespan::Span::new(field_start, field_end),
                    );
                    let field_source = env.get_source(&field_loc);
                    // log::info!("lll >> field_source = {:?}", field_source);
                    if let Ok(atomic_field_str) = field_source {
                        if let Some(index) = atomic_field_str.find("\n".to_string().as_str()) {
                            let atomic_field_end =
                                field_start + codespan::ByteOffset(index.try_into().unwrap());
                            let atomic_field_loc = move_model::model::Loc::new(
                                target_struct_loc.file_id(),
                                codespan::Span::new(field_start, atomic_field_end),
                            );
                            let atomic_field_source = env.get_source(&atomic_field_loc);
                            // todo: should check mouse_last_col between in scope by atomic_field_loc
                            if atomic_field_loc.span().end() < self.mouse_span.end()
                                || atomic_field_loc.span().start()
                                    > self.mouse_span.end()
                            {
                                continue;
                            }
                            log::info!("lll >> atomic_field_source = {:?}", atomic_field_source);
                            let field_type = field_env.get_type();
                            self.process_type(env, &atomic_field_loc, &field_type);
                        }
                    }
                }
            }
        }
    }
    
    fn process_expr(
        &mut self,
        env: &GlobalEnv,
        exp: &move_model::ast::Exp,
    ) {
        log::info!("\n\nlll >> process_expr -------------------------\n");
        exp.visit(&mut |e| {
            match e {
                Call(..) => {
                    self.process_call(env, e);
                },
                _ => {},
            }
        });
        log::info!("\nlll << process_expr ^^^^^^^^^^^^^^^^^^^^^^^^^\n");
    }

    fn process_call(
        &mut self,
        env: &GlobalEnv,
        expdata: &move_model::ast::ExpData,
    )
    {
        if let Call(node_id, MoveFunction(mid, fid), _) = expdata {
            let this_call_loc = env.get_node_loc(*node_id);
            log::info!(
                "lll >> exp.visit this_call_loc = {:?}",
                env.get_location(&this_call_loc)
            );
            if this_call_loc.span().start() < self.mouse_span.end()
                && self.mouse_span.end() < this_call_loc.span().end()
            {
                let called_module = env.get_module(*mid);
                let called_fun = called_module.get_function(*fid);
                log::info!(
                    "lll >> get_called_functions = {:?}",
                    called_fun.get_full_name_str()
                );
                if let Some(calling_fns) = called_fun.get_calling_functions() {
                    let mut result_candidates: Vec<FileRange> = Vec::new();
                    for caller in calling_fns {
                        let f = env.get_function(caller);
                        log::info!(
                            "lll >> caller f = {:?}",
                            f.get_full_name_str()
                        );
                        let mut caller_fun_loc = f.get_loc();
                        // need locate called_fun.get_full_name_str() in f's body source
                        let f_source = env.get_source(&caller_fun_loc);
                        if let Ok(f_source_str) = f_source {
                            if let Some(index) = f_source_str.find(called_fun.get_name_str().as_str()) {
                                let target_len: usize = called_fun.get_name_str().len();
                                let start = caller_fun_loc.span().start()
                                    + codespan::ByteOffset(index.try_into().unwrap());
                                let end = start + codespan::ByteOffset((target_len).try_into().unwrap());
                                caller_fun_loc = move_model::model::Loc::new(
                                    caller_fun_loc.file_id(), codespan::Span::new(start, end));
                            }
                        }
                        let (caller_fun_file, caller_fun_line) =
                            env.get_file_and_location(&caller_fun_loc).unwrap();

                        let result = FileRange {
                            path: PathBuf::from(caller_fun_file),
                            line_start: caller_fun_line.line.0,
                            col_start: caller_fun_line.column.0,
                            line_end: caller_fun_line.line.0,
                            col_end: caller_fun_line.column.0
                                + called_fun.get_name_str().len() as u32,
                        };
                        result_candidates.push(result);
                    }
                    self.result_ref_candidates.push(result_candidates);
                    self.capture_items_span.push(this_call_loc.span());
                }
            }
        }
    }

    fn process_type(
        &mut self,
        env: &GlobalEnv,
        capture_items_loc: &move_model::model::Loc,
        ty: &move_model::ty::Type,
    ) {
        use move_model::ty::Type::*;
        match ty {
            Struct(mid, stid, ty_vec) => {
                let struct_from_module = env.get_module(*mid);
                let type_struct = struct_from_module.get_struct(*stid);
                log::info!("lll >> type_struct = {:?}", type_struct.get_full_name_str());
                let type_struct_loc = type_struct.get_loc();
                log::info!(
                    "lll >> type_struct_loc = {:?}",
                    env.get_file_and_location(&type_struct_loc)
                );
                let (type_struct_file, type_struct_pos) =
                    env.get_file_and_location(&type_struct_loc).unwrap();
                let result = FileRange {
                    path: PathBuf::from(type_struct_file).clone(),
                    line_start: type_struct_pos.line.0,
                    col_start: type_struct_pos.column.0,
                    line_end: type_struct_pos.line.0,
                    col_end: type_struct_pos.column.0
                        + type_struct.get_full_name_str().len() as u32,
                };
                self.capture_items_span.push((*capture_items_loc).span());
                self.result_candidates.push(result);

                // 20230907: add generic type parameter parse
                for ty in ty_vec {
                    if let Some(generic_struct_ty) = ty.get_struct(env) {
                        let generic_struct_ty_symbol = generic_struct_ty.0.get_name();
                        let capture_generic_ty_source = env.get_source(&capture_items_loc);
                        if let Ok(capture_generic_ty_str) = capture_generic_ty_source {
                            let generic_struct_ty_symbol_display = generic_struct_ty_symbol.display(env.symbol_pool());
                            if let Some(index) = capture_generic_ty_str.find(generic_struct_ty_symbol_display.to_string().as_str()) {
                                let capture_generic_ty_str_len = generic_struct_ty_symbol_display.to_string().len();
                                let capture_generic_ty_start =
                                    (*capture_items_loc).span().start() + codespan::ByteOffset(index.try_into().unwrap());
                                let capture_generic_ty_end =
                                    capture_generic_ty_start + codespan::ByteOffset(capture_generic_ty_str_len.try_into().unwrap());
                                let capture_generic_ty_loc = move_model::model::Loc::new(
                                    (*capture_items_loc).file_id(),
                                    codespan::Span::new(capture_generic_ty_start, capture_generic_ty_end),
                                );
                                log::info!("capture_generic_ty_str = {:?}", env.get_source(&capture_generic_ty_loc));
                                let capture_line = env.get_location(&capture_generic_ty_loc).unwrap();
                                let (generic_struct_belong_file, generic_struct_belong_pos) = env.get_file_and_location(&generic_struct_ty.0.get_loc()).unwrap();
                                if self.line.eq(&capture_line.line.0) && 
                                   capture_generic_ty_loc.span().start() <= self.mouse_span.end() &&
                                   self.mouse_span.end() <= capture_generic_ty_loc.span().end() {
                                    let result = FileRange {
                                        path: PathBuf::from(generic_struct_belong_file).clone(),
                                        line_start: generic_struct_belong_pos.line.0,
                                        col_start: generic_struct_belong_pos.column.0,
                                        line_end: generic_struct_belong_pos.line.0,
                                        col_end: generic_struct_belong_pos.column.0
                                            + capture_generic_ty_str_len as u32,
                                    };
                                    log::info!("capture_generic_ty result = {:?}", result.clone());
                                    self.capture_items_span.push(capture_generic_ty_loc.span());
                                    self.result_candidates.push(result);
                                }
                            }
                        }
                    }
                }
            },
            _ => {
                log::info!("lll >> type_var is default");
            },
        }
    }

    fn run_move_model_visitor_internal(
        &mut self,
        env: &GlobalEnv,
        move_file_path: &Path
    ) {
        self.process_func(env, move_file_path);
        self.process_struct(env, move_file_path);
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

pub fn find_smallest_length_index(spans: &Vec<codespan::Span>) -> Option<usize> {
    let mut smallest_length = i64::MAX;
    let mut smallest_index = None;

    for (index, span) in spans.iter().enumerate() {
        let length = span.end() - span.start();
        if length.0 < smallest_length {
            smallest_length = length.0;
            smallest_index = Some(index);
        }
    }

    smallest_index
}
