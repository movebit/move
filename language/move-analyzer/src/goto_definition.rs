// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    analyzer_handler::*,
    context::*,
    utils::{path_concat, FileRange},
};
use lsp_server::*;
use lsp_types::*;
use move_ir_types::location::sp;
use move_model::{
    ast::{ExpData::*, Operation::*, Value, Value::*, Spec, SpecBlockInfo, SpecBlockTarget},
    model::{FunId, SpecFunId, GlobalEnv, ModuleId, FunctionEnv, StructId},
};
use std::path::{Path, PathBuf};
use itertools::Itertools;

/// Handles go-to-def request of the language server.
pub fn on_go_to_def_request(context: &Context, request: &Request) -> lsp_server::Response {
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
    .run_visitor_for_file(&mut handler, &fpath, String::default());
    let locations = handler.to_locations();
    let r = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(GotoDefinitionResponse::Array(locations)).unwrap(),
    );
    let ret_response = r.clone();
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
    pub(crate) result: Option<FileRange>,

    pub(crate) mouse_span: codespan::Span,
    pub(crate) capture_items_span: Vec<codespan::Span>,
    pub(crate) result_candidates: Vec<FileRange>,
    pub(crate) target_module_id: ModuleId,
}

impl Handler {
    pub(crate) fn new(filepath: impl Into<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            filepath: filepath.into(),
            line,
            col,
            result: None,
            mouse_span: Default::default(),
            capture_items_span: vec![],
            result_candidates: vec![],
            target_module_id: ModuleId::new(0),
        }
    }

    fn check_move_model_loc_contains_mouse_pos(&self, env: &GlobalEnv, loc: &move_model::model::Loc) -> bool {
        if let Some(obj_first_col) = env.get_location(&
            move_model::model::Loc::new(
                loc.file_id(),
                codespan::Span::new(
                    loc.span().start(),
                    loc.span().start() + codespan::ByteOffset(1),
                )
            )
        ) {
            if let Some(obj_last_col) = env.get_location(&
                move_model::model::Loc::new(
                    loc.file_id(),
                    codespan::Span::new(
                        loc.span().end(),
                        loc.span().end() + codespan::ByteOffset(1),
                    )
                )
            ) {
                if u32::from(obj_first_col.line) == self.line && 
                   u32::from(obj_first_col.column) < self.col && 
                   self.col < u32::from(obj_last_col.column) {
                    return true;
                }
            }
        }
        false
    }

    fn to_locations(&mut self) -> Vec<Location> {
        // log::info!("result --> self.capture_items_span = {:?}", self.capture_items_span);
        // log::info!("result --> self.result_candidates = {:?}\n\n", self.result_candidates);
        if let Some(most_clost_item_idx) = find_smallest_length_index(&self.capture_items_span) {
            if most_clost_item_idx < self.result_candidates.len() {
                self.result = Some(self.result_candidates[most_clost_item_idx].clone());
            }
        }

        let mut ret = Vec::with_capacity(2);
        if let Some(x) = self.result.as_ref() {
            ret.push(x.mk_location());
        }
        self.capture_items_span.clear();
        self.result_candidates.clear();
        ret
    }

    fn get_mouse_loc(&mut self, env: &GlobalEnv, target_fn_or_struct_loc: &move_model::model::Loc) {
        let mut mouse_line_first_col = move_model::model::Loc::new(
            target_fn_or_struct_loc.file_id(),
            codespan::Span::new(
                target_fn_or_struct_loc.span().start(),
                target_fn_or_struct_loc.span().start() + codespan::ByteOffset(1),
            ),
        );
        let mut mouse_loc = env.get_location(&mouse_line_first_col).unwrap();
        // locate to self.line first column
        while mouse_loc.line.0 < self.line {
            mouse_line_first_col = move_model::model::Loc::new(
                target_fn_or_struct_loc.file_id(),
                codespan::Span::new(
                    mouse_line_first_col.span().start() + codespan::ByteOffset(1),
                    mouse_line_first_col.span().start() + codespan::ByteOffset(2),
                ),
            );
            mouse_loc = env.get_location(&mouse_line_first_col).unwrap();
        }
        // locate to self.line last column
        let mut mouse_line_last_col = move_model::model::Loc::new(
            target_fn_or_struct_loc.file_id(),
            codespan::Span::new(
                mouse_line_first_col.span().end(),
                mouse_line_first_col.span().end() + codespan::ByteOffset(1),
            ),
        );
    
        mouse_loc = env.get_location(&mouse_line_last_col).unwrap();
        // locate to self.line first column
        while mouse_loc.column.0 < self.col && mouse_loc.line.0 == self.line {
            mouse_line_last_col = move_model::model::Loc::new(
                target_fn_or_struct_loc.file_id(),
                codespan::Span::new(
                    mouse_line_last_col.span().start() + codespan::ByteOffset(1),
                    mouse_line_last_col.span().start() + codespan::ByteOffset(2),
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

    fn process_use_decl(&mut self, env: &GlobalEnv) {
        log::info!("lll >> process_use_decl =======================================\n\n");
        let mut target_module = env.get_module(self.target_module_id);
        let spool = env.symbol_pool();
        let mut ref_module = String::default();
        let mut target_stct_or_fn = String::default();
        let mut found_target_stct_or_fn = false;
        let mut capture_items_loc = move_model::model::Loc::default();
        for use_decl in target_module.get_use_decls() {
            if let Some(use_pos) = env.get_location(&use_decl.loc) {
                if u32::from(use_pos.line) != self.line {
                    continue;
                }
            }
            // log::info!("use_decl.loc = {:?}, use_decl.loc.len = {:?}", 
            //     use_decl.loc, use_decl.loc.span().end() - use_decl.loc.span().start());
            if !use_decl.members.is_empty() {
                for (member_loc, name, alias_name) in use_decl.members.clone().into_iter() {
                    log::info!("member_loc = {:?} ---", env.get_location(&member_loc));
                    if self.check_move_model_loc_contains_mouse_pos(env, &member_loc) {
                        log::info!("find use symbol = {}", name.display(spool));
                        target_stct_or_fn = name.display(spool).to_string();
                        found_target_stct_or_fn = true;
                        ref_module = use_decl.module_name.display_full(env).to_string();
                        capture_items_loc = member_loc;
                        break;
                    }
                }
            }
            if found_target_stct_or_fn {
                break;
            }
        }

        if !found_target_stct_or_fn {
            return;
        }

        for module in env.get_modules() {
            let module_name = module.get_name().display(env).to_string();
            if ref_module.contains(&module_name) {
                target_module = module;
                break;
            }
        }
        for stct in target_module.get_structs() {
            if stct.get_full_name_str().contains(&target_stct_or_fn) {
                log::info!("stct.get_full_name_str() = {:?}", stct.get_full_name_str());
                let (type_struct_file, type_struct_pos) =
                    env.get_file_and_location(&stct.get_loc()).unwrap();
                let result = FileRange {
                    path: PathBuf::from(type_struct_file).clone(),
                    line_start: type_struct_pos.line.0,
                    col_start: type_struct_pos.column.0,
                    line_end: type_struct_pos.line.0,
                    col_end: type_struct_pos.column.0,
                };
                self.capture_items_span.push(capture_items_loc.span());
                self.result_candidates.push(result);
                return;
            }
        }
        for func in target_module.get_functions() {
            if func.get_name_str().contains(&target_stct_or_fn) {
                log::info!("func.get_name_str() = {:?}", func.get_name_str());
                let (use_fun_file, use_fun_line) =
                    env.get_file_and_location(&func.get_loc()).unwrap();
                let path_buf = PathBuf::from(use_fun_file);
                let result = FileRange {
                    path: path_buf,
                    line_start: use_fun_line.line.0,
                    col_start: use_fun_line.column.0,
                    line_end: use_fun_line.line.0,
                    col_end: use_fun_line.column.0,
                };
                self.capture_items_span.push(capture_items_loc.span());
                self.result_candidates.push(result);
                return;
            }
        }
    }

    fn process_func(&mut self, env: &GlobalEnv) {
        log::info!("lll >> process_func =======================================\n\n");
        let mut found_target_fun = false;
        let mut target_fun_id = FunId::new(env.symbol_pool().make("name"));

        let target_module = env.get_module(self.target_module_id);
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

        if !found_target_fun {
            return;
        }

        let target_module = env.get_module(self.target_module_id);
        let target_fun = target_module.get_function(target_fun_id);
        let target_fun_loc = target_fun.get_loc();
        self.get_mouse_loc(env, &target_fun_loc);

        if let Some(exp) = target_fun.get_def() {
            self.process_expr(env, exp);
        }
    }

    fn process_spec_file(&mut self, env: &GlobalEnv) {
        log::info!("lll >> process_spec_file =======================================");
        let mut found_target_fun = false;
        let mut target_fun_id = FunId::new(env.symbol_pool().make("name"));
        let target_module = env.get_module(self.target_module_id);
        let mut spec_fn_span_loc = target_module.get_loc();

        for spec_block_info in target_module.get_spec_block_infos() {
            if let SpecBlockTarget::Function(_, fun_id) = spec_block_info.target {
                // log::info!("lll >> spec_block_info spec_source = {:?}", env.get_source(&spec_block_info.loc));
                let mut span_first_col = move_model::model::Loc::new(
                    spec_block_info.loc.file_id(),
                    codespan::Span::new(
                        spec_block_info.loc.span().start(),
                        spec_block_info.loc.span().start() + codespan::ByteOffset(1),
                    ),
                );
                let mut span_last_col = move_model::model::Loc::new(
                    spec_block_info.loc.file_id(),
                    codespan::Span::new(
                        spec_block_info.loc.span().end(),
                        spec_block_info.loc.span().end() + codespan::ByteOffset(1),
                    )
                );
    
                if let Some(s_loc) = env.get_location(&span_first_col) {
                    if let Some(e_loc) = env.get_location(&span_last_col) {
                        if u32::from(s_loc.line) <= self.line && self.line <= u32::from(e_loc.line) {
                            target_fun_id = fun_id;
                            found_target_fun = true;
                            spec_fn_span_loc = spec_block_info.loc.clone();
                            break;
                        }
                    }
                }
            }
        }

        if !found_target_fun {
            log::info!("<< not found_target_spec_fun");
            return;
        }

        let target_fn = target_module.get_function(target_fun_id);
        let target_fn_spec = target_fn.get_spec();
        log::info!("target_fun's spec = {}",
            env.display(&*target_fn_spec));
        self.get_mouse_loc(env, &spec_fn_span_loc);
        for cond in target_fn_spec.conditions.clone() {
            for exp in cond.all_exps() {
                self.process_expr(env, &exp);
            }
        }
    }

    fn process_struct(&mut self, env: &GlobalEnv) {
        log::info!("lll >> process_struct =======================================\n\n");
        let mut found_target_struct = false;
        let mut target_struct_id = StructId::new(env.symbol_pool().make("name"));
        let target_module = env.get_module(self.target_module_id);
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
        if !found_target_struct {
            return;
        }
    
        let target_module = env.get_module(self.target_module_id);
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
                Value(node_id, v) => {
                    // Const variable
                    let value_loc = env.get_node_loc(*node_id);
                    if self.check_move_model_loc_contains_mouse_pos(env, &value_loc) {
                        let mut value_str = String::default();
                        if let Ok(capture_value_str) = env.get_source(&value_loc) {
                            value_str = capture_value_str.to_string();
                        }
                        for named_const in env.get_module(self.target_module_id).get_named_constants() {
                            let spool = env.symbol_pool();
                            log::info!("named_const.get_name() = {}", named_const.get_name().display(spool));
                            let named_const_str = named_const.get_name().display(spool).to_string();
                            if value_str.contains(&named_const_str) {
                                let (def_file, def_line) =
                                    env.get_file_and_location(&named_const.get_loc()).unwrap();
                                let result = FileRange {
                                    path: PathBuf::from(def_file),
                                    line_start: def_line.line.0,
                                    col_start: def_line.column.0,
                                    line_end: def_line.line.0,
                                    col_end: def_line.column.0,
                                };
                                self.capture_items_span.push(value_loc.span());
                                self.result_candidates.push(result); 
                            }
                        }
                    }
                },
                LocalVar(node_id, localvar_symbol) => {
                    let localvar_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit localvar_loc = {:?}",
                        env.get_location(&localvar_loc)
                    );
                    log::info!(
                        "lll >> exp.visit localvar_symbol = {}",
                        localvar_symbol.display(env.symbol_pool())
                    );
                    let local_source = env.get_source(&localvar_loc);
                    log::info!("lll >> local_source = {:?}", local_source);
                    if localvar_loc.span().start() > self.mouse_span.end()
                        || self.mouse_span.end() > localvar_loc.span().end()
                    {
                        // log::info!("??? localvar return");
                        return;
                    }
                    if let Some(node_type) = env.get_node_type_opt(*node_id) {
                        self.process_type(env, &localvar_loc, &node_type);
                    }
                },
                Temporary(node_id, _) => {
                    let tmpvar_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit tmpvar_loc = {:?}",
                        env.get_location(&tmpvar_loc)
                    );
                    if tmpvar_loc.span().start() > self.mouse_span.end()
                        || self.mouse_span.end() > tmpvar_loc.span().end()
                    {
                        return;
                    }

                    if let Some(node_type) = env.get_node_type_opt(*node_id) {
                        self.process_type(env, &tmpvar_loc, &node_type);
                    }
                },
                Call(..) => {
                    self.process_call_spec_func(env, e);
                    self.process_call(env, e);
                },
                Invoke(node_id, ..) => {
                    log::info!("lll >> expdata is Invoke");
                    let invoke_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit invoke_loc = {:?}",
                        env.get_location(&invoke_loc)
                    );
                },
                Lambda(node_id, ..) => {
                    log::info!("lll >> expdata is Lambda");
                    let lambda_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit lambda_loc = {:?}",
                        env.get_location(&lambda_loc)
                    );
                },
                Quant(node_id, ..) => {
                    log::info!("lll >> expdata is Quant");
                    let quant_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit quant_loc = {:?}",
                        env.get_location(&quant_loc)
                    );
                },
                Return(node_id, ..) => {
                    log::info!("lll >> expdata is Return");
                    let return_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit return_loc = {:?}",
                        env.get_location(&return_loc)
                    );
                },
                Sequence(node_id, ..) => {
                    // Statement block in '{' and '}'
                    log::info!("lll >> expdata is Sequence");
                    let sequence_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit sequence_loc = {:?}",
                        env.get_location(&sequence_loc)
                    );
                    // let sequence_source = env.get_source(&sequence_loc);
                    // log::info!("lll >> sequence_source = {:?}", sequence_source);
                },
                Assign(node_id, ..) => {
                    log::info!("lll >> expdata is Assign");
                    let assign_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit assign_loc = {:?}",
                        env.get_location(&assign_loc)
                    );
                },
                Mutate(node_id, ..) => {
                    log::info!("lll >> expdata is Mutate");
                    let mutate_loc = env.get_node_loc(*node_id);
                    log::info!(
                        "lll >> exp.visit mutate_loc = {:?}",
                        env.get_location(&mutate_loc)
                    );
                    let mutate_source = env.get_source(&mutate_loc);
                    log::info!("lll >> mutate_source = {:?}", mutate_source);
                },
                SpecBlock(node_id, spec) => {
                    self.process_spec_block(env, &env.get_node_loc(*node_id), spec);
                },
                _ => {
                    log::info!("0000000________");
                },
            }
        });
        log::info!("\nlll << process_expr ^^^^^^^^^^^^^^^^^^^^^^^^^\n");
    }

    fn process_call_spec_func(
        &mut self,
        env: &GlobalEnv,
        expdata: &move_model::ast::ExpData,
    ) {
        log::info!("lll >> process_call_spec_func, expdata = {:?}", expdata);
        if let Call(node_id, SpecFunction(mid, fid, _), _) = expdata {
            let this_call_loc = env.get_node_loc(*node_id);
            log::info!(
                "lll >> exp.visit this_call_loc = {:?}",
                env.get_file_and_location(&this_call_loc)
            );
            if this_call_loc.span().start() < self.mouse_span.end()
                && self.mouse_span.end() < this_call_loc.span().end()
            {
                let called_module = env.get_module(*mid);
                let spec_fun = called_module.get_spec_fun(*fid);
                log::info!(
                    "lll >> get_spec_functions = {}",
                    spec_fun.name.display(env.symbol_pool())
                );
                let spec_fun_loc = spec_fun.loc.clone();
                let (spec_fun_file, spec_fun_line) =
                    env.get_file_and_location(&spec_fun_loc).unwrap();
                let path_buf = PathBuf::from(spec_fun_file);
                let result = FileRange {
                    path: path_buf,
                    line_start: spec_fun_line.line.0,
                    col_start: spec_fun_line.column.0,
                    line_end: spec_fun_line.line.0,
                    col_end: spec_fun_line.column.0,
                };
                self.result_candidates.push(result);
                self.capture_items_span.push(this_call_loc.span());

                let inst_vec = &env.get_node_instantiation(*node_id);
                for inst in inst_vec {
                    let mut generic_ty_loc = this_call_loc.clone();
                    let capture_call_source = env.get_source(&this_call_loc);
                    if let Ok(capture_call_source_str) = capture_call_source {
                        if let Some(index) = capture_call_source_str.find("<".to_string().as_str()) {
                            generic_ty_loc = move_model::model::Loc::new(
                                this_call_loc.file_id(),
                                codespan::Span::new(
                                    this_call_loc.span().start()  + codespan::ByteOffset(index.try_into().unwrap()),
                                    this_call_loc.span().end(),
                                ),
                            );
                        }
                    }
                    self.process_type(env, &generic_ty_loc, inst);
                }
            }
        }
        log::info!("lll << process_call_spec_func");
    }

    fn process_call(
        &mut self,
        env: &GlobalEnv,
        expdata: &move_model::ast::ExpData,
    )
    {
        log::info!("lll >> process_call");
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
                let called_fun_loc = called_fun.get_loc();
                let (called_fun_file, called_fun_line) =
                    env.get_file_and_location(&called_fun_loc).unwrap();
                let path_buf = PathBuf::from(called_fun_file);
                let result = FileRange {
                    path: path_buf,
                    line_start: called_fun_line.line.0,
                    col_start: called_fun_line.column.0,
                    line_end: called_fun_line.line.0,
                    col_end: called_fun_line.column.0
                        + called_fun.get_full_name_str().len() as u32,
                };
                self.result_candidates.push(result);
                self.capture_items_span.push(this_call_loc.span());

                let inst_vec = &env.get_node_instantiation(*node_id);
                for inst in inst_vec {
                    let mut generic_ty_loc = this_call_loc.clone();
                    let capture_call_source = env.get_source(&this_call_loc);
                    if let Ok(capture_call_source_str) = capture_call_source {
                        if let Some(index) = capture_call_source_str.find("<".to_string().as_str()) {
                            generic_ty_loc = move_model::model::Loc::new(
                                this_call_loc.file_id(),
                                codespan::Span::new(
                                    this_call_loc.span().start()  + codespan::ByteOffset(index.try_into().unwrap()),
                                    this_call_loc.span().end(),
                                ),
                            );
                        }
                    }
                    self.process_type(env, &generic_ty_loc, inst);
                }
            }
        }

        if let Call(node_id, BorrowGlobal(..), _) = expdata {
            let this_call_loc = env.get_node_loc(*node_id);
            log::info!(
                "lll >> exp.visit this_call_loc = {:?}",
                env.get_location(&this_call_loc)
            );
            if this_call_loc.span().start() < self.mouse_span.end()
                && self.mouse_span.end() < this_call_loc.span().end()
            {
                let inst_vec = &env.get_node_instantiation(*node_id);
                for inst in inst_vec {
                    log::info!("lll >> inst = {:?}", inst);    
                    self.process_type(env, &this_call_loc, inst);
                }
            }
        }

        if let Call(node_id, Select(mid, sid, fid), _) = expdata {
            let this_call_loc = env.get_node_loc(*node_id);
            log::info!(
                "lll >> exp.visit this_call_loc = {:?}",
                env.get_location(&this_call_loc)
            );
            if this_call_loc.span().start() > self.mouse_span.end()
                || self.mouse_span.end() > this_call_loc.span().end()
            {
                return;
            }
            let called_module = env.get_module(*mid);
            let called_struct = called_module.get_struct(*sid);
            log::info!(
                "lll >> called_struct = {:?}",
                called_struct.get_full_name_str()
            );
            let called_field = called_struct.get_field(*fid);
            let field_name = called_field.get_name();
            let field_name_str = field_name.display(env.symbol_pool());
            log::info!("lll >> field_name = {}", field_name_str);

            let called_struct_loc = called_struct.get_loc();
            // let (called_struct_file, called_struct_line) = env.get_file_and_location(&called_struct_loc).unwrap();
            let call_struct_source = env.get_source(&called_struct_loc);
            // log::info!("lll >> call_struct_source = {:?}", call_struct_source);
            if let Ok(call_struct_str) = call_struct_source {
                if let Some(index) =
                    call_struct_str.find(field_name_str.to_string().as_str())
                {
                    let field_start = called_struct_loc.span().start()
                        + codespan::ByteOffset(index.try_into().unwrap());
                    let field_len = field_name_str.to_string().len();
                    let field_end =
                        field_start + codespan::ByteOffset(field_len.try_into().unwrap());
                    let field_loc = move_model::model::Loc::new(
                        called_struct_loc.file_id(),
                        codespan::Span::new(field_start, field_end),
                    );
                    let call_field_source = env.get_source(&field_loc);
                    log::info!("lll >> call_field_source = {:?}", call_field_source);
                    let (called_field_file, called_field_line) =
                        env.get_file_and_location(&field_loc).unwrap();
                    let path_buf = PathBuf::from(called_field_file);
                    let result = FileRange {
                        path: path_buf,
                        line_start: called_field_line.line.0,
                        col_start: called_field_line.column.0,
                        line_end: called_field_line.line.0,
                        col_end: called_field_line.column.0,
                    };
                    self.result_candidates.push(result);
                    self.capture_items_span.push(this_call_loc.span());
                }
            }
        }
        log::info!("lll << process_call");
    }

    fn print_spec_block_info(&mut self, info: &SpecBlockInfo) {
        log::info!("Spec block location: {:?}", info.loc);
        log::info!("Spec block target:");
        match &info.target {
            SpecBlockTarget::Module => log::info!("  - Module"),
            SpecBlockTarget::Struct(module_id, struct_id) => {
                log::info!("  - Struct: {:?}, {:?}", module_id, struct_id)
            }
            SpecBlockTarget::Function(module_id, fun_id) => {
                log::info!("  - Function: {:?}, {:?}", module_id, fun_id)
            }
            SpecBlockTarget::FunctionCode(module_id, fun_id, code_offset) => {
                log::info!(
                    "  - Function code: {:?}, {:?}, code offset: {}",
                    module_id, fun_id, code_offset
                )
            }
            SpecBlockTarget::Schema(module_id, schema_id, type_params) => {
                log::info!("  - Schema: {:?}, {:?}", module_id, schema_id);
                log::info!("    type parameters:");
                for param in type_params {
                    log::info!("    - {:?}", param);
                }
            }
        }
        log::info!("Spec block member locations:");
        for loc in &info.member_locs {
            log::info!("{:?}", loc);
        }
    }
    
    fn process_spec_block(
        &mut self,
        env: &GlobalEnv,
        capture_items_loc: &move_model::model::Loc,
        speck_block: &Spec,
    ) {
        log::info!("\n\n");
        let target_module = env.get_module(self.target_module_id);
        // for spec_block_info in target_module.get_spec_block_infos() {
            // log::info!("lll >> process_spec_block loc = {:?}", env.get_location(capture_items_loc));
            // log::info!("lll >> spec_block_info.loc = {:?}", env.get_location(&spec_block_info.loc));
            // if spec_block_info.loc == *capture_items_loc {
            //     self.print_spec_block_info(spec_block_info);
            // }
            // if let SpecBlockTarget::Function(..) = spec_block_info.target {
            //     let mut spec_source = env.get_source(capture_items_loc);
            //     log::info!("lll >> capture_items_loc spec_source = {:?}", spec_source);

            //     spec_source = env.get_source(&spec_block_info.loc);
            //     log::info!("lll >> spec_block_info spec_source = {:?}", spec_source);
            //     // self.print_spec_block_info(spec_block_info);
            // }
        //     log::info!("lll >> spec_block_info    loc = {:?}", env.get_file_and_location(&spec_block_info.loc));
        // }
        log::info!("lll >> process_spec_block loc = {:?}", env.get_file_and_location(capture_items_loc));
        log::info!("lll << process_spec_block loc");
    }

    fn process_type(
        &mut self,
        env: &GlobalEnv,
        capture_items_loc: &move_model::model::Loc,
        ty: &move_model::ty::Type,
    ) {
        use move_model::ty::Type::*;
        match ty {
            Tuple(..) => {
                log::info!("lll >> type_var is Tuple");
            },
            Vector(type_ptr) => {
                log::info!("lll >> type_var is Vector");
                self.process_type(env, capture_items_loc, &**type_ptr);
            },
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
            TypeParameter(..) => {
                log::info!("lll >> type_var is TypeParameter");
            },
            Reference(kind, type_ptr) => {
                log::info!("lll >> type_var is Reference {:?}-{:?}", kind, type_ptr);
                // local_var is Reference Mutable-Struct(ModuleId(37), StructId(Symbol(1531)), [TypeParameter(0)])
                self.process_type(env, capture_items_loc, &**type_ptr);
            },
            Fun(..) => {
                log::info!("lll >> type_var is Fun");
            },
            TypeDomain(..) => {
                log::info!("lll >> type_var is TypeDomain");
            },
            ResourceDomain(..) => {
                log::info!("lll >> type_var is ResourceDomain");
            },
            Var(..) => {
                log::info!("lll >> type_var is Var");
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
        if !crate::utils::get_target_module(env, move_file_path, &mut self.target_module_id) {
            log::info!("<goto def>cannot get target module\n");
            return;
        }
        if let Some(s) = move_file_path.to_str() {
            if s.contains(".spec") {
                self.process_spec_file(env);
            } else {
                self.process_use_decl(env);
                self.process_func(env);
                self.process_struct(env);    
            }
        }
    }
}

impl ItemOrAccessHandler for Handler {
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }

    fn finished(&self) -> bool {
        false
    }

    fn handle_project_env(
        &mut self,
        _services: &dyn HandleItemService,
        env: &GlobalEnv,
        move_file_path: &Path,
        _: String
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
