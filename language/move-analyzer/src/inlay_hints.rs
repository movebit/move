// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{
    analyzer_handler::*,
    context::*,
    utils::*,
    // {path_concat, FileRange},
};
use lsp_server::*;
use lsp_types::*;
use move_model::{
    ast::{ExpData::*, Operation::*, SpecBlockTarget},
    model::{GlobalEnv, FunctionEnv, ModuleId},
};
use std::path::{Path, PathBuf};

/// Handles inlay_hints request of the language server.
pub fn on_inlay_hints(context: &Context, request: &Request, config: InlayHintsConfig) -> lsp_server::Response {
    log::info!("on_inlay_hints request = {:?}", request);
    let parameters = serde_json::from_value::<InlayHintParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = parameters.text_document.uri.to_file_path().unwrap();
    let fpath = path_concat(
        std::env::current_dir().unwrap().as_path(),
        fpath.as_path(),
    );
    let mut handler = Handler::new(fpath.clone(), parameters.range, config);
    let _ = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return Response {
                id: "".to_string().into(),
                result: Some(serde_json::json!({"msg": "No available project"})),
                error: None,
            };
        }
    }
    .run_visitor_for_file(&mut handler, &fpath, String::default());
    let hints = Some(handler.reuslts);
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(hints).unwrap());
    let ret_response = r.clone();
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
    ret_response
}

#[derive(Clone, Copy, serde::Deserialize, Debug)]
pub struct InlayHintsConfig {
    field_type: bool,
    parameter: bool,
    declare_var: bool,
}

impl Default for InlayHintsConfig {
    fn default() -> Self {
        Self {
            field_type: true,
            parameter: true,
            declare_var: true,
        }
    }
}

struct Handler {
    range: FileRange,
    reuslts: Vec<InlayHint>,
    config: InlayHintsConfig,
    target_module_id: ModuleId,
}

impl Handler {
    fn new(fpath: PathBuf, range: lsp_types::Range, config: InlayHintsConfig) -> Self {
        Self {
            range: FileRange {
                path: fpath,
                line_start: range.start.line,
                col_start: range.end.character,
                line_end: range.end.line,
                col_end: range.end.character + 1,
            },
            reuslts: Default::default(),
            config,
            target_module_id: ModuleId::new(0),
        }
    }

    fn process_func(&mut self, env: &GlobalEnv, move_file_path: &Path) {
        log::info!("lll >> <on_inlay_hints>process_func =======================================\n\n");
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

            if self.range.line_start <= func_start_pos.line.0 && func_end_pos.line.0 <= self.range.line_end {
                if let Some(exp) = fun.get_def() {
                    self.process_expr(env, &fun, exp);
                }
            }
        }
    }

    fn process_spec_func(&mut self, env: &GlobalEnv) {
        log::info!("lll >> process_spec_func =======================================");
        let target_module = env.get_module(self.target_module_id);
        for spec_block_info in target_module.get_spec_block_infos() {
            if let SpecBlockTarget::Function(_, fun_id) = spec_block_info.target {
                let (_, block_start_pos) = env.get_file_and_location(&spec_block_info.loc).unwrap();
                let (_, block_end_pos) = env
                    .get_file_and_location(&move_model::model::Loc::new(
                        spec_block_info.loc.file_id(),
                        codespan::Span::new(spec_block_info.loc.span().end(), spec_block_info.loc.span().end()),
                    ))
                    .unwrap();
                if self.range.line_start <= block_start_pos.line.0 && block_end_pos.line.0 <= self.range.line_end {
                    let target_fn = target_module.get_function(fun_id);
                    let target_fn_spec = target_fn.get_spec();
                    log::info!("target_fun's spec = {}",
                        env.display(&*target_fn_spec));
                    for cond in target_fn_spec.conditions.clone() {
                        for exp in cond.all_exps() {
                            self.process_expr(env, &target_fn, &exp);
                        }
                    }
                }
            }
    
            if let SpecBlockTarget::FunctionCode(_, fun_id, _) = spec_block_info.target {
                let (_, block_start_pos) = env.get_file_and_location(&spec_block_info.loc).unwrap();
                let (_, block_end_pos) = env
                    .get_file_and_location(&move_model::model::Loc::new(
                        spec_block_info.loc.file_id(),
                        codespan::Span::new(spec_block_info.loc.span().end(), spec_block_info.loc.span().end()),
                    ))
                    .unwrap();
                if self.range.line_start <= block_start_pos.line.0 && block_end_pos.line.0 <= self.range.line_end {
                    let target_fn = target_module.get_function(fun_id);
                    let target_fn_spec = target_fn.get_spec();
                    log::info!("target_fun_code's spec = {}",
                        env.display(&*target_fn_spec));
                    for cond in target_fn_spec.conditions.clone() {
                        for exp in cond.all_exps() {
                            self.process_expr(env, &target_fn, &exp);
                        }
                    }
                }
            }
    
            if let SpecBlockTarget::Schema(_, _, _) = spec_block_info.target {
                let (_, block_start_pos) = env.get_file_and_location(&spec_block_info.loc).unwrap();
                let (_, block_end_pos) = env
                    .get_file_and_location(&move_model::model::Loc::new(
                        spec_block_info.loc.file_id(),
                        codespan::Span::new(spec_block_info.loc.span().end(), spec_block_info.loc.span().end()),
                    ))
                    .unwrap();
                if self.range.line_start <= block_start_pos.line.0 && block_end_pos.line.0 <= self.range.line_end {
                    log::info!("SpecBlockTarget::Schema, spec_block_info.loc = {:?}", env
                        .get_file_and_location(&spec_block_info.loc.clone()));
                    // let target_stct = target_module.get_struct(stct_id);
                    // let target_stct_spec = target_stct.get_spec();
                    // log::info!("target_stct_spec's spec = {}",
                    //     env.display(&*target_stct_spec));
                }
            }
        }
    }
    
    fn process_expr(
        &mut self,
        env: &GlobalEnv,
        fun: &FunctionEnv,
        exp: &move_model::ast::Exp,
    ) {
        log::info!("\n\nlll >> process_expr -------------------------\n");
        exp.visit(&mut |e| {
            match e {
                Call(..) => {
                    self.process_call(env, e);
                },
                LocalVar(node_id, localvar_symbol) => {
                    let mut localvar_loc = env.get_node_loc(*node_id);
                    localvar_loc = move_model::model::Loc::new(
                        localvar_loc.file_id(),
                        codespan::Span::new(
                            localvar_loc.span().start(),
                            localvar_loc.span().end() + codespan::ByteOffset((2).try_into().unwrap())));

                    log::info!(
                        "lll >> exp.visit localvar_loc = {:?}",
                        env.get_location(&localvar_loc)
                    );
                    log::info!(
                        "lll >> exp.visit localvar_symbol = {}",
                        localvar_symbol.display(env.symbol_pool())
                    );
                    if let Some(node_type) = env.get_node_type_opt(*node_id) {
                        if let Ok(local_var_str) = env.get_source(&localvar_loc) {
                            if let Some(index) = local_var_str.find(localvar_symbol.display(env.symbol_pool()).to_string().as_str()) {
                                let inlay_hint_pos = move_model::model::Loc::new(
                                    localvar_loc.file_id(),
                                    codespan::Span::new(
                                        localvar_loc.span().start() + 
                                            codespan::ByteOffset((index + localvar_symbol.display(env.symbol_pool()).to_string().len()).try_into().unwrap()),
                                        localvar_loc.span().end()));
                                let next_char = local_var_str.chars().nth(index + localvar_symbol.display(env.symbol_pool()).to_string().len());
                                match next_char {
                                    Some('.') => return,
                                    Some(':') => return,
                                    _ => {
                                        log::info!("local_var_str[{:?}] inlay_hint_pos = {:?}", local_var_str, inlay_hint_pos);
                                        self.process_type(env, &inlay_hint_pos, &node_type);
                                    }
                                }
                            }
                        }
                    }
                },
                Temporary(node_id, idx) => {
                    let mut tmpvar_loc = env.get_node_loc(*node_id);
                    tmpvar_loc = move_model::model::Loc::new(
                        tmpvar_loc.file_id(),
                        codespan::Span::new(
                            tmpvar_loc.span().start(),
                            tmpvar_loc.span().end() + codespan::ByteOffset((2).try_into().unwrap())));
                    log::info!(
                        "lll >> exp.visit tmpvar_loc = {:?}",
                        env.get_location(&tmpvar_loc)
                    );

                    let mut tmp_var_name_str = "".to_string();
                    if let Some(name) = fun.get_parameters().get(*idx).map(|p| p.0)
                    {
                        tmp_var_name_str = name.display(env.symbol_pool()).to_string();
                    }
                    if let Some(node_type) = env.get_node_type_opt(*node_id) {
                        if let Ok(tmp_var_str) = env.get_source(&tmpvar_loc) {
                            if let Some(index) = tmp_var_str.find(tmp_var_name_str.as_str()) {
                                let inlay_hint_pos = move_model::model::Loc::new(
                                    tmpvar_loc.file_id(),
                                    codespan::Span::new(
                                        tmpvar_loc.span().start() + 
                                            codespan::ByteOffset((index + tmp_var_name_str.len()).try_into().unwrap()),
                                        tmpvar_loc.span().end()));
                                let next_char = tmp_var_str.chars().nth(index + tmp_var_name_str.len());
                                match next_char {
                                    Some('.') => return,
                                    Some(':') => return,
                                    _ => {
                                        log::info!("tmp_var_str[{:?}] inlay_hint_pos = {:?}", tmp_var_str, inlay_hint_pos);
                                        self.process_type(env, &inlay_hint_pos, &node_type);
                                    }
                                }
                            }
                        }
                    }
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
        if let Call(node_id, Select(mid, sid, fid), _) = expdata {
            let this_call_loc = env.get_node_loc(*node_id);
            let called_module = env.get_module(*mid);
            let called_struct = called_module.get_struct(*sid);
            let called_field = called_struct.get_field(*fid);
            let field_type = called_field.get_type();

            if let Ok(link_access_source) = env.get_source(&this_call_loc) {
                let called_field_name = ".".to_string() + called_field.get_name().display(env.symbol_pool()).to_string().as_str();
                log::info!(
                    "lll >> called_struct = {:?}, link_access_source = {:?}, called_field_name = {:?}",
                    called_struct.get_full_name_str(), link_access_source, called_field_name
                );

                if let Some(index) = link_access_source.find(called_field_name.as_str()) {
                    let dis = index + called_field_name.len();
                    log::info!("lll >> index = {:?}, dis = {:?}", index, dis);  
                    let inlay_hint_pos = move_model::model::Loc::new(
                        this_call_loc.file_id(),
                        codespan::Span::new(
                            this_call_loc.span().start() + codespan::ByteOffset((dis).try_into().unwrap()),
                            this_call_loc.span().end()
                        ));
                    log::info!("lll >> exp.visit this_call_loc = {:?}", this_call_loc);
                    log::info!("lll >> called_struct inlay_hint_pos = {:?}", inlay_hint_pos);
                    self.process_type(env, &inlay_hint_pos, &field_type);        
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
        use move_model::ty::TypeDisplayContext;
        let display_context = TypeDisplayContext::new(env);
        let type_display = ty.display(&display_context);
        let (capture_items_file, capture_items_pos) =
            env.get_file_and_location(&capture_items_loc).unwrap();
        let label_pos = FileRange {
            path: PathBuf::from(capture_items_file).clone(),
            line_start: capture_items_pos.line.0,
            col_start: capture_items_pos.column.0,
            line_end: capture_items_pos.line.0,
            col_end: capture_items_pos.column.0,
        };
        
        self.reuslts.push(mk_inlay_hits(
            Position {
                line: label_pos.line_start,
                character: label_pos.col_end,
            },
            para_inlay_hints_parts(&type_display.to_string(), label_pos),
            // ty_inlay_hints_label_parts(ty, label_pos),
            // InlayHintKind::PARAMETER,
            InlayHintKind::TYPE,
        ));
    }

    fn run_move_model_visitor_internal(
        &mut self,
        env: &GlobalEnv,
        move_file_path: &Path
    ) {
        if !crate::utils::get_target_module(env, move_file_path, &mut self.target_module_id) {
            return;
        }
        if let Some(s) = move_file_path.to_str() {
            if !s.contains(".spec") {
                self.process_func(env, move_file_path);
            } else {
                self.process_spec_func(env);
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
            "inlay hints"
        )
    }
}


fn mk_inlay_hits(pos: Position, label: InlayHintLabel, kind: InlayHintKind) -> InlayHint {
    InlayHint {
        position: pos,
        label,
        kind: Some(kind),
        text_edits: None,
        tooltip: None,
        padding_left: Some(true),
        padding_right: Some(true),
        data: None,
    }
}

fn para_inlay_hints_parts(name: &str, label_pos: FileRange) -> InlayHintLabel {
    log::info!("para_inlay_hints_parts name = {:?}", name);
    InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
        value: format!(": {}", name),
        tooltip: None,
        location: None,
        command: mk_command(label_pos),
    }])
}

// use move_model::ty::Type::*;
// fn ty_inlay_hints_label_parts(
//     ty: &move_model::ty::Type,
//     label_pos: FileRange,
// ) -> InlayHintLabel {
//     let mut ret = Vec::new();
//     ret.push(InlayHintLabelPart {
//         value: ": ".to_string(),
//         tooltip: None,
//         location: None,
//         command: None,
//     });
//     ty_inlay_hints_label_parts_(&mut ret, ty, label_pos);
//     InlayHintLabel::LabelParts(ret)
// }

fn mk_command(label_pos: FileRange) -> Option<Command> {
    // let loc = move_ir_types::location::Loc::new(FileHash::empty(), 0, 0);
    // services.convert_loc_range(&loc).map(|r| MoveAnalyzerClientCommands::GotoDefinition(r.mk_location()).to_lsp_command())
    Some(MoveAnalyzerClientCommands::GotoDefinition(label_pos.mk_location()).to_lsp_command())
}

// fn ty_inlay_hints_label_parts_(
//     ret: &mut Vec<InlayHintLabelPart>,
//     ty: &move_model::ty::Type,
//     label_pos: FileRange,
// ) {
//     let type_args = |ret: &mut Vec<InlayHintLabelPart>, types: &Vec<move_model::ty::Type>| {
//         if types.is_empty() {
//             return;
//         }
//         let last = types.len() - 1;
//         ret.push(InlayHintLabelPart {
//             value: "<".to_string(),
//             tooltip: None,
//             location: None,
//             command: None,
//         });
//         for (index, ty) in types.iter().enumerate() {
//             ty_inlay_hints_label_parts_(ret, ty, label_pos.clone());
//             if index != last {
//                 ret.push(InlayHintLabelPart {
//                     value: ",".to_string(),
//                     tooltip: None,
//                     location: None,
//                     command: None,
//                 });
//             }
//         }
//         ret.push(InlayHintLabelPart {
//             value: ">".to_string(),
//             tooltip: None,
//             location: None,
//             command: None,
//         });
//     };
//     use move_model::ty::*;
//     use move_model::ty::Type::*;
//     match ty {
//         Error => {},
//         Struct(_, _, tys) => {
//             ret.push(InlayHintLabelPart {
//                 value: "struct".to_string(),
//                 tooltip: None,
//                 location: None,
//                 command: mk_command(label_pos.clone()),
//             });
//             type_args(ret, tys);
//         },
//         Primitive(x) => ret.push(InlayHintLabelPart {
//             value: x.to_string(),
//             tooltip: None,
//             location: None,
//             command: None,
//         }),
//         TypeParameter(x) => ret.push(InlayHintLabelPart {
//             value: x.to_string(),
//             tooltip: None,
//             location: None,
//             command: mk_command(label_pos),
//         }),
//         Reference(is_mut, ty) => {
//             ret.push(InlayHintLabelPart {
//                 value: format!("&{}", if *is_mut == ReferenceKind::Mutable  { "mut " } else { "" }),
//                 tooltip: None,
//                 location: None,
//                 command: None,
//             });
//             ty_inlay_hints_label_parts_(ret, ty.as_ref(), label_pos);
//         },
//         Vector(v) => {
//             ret.push(InlayHintLabelPart {
//                 value: "vector<".to_string(),
//                 tooltip: None,
//                 location: None,
//                 command: None,
//             });
//             ty_inlay_hints_label_parts_(ret, v.as_ref(), label_pos);
//             ret.push(InlayHintLabelPart {
//                 value: ">".to_string(),
//                 tooltip: None,
//                 location: None,
//                 command: None,
//             });
//         },
//         _ => {}
//     };
// }