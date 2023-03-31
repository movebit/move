use super::context::*;
use super::item::*;
use super::project::*;
use super::project_context::*;
use super::types::ResolvedType;

use crate::utils::{
    path_concat, FileRange, GetPosition, GetPositionStruct, MoveAnalyzerClientCommands,
};
use lsp_server::*;

use lsp_types::*;
use move_compiler::{
    parser::ast::Exp_,
    shared::{Identifier, Name},
};
use move_ir_types::location::Loc;
use std::path::PathBuf;

/// Handles go-to-def request of the language server.
pub fn on_inlay_hints(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<InlayHintParams>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = parameters.text_document.uri.to_file_path().unwrap();
    let fpath = path_concat(
        PathBuf::from(std::env::current_dir().unwrap()).as_path(),
        fpath.as_path(),
    );
    let mut handler = Handler::new(fpath.clone(), parameters.range);
    let _ = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return;
        }
    }
    .run_visitor_for_file(&mut handler, &fpath, false);
    let hints = Some(handler.reuslts);
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(hints).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

struct Handler {
    range: FileRange,
    reuslts: Vec<InlayHint>,
}

impl Handler {
    fn new(fpath: PathBuf, range: Range) -> Self {
        Self {
            range: FileRange {
                path: fpath,
                line_start: range.start.line,
                col_start: range.end.character,
                line_end: range.end.line,
                col_end: range.end.character + 1,
            },
            reuslts: Default::default(),
        }
    }
    #[allow(dead_code)]
    fn in_range(&self, loc: Loc, services: &dyn HandleItemService) -> bool {
        services
            .convert_loc_range(&loc)
            .map(|x| self.in_range_range(&x))
            .unwrap_or(false)
    }
    fn in_range_range(&self, x: &FileRange) -> bool {
        GetPositionStruct::in_range(
            &GetPositionStruct {
                fpath: x.path.clone(),
                line: x.line_start,
                col: (x.col_start + x.col_end) / 2,
            },
            &self.range,
        )
    }
}

impl ItemOrAccessHandler for Handler {
    fn need_para_arg_pair(&self) -> bool {
        true
    }
    fn handle_para_arg_pair(
        &mut self,
        services: &dyn HandleItemService,
        para: move_compiler::shared::Name,
        exp: &move_compiler::parser::ast::Exp,
    ) {
        match &exp.value {
            Exp_::Name(x, _) => match &x.value {
                move_compiler::parser::ast::NameAccessChain_::One(x) => {
                    if x.value.as_str() == para.value.as_str() {
                        return;
                    }
                }
                move_compiler::parser::ast::NameAccessChain_::Two(_, _) => {}
                move_compiler::parser::ast::NameAccessChain_::Three(_, _) => {}
            },
            _ => {}
        }
        let l = services.convert_loc_range(&exp.loc);
        let l = match l {
            Some(x) => x,
            None => {
                return;
            }
        };

        self.reuslts.push(mk_inlay_hits(
            Position {
                line: l.line_start,
                character: l.col_start,
            },
            para_inlay_hints_parts(&para, services),
            InlayHintKind::PARAMETER,
        ));
    }
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        _project_context: &ProjectContext,
        item: &ItemOrAccess,
    ) {
        match item {
            ItemOrAccess::Item(item) => match item {
                Item::Var {
                    var,
                    ty,
                    has_decl_ty: false,
                    ..
                } => {
                    if ty.is_err() {
                        return;
                    }
                    let var_range = if let Some(from_range) = services.convert_loc_range(&var.loc())
                    {
                        from_range
                    } else {
                        return;
                    };
                    if !self.in_range_range(&var_range) {
                        return;
                    }
                    self.reuslts.push(mk_inlay_hits(
                        Position {
                            line: var_range.line_end,
                            character: var_range.col_end,
                        },
                        ty_inlay_hints_label_parts(ty, services),
                        InlayHintKind::TYPE,
                    ));
                }
                _ => {}
            },

            ItemOrAccess::Access(acc) => match acc {
                Access::AccessFiled(AccessFiled {
                    from,
                    to: _to,
                    ty,
                    all_fields: _all_fields,
                    item: _item,
                    has_ref,
                }) => {
                    if ty.is_err() {
                        return;
                    }
                    let ty = if let Some(is_mut) = has_ref {
                        ResolvedType::new_ref(*is_mut, ty.clone())
                    } else {
                        ty.clone()
                    };
                    let from_range =
                        if let Some(from_range) = services.convert_loc_range(&from.loc()) {
                            from_range
                        } else {
                            return;
                        };
                    if !self.in_range_range(&from_range) {
                        return;
                    }
                    self.reuslts.push(mk_inlay_hits(
                        Position {
                            line: from_range.line_end,
                            character: from_range.col_end,
                        },
                        ty_inlay_hints_label_parts(&ty, services),
                        InlayHintKind::TYPE,
                    ));
                }
                _ => {}
            },
        }
    }
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }
    fn function_or_spec_body_should_visit(&self, _range: &FileRange) -> bool {
        true
    }
    fn finished(&self) -> bool {
        false
    }
}

impl std::fmt::Display for Handler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "run visit for inlay hits")
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

fn para_inlay_hints_parts(name: &Name, services: &dyn HandleItemService) -> InlayHintLabel {
    InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
        value: format!("{}:", name.value.as_str()),
        tooltip: None,
        location: None,
        command: mk_command(name.loc, services),
    }])
}

fn ty_inlay_hints_label_parts(
    ty: &ResolvedType,
    services: &dyn HandleItemService,
) -> InlayHintLabel {
    let mut ret = Vec::new();
    ret.push(InlayHintLabelPart {
        value: ": ".to_string(),
        tooltip: None,
        location: None,
        command: None,
    });
    ty_inlay_hints_label_parts_(&mut ret, ty, services);
    InlayHintLabel::LabelParts(ret)
}

fn mk_command(loc: Loc, services: &dyn HandleItemService) -> Option<Command> {
    if let Some(r) = services.convert_loc_range(&loc) {
        Some(MoveAnalyzerClientCommands::GotoDefinition(r.mk_location()).to_lsp_command())
    } else {
        None
    }
}

fn ty_inlay_hints_label_parts_(
    ret: &mut Vec<InlayHintLabelPart>,
    ty: &ResolvedType,
    services: &dyn HandleItemService,
) {
    let type_args = |ret: &mut Vec<InlayHintLabelPart>, types: &Vec<ResolvedType>| {
        if types.len() == 0 {
            return;
        }
        let last = types.len() - 1;
        ret.push(InlayHintLabelPart {
            value: "<".to_string(),
            tooltip: None,
            location: None,
            command: None,
        });
        for (index, ty) in types.iter().enumerate() {
            ty_inlay_hints_label_parts_(ret, ty, services);
            if index != last {
                ret.push(InlayHintLabelPart {
                    value: ",".to_string(),
                    tooltip: None,
                    location: None,
                    command: None,
                });
            }
        }
        ret.push(InlayHintLabelPart {
            value: ">".to_string(),
            tooltip: None,
            location: None,
            command: None,
        });
    };

    match ty {
        ResolvedType::UnKnown => {}
        ResolvedType::Struct(x, tys) => {
            ret.push(InlayHintLabelPart {
                value: format!("{}", x.name.0.value.as_str()),
                tooltip: None,
                location: None,
                command: mk_command(x.name.loc(), services),
            });
            type_args(ret, tys);
        }
        ResolvedType::BuildInType(x) => ret.push(InlayHintLabelPart {
            value: format!("{}", x.to_static_str()),
            tooltip: None,
            location: None,
            command: None,
        }),
        ResolvedType::TParam(x, _) => ret.push(InlayHintLabelPart {
            value: format!("{}", x.value.as_str()),
            tooltip: None,
            location: None,
            command: mk_command(x.loc, services),
        }),
        ResolvedType::Ref(is_mut, ty) => {
            ret.push(InlayHintLabelPart {
                value: format!("&{}", if *is_mut { "mut " } else { "" }),
                tooltip: None,
                location: None,
                command: None,
            });
            ty_inlay_hints_label_parts_(ret, ty.as_ref(), services);
        }
        ResolvedType::Unit => ret.push(InlayHintLabelPart {
            value: "()".to_string(),
            tooltip: None,
            location: None,
            command: None,
        }),
        ResolvedType::Multiple(x) => {
            if x.len() == 0 {
                ret.push(InlayHintLabelPart {
                    value: "()".to_string(),
                    tooltip: None,
                    location: None,
                    command: None,
                });
            } else {
                let last = x.len() - 1;
                ret.push(InlayHintLabelPart {
                    value: "(".to_string(),
                    tooltip: None,
                    location: None,
                    command: None,
                });
                for (index, ty) in x.iter().enumerate() {
                    ty_inlay_hints_label_parts_(ret, ty, services);
                    if index != last {
                        ret.push(InlayHintLabelPart {
                            value: ",".to_string(),
                            tooltip: None,
                            location: None,
                            command: None,
                        });
                    }
                }
                ret.push(InlayHintLabelPart {
                    value: ")".to_string(),
                    tooltip: None,
                    location: None,
                    command: None,
                });
            }
        }
        ResolvedType::Fun(_) => {}
        ResolvedType::Vec(v) => {
            ret.push(InlayHintLabelPart {
                value: "vector<".to_string(),
                tooltip: None,
                location: None,
                command: None,
            });
            ty_inlay_hints_label_parts_(ret, v.as_ref(), services);
            ret.push(InlayHintLabelPart {
                value: ">".to_string(),
                tooltip: None,
                location: None,
                command: None,
            });
        }
        ResolvedType::Range => ret.push(InlayHintLabelPart {
            value: "range".to_string(),
            tooltip: None,
            location: None,
            command: None,
        }),
        ResolvedType::Lambda { args, ret_ty } => {
            for a in args.iter() {
                ty_inlay_hints_label_parts_(ret, a, services);
            }
            ty_inlay_hints_label_parts_(ret, ret_ty.as_ref(), services);
        }
    };
}
