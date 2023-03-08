use super::context::*;
use super::move_generate_spec::*;
use crate::project::ConvertLoc;
use crate::project::{attributes_has_test, AstProvider};
use crate::utils::GetPosition;
use lsp_server::*;
use move_compiler::{parser::ast::*, shared::Identifier};
use move_ir_types::location::Loc;
use serde::Deserialize;
use std::{path::PathBuf, str::FromStr};

pub fn on_generate_spec_sel(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let parameters = ReqParametersPath::from(parameters);
    let send_err = |context: &Context, msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let project = match context.projects.get_project(&parameters.fpath) {
        Some(x) => x,
        None => return,
    };
    let mut result: Option<Resp> = None;

    fn insert_pos(loc: Loc, context: &Context) -> Option<(u32, u32)> {
        context
            .projects
            .convert_loc_range(&loc)
            .map(|x| (x.line_end, x.col_end))
    }

    let mut process_member = |m: &ModuleMember| match m {
        ModuleMember::Function(f) => {
            if let Some(range) = context.projects.convert_loc_range(&f.loc) {
                if ReqParametersPath::in_range(&parameters, &range) {
                    if let Some(insert_pos) = insert_pos(f.loc, context) {
                        let mut g = FunSpecGenerator::new();
                        g.generate(f);
                        let s = g.to_string();
                        result = Some(Resp {
                            line: insert_pos.0,
                            col: insert_pos.1,
                            content: s,
                        });
                    }
                }
            }
        }
        ModuleMember::Struct(f) => {
            if let Some(range) = context.projects.convert_loc_range(&f.loc) {
                if ReqParametersPath::in_range(&parameters, &range) {
                    if let Some(insert_pos) = insert_pos(f.loc, context) {
                        let mut g = StructSpecGenerator::new();
                        g.generate(f);
                        let s = g.to_string();
                        result = Some(Resp {
                            line: insert_pos.0,
                            col: insert_pos.1,
                            content: s,
                        });
                    }
                }
            }
        }
        _ => {}
    };
    let call_back = |ds: &Definition| {
        match ds {
            Definition::Module(x) => {
                if x.is_spec_module {
                    return;
                }
                if attributes_has_test(&x.attributes).is_test() {
                    return;
                }
                for m in x.members.iter() {
                    process_member(m);
                }
            }
            Definition::Address(x) => {
                if attributes_has_test(&x.attributes).is_test() {
                    return;
                }
                for m in x.modules.iter() {
                    for m in m.members.iter() {
                        process_member(m);
                    }
                }
            }
            Definition::Script(_) => {}
        };
    };
    let _ = project.get_defs(&parameters.fpath, |x| x.with_definition(call_back));
    let result = match result {
        Some(x) => x,
        None => {
            send_err(context, "not found".to_string());
            return;
        }
    };
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(result).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

#[derive(Clone, Deserialize)]
pub struct ReqParameters {
    fpath: String,
    line: u32,
    col: u32,
}

pub struct ReqParametersPath {
    fpath: PathBuf,
    line: u32,
    col: u32,
}

impl From<ReqParameters> for ReqParametersPath {
    fn from(value: ReqParameters) -> Self {
        Self {
            fpath: PathBuf::from_str(value.fpath.as_str()).unwrap(),
            line: value.line,
            col: value.col,
        }
    }
}
impl GetPosition for ReqParametersPath {
    fn get_position(&self) -> (PathBuf, u32 /* line */, u32 /* col */) {
        (self.fpath.clone(), self.line, self.col)
    }
}

#[derive(Clone, serde::Serialize)]
pub struct Resp {
    line: u32,
    col: u32,
    content: String,
}
