use super::context::*;
use super::move_generate_spec::*;
use crate::project::{attributes_has_test, AstProvider};
use crate::utils::GetPosition;
use lsp_server::*;
use move_compiler::{parser::ast::*, shared::Identifier};
use serde::Deserialize;
use std::{path::PathBuf, str::FromStr};

pub fn on_generate_spec_file(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = PathBuf::from_str(parameters.fpath.as_str()).unwrap();
    let send_err = |context: &Context, msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let project = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => return,
    };
    let mut result: Option<String> = None;
    let pos: Option<lsp_types::Position> = None;
    let mut process_member = |m: &ModuleMember| match m {
        ModuleMember::Function(f) => {
            todo!()
        }
        ModuleMember::Struct(f) => {
            todo!()
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
                    let module_name = m.name.value();
                    for m in m.members.iter() {
                        process_member(m);
                    }
                }
            }
            Definition::Script(_) => {}
        };
    };
    let _ = project.get_defs(&fpath, |x| x.with_definition(call_back));
    let result = match result {
        Some(x) => x,
        None => send_err(context, "not found".to_string()),
    };
    let pos = pos.unwrap();
    let engine = base64::
    let r = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(Resp {
            line: pos.line,
            col: pos.character,
            content: base64::Engine::encode(&self, input),
        })
        .unwrap(),
    );
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

pub struct ReqParameters2 {
    fpath: PathBuf,
    line: u32,
    col: u32,
}

impl From<ReqParameters> for ReqParameters2 {
    fn from(value: ReqParameters) -> Self {
        Self {
            fpath: PathBuf::from_str(value.fpath.as_str()).unwrap(),
            line: value.line,
            col: value.col,
        }
    }
}
impl GetPosition for ReqParameters2 {
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
