// use super::context_ori::*;
use super::context::*;
use super::project_ori::*;
use super::move_generate_spec::*;
use crate::context_ori;
use crate::project_ori;
use crate::project_ori::ConvertLoc;
use crate::project_ori::{attributes_has_test, AstProvider};

use crate::utils_ori::GetPosition;
use lsp_server::*;
use move_compiler::parser::ast::*;
use move_ir_types::location::Loc;
use serde::Deserialize;
use std::{path::PathBuf, str::FromStr};

pub fn on_generate_spec_sel(context: &mut Context, request: &Request) {
    log::info!("on_generate_spec_sel request = {:?}", request);
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let send_err = |context: &Context, msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    let parameters = match ReqParametersPath::try_from(parameters) {
        Ok(p) => p,
        Err(_) => {
            send_err(context, "not a valid path".to_string());
            return;
        }
    };

    // let project = match context.projects.get_project(&parameters.fpath) {
    //     Some(x) => x,
    //     None => return,
    // };

    let mut manifest = PathBuf::new();    
    match super::utils::discover_manifest_and_kind(&parameters.fpath.as_path()) {
        Some((mani, _)) => { manifest = mani; },
        _ => {}
    }

    let mut projects = context_ori::MultiProject::new();
    let project = match project_ori::Project::new(manifest, &mut projects, |msg: String| {
        log::info!("msg = {:?}", msg);
    }) {
        Ok(x) => x,
        Err(err) => {
            return;
        }
    };

    let mut result: Option<Resp> = None;
    let insert_pos = |loc: Loc, module_loc: Loc, context: &Context| -> Option<(u32, u32)> {
        let module_loc = match project.convert_loc_range(&module_loc) {
            Some(x) => x,
            None => return None,
        };
        project.convert_loc_range(&loc).map(|x| {
            let mut r = parameters.clone();
            r.line = x.line_end + 1;
            r.col = 4;
            if ReqParametersPath::in_range(&r, &module_loc) {
                (x.line_end + 1, x.col_end)
            } else {
                (x.line_end, x.col_end)
            }
        })
    };
    let mut process_member = |m: &ModuleMember, module_loc: Loc| {
        match m {
            ModuleMember::Function(f) => {
                if attributes_has_test(&f.attributes).is_test() {
                    return;
                }
                if let Some(range) = project.convert_loc_range(&f.loc) {
                    if ReqParametersPath::in_range(&parameters, &range) {
                        if let Some(insert_pos) = insert_pos(f.loc, module_loc, context) {
                            let s = generate_fun_spec(
                                f,
                                &GetExprTypeImpl::new(&parameters.fpath.clone(), &project),
                            );
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
                if attributes_has_test(&f.attributes).is_test() {
                    return;
                }
                if let Some(range) = project.convert_loc_range(&f.loc) {
                    if ReqParametersPath::in_range(&parameters, &range) {
                        if let Some(insert_pos) = insert_pos(f.loc, module_loc, context) {
                            let s = genrate_struct_spec(f);
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
                let module_loc = x.loc;
                for m in x.members.iter() {
                    process_member(m, module_loc);
                }
            }
            Definition::Address(x) => {
                if attributes_has_test(&x.attributes).is_test() {
                    return;
                }
                for m in x.modules.iter() {
                    let module_loc = x.loc;
                    for m in m.members.iter() {
                        process_member(m, module_loc);
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
            send_err(context, "spec target not found.".to_string());
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

#[derive(Clone)]
pub struct ReqParametersPath {
    fpath: PathBuf,
    line: u32,
    col: u32,
}

impl TryFrom<ReqParameters> for ReqParametersPath {
    type Error = core::convert::Infallible;
    fn try_from(value: ReqParameters) -> Result<Self, Self::Error> {
        match PathBuf::from_str(value.fpath.as_str()) {
            Ok(x) => Ok(Self {
                fpath: x,
                line: value.line,
                col: value.col,
            }),
            Err(err) => Err(err),
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

#[cfg(test)]
#[test]
fn test_fun_pure() {
    use move_command_line_common::files::FileHash;
    use move_compiler::{parser::syntax::parse_file_string, shared::CompilationEnv, Flags};
    let fh = FileHash::empty();
    let mut env = CompilationEnv::new(Flags::empty());
    let defs = parse_file_string(
        &mut env,
        fh,
        r#"
        module 0x1::xxx { 
            fun aaa() :u8 {1}
        }
    "#,
    )
    .unwrap();
    let defs = defs.0;

    let mut f = None;
    for d in defs.iter() {
        match d {
            Definition::Module(x) => {
                for xx in x.members.iter() {
                    match xx {
                        ModuleMember::Function(x) => f = Some(x),
                        ModuleMember::Struct(_) => unreachable!(),
                        ModuleMember::Use(_) => unreachable!(),
                        ModuleMember::Friend(_) => unreachable!(),
                        ModuleMember::Constant(_) => unreachable!(),
                        ModuleMember::Spec(_) => unreachable!(),
                    }
                }
            }
            Definition::Address(_) => unreachable!(),
            Definition::Script(_) => unreachable!(),
        }
    }
    let f = f.unwrap();

    let (_, pure) = fun_may_pure(f);
    assert!(pure)
}
