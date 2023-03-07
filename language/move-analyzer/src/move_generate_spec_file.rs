use std::{collections::HashMap, path::PathBuf, str::FromStr};

use crate::project::{attributes_has_test, AstProvider};

use super::context::*;
use super::move_generate_spec::*;
use super::project::AddressSpace;
use lsp_server::*;
use move_compiler::{parser::ast::*, shared::Identifier};
use move_symbol_pool::Symbol;
use serde::Deserialize;

pub fn on_generate_spec_file(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = PathBuf::from_str(parameters.fpath.as_str()).unwrap();
    let result_file_path = Resp::mk_result_filepath(&fpath);
    let send_err = |context: &Context, msg: String| {
        let r = Response::new_err(request.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
        context
            .connection
            .sender
            .send(Message::Response(r))
            .unwrap();
    };
    if result_file_path.exists() {
        send_err(context, "file already exists.".to_string());
        return;
    }
    let project = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => return,
    };
    let mut result = ModuleSpecBuilder::new();
    let mut process_member = |addr, module_name, m: &ModuleMember| {
        let spec = match m {
            ModuleMember::Function(x) => {
                let mut g = FunSpecGenerator::new();
                g.generate(x);
                let r = g.to_string();
                Some(r)
            }
            ModuleMember::Struct(x) => {
                let mut g = StructSpecGenerator::new();
                g.generate(x);
                let r = g.to_string();
                Some(r)
            }
            _ => None,
        };
        if let Some(spec) = spec {
            result.insert(AddrAndModuleName::new(addr, module_name), spec);
        }
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
                let addr = match x.address.unwrap().value {
                    LeadingNameAccess_::AnonymousAddress(x) => AddressSpace::Addr(x.into_inner()),
                    LeadingNameAccess_::Name(name) => AddressSpace::Name(name.value),
                };
                let module_name = x.name.value();
                for m in x.members.iter() {
                    process_member(addr, module_name, m);
                }
            }
            Definition::Address(x) => {
                if attributes_has_test(&x.attributes).is_test() {
                    return;
                }
                let addr = match x.addr.value {
                    LeadingNameAccess_::AnonymousAddress(x) => AddressSpace::Addr(x.into_inner()),
                    LeadingNameAccess_::Name(name) => AddressSpace::Name(name.value),
                };

                for m in x.modules.iter() {
                    let module_name = m.name.value();
                    for m in m.members.iter() {
                        process_member(addr, module_name, m);
                    }
                }
            }
            Definition::Script(_) => {}
        };
    };
    let _ = project.get_defs(&fpath, |x| x.with_definition(call_back));
    let file_content = result.to_string();
    match std::fs::write(result_file_path.clone(), file_content) {
        Ok(_) => {}
        Err(err) => send_err(context, format!("write to file failed,err:{:?}", err)),
    };
    let r = Response::new_ok(
        request.id.clone(),
        serde_json::to_value(Resp {
            fpath: result_file_path.to_str().unwrap().to_string(),
        })
        .unwrap(),
    );
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

#[derive(Default)]
struct ModuleSpecBuilder {
    results: HashMap<AddrAndModuleName, Vec<String>>,
}

impl ModuleSpecBuilder {
    fn new() -> Self {
        Self::default()
    }
    fn insert(&mut self, k: AddrAndModuleName, v: String) {
        if let Some(x) = self.results.get_mut(&k) {
            x.push(v);
        } else {
            self.results.insert(k, vec![v]);
        }
    }
    fn to_string(self) -> String {
        let mut ret = String::new();
        for (k, vv) in self.results.into_iter() {
            let mut x = String::default();
            x.push_str(format!("spec {} {{\n\n", k.to_string()).as_str());
            for v in vv.into_iter() {
                x.push_str(v.as_str());
            }
            x.push_str("}\n\n");
            ret.push_str(x.as_str());
        }
        ret
    }
}

#[derive(Clone, Copy, Hash, PartialEq, Eq)]
pub struct AddrAndModuleName {
    addr: AddressSpace,
    module_name: Symbol,
}

impl AddrAndModuleName {
    fn new(addr: AddressSpace, module_name: Symbol) -> Self {
        Self { addr, module_name }
    }
}
#[derive(Clone, Deserialize)]
pub struct ReqParameters {
    fpath: String,
}
impl ToString for AddrAndModuleName {
    fn to_string(&self) -> String {
        format!("{}::{}", self.addr.to_string(), self.module_name.as_str())
    }
}
#[derive(Clone, serde::Serialize)]
pub struct Resp {
    fpath: String,
}

impl Resp {
    fn mk_result_filepath(x: &PathBuf) -> PathBuf {
        let mut x = x.clone();
        let b = x
            .components()
            .last()
            .map(|x| x.as_os_str().to_str())
            .flatten()
            .unwrap()
            .to_string();
        let index = b.as_str().rfind(".").unwrap();
        x.pop();
        let mut ret = x.clone();
        ret.push(format!(
            "{}{}",
            b.as_str()[0..index].to_string(),
            ".spec.move"
        ));
        ret
    }
}
