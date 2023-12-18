use std::{collections::HashMap, path::PathBuf, str::FromStr};



use super::move_generate_spec::*;
use lsp_server::*;
use serde::Deserialize;

// ----------
use crate::{
    utils::get_target_module_by_fpath,
    project::Project,
    context::Context,
};

pub fn on_generate_spec_file(context: &Context, request: &Request) {
    log::info!("on_generate_spec_file request = {:?}", request);
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize on_generate_spec_file request");
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
        None => {
            log::error!("project not found:{:?}", parameters.fpath.as_str());
            return ;
        }
    };
    
    for module_env in get_target_module_by_fpath(&project.global_env, &fpath) {
        let mut result = ModuleSpecBuilder::new();

        for fun_env in module_env.get_functions() {
            eprintln!("fun id = {:?}, fun name = {}", fun_env.get_id(), fun_env.get_name_str());
        }

        for struct_env in module_env.get_structs() {
            eprintln!("struct id = {:?}, struct name = {}", struct_env.get_id(), struct_env.get_full_name_str());
        }
    }

    
}

#[derive(Default)]
struct ModuleSpecBuilder {
    // results: HashMap<AddrAndModuleName, Vec<String>>,
}

impl ModuleSpecBuilder {
    fn new() -> Self {
        Self::default()
    }
    // fn insert(&mut self, k: AddrAndModuleName, v: String) {
    //     if let Some(x) = self.results.get_mut(&k) {
    //         x.push(v);
    //     } else {
    //         self.results.insert(k, vec![v]);
    //     }
    // }
    // fn to_string(self) -> String {
    //     let mut ret = String::new();
    //     for (k, vv) in self.results.into_iter() {
    //         let mut x = String::default();
    //         x.push_str(format!("spec {} {{\n\n", k.to_string()).as_str());
    //         x.push_str(format!("{}spec module {{\n", indent(1)).as_str());
    //         x.push_str(format!("{}pragma verify = true;\n", indent(2)).as_str());
    //         x.push_str(format!("{}pragma aborts_if_is_strict;\n", indent(2)).as_str());
    //         x.push_str(format!("{}}}\n", indent(1)).as_str());
    //         for v in vv.into_iter() {
    //             x.push_str(v.as_str());
    //         }
    //         x.push_str("}\n\n");
    //         ret.push_str(x.as_str());
    //     }
    //     ret
    // }
}

// #[derive(Clone, Hash, PartialEq, Eq)]
// pub struct AddrAndModuleName {
//     addr: AddressSpace,
//     module_name: Symbol,
// }

// impl AddrAndModuleName {
//     fn new(addr: AddressSpace, module_name: Symbol) -> Self {
//         Self { addr, module_name }
//     }
// }
#[derive(Clone, Deserialize)]
pub struct ReqParameters {
    fpath: String,
}
// impl ToString for AddrAndModuleName {
//     fn to_string(&self) -> String {
//         format!("{}::{}", self.addr.to_string(), self.module_name.as_str())
//     }
// }
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
