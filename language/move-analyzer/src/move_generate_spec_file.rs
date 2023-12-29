use std::{collections::HashMap, path::PathBuf, str::FromStr};



use super::move_generate_spec::*;
use lsp_server::*;
use serde::Deserialize;

// ----------
use crate::{
    utils::get_modules_by_fpath_in_target_modules,
    project::Project,
    context::Context,
};

use move_model::{model::{StructEnv, FunctionEnv}, exp_generator::ExpGenerator};

pub fn on_generate_spec_file<'a>(context: &Context, request: &Request) 
where
    'a: 'static,
{
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
    
    let mut addr_2_addrname = &project.addr_2_addrname;
    
    let mut result = ModuleSpecBuilder::new();
    for module_env in get_modules_by_fpath_in_target_modules(&project.global_env, &fpath) {
        eprintln!("generate spec module: {}", module_env.get_full_name_str());
        // find module_env's namespace
        let module_env_full_name = module_env.get_full_name_str();
        let addr_end = module_env_full_name.find("::").unwrap_or_default();
        let addr = module_env_full_name[0..addr_end].to_string();
        let addr_name_default = String::from("0x0");
        let addr_name = addr_2_addrname.get(&addr).unwrap_or(&addr_name_default);
        let module_name = module_env_full_name[addr_end+2..].to_string();

        // find all available StructEnv and FunctionEnv
        let mut env_item_list: Vec<EnvItem> = module_env
        .get_functions()
        .filter(|fun_env| !fun_env.is_test_only())
        .map(|fun_env| EnvItem {
            struct_env: None,
            function_env: Some(fun_env.clone()),
            line: fun_env.get_loc().span().start().0,
        })
        .chain(
            module_env
                .get_structs()
                .filter(|struct_env| !struct_env.is_test_only())
                .map(|struct_env| EnvItem {
                    struct_env: Some(struct_env.clone()),
                    function_env: None,
                    line: struct_env.get_loc().span().start().0,
                }),
        )
        .collect();
    
        env_item_list.sort_by(|a, b| a.line.cmp(&b.line));

        for item in env_item_list {
            let spec = match item {
                EnvItem { struct_env: Some(struct_env), function_env: None, .. } => {
                    genrate_struct_spec(&struct_env)
                }
                EnvItem { struct_env: None, function_env: Some(f_env), .. } => {
                    generate_fun_spec_zx(project, &project.global_env, &f_env, &fpath)
                }
                _ => continue,
            };
        
            result.insert(
                AddrAndModuleName::new(addr_name.clone(), module_name.clone()),
                spec,
            );
        }
    } // for module_env

    let file_content = result.to_string();
    match std::fs::write(result_file_path.clone(), file_content) {
        Ok(_) => {}
        Err(err) => {
            send_err(context, format!("write to file failed,err:{:?}", err));
            return;
        }
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

#[derive(Debug, Clone)]
struct EnvItem<'a> {
    struct_env: Option<StructEnv<'a>>,
    function_env: Option<FunctionEnv<'a>>,
    line: u32,
}

impl<'a> PartialEq for EnvItem<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.line == other.line
    }
}

impl<'a> Eq for EnvItem<'a> {}


impl<'a> Ord for EnvItem<'a> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line.cmp(&other.line)
    }
}

impl<'a> PartialOrd for EnvItem<'a>  {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.line.cmp(&other.line))
    }
}

impl<'a> EnvItem<'a>  {
    fn get_function_env(&self) -> Option<&FunctionEnv> {
        self.function_env.as_ref()
    }

    fn get_struct_env(&self) -> Option<&StructEnv> {
        self.struct_env.as_ref()
    }
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
            x.push_str(
            format!("spec {}::{} {{\n\n", 
                    &k.addr_name.as_str(), 
                    &k.module_name.as_str()
                )
                .as_str()
            );
            x.push_str(format!("{}spec module {{\n", indent(1)).as_str());
            x.push_str(format!("{}pragma verify = true;\n", indent(2)).as_str());
            x.push_str(format!("{}pragma aborts_if_is_strict;\n", indent(2)).as_str());
            x.push_str(format!("{}}}\n", indent(1)).as_str());
            for v in vv.into_iter() {
                x.push_str(v.as_str());
            }
            x.push_str("}\n\n");
            ret.push_str(x.as_str());
        }
        ret
    }
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub struct AddrAndModuleName {
    addr_name: String,
    module_name: String,
}

impl AddrAndModuleName {
    fn new(addr_name: String, module_name: String) -> Self {
        Self { addr_name, module_name }
    }
}
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
