// use super::context_ori::*;

use crate::{
    utils::{GetPosition, get_target_module_by_fpath},
    project::Project,
    context::Context,
    move_generate_spec::{genrate_struct_spec, generate_fun_spec_zx},
    
};
use move_model::model::{ModuleEnv, GlobalEnv, Loc};
use std::{path::PathBuf, str::FromStr};
use serde::{Deserialize, Serialize};
// use lsp_server::{Request,Response};
use lsp_server::{*, Message, Request, Response};
use codespan::Location;


pub fn on_generate_spec_sel(context: &mut Context, request: &Request) {
    log::info!("on_generate_spec_sel request = {:?}", request);
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    
    let parameters = match ReqParametersPath::try_from(parameters) {
        Ok(p) => p,
        Err(_) => {
            send_err(context, request,"not a valid path".to_string());
            return;
        }
    };

    let project = match context.projects.get_project(&parameters.fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", parameters.fpath.as_path());
            return ;
        }
    };

    let mut result = Default::default();
    let mut insert_pos: (u32, u32) = (0,0);
    let mut result_string :String = String::new();
    let mut is_find = false;

    // get_target_module_by_fpath(&project.global_env, &parameters.fpath);

    for module_env in get_target_module_by_fpath(&project.global_env, &parameters.fpath) {
        if handle_struct(&project, &module_env, &parameters, &mut insert_pos, &mut result_string) ||
            handle_function(&project, &module_env, &parameters, &mut insert_pos, &mut result_string) {
                is_find = true;
                break;
        }
    }

    if !is_find {
        send_err(context, request,"spec target not found.".to_string());
        return ;
    }

    result = Some(Resp {line: insert_pos.0, 
                        col: insert_pos.1,
                        content: result_string.clone(),
                    }
                );

    let r = Response::new_ok(request.id.clone(), serde_json::to_value(result).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

fn send_err(context: &Context, requset: &Request, msg: String) {
    let r = Response::new_err(requset.id.clone(), ErrorCode::UnknownErrorCode as i32, msg);
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

fn handle_struct(project :&Project, module_env : &ModuleEnv, 
                parameters: &ReqParametersPath, insert_pos: &mut(u32, u32),
                result_string: &mut String) -> bool 
{
    for struct_env in module_env.get_structs() {
        if !ReqParametersPath::is_linecol_in_loc(parameters.line, parameters.col, 
                                                &struct_env.get_loc(), &project.global_env) 
        {
            continue;
        }
        let end_location = match ReqParametersPath::get_loc_end_location(&struct_env.get_loc(), &project.global_env) {
            Some(x) => x,
            None => return false,
        };

        let mut new_parameters = parameters.clone();
        new_parameters.line = end_location.line.0 + 1;
        new_parameters.col = end_location.column.0;

        insert_pos.0 = end_location.line.0;
        insert_pos.1 = 4;
        if ReqParametersPath::is_linecol_in_loc(new_parameters.line, new_parameters.col, 
                                                &module_env.get_loc(), &project.global_env) 
        {
            insert_pos.0 = insert_pos.0 + 1;
        }

        result_string.push_str(genrate_struct_spec(&struct_env).as_str());
        
        return true;
    }
    return false;
}

fn handle_function(project :&Project, module_env : &ModuleEnv, 
    parameters: &ReqParametersPath, insert_pos: &mut(u32, u32),
    result_string: &mut String) -> bool 
{
    for func_env in module_env.get_functions() {
        if !ReqParametersPath::is_linecol_in_loc(parameters.line, parameters.col, 
                                                &func_env.get_loc(), &project.global_env) 
        {
            continue;
        }

        let end_location = match ReqParametersPath::get_loc_end_location(&func_env.get_loc(), &project.global_env) {
            Some(x) => x,
            None => return false,
        };

        eprintln!("function end line = {:?}", end_location.line);

        let mut new_parameters = parameters.clone();
        new_parameters.line = end_location.line.0 + 1;
        new_parameters.col = 4;

        insert_pos.0 = end_location.line.0;
        insert_pos.1 = end_location.column.0;

        eprintln!("insert spec function end line = {:?}, before", insert_pos.0);
        if ReqParametersPath::is_linecol_in_loc(new_parameters.line, new_parameters.col, 
                                                &module_env.get_loc(), &project.global_env) 
        {
            insert_pos.0 = insert_pos.0 + 1;
        }
        eprintln!("insert spec function end line = {:?}, before", insert_pos.0);

   
        result_string.push_str(
            generate_fun_spec_zx(
                project,
                &project.global_env, 
                &func_env, 
                &new_parameters.fpath)
            .as_str()
        );
        return true;
    }
    return false;
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

impl ReqParametersPath {
    fn is_linecol_in_loc(line :u32, col :u32, loc : &Loc, env:&GlobalEnv) -> bool {
        let start_location = match env.get_location(loc) {
            Some(x) => x,
            None => return false,
        };

        let end_location = match Self::get_loc_end_location(loc, env) {
            Some(x) => x,
            None => return false,
        };


        if line < start_location.line.0 {
            return false;
        }
        if line == start_location.line.0 && col < start_location.column.0 {
            return false;
        }
        if line > end_location.line.0 {
            return false;
        }
        if line == end_location.line.0 && col > end_location.column.0 {
            return false;
        }

        return true;
    }

    fn get_loc_end_location(loc: &Loc, env: &GlobalEnv) -> Option<Location> {
        env.get_location(
            &move_model::model::Loc::new(
                loc.file_id(), 
                codespan::Span::new(
                    loc.span().end(), 
                    loc.span().end()
                )
            )
        )
    }
}



#[derive(Clone, Serialize)]
pub struct Resp {
    line: u32,
    col: u32,
    content: String,
}