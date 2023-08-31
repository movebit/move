// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{context::*, item::*, analyzer_handler::*, project_context::*, types::ResolvedType};

use crate::utils::{path_concat, FileRange, GetPosition, GetPositionStruct};
use lsp_server::*;

use lsp_types::*;
use move_compiler::shared::Identifier;
use move_ir_types::location::Loc;
use std::{
    collections::{HashMap, HashSet},
    time::SystemTime, str::FromStr,
};
use crate::{project::Project, project_context::*, multiproject::MultiProject, analyzer_handler::*};
use move_compiler::{
    diagnostics::Diagnostics,
    parser::ast::*,
    MatchedFileCommentMap,
};
use move_command_line_common::files::FileHash;
use move_ir_types::location::*;
use move_package::{source_package::layout::SourcePackageLayout, BuildConfig, ModelConfig};
use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
use move_model::model::{GlobalEnv, ModuleEnv, FunctionEnv, ModuleId, FunId};
use std::{
    fs,
    hash::Hash, path::{Path, PathBuf},
    fmt::format,
};
use move_symbol_pool::Symbol;


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
        }
    }
    .run_visitor_for_file(&mut handler, &fpath, false);
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
    /// AccessFiled ... can have this field.
    pub(crate) result2: Option<FileRange>,

    /// result_loc not convert to a FileRange
    /// Current references find depend on this field.
    pub(crate) result_loc: Option<Loc>,

    pub(crate) result_item_or_access: Option<ItemOrAccess>,
}

impl Handler {
    pub(crate) fn new(filepath: impl Into<PathBuf>, line: u32, col: u32) -> Self {
        Self {
            filepath: filepath.into(),
            line,
            col,
            result: None,
            result_loc: None,
            result2: None,
            result_item_or_access: None,
        }
    }

    fn match_loc(&self, loc: &Loc, services: &dyn HandleItemService) -> bool {
        let r = services.convert_loc_range(loc);
        match &r {
            Some(r) => GetPositionStruct::in_range(
                &GetPositionStruct {
                    fpath: self.filepath.clone(),
                    line: self.line,
                    col: self.col,
                },
                r,
            ),
            None => false,
        }
    }
    fn to_locations(&self) -> Vec<Location> {
        let mut ret = Vec::with_capacity(2);
        if let Some(x) = self.result.as_ref() {
            ret.push(x.mk_location());
        }
        if let Some(x) = self.result2.as_ref() {
            ret.push(x.mk_location());
        }
        ret
    }

    
    fn process_func(&mut self, env: &GlobalEnv, move_file_path: &Path, line: u32, col: u32) {
        let mut found_target_module = false;
        let mut found_target_fun = false;
        let mut target_module_id = ModuleId::new(0);
        let mut target_fun_id = FunId::new(env.symbol_pool().make("name"));

        for module in env.get_target_modules() {
            let move_file_name = module.get_full_name_str();
            if let Some(file_stem) = move_file_path.file_stem() {
                if let Some(file_stem_str) = file_stem.to_str() {
                    if move_file_name.contains(file_stem_str) {
                        target_module_id = module.get_id();
                        found_target_module = true;
                    }
                }
            }
        }

        if found_target_module {
            let target_module = env.get_module(target_module_id);
            for fun in target_module.get_functions() {
                let this_fun_loc = fun.get_loc();
                let (_, func_start_pos) = env.get_file_and_location(&this_fun_loc).unwrap();
                if line > func_start_pos.line.0 {
                    target_fun_id = fun.get_id();
                    found_target_fun = true;
                }
            }
        }

        if !found_target_fun {
            return;
        }

        let target_module = env.get_module(target_module_id);
        let target_fun = target_module.get_function(target_fun_id);
        let this_fun_loc = target_fun.get_loc();
        let func_start_pos = env.get_location(&this_fun_loc).unwrap();
        log::info!("lll >> func_start_pos = {:?}", func_start_pos);

        // called func
        if let Some(called_func) = target_fun.get_called_functions() {
            for item in called_func.iter() {
                let called_module = env.get_module(item.module_id);
                let called_fun = called_module.get_function(item.id);
                log::info!("lll >> get_called_functions = {:?}", called_fun.get_full_name_str());
                let called_fun_loc = called_fun.get_loc();
                // let called_fun_file_and_line = env.get_file_and_location(&called_fun_loc).unwrap();
                let (called_fun_file, called_fun_line) = env.get_file_and_location(&called_fun_loc).unwrap();
                let path_buf = PathBuf::from(called_fun_file);
                let result = FileRange {
                    path: path_buf,
                    /// Start.
                    line_start: called_fun_line.line.0,
                    col_start: called_fun_line.column.0,
                
                    /// End.
                    line_end: called_fun_line.line.0,
                    col_end: called_fun_line.column.0 + called_fun.get_full_name_str().len()as u32,
                };
                self.result = Some(result);
                // log::info!("lll >> called_fun_start_pos = {:?}", env.get_file_and_location(&called_fun_loc));
            }
        }
        
        // log::info!("lll >> get_parameter_types = {:?}", target_fun.get_parameter_types());
        // log::info!("lll >> get_parameters = {:?}", target_fun.get_parameters());
        // log::info!("lll >> get_result_type = {:?}", target_fun.get_result_type());
        // log::info!("lll >> get_type_parameters = {:?}", target_fun.get_type_parameters());

        let local_cnt = target_fun.get_local_count();
        for local_idx in local_cnt {
            log::info!("lll >> get_local_type = {:?}", target_fun.get_local_type(local_idx));    
        }
    }


    fn process_struct(&self, env: &GlobalEnv, move_file_path: &Path, line: u32, col: u32) {

    }

    fn process_expr(&self) {
        
    }

    fn run_move_model_visitor_internal(&mut self, env: &GlobalEnv, move_file_path: &Path, line: u32, col: u32) {
        self.process_func(env, move_file_path, line, col);
    }
}

impl ItemOrAccessHandler for Handler {
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }
    fn handle_item_or_access(
        &mut self,
        services: &dyn HandleItemService,
        _project_context: &ProjectContext,
        item_or_access: &ItemOrAccess,
    ) {
        match item_or_access {
            ItemOrAccess::Item(item) => {
                let loc = item.def_loc();
                if self.match_loc(&loc, services) {
                    if let Some(t) = services.convert_loc_range(&loc) {
                        self.result = Some(t);
                        self.result_loc = Some(loc);
                        self.result_item_or_access = Some(item_or_access.clone());
                    }
                }
            },
            ItemOrAccess::Access(access) => match access {
                Access::AccessFiled(AccessFiled { from, to, item, .. }) => {
                    log::info!("goto definition, handle_item_or_access(from({}) --> to({}))", from, to);
                    if self.match_loc(&from.loc(), services) {
                        if let Some(t) = services.convert_loc_range(&to.loc()) {
                            self.result = Some(t);
                            self.result_loc = Some(to.loc());
                            self.result_item_or_access = Some(item_or_access.clone());
                            if let Some(item) = item {
                                self.result2 = services.convert_loc_range(&item.def_loc());
                            }
                        }
                    }
                }
                Access::ExprAccessChain(chain, _, item) if item.is_build_in() => {
                    if self.match_loc(&chain.loc, services) {
                        if let Some(t) = services.convert_loc_range(&chain.loc) {
                            self.result = Some(t);
                            self.result_item_or_access = Some(item_or_access.clone());
                        }
                    }
                }
                _ => {
                    log::trace!("access:{}", access);
                    if let Some((access, def)) = access.access_module() {
                        if self.match_loc(&access, services) {
                            if let Some(t) = services.convert_loc_range(&def) {
                                self.result = Some(t);
                                self.result_loc = Some(def);
                                self.result_item_or_access = Some(item_or_access.clone());
                                return;
                            }
                        }
                    }
                    let locs = access.access_def_loc();
                    if self.match_loc(&locs.0, services) {
                        if let Some(t) = services.convert_loc_range(&locs.1) {
                            self.result = Some(t);
                            self.result_loc = Some(locs.1);
                            self.result_item_or_access = Some(item_or_access.clone());
                        }
                    }
                }
            },
        }
    }

    fn function_or_spec_body_should_visit(&self, range: &FileRange) -> bool {
        Self::in_range(self, range)
    }

    fn finished(&self) -> bool {
        self.result.is_some()
    }

    fn handle_project_env(&mut self, _services: &dyn HandleItemService, env: &GlobalEnv, move_file_path: &Path) {
        self.run_move_model_visitor_internal(env, move_file_path, self.line, self.col);
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

impl GetPosition for Handler {
    fn get_position(&self) -> (PathBuf, u32, u32) {
        (self.filepath.clone(), self.line, self.col)
    }
}



    /*
    // fun.get_friend_env()
                    
    // fun.get_friend_name()

    // fun.get_full_name_str()

    // fun.get_type_parameter_count()

    // fun.get_type_parameters()

    // fun.get_parameter_count()

    // fun.get_parameter_types()

    // fun.get_result_type()

    // fun.get_attributes()

    // fun.visibility()
    
    if let Some(exp) = target_fun.get_def() {
        log::info!("lll >> get_ast_func, fn body = {}", exp.display_for_fun(target_fun.clone()));
        let output_file = format!("{}{}.txt", "./output_global_env-", target_fun.get_full_name_str());
        let mut func_exp_content = String::from("");

        exp.visit(&mut |e| {
            use move_model::ast::ExpData::*;
            use move_model::ast::Value::*;
            // log::info!("lll >> exp.visit e = {:?}", e);
            // func_exp_content.push_str(format!("{:?}", e).as_str());
            match e {
                Call(node_id, operation, args) => {
                    let this_call_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit call loc = {:?}", env.get_location(&this_call_loc));
                },
                Invoke(node_id, target, args) => {
                    let this_invoke_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_invoke_loc = {:?}", env.get_location(&this_invoke_loc));
                    // log::info!("lll >> exp.visit args = {:?}", args);
                    // for exp in args {
                        
                    // }
                },
                Lambda(node_id, _, body) => {
                    let this_lambda_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_lambda_loc = {:?}", env.get_location(&this_lambda_loc));
                    // log::info!("lll >> exp.visit Lambda body = {:?}", body);
                },
                Quant(node_id, _, ranges, triggers, condition, body) => {
                    let this_quant_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_quant_loc = {:?}", env.get_location(&this_quant_loc));
                    // log::info!("lll >> exp.visit Quant ranges = {:?}", ranges);
                    for (node_id, range) in ranges {

                    }
                    // log::info!("lll >> exp.visit Quant triggers = {:?}", triggers);
                    for trigger in triggers {
                        // for e in trigger {
                        // }
                    }
                    // log::info!("lll >> exp.visit Quant condition = {:?}", condition);
                    if let Some(exp) = condition {
                        
                    }
                },
                Block(node_id, _, binding, body) => {
                    let this_block_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_block_loc = {:?}", env.get_location(&this_block_loc));
                    // log::info!("lll >> exp.visit Block binding = {:?}", binding);
                    if let Some(exp) = binding {
                    
                    }
                },
                IfElse(node_id, c, t, e) => {
                    let this_ifelse_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_ifelse_loc = {:?}", env.get_location(&this_ifelse_loc));
                    // log::info!("lll >> exp.visit IfElse e = {:?}", e);
                },
                Loop(node_id, e) => {
                    let this_loop_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_loop_loc = {:?}", env.get_location(&this_loop_loc));
                    // log::info!("lll >> exp.visit Loop e = {:?}", e);
                },
                Return(node_id, e) => {
                    let this_return_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_return_loc = {:?}", env.get_location(&this_return_loc));
                    // log::info!("lll >> exp.visit Return e = {:?}", e);
                },
                Sequence(node_id, es) => {
                    let this_sequence_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_sequence_loc = {:?}", env.get_location(&this_sequence_loc));
                    // log::info!("lll >> exp.visit Sequence es = {:?}", es);
                    // for e in es {
                        
                    // }
                },
                Assign(node_id, _, e) => {
                    let this_assign_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_assign_loc = {:?}", env.get_location(&this_assign_loc));
                    // log::info!("lll >> exp.visit Assign e = {:?}", e);
                },
                Mutate(node_id, lhs, rhs) => {
                    let this_mutate_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_mutate_loc = {:?}", env.get_location(&this_mutate_loc));
                    // log::info!("lll >> exp.visit Mutate rhs = {:?}", rhs);
                },
                Value(node_id, v) => {
                    let this_value_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_value_loc = {:?}", env.get_location(&this_value_loc));
                    match v {
                        Address(address) => log::info!("address = {}", env.display(address)),
                        Number(int) => log::info!("int = {}", int),
                        Bool(b) => log::info!("b = {}", b),
                        ByteArray(bytes) => log::info!("bytes = {:?}", bytes),
                        AddressArray(array) => log::info!("array = {:?}", array),
                        Vector(array) => log::info!("array = {:?}", array),
                    }
                    // log::info!("lll >> exp.visit Explicitly list all enum variants");
                    // if let Some(name) = get_name_from_value(v) {
                    //     let item = ItemOrAccess::Access(Access::ExprAddressName(*name));
                    //     visitor.handle_item_or_access(self, project_context, &item);
                    // }
                },
                // Explicitly list all enum variants
                LoopCont(node_id, _) | LocalVar(node_id, _) | 
                Temporary(node_id, _) | Invalid(node_id) => {
                    let this_default_loc = env.get_node_loc(*node_id);
                    log::info!("lll >> exp.visit this_default_loc = {:?}", env.get_location(&this_default_loc));
                },
            }
        });
        let _ = fs::write(output_file, func_exp_content);
    }
    */