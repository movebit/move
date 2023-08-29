// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

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
use move_model::model::{GlobalEnv, ModuleEnv, ModuleId};
use std::{
    fs,
    path::{Path, PathBuf},
    fmt::format,
};
use move_symbol_pool::Symbol;

// step1: get parser::ast::Function
pub fn get_ast_func(module_env: &ModuleEnv) -> Vec<Function> {
    eprintln!("lll >> get_ast_func, env.get_function_count() = {:?}", module_env.get_function_count());
    // eprintln!("sava env '{:?}'", env);
    // let _ = fs::write(output_file, env.dump_env());

    for fun in module_env.get_functions() {
        if let Some(exp) = fun.get_def() {            
            log::info!("lll >> get_ast_func, fn body = {}", exp.display_for_fun(fun.clone()));
            let output_file = format!("{}{}.txt", "./output_global_env-", fun.get_full_name_str());
            let mut func_exp_content = String::from("");

            exp.visit(&mut |e| {
                // log::info!("lll >> exp.visit e = {:?}", e);
                func_exp_content.push_str(format!("{:?}", e).as_str());
            });
            let _ = fs::write(output_file, func_exp_content);
        }
        // fun.get_def()

        // fun.get_friend_env()
        
        // fun.get_friend_name()

        // fun.get_full_name_str()
      
        // fun.get_type_parameter_count()

        // fun.get_type_parameters()

        // fun.get_parameter_count()

        // fun.get_parameter_types()

        // fun.get_result_type()

        // fun.get_loc()

        // fun.get_attributes()

        // fun.visibility()
    }


    vec![Function {
        attributes: Vec::new(),
        loc: Loc::new(FileHash::empty(), 0, 0),
        visibility: Visibility::Public(Loc::new(FileHash::empty(), 0, 0)),
        entry: None,
        signature: FunctionSignature {
            type_parameters: Vec::new(),
            parameters: Vec::new(),
            return_type: Type::new(Loc::new(FileHash::empty(), 0, 0), Type_::Unit),
        },
        acquires: Vec::new(),
        name: FunctionName(Spanned {
            loc: Loc::new(FileHash::empty(), 0, 0),
            value: Symbol::from("_"),
        }),
        inline: false,
        body: FunctionBody::new(Loc::new(FileHash::empty(), 0, 0), FunctionBody_::Native),
    }]
}

// step2: get parser::ast::StructDefinition
pub fn get_ast_struct() -> StructDefinition {
    StructDefinition {
        attributes: Vec::new(),
        loc: Loc::new(FileHash::empty(), 0, 0),
        abilities: Vec::new(),
        name: StructName(Spanned {
            loc: Loc::new(FileHash::empty(), 0, 0),
            value: Symbol::from("_"),
        }),
        type_parameters: Vec::new(),
        fields: StructFields::Native(Loc::new(FileHash::empty(), 0, 0)),
    }
}

// step3: get parser::ast::UseDecl
pub fn get_ast_usedecl() {
    
}

// step4: get parser::ast::FriendDecl
pub fn get_ast_frind() {
    
}

// step5: get parser::ast::Constant
pub fn get_ast_constant() {
    
}

pub fn run_move_model_visitor_for_file(env: &GlobalEnv, move_file_path: &Path) {
    let get_target_module_id = |env: &GlobalEnv, move_file_path: &Path| -> Option<ModuleId> {
        for module in env.get_target_modules() {
            let move_file_name = module.get_full_name_str();
            if let Some(file_stem) = move_file_path.file_stem() {
                if let Some(file_stem_str) = file_stem.to_str() {
                    if move_file_name.contains(file_stem_str) {
                        let module_id = module.get_id();
                        return Some(module_id);
                    }
                }
            }
        }
        None
    };

    let current_module = ModuleDefinition {
        loc: Spanned::unsafe_no_loc(0).loc,
        attributes: Vec::new(),
        address: None,
        name: ModuleName(Spanned {
            loc: Loc::new(FileHash::empty(), 0, 0),
            value: Symbol::from("_"),
        }),
        is_spec_module: false,
        members: Vec::new(),
    };

    if let Some(target_module_id) = get_target_module_id(env, move_file_path) {
        if let target_module = env.get_module(target_module_id) {
            get_ast_func(&target_module);
        }
    }
}

impl Project {
    /// Entrance for `ItemOrAccessHandler` base on analyze.
    pub fn run_full_visitor_by_move_model(&self, visitor: &mut dyn ItemOrAccessHandler) {
        log::info!("lll >> run_full_visitor_by_move_model {} ", visitor);
        log::info!("lll >> after load project, self.manifest_paths.len = {:?}", self.manifest_paths.len());
        self.project_context.clear_scopes_and_addresses();
        for module in self.global_env.get_target_modules() {
            get_ast_func(&module);
        }
    }

    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ItemOrAccessHandler,
        filepath: &PathBuf,
        enter_import: bool,
    ) {
        log::info!("run visitor part for {} ", visitor);
        run_move_model_visitor_for_file(&self.global_env, &filepath);
    }
}
