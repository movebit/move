// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{item::*, project_context::*, types::*, utils::*};
use crate::{project::Project, multiproject::MultiProject, analyzer_handler::*};
use anyhow::{Ok, Result};
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::Definition,
    parser::ast::Exp,
    parser::ast::FunctionSignature,
    parser::ast::Ability,
    parser::ast::StructTypeParameter,
    parser::ast::Type,
    parser::ast::NameAccessChain,
    parser::ast::Exp_,
    parser::ast::Value_,
    parser::ast::NameAccessChain_,
    parser::ast::BinOp_,
    shared::Identifier,
    shared::Name,
};
use move_core_types::account_address::*;
use move_ir_types::location::*;
use move_package::source_package::{layout::SourcePackageLayout, manifest_parser::*};
use move_symbol_pool::Symbol;
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fs,
    path::Path,
};
use std::{path::PathBuf, rc::Rc};
use walkdir::WalkDir;
use std::collections::BTreeMap;
use move_compiler::shared::NumericalAddress;
// use move_symbol_pool::Symbol;
use move_compiler::shared::{NamedAddressMap, PackagePaths};
use move_model::{model::GlobalEnv, options::ModelBuilderOptions, run_model_builder_with_options};

use anyhow::*;
use move_package::compilation::build_plan::BuildPlan;
use tempfile::tempdir;
use num_bigint::BigUint;


// pub enum NumberFormat {
//     Decimal = 10,
//     Hex = 16,
// }

// Determines the base of the number literal, depending on the prefix
pub(crate) fn determine_num_text_and_base22(s: &str) -> (&str, move_compiler::shared::NumberFormat) {
    for c in s.chars() {
        if c.is_alphabetic() {
            return (s, move_compiler::shared::NumberFormat::Hex)
        }
    }
    (s, move_compiler::shared::NumberFormat::Decimal)
}

// Parse an address from a decimal or hex encoding
// pub fn parse_address_number22(s: &str) -> Option<([u8; AccountAddress::LENGTH], move_compiler::shared::NumberFormat)> {
//     let before_after = s.split('=').collect::<Vec<_>>();
//     if before_after.len() != 2 {
//         log::info!(
//             "Invalid named address assignment. Must be of the form <address_name>=<address>, but \
//              found '{}'",
//             s
//         );
//         return None;
//     }
//     let name = before_after[0].parse()?;
//     let addr_str = &before_after[1][before_after[1].len() / 2..];    
//     let (txt, base) = determine_num_text_and_base22(addr_str);
//     log::info!("lll -- txt = {}", txt);
//     log::info!("lll -- txt.as_bytes() = {:?}", txt.as_bytes());
//     let parsed = BigUint::parse_bytes(txt.as_bytes(), match base {
//         move_compiler::shared::NumberFormat::Hex => 16,
//         move_compiler::shared::NumberFormat::Decimal => 10,
//     });

//     log::info!("lll -- parsed = {:?}", parsed);
//     if let Some(parsed_real) = parsed {
//         let bytes = parsed_real.to_bytes_be();
//         log::info!("lll -- bytes = {:?}", bytes);
//         log::info!("lll -- bytes.len() = {:?}", bytes.len());
//         if bytes.len() < AccountAddress::LENGTH {
//             let mut result = [0u8; AccountAddress::LENGTH];
//             result[(AccountAddress::LENGTH - bytes.len())..].clone_from_slice(&bytes);
//             return Some((result, base));
//         }
//     }
//     None
// }
// Parse an address from a decimal or hex encoding
pub fn parse_address_number22(s: &str) -> Option<([u8; AccountAddress::LENGTH], move_compiler::shared::NumberFormat)> {
    let (txt, base) = determine_num_text_and_base22(s);
    let parsed = BigUint::parse_bytes(txt.as_bytes(), match base {
        move_compiler::shared::NumberFormat::Hex => 16,
        move_compiler::shared::NumberFormat::Decimal => 10,
    })?;
    let bytes = parsed.to_bytes_be();
    if bytes.len() > AccountAddress::LENGTH {
        return None;
    }
    let mut result = [0u8; AccountAddress::LENGTH];
    result[(AccountAddress::LENGTH - bytes.len())..].clone_from_slice(&bytes);
    Some((result, base))
}

pub fn parse_str22(s: &str) -> Option<NumericalAddress> {
    match parse_address_number22(s) {
        Some((n, format)) => Some(NumericalAddress::new(n, format)),
        None => None,
    }
}

pub fn parse_named_address22(s: &str) -> anyhow::Result<(String, NumericalAddress)> {
    let before_after = s.split('=').collect::<Vec<_>>();

    if before_after.len() != 2 {
        anyhow::bail!(
            "Invalid named address assignment. Must be of the form <address_name>=<address>, but \
             found '{}'",
            s
        );
    }
    let name = before_after[0].parse()?;
    let addr = parse_str22(before_after[1]).unwrap();
    Ok((name, addr))
}

pub fn parse_addresses_from_options(
    named_addr_strings: Vec<String>,
) -> anyhow::Result<BTreeMap<String, NumericalAddress>> {
    named_addr_strings
        .iter()
        .map(|x| parse_named_address22(x))
        .collect()
}

impl Project {
    pub(crate) fn mk_multi_project_key(&self) -> im::HashSet<PathBuf> {
        use im::HashSet;
        let mut v = HashSet::default();
        for x in self.manifest_paths.iter() {
            v.insert(x.clone());
        }
        v
    }

    pub fn load_ok(&self) -> bool {
        self.manifest_not_exists.is_empty() && self.manifest_load_failures.is_empty()
    }

    pub fn new(
        root_dir: impl Into<PathBuf>,
        multi: &mut MultiProject,
        report_err: impl FnMut(String) + Clone,
    ) -> Result<Self> {
        let working_dir = root_dir.into();
        log::info!("scan modules at {:?}", &working_dir);
        let mut new_project = Self {
            modules: Default::default(),
            manifests: Default::default(),
            hash_file: multi.hash_file.clone(),
            file_line_mapping: multi.file_line_mapping.clone(),
            manifest_paths: Default::default(),
            project_context: ProjectContext::new(),
            manifest_not_exists: Default::default(),
            manifest_load_failures: Default::default(),
            manifest_mod_time: Default::default(),
            global_env: Default::default(),
        };

        let mut targets_paths: Vec<PathBuf> = Vec::new();
        let mut dependents_paths: Vec<PathBuf> = Vec::new();
        new_project.load_project(
            &working_dir,
            multi,
            report_err,
            true,
            &mut targets_paths,
            &mut dependents_paths,
        )?;
        log::info!("targets_paths.len() = {:?}", targets_paths.len());
        log::info!("dependents_paths.len() = {:?}", dependents_paths.len());
        // let parsedtest = BigUint::parse_bytes("0000000000000000000000000a550c18".as_bytes(), 16);
        // log::info!("lll -- parsedtest = {:?}", parsedtest);

        let build_config = move_package::BuildConfig {
            test_mode: true,
            install_dir: Some(tempdir().unwrap().path().to_path_buf()),
            skip_fetch_latest_git_deps: true,
            ..Default::default()
        };
        // resolution graph diagnostics are only needed for CLI commands so ignore them by passing a
        // vector as the writer
        let resolution_graph = build_config.resolution_graph_for_package(&working_dir, &mut Vec::new())?;
        let named_address_mapping: Vec<_> = resolution_graph
                .extract_named_address_mapping()
                .map(|(name, addr)| format!("{}={}", name.as_str(), addr))
                .collect();
        log::info!("named_address_mapping = {:?}", named_address_mapping);
        let addrs = parse_addresses_from_options(named_address_mapping.clone())?;

        let targets = vec![PackagePaths {
            name: None,
            paths: targets_paths
                .into_iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>().clone(),
            named_address_map: addrs.clone(),
        }];
        let dependents = vec![PackagePaths {
            name: None,
            paths: dependents_paths
                .into_iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>().clone(),
            named_address_map: addrs,
        }];
        new_project.global_env = run_model_builder_with_options(
            targets, dependents, ModelBuilderOptions {
                    compile_via_model: true,
                    ..Default::default()
                }
        ).expect("Failed to create GlobalEnv!");
        use codespan_reporting::{diagnostic::Severity, term::termcolor::Buffer};
        if new_project.global_env.has_errors() {
            let mut error_writer = Buffer::no_color();
            new_project.global_env.report_diag(&mut error_writer, Severity::Error);
            eprintln!(
                "{}",
                String::from_utf8_lossy(&error_writer.into_inner()).to_string()
            );
        } else {
            eprintln!("env.get_module_count() = {:?}", &new_project.global_env.get_module_count());
            // for module in env.get_modules() {
            //     for fun in module.get_functions() {
            //         let id = fun.get_qualified_id();
            //     }
            // }
        }

        eprintln!("lll >> 00, env.get_module_count() = {:?}", new_project.global_env.get_module_count());
        for module in new_project.global_env.get_modules() {
            eprintln!("lll >> 00, env.get_function_count() = {:?}", module.get_function_count());
            for fun in module.get_functions() {
                // let id = fun.get_qualified_id();
                log::info!("lll >> 01, fun.get_def() = {:?}", fun.get_def());
                if let Some(exp) = fun.get_def() {
                    log::info!("lll >> 02, fn body = {}", exp.display_for_fun(fun.clone()));
                }
            }
        }
        let mut dummy = DummyHandler;
        // new_project.run_full_visitor(&mut dummy);
        new_project.run_full_visitor_by_move_model(&mut dummy);
        Ok(new_project)
    }

    pub fn update_defs(&mut self, file_path: &PathBuf, old_defs: Option<&Vec<Definition>>) {
        let manifest = super::utils::discover_manifest_and_kind(file_path.as_path());
        if manifest.is_none() {
            log::error!("path can't find manifest file:{:?}", file_path);
            return;
        }
        let (manifest, layout) = manifest.unwrap();
        log::info!(
            "update defs for {:?} manifest:{:?} layout:{:?}",
            file_path.as_path(),
            manifest.as_path(),
            layout
        );
        // delete old items.
        if let Some(defs) = old_defs.as_ref() {
            let x = VecDefAstProvider::new(defs, self, layout);
            x.with_module(|addr, d| {
                self.project_context
                    .delete_module_items(addr, d.name.value(), d.is_spec_module);
            });
        };
        // Update defs.
        let mut dummy = DummyHandler;
        let _ = self.run_visitor_for_file(&mut dummy, file_path, true);
    }

    /// Load a Move.toml project.
    pub(crate) fn load_project(
        &mut self,
        manifest_path: &Path,
        multi: &mut MultiProject,
        mut report_err: impl FnMut(String) + Clone,
        is_main_source: bool,
        targets_paths: &mut Vec<PathBuf>,
        dependents_paths: &mut Vec<PathBuf>,
    ) -> Result<()> {
        let manifest_path = normal_path(manifest_path);
        if self.modules.get(&manifest_path).is_some() {
            log::info!("manifest '{:?}' loaded before skipped.", &manifest_path);
            return Ok(());
        }
        self.manifest_paths.push(manifest_path.clone());
        eprintln!("load manifest file at {:?}", &manifest_path);
        if let Some(x) = multi.asts.get(&manifest_path) {
            self.modules.insert(manifest_path.clone(), x.clone());
        } else {
            let d: Rc<RefCell<SourceDefs>> = Default::default();
            self.modules.insert(manifest_path.clone(), d.clone());
            multi.asts.insert(manifest_path.clone(), d);
            let mut source_paths1 = self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Sources)?;
            let mut source_paths2 = self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Tests)?;
            let mut source_paths3 = self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Scripts)?;
            if is_main_source {
                // source_paths1.append(&mut source_paths2);
                // source_paths1.append(&mut source_paths3);
                // targets_paths.clone_from_slice(&source_paths1);
                targets_paths.extend(source_paths1);
                targets_paths.extend(source_paths2);
                targets_paths.extend(source_paths3);
            } else {
                dependents_paths.extend(source_paths1);
                dependents_paths.extend(source_paths2);
                dependents_paths.extend(source_paths3);
            }
        }
        if !manifest_path.exists() {
            self.manifest_not_exists.insert(manifest_path);
            return anyhow::Result::Ok(());
        }
        {
            let mut file = manifest_path.clone();
            file.push(PROJECT_FILE_NAME);

            self.manifest_mod_time
                .insert(file.clone(), file_modify_time(file.as_path()));
        }

        let manifest = match parse_move_manifest_from_file(&manifest_path) {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(err) => {
                report_err(format!(
                    "parse manifest '{:?} 'failed.\n addr must exactly 32 length or start with '0x' like '0x2'\n{:?}",
                    manifest_path,
                    err
                ));
                log::error!("parse_move_manifest_from_file failed,err:{:?}", err);
                self.manifest_load_failures.insert(manifest_path.clone());
                return anyhow::Result::Ok(());
            }
        };
        self.manifests.push(manifest.clone());
        // load depends.
        for (dep_name, de) in manifest
            .dependencies
            .iter()
            .chain(manifest.dev_dependencies.iter())
        {
            let de_path = de.local.clone();
            let p = path_concat(manifest_path.as_path(), &de_path);
            log::debug!(
                "load dependency for p '{:?}' manifest_path '{:?}' dep_name '{}'",
                &p,
                &manifest_path,
                dep_name
            );
            self.load_project(&p, multi, report_err.clone(), 
                false, targets_paths, dependents_paths)?;
            // log::info!("dependency = '{:?}'", de);
        }
        Ok(())
    }

    /// Load move files locate in sources and tests ...
    pub(crate) fn load_layout_files(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
        use super::syntax::parse_file_string;
        let mut p = manifest_path.clone();
        p.push(kind.location_str());
        for item in WalkDir::new(&p) {
            let file = match item {
                std::result::Result::Err(_e) => continue,
                std::result::Result::Ok(x) => x,
            };
            if file.file_type().is_file()
                && match file.file_name().to_str() {
                    Some(s) => s.ends_with(".move"),
                    None => continue,
                }
            {
                if file
                    .file_name()
                    .to_str()
                    .map(|x| x.starts_with('.'))
                    .unwrap_or(false)
                {
                    continue;
                }
                let file_content = fs::read_to_string(file.path())
                    .unwrap_or_else(|_| panic!("'{:?}' can't read_to_string", file.path()));
                // log::info!("load source file {:?}", file.path());
                let file_hash = FileHash::new(file_content.as_str());

                // This is a move file.
                let defs = parse_file_string(file.path().to_path_buf());
                let defs = match defs {
                    std::result::Result::Ok(x) => x,
                    std::result::Result::Err(diags) => {
                        let mut m = HashMap::new();
                        m.insert(
                            file_hash,
                            (
                                Symbol::from(file.path().to_str().unwrap()),
                                file_content.clone(),
                            ),
                        );
                        let buffer =
                            move_compiler::diagnostics::report_diagnostics_to_buffer(&m, diags);
                        let s = String::from_utf8_lossy(buffer.as_slice());
                        log::error!("{}", s);
                        continue;
                    }
                };

                let defs = defs.0;

                if kind == SourcePackageLayout::Sources {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .sources
                        .insert(file.path().to_path_buf().clone(), defs);
                } else if kind == SourcePackageLayout::Tests {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .tests
                        .insert(file.path().to_path_buf().clone(), defs);
                } else {
                    self.modules
                        .get_mut(manifest_path)
                        .unwrap()
                        .as_ref()
                        .borrow_mut()
                        .scripts
                        .insert(file.path().to_path_buf().clone(), defs);
                }
                // update hash
                self.hash_file
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_hash);
                // update line mapping.
                self.file_line_mapping
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_content.as_str());
            }
        }
    }

    pub(crate) fn load_layout_files_v2(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) 
        -> Result<Vec<PathBuf>> {
        let mut ret_paths = Vec::new();
        let mut p = manifest_path.clone();
        p.push(kind.location_str());
        for item in WalkDir::new(&p) {
            let file = match item {
                std::result::Result::Err(_e) => continue,
                std::result::Result::Ok(x) => x,
            };
            if file.file_type().is_file()
                && match file.file_name().to_str() {
                    Some(s) => s.ends_with(".move"),
                    None => continue,
                }
            {
                if file
                    .file_name()
                    .to_str()
                    .map(|x| x.starts_with('.'))
                    .unwrap_or(false)
                {
                    continue;
                }
                let file_content = fs::read_to_string(file.path())
                    .unwrap_or_else(|_| panic!("'{:?}' can't read_to_string", file.path()));
                log::debug!("load source file {:?}", file.path());
                let file_hash = FileHash::new(file_content.as_str());
                ret_paths.push(file.path().to_path_buf());
                // update hash
                self.hash_file
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_hash);
                // update line mapping.
                self.file_line_mapping
                    .as_ref()
                    .borrow_mut()
                    .update(file.path().to_path_buf(), file_content.as_str());
            }
        }
        log::info!("lll << load_layout_files_v2");
        Ok(ret_paths)
    }

    pub(crate) fn compile_project_package(
        &mut self, 
        manifest_path: &PathBuf, 
        kind: SourcePackageLayout, 
        env: &move_model::model::GlobalEnv) {
        use super::syntax::get_definition_in_global_env_by_move_file;
        let mut p = manifest_path.clone();
        p.push(kind.location_str());
        for item in WalkDir::new(&p) {
            let file = match item {
                std::result::Result::Err(_e) => continue,
                std::result::Result::Ok(x) => x,
            };
            if file.file_type().is_file()
                && match file.file_name().to_str() {
                    Some(s) => s.ends_with(".move"),
                    None => continue,
                }
            {
                if file
                    .file_name()
                    .to_str()
                    .map(|x| x.starts_with('.'))
                    .unwrap_or(false)
                {
                    continue;
                }
                let file_content = fs::read_to_string(file.path())
                    .unwrap_or_else(|_| panic!("'{:?}' can't read_to_string", file.path()));
                log::info!("parse source file {:?}", file.path());
                let file_hash = FileHash::new(file_content.as_str());

                let defs = get_definition_in_global_env_by_move_file(&env, file.path());
                let defs = match defs {
                    std::result::Result::Ok(x) => x,
                    std::result::Result::Err(diags) => {
                        let mut m = HashMap::new();
                        m.insert(
                            file_hash,
                            (
                                Symbol::from(file.path().to_str().unwrap()),
                                file_content.clone(),
                            ),
                        );
                        let buffer =
                            move_compiler::diagnostics::report_diagnostics_to_buffer(&m, diags);
                        let s = String::from_utf8_lossy(buffer.as_slice());
                        log::error!("{}", s);
                        continue;
                    }
                };

                let defs = defs.0;
                if kind == SourcePackageLayout::Sources {
                    self.modules.get_mut(manifest_path).unwrap()
                        .as_ref().borrow_mut().sources
                        .insert(file.path().to_path_buf().clone(), defs);
                } else if kind == SourcePackageLayout::Tests {
                    self.modules
                        .get_mut(manifest_path).unwrap()
                        .as_ref().borrow_mut().tests
                        .insert(file.path().to_path_buf().clone(), defs);
                } else {
                    self.modules
                        .get_mut(manifest_path).unwrap()
                        .as_ref().borrow_mut().scripts
                        .insert(file.path().to_path_buf().clone(), defs);
                }
            }
        }
        log::info!("lll << compile_project_package");
    }

    pub(crate) fn name_to_addr_impl(&self, name: Symbol) -> AccountAddress {
        for x in self.manifests.iter() {
            if let Some(ref x) = x.dev_address_assignments {
                if let Some(x) = x.get(&name) {
                    return *x;
                }
            }
            if let Some(ref x) = x.addresses {
                if let Some(Some(x)) = x.get(&name) {
                    return *x;
                }
            }
        }
        *ERR_ADDRESS
    }

    pub(crate) fn get_spec_build_in_call_type(
        &self,
        project_context: &ProjectContext,
        b: SpecBuildInFun,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        let exprs_types: Vec<_> = exprs
            .value
            .iter()
            .map(|e| self.get_expr_type(e, project_context))
            .collect();
        // vec<T>(x): vector<T> returns a singleton vector.
        // A lot of those build in function.
        let t_in_vector = exprs_types
            .get(0)
            .cloned()
            .map(|x| match x {
                ResolvedType::Vec(x) => x.as_ref().clone(),
                _ => x,
            })
            .unwrap_or(ResolvedType::UnKnown);
        let first_t = exprs_types.get(0).cloned().unwrap_or(ResolvedType::UnKnown);

        match b {
            SpecBuildInFun::Exists => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::Global => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        project_context.resolve_type(ty, self)
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            SpecBuildInFun::Len => ResolvedType::new_build_in(BuildInType::NumType),
            SpecBuildInFun::Update => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
                    } else {
                        ResolvedType::new_vector(t_in_vector)
                    }
                } else {
                    ResolvedType::new_vector(t_in_vector)
                }
            }
            SpecBuildInFun::Vec => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
                    } else {
                        // TODO infer from expr.
                        ResolvedType::new_vector(first_t)
                    }
                } else {
                    ResolvedType::new_vector(first_t)
                }
            }
            SpecBuildInFun::Concat => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(project_context.resolve_type(ty, self))
                    } else {
                        ResolvedType::new_vector(t_in_vector)
                    }
                } else {
                    ResolvedType::new_vector(t_in_vector)
                }
            }
            SpecBuildInFun::Contains => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::IndexOf => ResolvedType::new_build_in(BuildInType::NumType),
            SpecBuildInFun::Range => ResolvedType::Range,
            SpecBuildInFun::InRange => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::UpdateField => first_t,
            SpecBuildInFun::Old => first_t,
            SpecBuildInFun::TRACE => first_t,
        }
    }

    /// return type for `borrow_global`  ...
    pub(crate) fn get_move_build_in_call_type(
        &self,
        project_context: &ProjectContext,
        b: MoveBuildInFun,
        type_args: &Option<Vec<Type>>,
        _exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> ResolvedType {
        match b {
            MoveBuildInFun::MoveTo => ResolvedType::new_unit(),
            MoveBuildInFun::MoveFrom => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        project_context.resolve_type(ty, self)
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            MoveBuildInFun::BorrowGlobalMut | MoveBuildInFun::BorrowGlobal => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        let ty = project_context.resolve_type(ty, self);
                        ResolvedType::new_ref(b == MoveBuildInFun::BorrowGlobalMut, ty)
                    } else {
                        ResolvedType::UnKnown
                    }
                } else {
                    ResolvedType::UnKnown
                }
            }
            MoveBuildInFun::Exits => ResolvedType::new_build_in(BuildInType::Bool),
        }
    }

    pub(crate) fn initialize_fun_call(
        &self,
        project_context: &ProjectContext,
        name: &NameAccessChain,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>,
    ) -> Option<ResolvedType> {
        let (fun_type, _) = project_context.find_name_chain_item(name, self);
        let fun_type = fun_type.unwrap_or_default().to_type().unwrap_or_default();
        match &fun_type {
            ResolvedType::Lambda { .. } => Some(fun_type),
            ResolvedType::Fun(x) => {
                let type_parameters = &x.type_parameters;
                let parameters = &x.parameters;
                let type_args: Option<Vec<ResolvedType>> = type_args.as_ref().map(|type_args| {
                    type_args
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect()
                });
                let mut fun_type = fun_type.clone();
                let mut types = HashMap::new();
                if let Some(ref ts) = type_args {
                    for (para, args) in type_parameters.iter().zip(ts.iter()) {
                        types.insert(para.0.value, args.clone());
                    }
                } else if !type_parameters.is_empty() {
                    let exprs_types: Vec<_> = exprs
                        .value
                        .iter()
                        .map(|e| self.get_expr_type(e, project_context))
                        .collect();
                    infer_type_parameter_on_expression(
                        &mut types,
                        &parameters
                            .iter()
                            .map(|(_, t)| t.clone())
                            .collect::<Vec<_>>(),
                        &exprs_types,
                    );
                }
                fun_type.bind_type_parameter(&types);
                Some(fun_type)
            }
            _ => None,
        }
    }

    /// Get A Type for exprme if possible otherwise Unknown is return.
    pub(crate) fn get_expr_type(
        &self,
        expr: &Exp,
        project_context: &ProjectContext,
    ) -> ResolvedType {
        match &expr.value {
            Exp_::Value(ref x) => match &x.value {
                Value_::Address(_) => ResolvedType::new_build_in(BuildInType::Address),
                Value_::Num(x) => {
                    let b = BuildInType::num_types()
                        .into_iter()
                        .find(|b| x.as_str().ends_with(b.to_static_str()));
                    ResolvedType::new_build_in(b.unwrap_or(BuildInType::NumType))
                }
                Value_::Bool(_) => ResolvedType::new_build_in(BuildInType::Bool),
                Value_::HexString(_) => ResolvedType::new_build_in(BuildInType::NumType),
                Value_::ByteString(_) => ResolvedType::new_build_in(BuildInType::String),
            },
            Exp_::Move(x) | Exp_::Copy(x) => project_context.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                let (item, _) = project_context.find_name_chain_item(name, self);
                item.unwrap_or_default().to_type().unwrap_or_default()
            }
            Exp_::Call(name, is_macro, ref type_args, exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(name).unwrap_or_default();
                    match c {
                        MacroCall::Assert => return ResolvedType::new_unit(),
                    }
                }
                if let NameAccessChain_::One(name) = &name.value {
                    if name.value.as_str() == SPEC_DOMAIN {
                        return exprs
                            .value
                            .get(0)
                            .map(|x| self.get_expr_type(x, project_context))
                            .unwrap_or(ResolvedType::UnKnown);
                    }
                }
                let (item, _) = project_context.find_name_chain_item(name, self);
                match item.unwrap_or_default() {
                    Item::SpecBuildInFun(b) => {
                        return self.get_spec_build_in_call_type(
                            project_context,
                            b,
                            type_args,
                            exprs,
                        )
                    }
                    Item::MoveBuildInFun(b) => {
                        return self.get_move_build_in_call_type(
                            project_context,
                            b,
                            type_args,
                            exprs,
                        )
                    }
                    _ => {}
                }
                let ty = self.initialize_fun_call(project_context, name, type_args, exprs);
                match ty.unwrap_or_default() {
                    ResolvedType::Fun(x) => x.ret_type.as_ref().clone(),
                    ResolvedType::Lambda { ret_ty, .. } => ret_ty.as_ref().clone(),
                    _ => ResolvedType::UnKnown,
                }
            }

            Exp_::Pack(name, type_args, fields) => {
                let (struct_item, _) = project_context.find_name_chain_item(name, self);
                let struct_item = match struct_item {
                    Some(x) => match x {
                        Item::Struct(x) => x,
                        Item::StructNameRef(_) => {
                            unreachable!()
                        }
                        _ => {
                            return ResolvedType::UnKnown;
                        }
                    },
                    None => return ResolvedType::UnKnown,
                };

                let mut types = HashMap::new();
                let type_args: Option<Vec<ResolvedType>> = type_args.as_ref().map(|type_args| {
                    type_args
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect()
                });

                if type_args.is_none() {
                    // try infer on field.
                    let fields_exprs: Vec<_> = fields
                        .iter()
                        .map(|(field, expr)| (*field, self.get_expr_type(expr, project_context)))
                        .collect();
                    let fields_exp_map = {
                        let mut m = HashMap::new();
                        fields_exprs.iter().for_each(|(f, t)| {
                            m.insert(f.0.value, t.clone());
                        });
                        m
                    };
                    let parameters: Vec<_> = struct_item
                        .fields
                        .iter()
                        .map(|(_, ty)| ty.clone())
                        .collect();
                    let expression_types: Vec<_> = fields
                        .iter()
                        .map(|(f, _)| {
                            fields_exp_map
                                .get(&f.0.value)
                                .unwrap_or(&UNKNOWN_TYPE)
                                .clone()
                        })
                        .collect();
                    infer_type_parameter_on_expression(&mut types, &parameters, &expression_types)
                }

                if let Some(ref ts) = type_args {
                    for (para, args) in struct_item.type_parameters.iter().zip(ts.iter()) {
                        types.insert(para.name.value, args.clone());
                    }
                }
                ResolvedType::Struct(
                    struct_item.to_struct_ref(),
                    struct_item
                        .type_parameters
                        .iter()
                        .map(|x| types.get(&x.name.value).cloned().unwrap_or_default())
                        .collect(),
                )
            }
            Exp_::Vector(_, ty, exprs) => {
                let mut ty = if let Some(ty) = ty {
                    ty.get(0).map(|ty| project_context.resolve_type(ty, self))
                } else {
                    None
                };
                if !option_ty_is_valid(&ty) {
                    for e in exprs.value.iter() {
                        let ty2 = self.get_expr_type(e, project_context);
                        if !ty2.is_err() {
                            ty = Some(ty2);
                            break;
                        }
                    }
                }
                ResolvedType::new_vector(ty.unwrap_or_default())
            }
            Exp_::IfElse(_, then_, else_) => {
                let mut ty = self.get_expr_type(then_.as_ref(), project_context);
                if ty.is_err() {
                    if let Some(else_) = else_ {
                        ty = self.get_expr_type(else_, project_context);
                    }
                }
                ty
            }
            Exp_::While(_, _) | Exp_::Loop(_) => ResolvedType::new_unit(),
            Exp_::Block(b) => {
                if let Some(expr) = b.3.as_ref() {
                    project_context.enter_scope(|scopes| {
                        let mut handler = DummyHandler;
                        self.visit_block(b, scopes, &mut handler);
                        self.get_expr_type(expr, scopes)
                    })
                } else {
                    ResolvedType::new_unit()
                }
            }
            Exp_::Lambda(_, _) => {
                // TODO.
                ResolvedType::UnKnown
            }
            Exp_::Quant(_, _, _, _, _) => ResolvedType::UnKnown,
            Exp_::ExpList(e) => {
                let tys: Vec<_> = e
                    .iter()
                    .map(|x| self.get_expr_type(x, project_context))
                    .collect();
                ResolvedType::Multiple(tys)
            }

            Exp_::Unit => ResolvedType::new_unit(),
            Exp_::Assign(_, _) => ResolvedType::new_unit(),
            Exp_::Return(_) => ResolvedType::new_unit(),
            Exp_::Abort(_) => ResolvedType::new_unit(),
            Exp_::Break => ResolvedType::new_unit(),
            Exp_::Continue => ResolvedType::new_unit(),
            Exp_::Dereference(e) => {
                let ty = self.get_expr_type(e, project_context);
                match &ty {
                    ResolvedType::Ref(_, t) => t.as_ref().clone(),
                    _ => ty,
                }
            }
            Exp_::UnaryExp(_, e) => self.get_expr_type(e, project_context),
            Exp_::BinopExp(left, op, right) => {
                let left_ty = self.get_expr_type(left, project_context);
                let right_ty = self.get_expr_type(right, project_context);
                let binary_type = || {
                    if !left_ty.is_err() {
                        left_ty.clone()
                    } else if !right_ty.is_err() {
                        right_ty.clone()
                    } else {
                        ResolvedType::new_build_in(BuildInType::NumType)
                    }
                };
                match op.value {
                    BinOp_::Add => binary_type(),
                    BinOp_::Sub => binary_type(),
                    BinOp_::Mul => binary_type(),
                    BinOp_::Mod => binary_type(),
                    BinOp_::Div => binary_type(),
                    BinOp_::BitOr => binary_type(),
                    BinOp_::BitAnd => binary_type(),
                    BinOp_::Xor => binary_type(),
                    BinOp_::Shl => binary_type(),
                    BinOp_::Shr => binary_type(),
                    BinOp_::Range => ResolvedType::Range,
                    BinOp_::Implies => ResolvedType::new_unit(),
                    BinOp_::Iff => ResolvedType::new_unit(),
                    BinOp_::And => ResolvedType::new_build_in(BuildInType::Bool),
                    BinOp_::Or => ResolvedType::new_build_in(BuildInType::Bool),
                    BinOp_::Eq
                    | BinOp_::Neq
                    | BinOp_::Lt
                    | BinOp_::Gt
                    | BinOp_::Le
                    | BinOp_::Ge => ResolvedType::new_build_in(BuildInType::Bool),
                }
            }
            Exp_::Borrow(is_mut, e) => {
                let ty = self.get_expr_type(e, project_context);
                ResolvedType::new_ref(*is_mut, ty)
            }
            Exp_::Dot(e, name) => {
                let ty = self.get_expr_type(e, project_context);
                let ty = match &ty {
                    ResolvedType::Ref(_, ty) => ty.as_ref(),
                    _ => &ty,
                };
                match ty {
                    ResolvedType::Struct(_, _) => {
                        let s = ty.struct_ref_to_struct(project_context);
                        if let Some(field) = s.find_filed_by_name(name.value) {
                            field.1.clone()
                        } else {
                            ResolvedType::UnKnown
                        }
                    }
                    _ => ResolvedType::UnKnown,
                }
            }

            Exp_::Index(e, _index) => {
                let ty = self.get_expr_type(e, project_context);
                let ty = match &ty {
                    ResolvedType::Ref(_, x) => x.as_ref().clone(),
                    _ => ty,
                };
                match &ty {
                    ResolvedType::Vec(x) => x.as_ref().clone(),
                    _ => ty,
                }
            }

            Exp_::Cast(_, ty) => project_context.resolve_type(ty, self),
            Exp_::Annotate(_, ty) => project_context.resolve_type(ty, self),
            Exp_::Spec(_) => ResolvedType::new_unit(),
            Exp_::UnresolvedError => {
                // Nothings. didn't know what to do.
                ResolvedType::UnKnown
            }
        }
    }

    pub(crate) fn visit_struct_tparam(
        &self,
        t: &StructTypeParameter,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        self.visit_tparam(&(t.name, t.constraints.clone()), project_context, visitor);
    }

    pub(crate) fn visit_tparam(
        &self,
        t: &(Name, Vec<Ability>),
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        let (name, v) = t;
        let item = ItemOrAccess::Item(Item::TParam(*name, v.clone()));
        visitor.handle_item_or_access(self, project_context, &item);
        if visitor.finished() {
            return;
        }
        // Enter this.
        project_context.enter_types(self, name.value, item);
    }
    
    pub(crate) fn visit_signature(
        &self,
        signature: &FunctionSignature,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        for t in signature.type_parameters.iter() {
            self.visit_tparam(t, project_context, visitor);
            if visitor.finished() {
                return;
            }
        }
        for (v, t) in signature.parameters.iter() {
            self.visit_type_apply(t, project_context, visitor);
            let t = project_context.resolve_type(t, self);
            let item = ItemOrAccess::Item(Item::Parameter(*v, t));
            // found
            visitor.handle_item_or_access(self, project_context, &item);
            if visitor.finished() {
                return;
            }
            project_context.enter_item(self, v.value(), item)
        }
        self.visit_type_apply(&signature.return_type, project_context, visitor);
    }

    pub(crate) fn manifest_beed_modified(&self) -> bool {
        self.manifest_mod_time.iter().any(|(k, v)| {
            if file_modify_time(k.as_path()).cmp(v) != Ordering::Equal {
                eprintln!(
                    "going to reload project becasue of modify of '{:?}' {:?} {:?}",
                    k.as_path(),
                    file_modify_time(k.as_path()),
                    v
                );
                true
            } else {
                false
            }
        })
    }
}
