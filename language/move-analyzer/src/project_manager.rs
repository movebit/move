// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::utils::*;
use crate::{analyzer_handler::*, multiproject::MultiProject, project::Project};
use anyhow::{Ok, Result};
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::Definition,
    shared::{NumericalAddress, PackagePaths},
};
use move_core_types::account_address::*;
use move_package::source_package::{layout::SourcePackageLayout, manifest_parser::*};
use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::BTreeMap,
    fs,
    path::{Path, PathBuf},
    rc::Rc,
};
use walkdir::WalkDir;
use move_model::{options::ModelBuilderOptions, run_model_builder_with_options};

use num_bigint::BigUint;
use tempfile::tempdir;

// Determines the base of the number literal, depending on the prefix
pub(crate) fn determine_num_text_and_base22(
    s: &str,
) -> (&str, move_compiler::shared::NumberFormat) {
    for c in s.chars() {
        if c.is_alphabetic() {
            return (s, move_compiler::shared::NumberFormat::Hex);
        }
    }
    (s, move_compiler::shared::NumberFormat::Decimal)
}

// Parse an address from a decimal or hex encoding
pub fn parse_address_number22(
    s: &str,
) -> Option<(
    [u8; AccountAddress::LENGTH],
    move_compiler::shared::NumberFormat,
)> {
    let (txt, base) = determine_num_text_and_base22(s);
    let parsed = BigUint::parse_bytes(
        txt.as_bytes(),
        match base {
            move_compiler::shared::NumberFormat::Hex => 16,
            move_compiler::shared::NumberFormat::Decimal => 10,
        },
    )?;
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

        let build_config = move_package::BuildConfig {
            test_mode: true,
            install_dir: Some(tempdir().unwrap().path().to_path_buf()),
            skip_fetch_latest_git_deps: true,
            ..Default::default()
        };
        let resolution_graph =
            build_config.resolution_graph_for_package(&working_dir, &mut Vec::new())?;
        let named_address_mapping: Vec<_> = resolution_graph
            .extract_named_address_mapping()
            .map(|(name, addr)| format!("{}={}", name.as_str(), addr))
            .collect();
        // log::info!("named_address_mapping = {:?}", named_address_mapping);
        let addrs = parse_addresses_from_options(named_address_mapping.clone())?;

        let targets = vec![PackagePaths {
            name: None,
            paths: targets_paths
                .into_iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>()
                .clone(),
            named_address_map: addrs.clone(),
        }];
        let dependents = vec![PackagePaths {
            name: None,
            paths: dependents_paths
                .into_iter()
                .map(|p| p.to_string_lossy().to_string())
                .collect::<Vec<_>>()
                .clone(),
            named_address_map: addrs,
        }];
        new_project.global_env = run_model_builder_with_options(
            targets,
            dependents,
            ModelBuilderOptions {
                compile_via_model: true,
                ..Default::default()
            },
        )
        .expect("Failed to create GlobalEnv!");

        // new_project.get_project_def(&working_dir, SourcePackageLayout::Sources);
        // new_project.get_project_def(&working_dir, SourcePackageLayout::Tests);
        // new_project.get_project_def(&working_dir, SourcePackageLayout::Scripts);

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
            log::info!("defs.len = {:?}", defs.len());
            // let x = VecDefAstProvider::new(defs, self, layout);
            // x.with_module(|addr, d| {
            //     // self.project_context
            //     //     .delete_module_items(addr, d.name.value(), d.is_spec_module);
            // });
        };
        // Update defs.
        let mut dummy = DummyHandler;
        let _ = self.run_visitor_for_file(&mut dummy, file_path);
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
            let source_paths1 =
                self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Sources)?;
            let source_paths2 =
                self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Tests)?;
            let source_paths3 =
                self.load_layout_files_v2(&manifest_path, SourcePackageLayout::Scripts)?;
            if is_main_source {
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
            },
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
            self.load_project(
                &p,
                multi,
                report_err.clone(),
                false,
                targets_paths,
                dependents_paths,
            )?;
            // log::info!("dependency = '{:?}'", de);
        }
        Ok(())
    }

    pub(crate) fn load_layout_files_v2(
        &mut self,
        manifest_path: &PathBuf,
        kind: SourcePackageLayout,
    ) -> Result<Vec<PathBuf>> {
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
        // log::info!("lll << load_layout_files_v2");
        Ok(ret_paths)
    }

    // pub(crate) fn get_project_def(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
    //     use super::syntax::get_definition_in_global_env_by_move_file;
    //     let mut p = manifest_path.clone();
    //     p.push(kind.location_str());
    //     for item in WalkDir::new(&p) {
    //         let file = match item {
    //             std::result::Result::Err(_e) => continue,
    //             std::result::Result::Ok(x) => x,
    //         };
    //         if file.file_type().is_file()
    //             && match file.file_name().to_str() {
    //                 Some(s) => s.ends_with(".move"),
    //                 None => continue,
    //             }
    //         {
    //             if file
    //                 .file_name()
    //                 .to_str()
    //                 .map(|x| x.starts_with('.'))
    //                 .unwrap_or(false)
    //             {
    //                 continue;
    //             }
    //             let file_content = fs::read_to_string(file.path())
    //                 .unwrap_or_else(|_| panic!("'{:?}' can't read_to_string", file.path()));
    //             log::info!("parse source file {:?}", file.path());
    //             let file_hash = FileHash::new(file_content.as_str());

    //             let defs = get_definition_in_global_env_by_move_file(&self.global_env, file.path());
    //             let defs = match defs {
    //                 std::result::Result::Ok(x) => x,
    //                 std::result::Result::Err(diags) => {
    //                     let mut m = HashMap::new();
    //                     m.insert(
    //                         file_hash,
    //                         (
    //                             Symbol::from(file.path().to_str().unwrap()),
    //                             file_content.clone(),
    //                         ),
    //                     );
    //                     let buffer =
    //                         move_compiler::diagnostics::report_diagnostics_to_buffer(&m, diags);
    //                     let s = String::from_utf8_lossy(buffer.as_slice());
    //                     log::error!("{}", s);
    //                     continue;
    //                 },
    //             };

    //             let defs = defs.0;
    //             if kind == SourcePackageLayout::Sources {
    //                 self.modules
    //                     .get_mut(manifest_path)
    //                     .unwrap()
    //                     .as_ref()
    //                     .borrow_mut()
    //                     .sources
    //                     .insert(file.path().to_path_buf().clone(), defs);
    //             } else if kind == SourcePackageLayout::Tests {
    //                 self.modules
    //                     .get_mut(manifest_path)
    //                     .unwrap()
    //                     .as_ref()
    //                     .borrow_mut()
    //                     .tests
    //                     .insert(file.path().to_path_buf().clone(), defs);
    //             } else {
    //                 self.modules
    //                     .get_mut(manifest_path)
    //                     .unwrap()
    //                     .as_ref()
    //                     .borrow_mut()
    //                     .scripts
    //                     .insert(file.path().to_path_buf().clone(), defs);
    //             }
    //         }
    //     }
    //     // log::info!("lll << get_project_def");
    // }

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
