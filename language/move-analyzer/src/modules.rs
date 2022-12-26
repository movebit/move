// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::item::*;
use super::scopes::*;
use super::types::*;
use super::utils::*;
use anyhow::{Ok, Result};
use move_command_line_common::files::FileHash;
use move_compiler::parser::ast::Definition;
use move_compiler::shared::Identifier;
use move_compiler::CommentMap;
use move_compiler::MatchedFileCommentMap;
use move_compiler::{parser::ast::*, shared::*};
use move_core_types::account_address::*;
use move_ir_types::location::Loc;
use move_ir_types::location::Spanned;
use move_package::source_package::layout::SourcePackageLayout;
use move_package::source_package::manifest_parser::*;
use move_symbol_pool::Symbol;
use std::collections::HashMap;
use std::path::PathBuf;
use walkdir::WalkDir;

/// All Modules.
#[derive(Default, Debug)]
pub struct Modules {
    pub(crate) modules:
        HashMap<PathBuf /* this is a Move.toml like xxxx/Move.toml  */, IDEModule>,
    /// a field contains the root manifest file
    /// if Modules construct successful this field is never None.
    pub(crate) root_manifest: Option<move_package::source_package::parsed_manifest::SourceManifest>,
    pub(crate) hash_file: PathBufHashMap,
    pub(crate) file_line_mapping: FileLineMapping,
    pub(crate) manifests: Vec<PathBuf>,
    comments: HashMap<PathBuf, MatchedFileCommentMap>,
}

impl Modules {
    pub fn new(root_dir: impl Into<PathBuf>) -> Self {
        let working_dir = root_dir.into();
        log::info!("scan modules at {:?}", &working_dir);
        let mut modules = Self::default();
        modules.load_project(&working_dir).unwrap();
        modules
    }

    pub fn update_defs(&mut self, file_path: &PathBuf, file_contents: &str) {
        use super::syntax::parse_file_string;
        let file_hash = FileHash::new(file_contents);
        let mut env = CompilationEnv::new(Flags::testing());
        let defs = parse_file_string(&mut env, file_hash, file_contents);
        let defs = match defs {
            std::result::Result::Ok(x) => x,
            std::result::Result::Err(d) => {
                log::error!("update file failed,err:{:?}", d);
                return;
            }
        };
        let (defs, comment_map) = defs;
        self.comments.insert(file_path.clone(), comment_map);
        let mut manifest = None;
        // Find the right manifest file.
        for m in self.modules.keys() {
            let mut x = |layout: SourcePackageLayout| -> bool {
                let mut p = m.clone();
                p.push(layout.location_str()); // push source layout.
                p.push(file_path.file_name().unwrap()); //
                if p == file_path.clone() {
                    manifest = Some((m.clone(), layout));
                    true
                } else {
                    false
                }
            };
            if x(SourcePackageLayout::Sources) {
                break;
            }
            if x(SourcePackageLayout::Tests) {
                break;
            }
            if x(SourcePackageLayout::Scripts) {
                break;
            }
        }
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
        if layout == SourcePackageLayout::Sources {
            &mut self.modules.get_mut(&manifest).unwrap().sources
        } else if layout == SourcePackageLayout::Scripts {
            &mut self.modules.get_mut(&manifest).unwrap().scripts
        } else {
            &mut self.modules.get_mut(&manifest).unwrap().tests
        }
        .insert(file_path.clone(), defs);
        self.hash_file.update(file_path.clone(), file_hash);
        self.file_line_mapping
            .update(file_path.clone(), file_contents);
    }
    pub(crate) fn load_project(&mut self, manifest_path: &PathBuf) -> Result<()> {
        let manifest_path = normal_path(&manifest_path.as_path());
        if self.modules.get(&manifest_path).is_some() {
            log::info!("manifest '{:?}' loaded before skipped.", &manifest_path);
            return Ok(());
        }
        self.manifests.push(manifest_path.clone());
        log::info!("load manifest file at {:?}", &manifest_path);
        let manifest = parse_move_manifest_from_file(&manifest_path).unwrap();
        if self.root_manifest.is_none() {
            self.root_manifest = Some(manifest.clone());
        }
        // load depends.
        for (dep_name, de) in manifest
            .dependencies
            .iter()
            .chain(manifest.dev_dependencies.iter())
        {
            let p = path_concat(manifest_path.as_path(), de.local.as_path());
            log::info!(
                "load dependency for '{:?}' dep_name '{}'",
                &manifest_path,
                dep_name
            );
            self.load_project(&p).unwrap();
        }
        self.load_layout_files(&manifest_path, SourcePackageLayout::Sources);
        self.load_layout_files(&manifest_path, SourcePackageLayout::Tests);
        self.load_layout_files(&manifest_path, SourcePackageLayout::Scripts);

        Ok(())
    }

    /// Load move files  locate in sources and tests ...
    pub(crate) fn load_layout_files(&mut self, manifest_path: &PathBuf, kind: SourcePackageLayout) {
        use super::syntax::parse_file_string;
        use std::fs;
        let mut env = CompilationEnv::new(Flags::testing());
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
                let file_content = fs::read_to_string(file.path()).unwrap();
                log::info!("load source file {:?}", file.path());
                let file_hash = FileHash::new(file_content.as_str());
                // This is a move file.
                let defs = parse_file_string(&mut env, file_hash, file_content.as_str());
                let defs = match defs {
                    std::result::Result::Ok(x) => x,
                    std::result::Result::Err(e) => {
                        log::error!("parse file failed:{:?} d:{:?}", file.path(), e);
                        continue;
                    }
                };
                let comments_map = defs.1;
                self.comments
                    .insert(PathBuf::from(file.path()), comments_map);
                let defs = defs.0;

                if self.modules.get(manifest_path).is_none() {
                    self.modules
                        .insert(manifest_path.clone(), Default::default());
                }
                if kind == SourcePackageLayout::Sources {
                    &mut self.modules.get_mut(manifest_path).unwrap().sources
                } else if kind == SourcePackageLayout::Tests {
                    &mut self.modules.get_mut(manifest_path).unwrap().tests
                } else {
                    &mut self.modules.get_mut(manifest_path).unwrap().scripts
                }
                .insert(file.path().clone().to_path_buf(), defs);

                // update hash
                self.hash_file.update(file.path().to_path_buf(), file_hash);
                // update line mapping.
                self.file_line_mapping
                    .update(file.path().to_path_buf(), file_content.as_str());
            }
        }
    }

    pub(crate) fn with_module_member(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &ModuleMember),
    ) {
        let empty = Default::default();
        for (_, m) in if kind == SourcePackageLayout::Sources {
            &self.modules.get(manifest).unwrap_or(&empty).sources
        } else if kind == SourcePackageLayout::Tests {
            &self.modules.get(manifest).unwrap_or(&empty).tests
        } else {
            unreachable!()
        }
        .iter()
        {
            for d in m.iter() {
                match d {
                    Definition::Module(module) => {
                        for m in module.members.iter() {
                            call_back(
                                self.get_module_addr(module.address, module),
                                module.name.0.value,
                                m,
                            );
                        }
                    }
                    Definition::Address(a) => {
                        for module in a.modules.iter() {
                            for m in module.members.iter() {
                                call_back(
                                    self.get_module_addr(module.address, module),
                                    module.name.0.value,
                                    m,
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    pub(crate) fn with_module(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, &ModuleDefinition),
    ) {
        let empty = Default::default();
        for (_, m) in if kind == SourcePackageLayout::Sources {
            &self.modules.get(manifest).unwrap_or(&empty).sources
        } else if kind == SourcePackageLayout::Tests {
            &self.modules.get(manifest).unwrap_or(&empty).tests
        } else {
            unreachable!()
        }
        .iter()
        {
            for d in m.iter() {
                match d {
                    Definition::Module(module) => {
                        call_back(self.get_module_addr(module.address, module), module);
                    }
                    Definition::Address(a) => {
                        for module in a.modules.iter() {
                            call_back(self.get_module_addr(module.address, module), module);
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    pub(crate) fn with_const(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &Constant),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Constant(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }

    pub(crate) fn with_struct(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Struct(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }

    pub(crate) fn with_script(&self, manifest: &PathBuf, mut call_back: impl FnMut(&Script)) {
        for (_, scripts) in self
            .modules
            .get(manifest)
            .unwrap_or(&Default::default())
            .scripts
            .iter()
        {
            for script in scripts.iter() {
                match script {
                    Definition::Module(_) => log::error!("module definition in script."),
                    Definition::Address(_) => log::error!("address definition in script."),
                    Definition::Script(s) => {
                        call_back(s);
                    }
                }
            }
        }
    }

    pub(crate) fn with_use_decl(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &UseDecl),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Use(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    pub(crate) fn with_function(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &Function),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Function(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    pub(crate) fn with_friend(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &FriendDecl),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Friend(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }

    pub(crate) fn with_spec(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, &SpecBlock),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Spec(c) => call_back(addr, module_name, c),
            _ => {}
        });
    }
    pub(crate) fn with_spec_schema(
        &self,
        manifest: &PathBuf,
        kind: SourcePackageLayout,
        mut call_back: impl FnMut(AccountAddress, Symbol, Name, &SpecBlock),
    ) {
        self.with_module_member(manifest, kind, |addr, module_name, member| match member {
            ModuleMember::Spec(c) => match &c.value.target.value {
                SpecBlockTarget_::Schema(name, _) => {
                    call_back(addr, module_name, name.clone(), c);
                }
                _ => {}
            },
            _ => {}
        });
    }

    pub(crate) fn name_to_addr_impl(&self, name: Symbol) -> AccountAddress {
        if let Some(ref x) = self.root_manifest {
            if let Some(ref x) = x.dev_address_assignments {
                match x.get(&name) {
                    Some(x) => return x.clone(),
                    None => {}
                }
            }
            if let Some(ref x) = x.addresses {
                match x.get(&name) {
                    Some(x) => match x {
                        Some(x) => return x.clone(),
                        _ => {}
                    },
                    None => {}
                }
            }
        }
        return ERR_ADDRESS;
    }

    pub(crate) fn get_spec_build_in_call_type(
        &self,
        scopes: &Scopes,
        name: &NameAccessChain,
        type_args: &Option<Vec<Type>>,
        exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> Option<ResolvedType> {
        let b = match &name.value {
            NameAccessChain_::One(name) => SpecBuildInFun::from_symbol(name.value),
            NameAccessChain_::Two(_, _) => return None,
            NameAccessChain_::Three(_, _) => return None,
        }?;

        let exprs_types: Vec<_> = exprs
            .value
            .iter()
            .map(|e| self.get_expr_type(e, scopes))
            .collect();
        // vec<T>(x): vector<T> returns a singleton vector.
        // A lot of those build in function.
        let t_in_vector = exprs_types
            .get(0)
            .map(|x| x.clone())
            .map(|x| match x {
                ResolvedType::Vec(x) => x.as_ref().clone(),
                _ => x,
            })
            .unwrap_or(ResolvedType::new_unknown());
        let first_t = exprs_types
            .get(0)
            .map(|x| x.clone())
            .unwrap_or(ResolvedType::new_unknown());

        Some(match b {
            SpecBuildInFun::Exists => ResolvedType::new_build_in(BuildInType::Bool),
            SpecBuildInFun::Global => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        scopes.resolve_type(ty, self)
                    } else {
                        ResolvedType::new_unknown()
                    }
                } else {
                    ResolvedType::new_unknown()
                }
            }
            SpecBuildInFun::Len => ResolvedType::new_build_in(BuildInType::NumType),
            SpecBuildInFun::Update => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
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
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
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
                        ResolvedType::new_vector(scopes.resolve_type(ty, self))
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
            SpecBuildInFun::SpecDomain => first_t,
        })
    }

    /// If this is a build function call like `move_to`.
    pub(crate) fn get_move_build_in_call_type(
        &self,
        scopes: &Scopes,
        name: &NameAccessChain,
        type_args: &Option<Vec<Type>>,
        _exprs: &Spanned<Vec<Exp>>, // TODO need use _expr.
    ) -> Option<ResolvedType> {
        //
        let b = match &name.value {
            NameAccessChain_::One(name) => MoveBuildInFun::from_symbol(name.value),
            NameAccessChain_::Two(_, _) => return None,
            NameAccessChain_::Three(_, _) => return None,
        }?;

        match b {
            MoveBuildInFun::MoveTo => Some(ResolvedType::new_unit()),
            MoveBuildInFun::MoveFrom => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        let ty = scopes.resolve_type(ty, self);
                        Some(ty)
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            MoveBuildInFun::BorrowGlobalMut | MoveBuildInFun::BorrowGlobal => {
                if let Some(type_args) = type_args {
                    if let Some(ty) = type_args.get(0) {
                        let ty = scopes.resolve_type(ty, self);
                        Some(ResolvedType::new_ref(
                            b == MoveBuildInFun::BorrowGlobalMut,
                            ty,
                        ))
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            MoveBuildInFun::Exits => Some(ResolvedType::new_build_in(BuildInType::Bool)),
        }
    }

    /// Get A Type for expr if possible otherwise Unknown is return.
    pub(crate) fn get_expr_type(&self, expr: &Exp, scopes: &Scopes) -> ResolvedType {
        match &expr.value {
            Exp_::Value(ref x) => match &x.value {
                Value_::Address(_) => ResolvedType::new_build_in(BuildInType::Address),
                Value_::Num(_) => ResolvedType::new_build_in(BuildInType::NumType),
                Value_::Bool(_) => ResolvedType::new_build_in(BuildInType::Bool),
                Value_::HexString(_) | Value_::ByteString(_) => {
                    ResolvedType::new_build_in(BuildInType::String)
                }
            },
            Exp_::Move(x) | Exp_::Copy(x) => scopes.find_var_type(x.0.value),
            Exp_::Name(name, _ /*  TODO this is a error. */) => {
                let ty = scopes.find_name_chain_type(name, self);
                return ty;
            }
            Exp_::Call(name, is_macro, ref type_args, exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(name);
                    match c {
                        MacroCall::Assert => return ResolvedType::new_unit(),
                    }
                }
                if let Some(_) = scopes.under_spec() {
                    if let Some(ty) =
                        self.get_spec_build_in_call_type(scopes, name, type_args, exprs)
                    {
                        return ty;
                    }
                }

                if let Some(ty) = self.get_move_build_in_call_type(scopes, name, type_args, exprs) {
                    return ty;
                }
                let fun_type = scopes.find_name_chain_type(name, self);

                match &fun_type {
                    ResolvedType::Fun(x) => {
                        let type_parameters = &x.type_parameters;
                        let parameters = &x.parameters;
                        let type_args: Option<Vec<ResolvedType>> =
                            if let Some(type_args) = type_args {
                                Some(
                                    type_args
                                        .iter()
                                        .map(|x| scopes.resolve_type(x, self))
                                        .collect(),
                                )
                            } else {
                                None
                            };
                        let mut fun_type = fun_type.clone();
                        let mut types = HashMap::new();
                        if let Some(ref ts) = type_args {
                            for (para, args) in type_parameters.iter().zip(ts.iter()) {
                                types.insert(para.0.value, args.clone());
                            }
                        } else if type_parameters.len() > 0 {
                            //
                            let exprs_types: Vec<_> = exprs
                                .value
                                .iter()
                                .map(|e| self.get_expr_type(e, scopes))
                                .collect();
                            infer_type_parameter_on_expression(
                                &mut types,
                                &parameters.iter().map(|(_, t)| t.clone()).collect(),
                                &exprs_types,
                            );
                        }
                        fun_type.bind_type_parameter(&types, scopes);
                        match &fun_type {
                            ResolvedType::Fun(x) => x.ret_type.as_ref().clone(),
                            _ => unreachable!(),
                        }
                    }
                    // This maybe is a error.
                    _ => return UNKNOWN_TYPE.clone(),
                }
            }
            Exp_::Pack(name, type_args, fields) => {
                let struct_ty = scopes.find_name_chain_type(name, self);
                let mut struct_ty = struct_ty.struct_ref_to_struct(scopes);
                let mut types = HashMap::new();
                let mut struct_ty = match &struct_ty {
                    ResolvedType::Struct(ItemStruct {
                        name: _,
                        type_parameters,
                        type_parameters_ins: _,
                        fields: struct_fields,
                    }) => {
                        let type_args: Option<Vec<ResolvedType>> =
                            if let Some(type_args) = type_args {
                                Some(
                                    type_args
                                        .iter()
                                        .map(|x| scopes.resolve_type(x, self))
                                        .collect(),
                                )
                            } else {
                                None
                            };
                        if type_args.is_none() {
                            // try info on field.
                            let fields_exprs: Vec<_> = fields
                                .iter()
                                .map(|(field, expr)| {
                                    (field.clone(), self.get_expr_type(expr, scopes))
                                })
                                .collect();
                            let fields_exp_map = {
                                let mut m = HashMap::new();
                                fields_exprs.iter().for_each(|(f, t)| {
                                    m.insert(f.0.value, t.clone());
                                });
                                m
                            };
                            let parameters: Vec<_> =
                                struct_fields.iter().map(|(_, ty)| ty.clone()).collect();
                            let expression_types: Vec<_> = struct_fields
                                .iter()
                                .map(|(f, _)| {
                                    fields_exp_map
                                        .get(&f.0.value)
                                        .unwrap_or(&UNKNOWN_TYPE)
                                        .clone()
                                })
                                .collect();

                            infer_type_parameter_on_expression(
                                &mut types,
                                &parameters,
                                &expression_types,
                            )
                        }
                        if let Some(ref ts) = type_args {
                            for (para, args) in type_parameters.iter().zip(ts.iter()) {
                                types.insert(para.name.value, args.clone());
                            }
                        }
                        struct_ty.bind_type_parameter(&types, scopes);

                        struct_ty
                    }
                    _ => UNKNOWN_TYPE.clone(),
                };
                match &mut struct_ty {
                    ResolvedType::Struct(ItemStruct {
                        name: _,
                        ref type_parameters,
                        ref mut type_parameters_ins,
                        fields: _,
                    }) => {
                        let ins: Vec<ResolvedType> = type_parameters
                            .iter()
                            .map(|x| types.get(&x.name.value).unwrap_or(&UNKNOWN_TYPE).clone())
                            .collect();
                        let _ = std::mem::replace(type_parameters_ins, ins);
                    }
                    _ => {}
                };

                struct_ty
            }
            Exp_::Vector(loc, ty, exprs) => {
                let mut ty = if let Some(ty) = ty {
                    if let Some(ty) = ty.get(0) {
                        Some(scopes.resolve_type(ty, self))
                    } else {
                        None
                    }
                } else {
                    None
                };
                if option_ty_is_valid(&ty) {
                    for e in exprs.value.iter() {
                        let ty2 = self.get_expr_type(e, scopes);
                        if !ty2.is_unknown() {
                            ty = Some(ty2);
                            break;
                        }
                    }
                }
                ty.unwrap_or(ResolvedType::new_unknown())
            }
            Exp_::IfElse(_, then_, else_) => {
                let mut ty = self.get_expr_type(then_.as_ref(), scopes);
                if ty.is_err() {
                    if let Some(else_) = else_ {
                        ty = self.get_expr_type(else_, scopes);
                    }
                }
                ty
            }
            Exp_::While(_, _) | Exp_::Loop(_) => ResolvedType::new_unit(),
            Exp_::Block(b) => {
                if let Some(expr) = b.3.as_ref() {
                    scopes.enter_scope(|scopes| {
                        let mut visitor = DummyVisitor;
                        self.visit_block(&b, scopes, &mut visitor);
                        self.get_expr_type(expr, scopes)
                    })
                } else {
                    ResolvedType::new_unit()
                }
            }
            Exp_::Lambda(_, _) => {
                // TODO.

                ResolvedType::new_unknown()
            }
            Exp_::Quant(_, _, _, _, _) => {
                // TODO.
                ResolvedType::new_unknown()
            }
            Exp_::ExpList(e) => {
                let tys: Vec<_> = e.iter().map(|x| self.get_expr_type(x, scopes)).collect();
                ResolvedType::Multiple(tys)
            }
            Exp_::Unit => ResolvedType::new_unit(),
            Exp_::Assign(_, _) => ResolvedType::new_unit(),
            Exp_::Return(_) => ResolvedType::new_unit(),
            Exp_::Abort(_) => ResolvedType::new_unit(),
            Exp_::Break => ResolvedType::new_unit(),
            Exp_::Continue => ResolvedType::new_unit(),
            Exp_::Dereference(e) => {
                let ty = self.get_expr_type(e, scopes);
                match &ty {
                    ResolvedType::Ref(_, t) => t.as_ref().clone(),
                    _ => ty,
                }
            }
            Exp_::UnaryExp(_, e) => {
                let ty = self.get_expr_type(e, scopes);
                ty
            }
            Exp_::BinopExp(left, op, right) => {
                let left_ty = self.get_expr_type(left, scopes);
                let right_ty = self.get_expr_type(right, scopes);
                let pick = |prefer_left: bool| {
                    if prefer_left && !left_ty.is_err() {
                        left_ty.clone()
                    } else {
                        right_ty.clone()
                    }
                };
                match op.value {
                    BinOp_::Add => pick(true),
                    BinOp_::Sub => pick(true),
                    BinOp_::Mul => pick(true),
                    BinOp_::Mod => pick(true),
                    BinOp_::Div => pick(true),
                    BinOp_::BitOr => pick(true),
                    BinOp_::BitAnd => pick(true),
                    BinOp_::Xor => pick(true),
                    BinOp_::Shl => pick(true),
                    BinOp_::Shr => pick(true),
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
                let ty = self.get_expr_type(e, scopes);
                ResolvedType::new_ref(*is_mut, ty)
            }
            Exp_::Dot(e, name) => {
                let ty = self.get_expr_type(e, scopes);
                if let Some(field) = ty.find_filed_by_name(name.value) {
                    field.1.clone()
                } else {
                    ty
                }
            }
            Exp_::Index(e, _index) => {
                let ty = self.get_expr_type(e, scopes);
                let ty = match &ty {
                    ResolvedType::Ref(_, x) => x.as_ref().clone(),
                    _ => ty,
                };
                match &ty {
                    ResolvedType::Vec(x) => x.as_ref().clone(),
                    _ => ty,
                }
            }

            Exp_::Cast(_, ty) => {
                let ty = scopes.resolve_type(ty, self);
                ty
            }
            Exp_::Annotate(_, ty) => scopes.resolve_type(ty, self),
            Exp_::Spec(_) => ResolvedType::new_unit(),
            Exp_::UnresolvedError => {
                // Nothings. didn't know what to do.
                ResolvedType::new_unknown()
            }
        }
    }

    pub(crate) fn visit_struct_tparam(
        &self,
        t: &StructTypeParameter,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        self.visit_tparam(&(t.name.clone(), t.constraints.clone()), scopes, visitor);
    }

    pub(crate) fn visit_tparam(
        &self,
        t: &(Name, Vec<Ability>),
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        let (name, v) = t;
        let item = ItemOrAccess::Item(Item::TParam(name.clone(), v.clone()));
        visitor.handle_item(self, scopes, &item);
        if visitor.finished() {
            return;
        }
        // Enter this.
        scopes.enter_types(self, name.value, item);
    }
    pub(crate) fn visit_signature(
        &self,
        signature: &FunctionSignature,
        scopes: &Scopes,
        visitor: &mut dyn ScopeVisitor,
    ) {
        for t in signature.type_parameters.iter() {
            self.visit_tparam(t, scopes, visitor);
            if visitor.finished() {
                return;
            }
        }
        for (v, t) in signature.parameters.iter() {
            self.visit_type_apply(t, scopes, visitor);
            let t = scopes.resolve_type(t, self);
            let item = ItemOrAccess::Item(Item::Parameter(v.clone(), t));
            // found
            visitor.handle_item(self, scopes, &item);
            if visitor.finished() {
                return;
            }
            scopes.enter_item(self, v.value(), item)
        }
        self.visit_type_apply(&signature.return_type, scopes, visitor);
    }
}

/// Check is option is Some and ResolvedType is not unknown and not a error.
fn option_ty_is_valid(x: &Option<ResolvedType>) -> bool {
    if let Some(ref x) = x {
        !x.is_err()
    } else {
        false
    }
}

#[derive(Debug, Clone, Default)]
pub struct IDEModule {
    sources: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    /*
        TODO tests.
    */
    tests: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,

    scripts: HashMap<
        PathBuf, /*  file path  xxxx/abc.move  */
        Vec<move_compiler::parser::ast::Definition>,
    >,
}

pub(crate) const UNKNOWN_TYPE: ResolvedType = ResolvedType::new_unknown();

pub(crate) fn get_name_from_value(v: &Value) -> Option<&Name> {
    match &v.value {
        Value_::Address(ref x) => match &x.value {
            LeadingNameAccess_::AnonymousAddress(_) => None,
            LeadingNameAccess_::Name(ref name) => Some(name),
        },
        _ => None,
    }
}

pub(crate) fn infer_type_parameter_on_expression(
    ret: &mut HashMap<Symbol /*  name like T or ... */, ResolvedType>,
    parameters: &Vec<ResolvedType>,
    expression_types: &Vec<ResolvedType>,
) {
    for (p, expr_type) in parameters.iter().zip(expression_types.iter()) {
        bind(ret, &p, expr_type);
    }
    fn bind(
        ret: &mut HashMap<Symbol, ResolvedType>,
        // may be a type have type parameter.
        parameter_type: &ResolvedType,
        // a type that is certain.
        expr_type: &ResolvedType,
    ) {
        match &parameter_type {
            ResolvedType::UnKnown => {}
            ResolvedType::Struct(ItemStruct { fields, .. }) => match &expr_type {
                ResolvedType::Struct(ItemStruct {
                    fields: fields2, ..
                }) => {
                    for (l, r) in fields.iter().zip(fields2.iter()) {
                        bind(ret, &l.1, &r.1);
                    }
                }
                _ => {}
            },
            ResolvedType::BuildInType(_) => {}
            ResolvedType::TParam(name, _) => {
                ret.insert(name.value, expr_type.clone());
            }
            ResolvedType::ApplyTParam(_, _, _) => {}
            ResolvedType::Ref(_, l) => match &expr_type {
                ResolvedType::Ref(_, r) => bind(ret, l.as_ref(), r.as_ref()),
                _ => {}
            },
            ResolvedType::Unit => {}
            ResolvedType::Multiple(x) => match &expr_type {
                ResolvedType::Multiple(y) => {
                    for (index, l) in x.iter().enumerate() {
                        if let Some(r) = y.get(index) {
                            bind(ret, l, r);
                        } else {
                            break;
                        }
                    }
                }
                _ => {}
            },
            //  function is not expression
            ResolvedType::Fun(_) => {}
            ResolvedType::Vec(x) => match &expr_type {
                ResolvedType::Vec(y) => {
                    bind(ret, x.as_ref(), y.as_ref());
                }
                _ => {}
            },
            ResolvedType::ResolvedFailed(_) => {}
            ResolvedType::StructRef(_, _, _, _, _) => {}
            ResolvedType::Range => {}
        }
    }
}

pub trait ConvertLoc {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<&'_ PathBuf>;
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange>;
}

impl ConvertLoc for Modules {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<&'_ PathBuf> {
        self.hash_file.get_path(hash)
    }
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
        self.convert_file_hash_filepath(&loc.file_hash())
            .map(|file| {
                self.file_line_mapping
                    .translate(file, loc.start(), loc.end())
            })
            .flatten()
    }
}

pub trait Name2Addr {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress;
}
impl Name2Addr for Modules {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress {
        self.name_to_addr_impl(name)
    }
}

/// Visit scopes for inner to outer.
pub trait ScopeVisitor: std::fmt::Display {
    /// Handle this item.
    /// If `should_finish` return true. All `enter_scope` and enter_scope called function will return.
    fn handle_item(
        &mut self,
        services: &dyn HandleItemService,
        scopes: &Scopes,
        item: &ItemOrAccess,
    );
    /// Need not visit this structure???
    fn file_should_visit(&self, p: &PathBuf) -> bool;
    /// Visitor should finished.
    fn finished(&self) -> bool;
}

pub trait HandleItemService: ConvertLoc + ListAllNameSpace + Name2Addr {}
impl HandleItemService for Modules {}

#[allow(dead_code)]
pub(crate) struct Ending {
    pub(crate) msg: String,
}
impl Drop for Ending {
    fn drop(&mut self) {
        log::info!("ending {}", self.msg.as_str());
    }
}

#[derive(Debug)]
pub(crate) struct DummyVisitor;

impl ScopeVisitor for DummyVisitor {
    fn handle_item(
        &mut self,
        _services: &dyn HandleItemService,
        _scopes: &Scopes,
        _item: &ItemOrAccess,
    ) {
    }
    fn file_should_visit(&self, _p: &PathBuf) -> bool {
        unreachable!();
    }
    fn finished(&self) -> bool {
        false
    }
}

impl std::fmt::Display for DummyVisitor {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}
pub(crate) static ERR_ADDRESS: AccountAddress = AccountAddress::ONE;

pub trait ListAllNameSpace {
    fn list_all_name_spaces(&self) -> Vec<(Symbol, AccountAddress)>;
}

impl ListAllNameSpace for Modules {
    fn list_all_name_spaces(&self) -> Vec<(Symbol, AccountAddress)> {
        let mut ret = Vec::new();
        let empty = Default::default();
        let empty2 = Default::default();
        let x = self.root_manifest.as_ref().unwrap();
        for (name, addr) in x.addresses.as_ref().unwrap_or(&empty).iter() {
            ret.push((name.clone(), addr.clone().unwrap_or(AccountAddress::ONE)));
        }
        for (name, addr) in x.dev_address_assignments.as_ref().unwrap_or(&empty2).iter() {
            ret.push((name.clone(), addr.clone()));
        }
        ret
    }
}
