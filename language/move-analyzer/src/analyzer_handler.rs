// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{item::*, project::Project, utils::*};
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::{
        Ability, Attribute_, Attributes, Constant, Definition, Exp, FriendDecl, Function,
        LeadingNameAccess, LeadingNameAccess_, ModuleDefinition, ModuleMember, SpecBlock,
        SpecBlockTarget_, SpecConditionKind, SpecConditionKind_, StructDefinition, UseDecl, Value,
        Value_,
    },
    shared::Name,
};
use move_core_types::account_address::*;
use move_ir_types::location::*;
use move_model::model::GlobalEnv;
use move_package::source_package::layout::SourcePackageLayout;
use move_symbol_pool::Symbol;
use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    path::{Path, PathBuf},
    time::SystemTime,
};
// ======================================================================================
// static and const var
pub static ERR_ADDRESS: once_cell::sync::Lazy<AccountAddress> =
    once_cell::sync::Lazy::new(AccountAddress::random);

pub(crate) const SPEC_DOMAIN: &str = "$spec_domain";

// ======================================================================================
// enum and struct
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum AddressSpace {
    Addr(AccountAddress),
    Name(Symbol),
}

#[derive(Clone, serde::Serialize, Debug)]
pub struct FunID {
    pub(crate) addr: AccountAddress,
    pub(crate) addr_name: String,
    pub(crate) module_name: Symbol,
    pub(crate) function_name: Symbol,
}

#[derive(Debug, Clone, Default)]
pub struct SourceDefs {
    pub(crate) sources: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
    pub(crate) tests: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
    pub(crate) scripts: HashMap<PathBuf, Vec<move_compiler::parser::ast::Definition>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct DummyHandler;

#[derive(Clone)]
pub struct VecDefAstProvider<'a> {
    /// The actual Definition.
    defs: &'a Vec<Definition>,
    /// Help for convert name to addr.
    modules: &'a Project,
    layout: SourcePackageLayout,
}

#[derive(Clone)]
pub struct ModulesAstProvider<'a> {
    modules: &'a Project,
    layout: SourcePackageLayout,
    manifest_path: PathBuf,
}

// ======================================================================================
// pub func
pub fn get_spec_condition_type_parameters(
    x: &SpecConditionKind,
) -> Option<&Vec<(Name, Vec<Ability>)>> {
    match &x.value {
        SpecConditionKind_::Invariant(x)
        | SpecConditionKind_::InvariantUpdate(x)
        | SpecConditionKind_::Axiom(x) => Some(x),
        _ => None,
    }
}

pub(crate) fn file_modify_time(x: &Path) -> Option<SystemTime> {
    match x.metadata() {
        Ok(x) => match x.modified() {
            Ok(x) => Some(x),
            Err(_) => None,
        },
        Err(_) => None,
    }
}

pub(crate) fn get_name_from_value(v: &Value) -> Option<&Name> {
    match &v.value {
        Value_::Address(ref x) => match &x.value {
            LeadingNameAccess_::AnonymousAddress(_) => None,
            LeadingNameAccess_::Name(ref name) => Some(name),
        },
        _ => None,
    }
}

// ======================================================================================
// Handler a `ItemOrAccess` producced By `Project`.
pub trait ItemOrAccessHandler: std::fmt::Display {
    /// Handle this item.
    fn handle_item_or_access(&mut self, _services: &dyn HandleItemService, _item: &ItemOrAccess) {}

    /// Need visit function or spec body or not.
    /// Sometimes you want visit function body But not all the function Body.
    fn function_or_spec_body_should_visit(&self, range: &FileRange) -> bool;
    fn visit_fun_or_spec_body(&self) -> bool;

    /// Visitor should finished.
    fn finished(&self) -> bool;

    // need Expr type ??
    fn need_expr_type(&self) -> bool {
        false
    }

    // current vistor handler is inlay_hints ?
    fn current_vistor_handler_is_inlay_hints(&self) -> bool {
        false
    }

    // handle expr type.
    fn handle_expr_typ(&mut self, _exp: &Exp) {}

    fn need_call_pair(&self) -> bool {
        false
    }
    fn handle_call_pair(&mut self, _from: FunID, _to: FunID) {}
    fn need_para_arg_pair(&self) -> bool {
        false
    }
    fn handle_para_arg_pair(&mut self, _services: &dyn HandleItemService, _para: Name, _exp: &Exp) {
    }

    fn handle_project_env(
        &mut self,
        _services: &dyn HandleItemService,
        env: &GlobalEnv,
        move_file_path: &Path,
    ) {
    }
}

impl ItemOrAccessHandler for DummyHandler {
    fn handle_item_or_access(&mut self, _services: &dyn HandleItemService, _item: &ItemOrAccess) {}
    fn function_or_spec_body_should_visit(&self, _range: &FileRange) -> bool {
        false
    }
    fn finished(&self) -> bool {
        false
    }
    fn visit_fun_or_spec_body(&self) -> bool {
        false
    }
}

impl std::fmt::Display for DummyHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// ======================================================================================
// three trait
pub trait HandleItemService: ConvertLoc + Name2Addr {}
impl HandleItemService for Project {}

pub trait ConvertLoc {
    fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf>;
    fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange>;
}

impl_convert_loc!(Project);

impl From<Symbol> for AddressSpace {
    fn from(x: Symbol) -> Self {
        Self::Name(x)
    }
}

impl ToString for AddressSpace {
    fn to_string(&self) -> String {
        match self {
            AddressSpace::Addr(addr) => addr.to_hex_literal(),
            AddressSpace::Name(x) => x.as_str().to_string(),
        }
    }
}

impl From<AccountAddress> for AddressSpace {
    fn from(x: AccountAddress) -> Self {
        Self::Addr(x)
    }
}

pub trait Name2Addr {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress;
}
impl Name2Addr for Project {
    fn name_2_addr(&self, name: Symbol) -> AccountAddress {
        self.name_to_addr_impl(name)
    }
}

// ======================================================================================
// Various ast access methods.
pub trait AstProvider: Clone {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress;

    fn with_definition(&self, call_back: impl FnMut(&Definition));
    fn with_module(&self, mut call_back: impl FnMut(AccountAddress, &ModuleDefinition)) {
        self.with_definition(|x| match x {
            Definition::Module(module) => {
                call_back(self.get_module_addr(module.address, module), module);
            },
            Definition::Address(a) => {
                for module in a.modules.iter() {
                    call_back(self.get_module_addr(Some(a.addr), module), module);
                }
            },
            _ => {},
        })
    }

    fn found_in_test(&self) -> bool {
        *self.layout() == SourcePackageLayout::Tests
    }
    fn found_in_scripts(&self) -> bool {
        *self.layout() == SourcePackageLayout::Scripts
    }
    fn layout(&self) -> &SourcePackageLayout;

    fn with_module_member(
        &self,
        mut call_back: impl FnMut(
            AccountAddress,
            Symbol,
            &ModuleMember,
            bool, /* if is_spec_module */
        ),
    ) {
        self.with_definition(|x| match x {
            Definition::Module(module) => {
                for m in module.members.iter() {
                    call_back(
                        self.get_module_addr(module.address, module),
                        module.name.0.value,
                        m,
                        module.is_spec_module,
                    );
                }
            },
            Definition::Address(a) => {
                for module in a.modules.iter() {
                    for m in module.members.iter() {
                        call_back(
                            self.get_module_addr(Some(a.addr), module),
                            module.name.0.value,
                            m,
                            module.is_spec_module,
                        );
                    }
                }
            },
            _ => {},
        });
    }

    fn with_const(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Constant)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Constant(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }

    fn with_struct(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &StructDefinition)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Struct(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
    fn with_use_decl(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &UseDecl, bool)) {
        self.with_module_member(|addr, module_name, member, is_spec| {
            if let ModuleMember::Use(c) = member {
                call_back(addr, module_name, c, is_spec)
            }
        });
    }
    fn with_function(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &Function)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Function(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
    fn with_friend(&self, mut call_back: impl FnMut(AccountAddress, Symbol, &FriendDecl)) {
        self.with_module_member(|addr, module_name, member, _| {
            if let ModuleMember::Friend(c) = member {
                call_back(addr, module_name, c)
            }
        });
    }
}

impl<'a> VecDefAstProvider<'a> {
    pub(crate) fn new(
        defs: &'a Vec<Definition>,
        modules: &'a Project,
        layout: SourcePackageLayout,
    ) -> Self {
        Self {
            defs,
            modules,
            layout,
        }
    }
}

impl<'a> AstProvider for VecDefAstProvider<'a> {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        self.modules.get_module_addr(addr, m)
    }
    fn with_definition(&self, mut call_back: impl FnMut(&Definition)) {
        for d in self.defs.iter() {
            call_back(d);
        }
    }
    fn layout(&self) -> &SourcePackageLayout {
        &self.layout
    }
}

impl<'a> ModulesAstProvider<'a> {
    pub(crate) fn new(
        modules: &'a Project,
        manifest_path: PathBuf,
        kind: SourcePackageLayout,
    ) -> Self {
        Self {
            modules,
            layout: kind,
            manifest_path,
        }
    }
}

impl<'a> AstProvider for ModulesAstProvider<'a> {
    fn get_module_addr(
        &self,
        addr: Option<LeadingNameAccess>,
        m: &ModuleDefinition,
    ) -> AccountAddress {
        self.modules.get_module_addr(addr, m)
    }
    fn with_definition(&self, mut call_back: impl FnMut(&Definition)) {
        let empty = Default::default();
        let b = self
            .modules
            .modules
            .get(&self.manifest_path)
            .unwrap_or(&empty)
            .as_ref()
            .borrow();

        for (_, m) in if self.layout == SourcePackageLayout::Sources {
            &b.sources
        } else if self.layout == SourcePackageLayout::Tests {
            &b.tests
        } else if self.layout == SourcePackageLayout::Scripts {
            &b.scripts
        } else {
            unreachable!()
        }
        .iter()
        {
            for d in m.iter() {
                call_back(d);
            }
        }
    }
    fn layout(&self) -> &SourcePackageLayout {
        &self.layout
    }
}
