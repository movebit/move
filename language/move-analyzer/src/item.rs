// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0
use enum_iterator::Sequence;
use move_core_types::account_address::AccountAddress;

use move_command_line_common::files::FileHash;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
// use std::str::FromStr;

#[derive(Clone)]
pub struct ItemStruct {
    // pub(crate) name: StructName,
    // pub(crate) type_parameters: Vec<StructTypeParameter>,
    // pub(crate) type_parameters_ins: Vec<ResolvedType>,
    // pub(crate) fields: Vec<(Field, ResolvedType)>, /* TODO If this length is zero,maybe a native. */
    pub(crate) is_test: bool,
    pub(crate) addr: AccountAddress,
    pub(crate) module_name: Symbol,
}

impl ItemStruct {
    pub(crate) fn to_struct_ref(&self) -> ItemStructNameRef {
        ItemStructNameRef {
            addr: self.addr,
            module_name: self.module_name,
            // name: self.name,
            // type_parameters: self.type_parameters.clone(),
            is_test: self.is_test,
        }
    }
}

impl ItemStruct {
    pub(crate) fn bind_type_parameter() {}
}

impl std::fmt::Display for ItemStruct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "")
    }
}

#[derive(Clone)]
pub enum Item {
    // Parameter(Var, ResolvedType),
    Const(ItemConst),
    // Var {
    //     var: Var,
    //     ty: ResolvedType,
    //     lambda: Option<LambdaExp>,
    //     has_decl_ty: bool,
    // },
    // Field(Field, ResolvedType),
    Struct(ItemStruct),
    StructNameRef(ItemStructNameRef),
    Fun(ItemFun),
    MoveBuildInFun(MoveBuildInFun),
    SpecBuildInFun(SpecBuildInFun),
    SpecConst(ItemConst),
    /// build in types.
    // BuildInType(BuildInType),
    // TParam(Name, Vec<Ability>),
    // SpecSchema(Name, HashMap<Symbol, (Name, ResolvedType)>),
    /// a module name in 0x1111::module_name
    ModuleName(ItemModuleName),
    Use(Vec<ItemUse>),
    Dummy,
}

// #[derive(Clone)]
// pub struct LambdaExp {
//     pub(crate) bind_list: BindList,
//     pub(crate) exp: Exp,
// }

#[derive(Clone)]
pub enum ItemUse {
    Module(ItemUseModule),
    Item(ItemUseItem),
}

#[derive(Clone)]
pub struct ItemUseModule {
    // pub(crate) module_ident: ModuleIdent, // 0x111::xxxx
    // pub(crate) alias: Option<ModuleName>, // alias
    // pub(crate) members: Rc<RefCell<ModuleScope>>, // module scope.
    // pub(crate) s: Option<Name>, // Option Self
    #[allow(dead_code)]
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemUseItem {
    // pub(crate) module_ident: ModuleIdent, /* access name */
    // pub(crate) name: Name,
    // pub(crate) alias: Option<Name>, /* alias  */
    // pub(crate) members: Rc<RefCell<ModuleScope>>,
    #[allow(dead_code)]
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemModuleName {
    // pub(crate) name: ModuleName,
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemStructNameRef {
    pub(crate) addr: AccountAddress,
    pub(crate) module_name: Symbol,
    // pub(crate) name: StructName,
    // pub(crate) type_parameters: Vec<StructTypeParameter>,
    pub(crate) is_test: bool,
}

#[derive(Clone)]
pub struct ItemFun {
    // pub(crate) name: FunctionName,
    // pub(crate) type_parameters: Vec<(Name, Vec<Ability>)>,
    // pub(crate) parameters: Vec<(Var, ResolvedType)>,
    // pub(crate) ret_type: Box<ResolvedType>,
    // pub(crate) ret_type_unresolved: Type,
    pub(crate) is_spec: bool,
    // pub(crate) vis: Visibility,
    // pub(crate) addr_and_name: AddrAndModuleName,
    pub(crate) is_test: AttrTest,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum AttrTest {
    No,
    Test,
    TestOnly,
}

// impl AttrTest {
//     pub(crate) fn is_test(self) -> bool {
//         self == Self::Test || self == Self::TestOnly
//     }
// }

impl Item {
    pub(crate) fn def_loc(&self) -> Loc {
        match self {
            Item::Use(x) => Loc::new(FileHash::empty(), 0, 0),
            // Item::Struct(x) => x.name.loc(),
            // Item::TParam(name, _) => name.loc,
            // Item::Const(ItemConst { name, .. }) => name.loc(),
            // Item::StructNameRef(ItemStructNameRef { name, .. }) => name.0.loc,
            // Item::Fun(f) => f.name.0.loc,
            Item::Dummy => Loc::new(FileHash::empty(), 0, 0),
            // Item::ModuleName(ItemModuleName { name, .. }) => name.loc(),
            Item::MoveBuildInFun(_) => Loc::new(FileHash::empty(), 0, 0),
            Item::SpecBuildInFun(_) => Loc::new(FileHash::empty(), 0, 0),
            Item::SpecConst(_) => Loc::new(FileHash::empty(), 0, 0),
            _ => Loc::new(FileHash::empty(), 0, 0),
        }
    }

    pub(crate) fn is_build_in(&self) -> bool {
        matches!(self, Item::SpecBuildInFun(_) | Item::MoveBuildInFun(_))
    }
}

impl Default for Item {
    fn default() -> Self {
        Self::Dummy
    }
}

#[derive(Clone)]
pub struct ItemConst {
    // pub(crate) name: ConstantName,
    // pub(crate) ty: ResolvedType,
    /// only Const have this field,SpecConst ignore this field.
    pub(crate) is_test: bool,
}

// #[derive(Clone, Copy, Debug)]
// pub enum MacroCall {
//     Assert,
// }

// impl MacroCall {
//     pub(crate) fn from_chain(chain: &NameAccessChain) -> Option<Self> {
//         match &chain.value {
//             NameAccessChain_::One(name) => Self::from_symbol(name.value),
//             NameAccessChain_::Two(_, _) => None,
//             NameAccessChain_::Three(_, _) => None,
//         }
//     }
//     pub(crate) fn from_symbol(s: Symbol) -> Option<Self> {
//         match s.as_str() {
//             "assert" => Some(Self::Assert),
//             _ => None,
//         }
//     }
//     pub(crate) fn to_static_str(self) -> &'static str {
//         match self {
//             MacroCall::Assert => "assert",
//         }
//     }
// }

// impl Default for MacroCall {
//     fn default() -> Self {
//         Self::Assert
//     }
// }

/// Get the last name of a access chain.
// pub(crate) fn get_name_chain_last_name(x: &NameAccessChain) -> &Name {
//     match &x.value {
//         move_compiler::parser::ast::NameAccessChain_::One(name)
//         | move_compiler::parser::ast::NameAccessChain_::Two(_, name)
//         | move_compiler::parser::ast::NameAccessChain_::Three(_, name) => name,
//     }
// }

// impl std::fmt::Display for Item {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             // Item::Parameter(var, t) => {
//             //     write!(f, "{}:{}", var.0.value.as_str(), t)
//             // },
//             Item::ModuleName(ItemModuleName { name, .. }) => {
//                 write!(f, "{}", name.value().as_str())
//             },
//             Item::Use(x) => Ok(for x in x.iter() {
//                 match x {
//                     ItemUse::Module(ItemUseModule { module_ident, .. }) => {
//                         write!(f, "use {:?} _", module_ident)?;
//                     },
//                     ItemUse::Item(ItemUseItem {
//                         module_ident,
//                         name,
//                         alias,
//                         ..
//                     }) => {
//                         write!(
//                             f,
//                             "use {:?}::{:?} {}",
//                             module_ident,
//                             name,
//                             if let Some(alias) = alias {
//                                 format!(" as {}", alias.value.as_str())
//                             } else {
//                                 String::from_str("").unwrap()
//                             },
//                         )?;
//                     },
//                 }
//             }),

//             // Item::Const(ItemConst { name, ty, .. }) => {
//             //     write!(f, "{}:{}", name.0.value.as_str(), ty)
//             // },
//             // Item::SpecConst(ItemConst { name, ty, .. }) => {
//             //     write!(f, "{}:{}", name.0.value.as_str(), ty)
//             // },
//             Item::Struct(s) => {
//                 write!(f, "{}", s)
//             },
//             // Item::StructNameRef(ItemStructNameRef { name, .. }) => {
//             //     write!(f, "{}", name.value().as_str())
//             // },
//             // Item::Fun(x) => write!(f, "{}", x),
//             // Item::BuildInType(x) => {
//             //     write!(f, "{}", x.to_static_str())
//             // },
//             Item::TParam(tname, abilities) => {
//                 write!(f, "{}:", tname.value.as_str())?;
//                 for i in 0..abilities.len() {
//                     let x = abilities.get(i).unwrap();
//                     write!(f, "{:?},", x.value)?;
//                 }
//                 std::result::Result::Ok(())
//             },
//             // Item::Var { var, ty, .. } => {
//             //     write!(f, "{}:{}", var.0.value.as_str(), ty)
//             // },
//             // Item::Field(x, ty) => {
//             //     write!(f, "{}:{}", x.0.value.as_str(), ty)
//             // },
//             Item::Dummy => {
//                 write!(f, "dummy")
//             },
//             // Item::SpecSchema(name, _) => {
//             //     write!(f, "{}", name.value.as_str())
//             // },
//             Item::MoveBuildInFun(x) => write!(f, "move_build_in_fun {}", x.to_static_str()),
//             Item::SpecBuildInFun(x) => write!(f, "spec_build_in_fun {}", x.to_static_str()),
//             _ => write!(f, ""),
//         }
//     }
// }

#[derive(Clone)]
pub enum Access {
    // ApplyType(NameAccessChain, Option<ModuleName>, Box<ResolvedType>),
    // ExprVar(Var, Box<Item>),
    // ExprAccessChain(NameAccessChain, Option<AddrAndModuleName>, Box<Item>),
    // Maybe the same as ExprName.
    // ExprAddressName(Name),
    AccessFiled(AccessFiled),
    ///////////////
    /// key words
    KeyWords(&'static str),
    /////////////////
    // MacroCall(MacroCall, NameAccessChain),
    // Friend(NameAccessChain, ModuleName),

    // ApplySchemaTo(
    //     NameAccessChain, // Apply a schema to a item.
    //     Box<Item>,
    // ),
    // IncludeSchema(NameAccessChain, Box<Item>),
    // PragmaProperty(PragmaProperty),
    // SpecFor(Name, Box<Item>),
}

#[derive(Clone)]
pub struct AccessFiled {
    // pub(crate) from: Field,
    // pub(crate) to: Field,
    // #[allow(dead_code)]
    // pub(crate) ty: ResolvedType,
    // pub(crate) all_fields: HashMap<Symbol, (Name, ResolvedType)>,
    /// When dealing with below syntax can have this.
    /// ```move
    /// let x = XXX {x}
    ///```
    /// x is alas a field and a expr.
    /// and a expr can link to a item.
    pub(crate) item: Option<Item>,
    /// Does this field access contains a ref
    /// like &xxx.yyy
    pub(crate) has_ref: Option<bool>,
}

// impl Access {
//     pub(crate) fn access_def_loc(&self) -> (Loc /* access loc */, Loc /* def loc */) {
//         match self {
//             // Access::ApplyType(name, _, x) => {
//             //     (get_name_chain_last_name(name).loc, x.as_ref().def_loc())
//             // },
//             Access::ExprVar(var, x) => (var.loc(), x.def_loc()),
//             // Access::ExprAccessChain(name, _, item) => {
//             //     (get_name_chain_last_name(name).loc, item.as_ref().def_loc())
//             // },
//             Access::ExprAddressName(_) => (
//                 Loc::new(FileHash::empty(), 0, 0),
//                 Loc::new(FileHash::empty(), 0, 0),
//             ),
//             Access::AccessFiled(AccessFiled { from, to, .. }) => (from.loc(), to.loc()),
//             Access::KeyWords(_) => (
//                 Loc::new(FileHash::empty(), 0, 0),
//                 Loc::new(FileHash::empty(), 0, 0),
//             ),
//             Access::MacroCall(_, chain) => (chain.loc, chain.loc),
//             Access::Friend(name, item) => (get_name_chain_last_name(name).loc, item.loc()),
//             Access::ApplySchemaTo(chain, x) => (get_name_chain_last_name(chain).loc, x.def_loc()),
//             Access::PragmaProperty(x) => (x.loc, x.loc),
//             Access::SpecFor(name, item) => (name.loc, item.as_ref().def_loc()),
//             Access::IncludeSchema(a, d) => (get_name_chain_last_name(a).loc, d.def_loc()),
//         }
//     }
// }

#[derive(Clone)]
pub enum ItemOrAccess {
    Item(Item),
    Access(Access),
}

impl From<ItemOrAccess> for Item {
    fn from(x: ItemOrAccess) -> Self {
        match x {
            ItemOrAccess::Item(x) => x,
            _ => unreachable!(),
        }
    }
}

impl From<ItemOrAccess> for Access {
    fn from(x: ItemOrAccess) -> Self {
        match x {
            ItemOrAccess::Access(x) => x,
            _ => unreachable!(),
        }
    }
}

// impl std::fmt::Display for ItemOrAccess {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Self::Access(a) => a.fmt(f),
//             Self::Item(x) => x.fmt(f),
//         }
//     }
// }

#[derive(Clone, Copy, Debug, Eq, PartialEq, Sequence)]
pub enum MoveBuildInFun {
    MoveTo,
    MoveFrom,
    BorrowGlobalMut,
    BorrowGlobal,
    Exits,
}

impl MoveBuildInFun {
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            MoveBuildInFun::MoveTo => "move_to",
            MoveBuildInFun::MoveFrom => "move_from",
            MoveBuildInFun::BorrowGlobalMut => "borrow_global_mut",
            MoveBuildInFun::BorrowGlobal => "borrow_global",
            MoveBuildInFun::Exits => "exists",
        }
    }
}

impl std::fmt::Display for MoveBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Sequence)]
pub enum SpecBuildInFun {
    Exists,
    Global,
    Len,
    Update,
    Vec,
    Concat,
    Contains,
    IndexOf,
    Range,
    InRange,
    UpdateField,
    Old,
    TRACE,
}

impl SpecBuildInFun {
    pub(crate) fn to_static_str(self) -> &'static str {
        match self {
            Self::Exists => "exists",
            Self::Global => "global",
            Self::Len => "len",
            Self::Update => "update",
            Self::Vec => "vec",
            Self::Concat => "concat",
            Self::Contains => "contains",
            Self::IndexOf => "index_of",
            Self::Range => "range",
            Self::InRange => "in_range",
            Self::UpdateField => "update_field",
            Self::Old => "old",
            Self::TRACE => "TRACE",
        }
    }
}

impl std::fmt::Display for SpecBuildInFun {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_static_str())
    }
}
