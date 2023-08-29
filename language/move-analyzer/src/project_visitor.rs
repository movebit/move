// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{item::*, project_context::*, types::*, utils::*, scope::*};
use crate::{project::Project, analyzer_handler::*};
use move_command_line_common::files::FileHash;
use move_compiler::{
    parser::ast::StructFields,
    parser::ast::SpecBlock,
    parser::ast::LeadingNameAccess,
    parser::ast::ModuleDefinition,
    parser::ast::UseDecl,
    parser::ast::Visibility,
    parser::ast::ModuleName,
    parser::ast::ModuleIdent,
    parser::ast::Field,
    parser::ast::SpecBlockMember,
    parser::ast::Sequence,
    parser::ast::SequenceItem,
    parser::ast::FriendDecl,
    parser::ast::Use,
    parser::ast::Constant,
    parser::ast::Function,
    parser::ast::Bind,
    parser::ast::BindList,
    parser::ast::Var,
    parser::ast::Exp,
    parser::ast::Type,
    parser::ast::Type_,
    parser::ast::Bind_,
    parser::ast::SequenceItem_,
    parser::ast::FunctionBody_,
    parser::ast::LeadingNameAccess_,
    parser::ast::SpecBlockMember_,
    parser::ast::SpecBlockTarget_,
    parser::ast::SpecApplyFragment_,
    parser::ast::Exp_,
    parser::ast::NameAccessChain_,
    shared::Identifier,
    shared::Name,
};
use move_core_types::account_address::*;
use move_ir_types::location::*;
use move_package::source_package::layout::SourcePackageLayout;
use move_symbol_pool::Symbol;
use std::{
    vec,
    cell::RefCell,
    collections::{HashMap, HashSet},
};
use std::{path::PathBuf, rc::Rc};

impl Project {
    /// Entrance for `ItemOrAccessHandler` base on analyze.
    pub fn run_full_visitor(&self, visitor: &mut dyn ItemOrAccessHandler) {
        log::info!("run visitor for {} ", visitor);
        self.project_context.clear_scopes_and_addresses();

        // visit should `rev`.
        let manifests: Vec<_> = self.manifest_paths.iter().rev().cloned().collect();
        for m in manifests.iter() {
            self.visit(
                &self.project_context,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Sources),
                true,
            );
            if visitor.finished() {
                return;
            }
            self.visit(
                &self.project_context,
                visitor,
                ModulesAstProvider::new(self, m.clone(), SourcePackageLayout::Tests),
                true,
            );
            if visitor.finished() {
                return;
            }
        }
    }

    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ItemOrAccessHandler,
        filepath: &PathBuf,
        enter_import: bool,
    ) -> anyhow::Result<()> {
        log::info!("run visitor part for {} ", visitor);
        self.get_defs(filepath, |provider| {
            self.visit(
                &self.project_context,
                visitor,
                provider.clone(),
                enter_import,
            );
        })
    }

    fn try_fix_local_var_and_visit_lambda(
        &self,
        value: &Exp,
        ty: &ResolvedType,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        let u_ty = ResolvedType::UnKnown;
        let x = match &value.value {
            Exp_::Name(name, _) => match &name.value {
                NameAccessChain_::One(name) => {
                    self.project_context.try_fix_local_var_ty(name.value, ty)
                }
                NameAccessChain_::Two(_, _) => None,
                NameAccessChain_::Three(_, _) => None,
            },
            Exp_::Lambda(b, e) => Some(LambdaExp {
                bind_list: b.clone(),
                exp: e.as_ref().clone(),
            }),
            _ => None,
        };
        if let Some(lambda) = x {
            if let ResolvedType::Lambda {
                args,
                ret_ty: _ret_ty,
            } = ty
            {
                let _guard = self.project_context.enter_scope_guard(Scope::default());
                for (index, b) in lambda.bind_list.value.iter().enumerate() {
                    self.visit_bind(
                        b,
                        match args.get(index) {
                            Some(x) => x,
                            None => &u_ty,
                        },
                        &self.project_context,
                        visitor,
                        None,
                        false,
                    );
                    if visitor.finished() {
                        return;
                    }
                }
                // visit expr body
                self.visit_expr(&lambda.exp, &self.project_context, visitor);
            }
        }
    }

    pub fn visit(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        provider: impl AstProvider,
        enter_import: bool,
    ) {
        project_context.set_access_env(Default::default());
        let mut all_spec_module = HashSet::new();
        provider.with_module(|addr, module_def| {
            let item = ItemOrAccess::Item(Item::ModuleName(ItemModuleName {
                name: module_def.name,
                is_test: attributes_has_test(&module_def.attributes).is_test(),
            }));
            visitor.handle_item_or_access(self, project_context, &item);
            if !module_def.is_spec_module {
                project_context.set_up_module(
                    addr,
                    module_def.name,
                    provider.found_in_test()
                        || attributes_has_test(&module_def.attributes).is_test(),
                );
            } else {
                all_spec_module.insert((addr, module_def.name));
            }
        });
        // module created
        let mut spec_module_created = HashSet::new();
        project_context.visit_address(|address| {
            for (addr, module_name) in all_spec_module.iter() {
                let created = address
                    .address
                    .get(addr)
                    .map(|mm| mm.modules.get(&module_name.0.value).is_some())
                    .unwrap_or(false);
                if created {
                    spec_module_created.insert((*addr, *module_name));
                }
            }
        });
        for (addr, module_name) in all_spec_module.into_iter() {
            // skip if created.
            if !spec_module_created.contains(&(addr, module_name)) {
                project_context.set_up_module(addr, module_name, false);
            }
        }

        provider.with_const(|addr, name, c| {
            self.visit_const(Some((addr, name)), c, project_context, visitor);
        });
        provider.with_struct(|addr, module_name, c| {
            let item = Item::StructNameRef(ItemStructNameRef {
                addr,
                module_name,
                name: c.name,
                type_parameters: c.type_parameters.clone(),
                is_test: attributes_has_test(&c.attributes).is_test(),
            });
            project_context.enter_top_item(self, addr, module_name, c.name.0.value, item, false);
        });

        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            self.visit_use_decl(
                Some((addr, module_name)),
                u,
                project_context,
                None,
                is_spec_module,
                enter_import,
            )
        });

        provider.with_struct(|addr, module_name, s| {
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            project_context.enter_scope(|scopes| {
                scopes.set_access_env(
                    if provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default()
                        || attributes_has_test(&s.attributes).is_test()
                    {
                        AccessEnv::Test
                    } else {
                        Default::default()
                    },
                );
                for t in s.type_parameters.iter() {
                    self.visit_struct_tparam(t, scopes, visitor);
                }
                let fields = match &s.fields {
                    StructFields::Defined(x) => {
                        let mut fields = Vec::with_capacity(x.len());
                        for (f, ty) in x.iter() {
                            self.visit_type_apply(ty, scopes, visitor);
                            if visitor.finished() {
                                return;
                            }
                            let ty = scopes.resolve_type(ty, self);
                            {
                                let item = ItemOrAccess::Item(Item::Field(*f, ty.clone()));
                                visitor.handle_item_or_access(self, scopes, &item);
                                if visitor.finished() {
                                    return;
                                }
                            }
                            fields.push((*f, ty));
                        }
                        fields
                    }
                    StructFields::Native(_) => vec![],
                };
                let item = ItemOrAccess::Item(Item::Struct(ItemStruct {
                    name: s.name,
                    type_parameters: s.type_parameters.clone(),
                    type_parameters_ins: vec![],
                    fields,
                    is_test: attributes_has_test(&s.attributes).is_test(),
                    addr,
                    module_name,
                }));
                visitor.handle_item_or_access(self, scopes, &item);
                scopes.enter_top_item(self, addr, module_name, s.name.value(), item, false)
            });
        });

        let enter_function = |modules: &Project,
                              f: &Function,
                              project_context: &ProjectContext,
                              visitor: &mut dyn ItemOrAccessHandler,
                              addr: AccountAddress,
                              module_name: Symbol,
                              is_spec_module: bool,
                              is_spec: bool| {
            // This enter scope make sure the visit_tparam cannot override some module level item.
            project_context.enter_scope(|scopes| {
                scopes.set_access_env(
                    if provider.found_in_test()
                        || scopes.module_is_test(addr, module_name).unwrap_or_default()
                        || attributes_has_test(&f.attributes).is_test()
                    {
                        AccessEnv::Test
                    } else if is_spec_module || is_spec {
                        AccessEnv::Spec
                    } else {
                        Default::default()
                    },
                );

                let s = &f.signature;
                let ts = s.type_parameters.clone();
                for t in s.type_parameters.iter() {
                    self.visit_tparam(t, scopes, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
                let params: Vec<_> = s
                    .parameters
                    .iter()
                    .map(|(var, ty)| (*var, scopes.resolve_type(ty, self)))
                    .collect();
                let ret = scopes.resolve_type(&s.return_type, self);
                let item = Item::Fun(ItemFun {
                    name: f.name,
                    type_parameters: ts,
                    parameters: params,
                    ret_type: Box::new(ret),
                    ret_type_unresolved: s.return_type.clone(),
                    is_spec,
                    vis: f.visibility.clone(),
                    addr_and_name: AddrAndModuleName {
                        addr,
                        name: ModuleName(Spanned {
                            loc: Loc::new(FileHash::empty(), 0, 0),
                            value: module_name,
                        }),
                    },
                    is_test: attributes_has_test(&f.attributes),
                });
                let item = ItemOrAccess::Item(item);
                visitor.handle_item_or_access(modules, scopes, &item);
                scopes.enter_top_item(
                    self,
                    addr,
                    module_name,
                    f.name.value(),
                    item,
                    is_spec_module,
                );
            });
        };

        provider.with_function(|addr, module_name, f| {
            // This clone scope make sure we can visit module level item.
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            enter_function(
                self,
                f,
                project_context,
                visitor,
                addr,
                module_name,
                false,
                false,
            );
        });

        // visit use decl again.
        provider.with_use_decl(|addr, module_name, u, is_spec_module| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(if is_spec_module {
                AccessEnv::Spec
            } else if provider.found_in_test()
                || project_context
                    .module_is_test(addr, module_name)
                    .unwrap_or_default()
            {
                AccessEnv::Test
            } else {
                Default::default()
            });
            self.visit_use_decl(
                Some((addr, module_name)),
                u,
                project_context,
                Some(visitor),
                is_spec_module,
                false,
            );
        });

        provider.with_friend(|addr, module_name, f| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            self.visit_friend(f, addr, module_name, project_context, visitor);
        });

        if !visitor.visit_fun_or_spec_body() {
            //
            return;
        }

        // visit function body.
        provider.with_function(|addr, module_name, f| {
            project_context.set_current_addr_and_module_name(addr, module_name);
            project_context.set_access_env(
                if provider.found_in_test()
                    || project_context
                        .module_is_test(addr, module_name)
                        .unwrap_or_default()
                    || attributes_has_test(&f.attributes).is_test()
                {
                    AccessEnv::Test
                } else {
                    Default::default()
                },
            );
            let range = self.convert_loc_range(&f.loc);
            if range.is_none() {
                return;
            }
            if !visitor.function_or_spec_body_should_visit(range.as_ref().unwrap()) {
                return;
            }
            let _guard = project_context.clone_scope_and_enter(addr, module_name, false);
            log::info!("provider.with_function range = {:?}", range);
            self.visit_function(f, project_context, visitor);
        });
    }

    pub(crate) fn visit_bind(
        &self,
        bind: &Bind,
        infer_ty: &ResolvedType,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        expr: Option<&Exp>,
        has_decl_ty: bool,
    ) {
        log::info!("visit_bind:{:?}", bind);
        match &bind.value {
            Bind_::Var(var) => {
                let item = ItemOrAccess::Item(Item::Var {
                    has_decl_ty,
                    var: *var,
                    ty: infer_ty.clone(),
                    lambda: expr.and_then(|x| match &x.value {
                        Exp_::Lambda(b, e) => Some(LambdaExp {
                            bind_list: b.clone(),
                            exp: e.as_ref().clone(),
                        }),
                        _ => None,
                    }),
                });
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                project_context.enter_item(self, var.0.value, item);
            }
            Bind_::Unpack(chain, tys, field_binds) => {
                self.visit_type_apply(
                    &Spanned {
                        loc: chain.loc,
                        value: Type_::Apply(chain.clone(), vec![]),
                    },
                    project_context,
                    visitor,
                );
                let (struct_ty, _) = project_context.find_name_chain_item(chain, self);
                let struct_ty = struct_ty.unwrap_or_default().to_type().unwrap_or_default();
                if let Some(tys) = tys {
                    for t in tys.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                let mut struct_item = struct_ty.struct_ref_to_struct(project_context);
                let struct_item = if let Some(tys) = tys {
                    let tys: Vec<_> = tys
                        .iter()
                        .map(|x| project_context.resolve_type(x, self))
                        .collect();
                    let mut m = HashMap::new();
                    struct_item
                        .type_parameters
                        .iter()
                        .zip(tys.iter())
                        .for_each(|(t, ty)| {
                            m.insert(t.name.value, ty.clone());
                        });
                    struct_item.bind_type_parameter(Some(&m));
                    struct_item
                } else {
                    // use
                    infer_ty.clone().struct_ref_to_struct(project_context)
                };

                for (field, bind) in field_binds.iter() {
                    let field_and_ty = struct_item.find_filed_by_name(field.0.value);
                    let field_ty = if let Some(x) = field_and_ty {
                        if infer_ty.is_ref() {
                            ResolvedType::new_ref(false, x.1.clone())
                        } else {
                            x.1.clone()
                        }
                    } else {
                        ResolvedType::UnKnown
                    };
                    {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: *field,
                            to: if let Some(x) = field_and_ty {
                                x.0
                            } else {
                                *field
                            },
                            ty: field_ty.clone(),
                            all_fields: struct_item.all_fields(),
                            item: None,
                            has_ref: None,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                    }
                    self.visit_bind(bind, &field_ty, project_context, visitor, None, true);
                }
            }
        }
    }

    pub(crate) fn visit_bind_list(
        &self,
        bind_list: &BindList,
        ty: &Option<Type>,
        expr: Option<&Exp>,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        if let Some(expr) = expr {
            self.visit_expr(expr, project_context, visitor);
            if visitor.finished() {
                return;
            }
        }
        let has_decl_type = ty.is_some();
        let ty = if let Some(ty) = ty {
            self.visit_type_apply(ty, project_context, visitor);
            if visitor.finished() {
                return;
            }
            project_context.resolve_type(ty, self)
        } else if let Some(expr) = expr {
            self.get_expr_type(expr, project_context)
        } else {
            ResolvedType::UnKnown
        };

        for (index, bind) in bind_list.value.iter().enumerate() {
            let ty = ty.nth_ty(index);
            let unknown = ResolvedType::UnKnown;
            let ty = ty.unwrap_or(&unknown);
            self.visit_bind(
                bind,
                ty,
                project_context,
                visitor,
                match expr {
                    Some(x) => match &x.value {
                        Exp_::ExpList(es) => es.get(index),
                        _ => Some(x),
                    },
                    None => None,
                },
                has_decl_type,
            );
            if visitor.finished() {
                return;
            }
        }
    }

    pub(crate) fn visit_block(
        &self,
        seq: &Sequence,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        project_context.enter_scope(|scopes| {
            for u in seq.0.iter() {
                self.visit_use_decl(None, u, scopes, Some(visitor), false, true);
                if visitor.finished() {
                    return;
                }
            }
            for s in seq.1.iter() {
                log::trace!("visit_block sequence_item = {:?}", s);
                self.visit_sequence_item(s, scopes, visitor);
                if visitor.finished() {
                    return;
                }
            }
            if let Some(ref exp) = seq.3.as_ref() {
                log::trace!("visit_block exp = {:?}", exp);
                self.visit_expr(exp, scopes, visitor);
            }
        });
    }

    pub(crate) fn visit_const(
        &self,
        enter_top: Option<(AccountAddress, Symbol)>,
        c: &Constant,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        self.visit_type_apply(&c.signature, project_context, visitor);
        if visitor.finished() {
            return;
        }
        // const can only be declared at top scope
        let ty = project_context.resolve_type(&c.signature, self);

        let item = ItemOrAccess::Item(Item::Const(ItemConst {
            name: c.name,
            ty,
            is_test: attributes_has_test(&c.attributes).is_test(),
        }));
        visitor.handle_item_or_access(self, project_context, &item);
        let item: Item = item.into();
        if let Some((address, module)) = enter_top {
            project_context.enter_top_item(self, address, module, c.name.value(), item, false);
        } else {
            project_context.enter_item(self, c.name.value(), item);
        }
    }

    pub(crate) fn visit_expr(
        &self,
        exp: &Exp,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        log::trace!("visit_expr:{:?}", exp);
        if visitor.need_expr_type() {
            let ty = self.get_expr_type(exp, project_context);
            visitor.handle_expr_typ(exp, ty);
        }

        let handle_dot = |e: &Exp,
                          field: &Name,
                          project_context: &ProjectContext,
                          visitor: &mut dyn ItemOrAccessHandler,
                          _has_ref: Option<bool>| {
            log::trace!("handle_dot({})", field);
            self.visit_expr(e, project_context, visitor);
            if visitor.finished() {
                return;
            }
            log::trace!(
                "handle_dot --> inlay_hint.handle_item_or_access({}) continue",
                field
            );
            let struct_ty = self.get_expr_type(e, project_context);
            let struct_ty = match &struct_ty {
                ResolvedType::Ref(_, ty) => ty.as_ref(),
                _ => &struct_ty,
            };
            let struct_ty = struct_ty.struct_ref_to_struct(project_context);
            let all_fields = struct_ty.all_fields();
            if let Some(def_field) = struct_ty.find_filed_by_name(field.value) {
                let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                    from: Field(*field),
                    to: def_field.0,
                    ty: def_field.1.clone(),
                    all_fields,
                    item: None,
                    has_ref: _has_ref,
                }));
                visitor.handle_item_or_access(self, project_context, &item);
            } else {
                let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                    from: Field(*field),
                    to: Field(*field),
                    ty: ResolvedType::UnKnown,
                    all_fields,
                    item: None,
                    has_ref: _has_ref,
                }));
                visitor.handle_item_or_access(self, project_context, &item);
            }
        };

        match &exp.value {
            Exp_::Value(ref v) => {
                if let Some(name) = get_name_from_value(v) {
                    let item = ItemOrAccess::Access(Access::ExprAddressName(*name));
                    visitor.handle_item_or_access(self, project_context, &item);
                }
            }
            Exp_::Move(var) | Exp_::Copy(var) => {
                let item = project_context.find_var(var.value());
                let item =
                    ItemOrAccess::Access(Access::ExprVar(*var, Box::new(item.unwrap_or_default())));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Name(chain, tys) => {
                // let's try.
                if let Some(tys) = tys {
                    for ty in tys.iter() {
                        log::trace!("process Exp_::Name, ty = {:?}", ty);
                        self.visit_type_apply(ty, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                log::trace!("process Exp_::Name, chain = {}", chain);
                let (item, module) = project_context.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ExprAccessChain(
                    chain.clone(),
                    module,
                    Box::new(item.unwrap_or_default()),
                ));
                log::trace!("process Exp_::Name, item = {}", item);
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {}
            }

            Exp_::Call(ref chain, is_macro, ref types, ref exprs) => {
                if *is_macro {
                    let c = MacroCall::from_chain(chain).unwrap_or_default();
                    let item = ItemOrAccess::Access(Access::MacroCall(c, chain.clone()));
                    visitor.handle_item_or_access(self, project_context, &item);
                } else {
                    let (item, module) = project_context.find_name_chain_item(chain, self);
                    if visitor.need_call_pair() {
                        if let Item::Fun(_f) = item.clone().unwrap_or_default() {
                            let addr = project_context.get_current_addr_and_module_name();
                            visitor.handle_call_pair(
                                FunID {
                                    addr: addr.addr,
                                    addr_name: "".to_string(), // TODO
                                    module_name: addr.name.0.value,
                                    function_name: get_name_chain_last_name(chain).value,
                                },
                                FunID {
                                    addr: _f.addr_and_name.addr,
                                    addr_name: "".to_string(), // TODO
                                    module_name: _f.addr_and_name.name.0.value,
                                    function_name: _f.name.0.value,
                                },
                            );
                        }
                    }

                    // try visit lambda expr.
                    if let ResolvedType::Fun(x) = self
                        .initialize_fun_call(project_context, chain, types, exprs)
                        .unwrap_or_default()
                    {
                        // TODO we maybe need infer type parameter first.
                        let unkown = (
                            Var(Spanned {
                                loc: Loc::new(FileHash::empty(), 0, 0),
                                value: Symbol::from(""),
                            }),
                            ResolvedType::UnKnown,
                        );
                        for (index, e) in exprs.value.iter().enumerate() {
                            let ty = x.parameters.get(index).unwrap_or(&unkown);
                            self.try_fix_local_var_and_visit_lambda(e, &ty.1, visitor);
                            if visitor.need_para_arg_pair() {
                                visitor.handle_para_arg_pair(self, ty.0 .0, e);
                            }
                        }
                    }
                    let item = ItemOrAccess::Access(Access::ExprAccessChain(
                        chain.clone(),
                        module,
                        Box::new(item.unwrap_or_default()),
                    ));
                    if visitor.current_vistor_handler_is_inlay_hints() {
                        return;
                    }
                    log::trace!("process Exp_::Call, item = {}", item);
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                }
                if let Some(ref types) = types {
                    for t in types.iter() {
                        log::trace!("process Exp_::Call, t = {:?}", t);
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for expr in exprs.value.iter() {
                    log::trace!("process Exp_::Call, expr = {:?}", expr);
                    self.visit_expr(expr, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Pack(ref chain, ref types, fields) => {
                self.visit_type_apply(
                    &Spanned {
                        loc: chain.loc,
                        value: Type_::Apply(Box::new(chain.clone()), vec![]),
                    },
                    project_context,
                    visitor,
                );
                if visitor.finished() {
                    return;
                }
                let (struct_item, _) = project_context.find_name_chain_item(chain, self);
                let mut struct_item = match struct_item {
                    Some(Item::Struct(x)) => x,
                    _ => {
                        return;
                    }
                };
                if let Some(types) = types {
                    for t in types.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                    let types: Vec<_> = types
                        .iter()
                        .map(|ty| project_context.resolve_type(ty, self))
                        .collect();
                    struct_item.type_parameters_ins = types;
                    struct_item.bind_type_parameter(None);
                }
                for f in fields.iter() {
                    let field_type = struct_item.find_filed_by_name(f.0.value());
                    let all_fields = struct_item.all_fields();
                    let item = match &f.1.value {
                        Exp_::Name(chain, _) => match &chain.value {
                            NameAccessChain_::One(x) => {
                                if x.value == f.0.value() && x.loc == f.0.loc() {
                                    let (item, _) =
                                        project_context.find_name_chain_item(chain, self);
                                    item
                                } else {
                                    None
                                }
                            }
                            _ => None,
                        },
                        _ => None,
                    };

                    if let Some(field_type) = field_type {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: f.0,
                            to: field_type.0,
                            ty: field_type.1.clone(),
                            all_fields,
                            item,
                            has_ref: None,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                    } else {
                        let item = ItemOrAccess::Access(Access::AccessFiled(AccessFiled {
                            from: f.0,
                            to: f.0,
                            ty: ResolvedType::UnKnown,
                            all_fields,
                            item,
                            has_ref: None,
                        }));
                        visitor.handle_item_or_access(self, project_context, &item);
                    }
                    if visitor.finished() {
                        return;
                    }
                    self.visit_expr(&f.1, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }

            Exp_::Vector(_loc, ref ty, ref exprs) => {
                if let Some(ty) = ty {
                    for t in ty.iter() {
                        self.visit_type_apply(t, project_context, visitor);
                        if visitor.finished() {
                            return;
                        }
                    }
                }
                for e in exprs.value.iter() {
                    self.visit_expr(e, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::IfElse(condition, then_, else_) => {
                self.visit_expr(condition, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(then_, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                if let Some(else_) = else_ {
                    self.visit_expr(else_.as_ref(), project_context, visitor);
                }
            }
            Exp_::While(condition, body) => {
                self.visit_expr(condition, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(body.as_ref(), project_context, visitor);
            }
            Exp_::Loop(e) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
            }
            Exp_::Block(b) => self.visit_block(b, project_context, visitor),
            Exp_::Lambda(_, _) => {
                // TODO have lambda expression in ast structure.
                // But I don't find in msl spec.
                log::error!("lambda expression in ast.");
            }

            Exp_::Quant(_, binds, bodies, where_, result) => {
                // TODO look list t can be use a type alias.
                // forall t: type, addr: address where exists<R<t>>(addr): exists<T<t>>(addr)
                project_context.enter_scope(|scopes| {
                    for bind_expr in binds.value.iter() {
                        let bind = &bind_expr.value.0;
                        let expr = &bind_expr.value.1;
                        let ty = self.get_expr_type(expr, scopes);
                        let is_spec_domain: bool = match &expr.value {
                            Exp_::Call(chain, _, _, _) => match &chain.value {
                                NameAccessChain_::One(name) => name.value.as_str() == SPEC_DOMAIN,
                                NameAccessChain_::Two(_, _) => false,
                                NameAccessChain_::Three(_, _) => false,
                            },
                            _ => false,
                        };
                        if let Bind_::Var(var) = &bind.value {
                            let ty = if is_spec_domain {
                                ty
                            } else if let Some(vec_ty) = ty.is_vector() {
                                vec_ty.clone()
                            } else if ty.is_range().is_some() {
                                ResolvedType::new_build_in(BuildInType::NumType)
                            } else {
                                log::error!("bind the wrong type:{}", ty);
                                ty
                            };
                            let item = ItemOrAccess::Item(Item::Parameter(*var, ty));
                            visitor.handle_item_or_access(self, scopes, &item);
                            scopes.enter_item(self, var.value(), item);
                        }
                        self.visit_expr(expr, scopes, visitor);
                        if visitor.finished() {
                            return;
                        }

                        // bodies
                        for body in bodies.iter() {
                            scopes.enter_scope(|scopes| {
                                for exp in body.iter() {
                                    self.visit_expr(exp, scopes, visitor);
                                }
                            });
                        }
                        //
                        if let Some(exp) = where_ {
                            self.visit_expr(exp.as_ref(), scopes, visitor);
                        };
                        self.visit_expr(result.as_ref(), scopes, visitor);
                    }
                });
            }
            Exp_::ExpList(list) => {
                for e in list.iter() {
                    self.visit_expr(e, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Exp_::Unit => {
                // Nothing.
            }
            Exp_::Assign(left, right) => {
                self.visit_expr(left, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, project_context, visitor);
                let ty = self.get_expr_type(right.as_ref(), project_context);
                self.try_fix_local_var_and_visit_lambda(left, &ty, visitor);
            }
            Exp_::Return(e) => {
                if let Some(e) = e {
                    self.visit_expr(e, project_context, visitor);
                }
            }
            Exp_::Abort(e) => self.visit_expr(e.as_ref(), project_context, visitor),
            Exp_::Break => {
                let item = ItemOrAccess::Access(Access::KeyWords("break"));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Continue => {
                let item = ItemOrAccess::Access(Access::KeyWords("continue"));
                visitor.handle_item_or_access(self, project_context, &item);
            }
            Exp_::Dereference(x) => {
                self.visit_expr(x.as_ref(), project_context, visitor);
            }
            Exp_::UnaryExp(_, e) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
            }
            Exp_::BinopExp(left, _, right) => {
                self.visit_expr(left, project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_expr(right, project_context, visitor);
            }
            Exp_::Borrow(is_mut, e) => match &e.value {
                Exp_::Dot(e, f) => {
                    handle_dot(e, f, project_context, visitor, Some(*is_mut));
                }
                _ => {
                    self.visit_expr(e.as_ref(), project_context, visitor);
                }
            },
            Exp_::Dot(e, field) => {
                log::trace!("process Exp_::Dot, field = {}", field);
                handle_dot(e, field, project_context, visitor, None);
            }
            Exp_::Index(e, index) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                self.visit_expr(index.as_ref(), project_context, visitor)
            }
            Exp_::Cast(e, ty) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, project_context, visitor);
            }
            Exp_::Annotate(e, ty) => {
                self.visit_expr(e.as_ref(), project_context, visitor);
                if visitor.finished() {
                    return;
                }
                self.visit_type_apply(ty, project_context, visitor);
            }
            Exp_::UnresolvedError => {
                //
            }
            Exp_::Spec(_) => {}
        }
    }

    pub(crate) fn visit_friend(
        &self,
        friend_decl: &FriendDecl,
        addr: AccountAddress,
        module_name: Symbol,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        match &friend_decl.friend.value {
            NameAccessChain_::One(x) => {
                // This is absolute wrong syntax,But can be use for completion.
                let item = ItemOrAccess::Access(Access::Friend(
                    friend_decl.friend.clone(),
                    ModuleName(Spanned {
                        loc: x.loc,
                        value: Symbol::from("uOZKbQXVWi"),
                    }),
                ));
                visitor.handle_item_or_access(self, project_context, &item);
            }

            NameAccessChain_::Two(x, name) => {
                let addr_friend = match &x.value {
                    LeadingNameAccess_::AnonymousAddress(x) => x.into_inner(),
                    LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
                };
                let m = project_context.resolve_friend(addr_friend, name.value);
                let m = match m {
                    Some(x) => x,
                    None => ModuleName(*name),
                };
                let item = ItemOrAccess::Access(Access::Friend(friend_decl.friend.clone(), m));
                visitor.handle_item_or_access(self, project_context, &item);
                project_context.insert_friend(addr, module_name, (addr_friend, name.value));
            }

            NameAccessChain_::Three(_, _) => {
                log::error!("access friend three")
            }
        }
    }

    pub(crate) fn visit_function(
        &self,
        function: &Function,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        project_context.enter_scope(|s| {
            self.visit_signature(&function.signature, s, visitor);
            if visitor.finished() {
                return;
            }
            for v in function.acquires.iter() {
                let ty = &Spanned {
                    loc: v.loc,
                    value: Type_::Apply(Box::new(v.clone()), vec![]),
                };
                log::trace!("visit_function, ty = {:?}", ty);
                self.visit_type_apply(ty, project_context, visitor);
                if visitor.finished() {
                    return;
                }
            }
            log::info!(
                "visit_function, function.body.value = {:?}",
                function.body.value
            );
            match function.body.value {
                FunctionBody_::Native => {}
                FunctionBody_::Defined(ref seq) => self.visit_block(seq, project_context, visitor),
            }
        })
    }

    pub(crate) fn visit_sequence_item(
        &self,
        seq: &SequenceItem,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        match seq.value {
            SequenceItem_::Seq(ref e) => {
                self.visit_expr(e, project_context, visitor);
                if visitor.finished() {}
            }
            SequenceItem_::Declare(ref list, ref ty) => {
                self.visit_bind_list(list, ty, None, project_context, visitor);
                if visitor.finished() {}
            }
            SequenceItem_::Bind(ref list, ref ty, ref expr) => {
                self.visit_bind_list(list, ty, Some(expr), project_context, visitor);
                if visitor.finished() {}
            }
        }
    }

    pub(crate) fn visit_type_apply(
        &self,
        ty: &Type,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
    ) {
        match &ty.value {
            Type_::Apply(chain, types) => {
                let ty = project_context.resolve_type(ty, self);
                let (_, module) = project_context.find_name_chain_item(chain, self);
                let item = ItemOrAccess::Access(Access::ApplyType(
                    chain.as_ref().clone(),
                    module.map(|x| x.name),
                    Box::new(ty),
                ));
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                for t in types.iter() {
                    self.visit_type_apply(t, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
            Type_::Ref(_, ty) => self.visit_type_apply(ty, project_context, visitor),
            Type_::Fun(args, ret_ty) => {
                for a in args.iter() {
                    self.visit_type_apply(a, project_context, visitor);
                }
                self.visit_type_apply(ret_ty.as_ref(), project_context, visitor);
            }
            Type_::Unit => {}
            Type_::Multiple(types) => {
                for t in types.iter() {
                    self.visit_type_apply(t, project_context, visitor);
                    if visitor.finished() {
                        return;
                    }
                }
            }
        }
    }

    pub(crate) fn visit_use_decl(
        &self,
        is_global: Option<(AccountAddress, Symbol)>,
        use_decl: &UseDecl,
        project_context: &ProjectContext,
        visitor: Option<&mut dyn ItemOrAccessHandler>,
        is_spec_module: bool,
        enter_import: bool,
    ) {
        let mut dummy = DummyHandler;
        let visitor = visitor.unwrap_or(&mut dummy);
        let get_addr = |module: &ModuleIdent| -> AccountAddress {
            match &module.value.address.value {
                LeadingNameAccess_::AnonymousAddress(num) => num.into_inner(),
                LeadingNameAccess_::Name(name) => self.name_to_addr_impl(name.value),
            }
        };
        let get_module = |module: &ModuleIdent| -> Option<Rc<RefCell<ModuleScope>>> {
            let module_scope =
                project_context.visit_address(|top| -> Option<Rc<RefCell<ModuleScope>>> {
                    let x = top
                        .address
                        .get(&get_addr(module))?
                        .modules
                        .get(&module.value.module.0.value)?
                        .clone();
                    Some(x)
                });
            let module_scope = match module_scope {
                Some(x) => x,
                None => return None,
            };
            Some(module_scope)
        };

        match &use_decl.use_ {
            Use::Module(module, alias) => {
                let module_scope = get_module(module);
                let module_scope = module_scope.unwrap_or_else(|| {
                    ModuleScope::new_module_name(get_addr(module), module.value.module)
                });
                let item = ItemOrAccess::Item(Item::Use(vec![ItemUse::Module(ItemUseModule {
                    module_ident: *module,
                    alias: *alias,
                    members: module_scope,
                    s: None,
                    is_test: attributes_has_test(&use_decl.attributes) == AttrTest::TestOnly,
                })]));
                visitor.handle_item_or_access(self, project_context, &item);
                if visitor.finished() {
                    return;
                }
                let name = if let Some(alias) = alias {
                    alias.value()
                } else {
                    module.value.module.value()
                };
                if enter_import {
                    if let Some((addr, module_name)) = is_global {
                        project_context.enter_top_use_item(
                            self,
                            addr,
                            module_name,
                            name,
                            item,
                            is_spec_module,
                        );
                    } else {
                        project_context.enter_use_item(self, name, item);
                    }
                }
            }

            Use::Members(module, members) => {
                let module_scope = get_module(module);
                let module_scope = module_scope.unwrap_or_else(|| {
                    ModuleScope::new_module_name(get_addr(module), module.value.module)
                });
                let garbage = vec![(
                    Name {
                        loc: module.loc,
                        value: Symbol::from("BRKuUAoEna"),
                    },
                    None,
                )];
                for (member, alias) in if !members.is_empty() {
                    members.iter()
                } else {
                    // So can find module definition in statement like this.
                    // use std::vector::{}
                    garbage.iter()
                } {
                    if member.value.as_str() == "Self" {
                        // Special handle for Self.
                        let item =
                            ItemOrAccess::Item(Item::Use(vec![ItemUse::Module(ItemUseModule {
                                module_ident: *module,
                                alias: alias.map(ModuleName),
                                members: module_scope.clone(),
                                s: Some(*member),
                                is_test: attributes_has_test(&use_decl.attributes)
                                    == AttrTest::TestOnly,
                            })]));
                        visitor.handle_item_or_access(self, project_context, &item);
                        if visitor.finished() {
                            return;
                        }
                        let name = if let Some(alias) = alias {
                            alias.value
                        } else {
                            module.value.module.value()
                        };
                        if let Some((addr, module_name)) = is_global {
                            project_context.enter_top_use_item(
                                self,
                                addr,
                                module_name,
                                name,
                                item,
                                is_spec_module,
                            );
                        } else {
                            project_context.enter_use_item(self, name, item);
                        }
                        continue;
                    }
                    let name = if let Some(alias) = alias {
                        *alias
                    } else {
                        *member
                    };
                    let item = ItemOrAccess::Item(Item::Use(vec![ItemUse::Item(ItemUseItem {
                        module_ident: *module,
                        name: *member,
                        alias: *alias,
                        members: module_scope.clone(),
                        is_test: attributes_has_test(&use_decl.attributes) == AttrTest::TestOnly,
                    })]));
                    visitor.handle_item_or_access(self, project_context, &item);
                    if visitor.finished() {
                        return;
                    }
                    if enter_import {
                        if let Some((addr, module_name)) = is_global {
                            project_context.enter_top_use_item(
                                self,
                                addr,
                                module_name,
                                name.value,
                                item,
                                is_spec_module,
                            );
                        } else {
                            project_context.enter_use_item(self, name.value, item);
                        }
                    }
                }
            }
        }
    }
}
