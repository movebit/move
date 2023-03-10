#![allow(dead_code)]

use crate::item::MacroCall;
use im::HashMap;
use move_compiler::shared::Identifier;
use move_compiler::{parser::ast::*, shared::ast_debug::AstDebug};
use move_symbol_pool::Symbol;
use std::collections::HashSet;
use std::hash::Hash;
use std::result::Result::*;

#[derive(Default)]
pub struct StructSpecGenerator {
    result: String,
}

impl StructSpecGenerator {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn to_string(self) -> String {
        self.result
    }
    pub(crate) fn generate(&mut self, x: &StructDefinition) {
        self.result
            .push_str(format!("{}spec {}", indent(1), x.name.0.value.as_str()).as_str());
        self.result.push_str("{\n");
        self.result.push_str("\n");
        self.result.push_str(format!("{}}}\n", indent(1)).as_str())
    }
}

#[derive(Default)]
pub struct FunSpecGenerator {
    result: String,
}

pub fn generate_fun_spec(f: &Function) -> String {
    let mut g = FunSpecGenerator::new();
    g.generate(f);
    let r = g.to_string();
    r
}

pub fn genrate_struct_spec(s: &StructDefinition) -> String {
    let mut g = StructSpecGenerator::new();
    g.generate(s);
    let r = g.to_string();
    r
}

impl FunSpecGenerator {
    pub(crate) fn new() -> Self {
        Self::default()
    }
    pub(crate) fn to_string(self) -> String {
        self.result
    }
    pub(crate) fn generate(&mut self, f: &Function) {
        self.result
            .push_str(format!("{}spec {}", indent(1), f.name.0.value.as_str()).as_str());
        let para_len = f.signature.parameters.len();
        self.result.push_str("(");
        if para_len > 0 {
            for (index, (var, ty)) in f.signature.parameters.iter().enumerate() {
                self.result.push_str(var.0.value.as_str());
                self.result.push_str(": ");
                self.result.push_str(format_xxx(ty).as_str());
                if (index + 1) < para_len {
                    self.result.push_str(", ");
                }
            }
        }
        self.result.push_str(")");
        match f.signature.return_type.value {
            Type_::Unit => {}
            _ => {
                self.result.push_str(": ");
                self.result.push_str(&format_xxx(&f.signature.return_type));
            }
        }
        self.result.push_str("{\n");
        self.result.push_str("\n");
        let assert = Self::generate_assert(f);
        self.result.push_str(assert.as_str());
        self.result.push_str(format!("{}}}\n", indent(1)).as_str())
    }

    fn generate_assert(f: &Function) -> String {
        let mut statements = String::new();
        let body = match &f.body.value {
            FunctionBody_::Defined(x) => x,
            FunctionBody_::Native => return statements,
        };
        let mut shadow = ShadowItems::new();
        let mut imports = GroupShadowItemUse::new();
        let mut local_emited = HashSet::new();
        fn insert_bind(r: &mut ShadowItems, bind: &Bind, index: usize) {
            match &bind.value {
                Bind_::Var(var) => {
                    if var.0.value.as_str() != "_" {
                        r.insert(var.0.value, ShadowItem::Local(ShadowItemLocal { index }));
                    }
                }
                Bind_::Unpack(_, _, xs) => {
                    for (_, b) in xs.iter() {
                        insert_bind(r, b, index);
                    }
                }
            }
        }
        fn insert_bind_list(r: &mut ShadowItems, bind: &BindList, index: usize) {
            for b in bind.value.iter() {
                insert_bind(r, b, index)
            }
        }
        for u in body.0.iter() {
            shadow.insert_use(&u.use_);
        }
        fn emit_local(
            shadow: &ShadowItems,
            statements: &mut String,
            e: &Exp,
            imports: &mut GroupShadowItemUse,
            local_emited: &mut HashSet<usize>,
            body: &Sequence,
        ) {
            let mut names = HashSet::new();
            let mut modules = HashSet::new();
            let _ = name_and_modules_in_expr(&mut names, &mut modules, &e);
            for name in names {
                if let Some(x) = shadow.query(name) {
                    match x {
                        ShadowItem::Use(x) => {
                            imports.insert(x.clone());
                        }
                        ShadowItem::Local(index) => {
                            if local_emited.contains(&index.index) == false {
                                let seq = body.1.get(index.index).unwrap().clone();
                                match &seq.value {
                                    SequenceItem_::Seq(_) => unreachable!(),
                                    SequenceItem_::Declare(_, _) => {}
                                    SequenceItem_::Bind(_, _, e) => emit_local(
                                        shadow,
                                        statements,
                                        e.as_ref(),
                                        imports,
                                        local_emited,
                                        body,
                                    ),
                                }
                                // emit right here,right now.
                                statements.push_str(
                                    format!("{}{};\n", indent(2), format_xxx(&seq)).as_str(),
                                );
                                local_emited.insert(index.index);
                            }
                        }
                    }
                }
            }
        }
        fn emit_assert(
            shadow: &ShadowItems,
            statements: &mut String,
            e: &Exp,
            imports: &mut GroupShadowItemUse,
            local_emited: &mut HashSet<usize>,
            body: &Sequence,
        ) {
            match &e.value {
                Exp_::Call(_call, is_macro, should_be_none, es) => {
                    if MacroCall::from_chain(_call).is_some()
                        && *is_macro
                        && should_be_none.is_none()
                        && es.value.len() > 0
                    {
                        match FunSpecGenerator::inverse_expression(es.value.get(0).unwrap()) {
                            std::result::Result::Ok(e) => {
                                emit_local(shadow, statements, &e, imports, local_emited, body);
                                statements.push_str(
                                    format!(
                                        "{}aborts_if {}{};\n",
                                        indent(2),
                                        format_xxx(&e),
                                        match es.value.get(1) {
                                            Some(e) => format!(" with {}", format_xxx(e)),
                                            None => "".to_string(),
                                        }
                                    )
                                    .as_str(),
                                );
                            }
                            std::result::Result::Err(_) => {}
                        }
                    }
                }
                _ => {}
            }
        }

        for (index, seq) in body.1.iter().enumerate() {
            match &seq.value {
                SequenceItem_::Declare(b, _) | SequenceItem_::Bind(b, _, _) => {
                    insert_bind_list(&mut shadow, b, index);
                }
                SequenceItem_::Seq(e) => emit_assert(
                    &shadow,
                    &mut statements,
                    e,
                    &mut imports,
                    &mut local_emited,
                    body,
                ),
            }
        }
        if let Some(e) = body.3.as_ref() {
            emit_assert(
                &shadow,
                &mut statements,
                e,
                &mut imports,
                &mut local_emited,
                body,
            );
        }
        {
            let mut result = imports.to_string(2);
            result.push_str(statements.as_str());
            result
        }
    }

    /// Inverse a expr for `aborts_if` etc.
    fn inverse_expression(e: &Exp) -> std::result::Result<Exp, ()> {
        use std::result::Result::*;
        fn copy_expr(e: &Exp) -> Exp {
            e.clone()
        }
        let r = || {
            Ok({
                Exp {
                    loc: e.loc,
                    value: Exp_::UnaryExp(
                        UnaryOp {
                            loc: e.loc,
                            value: UnaryOp_::Not,
                        },
                        Box::new(copy_expr(e)),
                    ),
                }
            })
        };
        fn inverse_binop(op: BinOp_) -> Option<BinOp_> {
            match op {
                BinOp_::Eq => Some(BinOp_::Neq),
                BinOp_::Neq => Some(BinOp_::Eq),
                BinOp_::Lt => Some(BinOp_::Ge),
                BinOp_::Gt => Some(BinOp_::Le),
                BinOp_::Le => Some(BinOp_::Gt),
                BinOp_::Ge => Some(BinOp_::Lt),
                _ => None,
            }
        }
        match &e.value {
            Exp_::Value(_) => Err(()),
            Exp_::Move(_) => Err(()),
            Exp_::Copy(_) => Err(()),
            Exp_::Name(_, x) => {
                if x.is_none() {
                    r()
                } else {
                    Err(())
                }
            }
            Exp_::Call(_, _, _, _) => r(),
            Exp_::Pack(_, _, _) => Err(()),
            Exp_::Vector(_, _, _) => Err(()),
            // TODO
            Exp_::IfElse(_, _, _) => Err(()),
            Exp_::While(_, _) => Err(()),
            Exp_::Loop(_) => Err(()),
            Exp_::Block(_) => Err(()),
            Exp_::Lambda(_, _) => Err(()),
            Exp_::Quant(_, _, _, _, _) => Err(()),
            Exp_::ExpList(_) => Err(()),
            Exp_::Unit => Err(()),
            Exp_::Assign(_, _) => Err(()),
            Exp_::Return(_) => Err(()),
            Exp_::Abort(_) => Err(()),
            Exp_::Break => Err(()),
            Exp_::Continue => Err(()),
            Exp_::Dereference(_) => r(),
            Exp_::UnaryExp(_, e) => Ok(e.as_ref().clone()),
            Exp_::BinopExp(l, op, r) => {
                if let Some(x) = inverse_binop(op.value) {
                    Ok(Exp {
                        loc: e.loc,
                        value: Exp_::BinopExp(
                            l.clone(),
                            BinOp {
                                loc: op.loc,
                                value: x,
                            },
                            r.clone(),
                        ),
                    })
                } else {
                    Err(())
                }
            }
            Exp_::Borrow(_, _) => Err(()),
            Exp_::Dot(_, _) => r(),
            Exp_::Index(_, _) => r(),
            Exp_::Cast(_, _) => Err(()),
            Exp_::Annotate(_, _) => Err(()),
            Exp_::Spec(_) => Err(()),
            Exp_::UnresolvedError => Err(()),
        }
    }
}

pub(crate) fn format_xxx<T>(e: &T) -> String
where
    T: AstDebug,
{
    use move_compiler::shared::ast_debug::AstWriter;
    let mut w = AstWriter::new(false);
    e.ast_debug(&mut w);
    let x = w.to_string();
    // TOTO better way to do this.
    x.trim_end().to_string()
}

fn indent(num: usize) -> String {
    "    ".to_string().repeat(num)
}

fn name_and_modules_in_expr(
    names: &mut HashSet<Symbol>,
    modules: &mut HashSet<Symbol>,
    e: &Exp,
) -> Result<(), ()> {
    fn handle_name_access_chain(
        names: &mut HashSet<Symbol>,
        modules: &mut HashSet<Symbol>,
        chain: &NameAccessChain,
    ) {
        match &chain.value {
            NameAccessChain_::One(x) => {
                names.insert(x.value);
            }
            NameAccessChain_::Two(name, _) => match &name.value {
                LeadingNameAccess_::AnonymousAddress(_) => {}
                LeadingNameAccess_::Name(name) => {
                    modules.insert(name.value);
                }
            },
            NameAccessChain_::Three(_, _) => {}
        }
    }
    fn handle_ty(
        names: &mut HashSet<Symbol>,
        modules: &mut HashSet<Symbol>,
        ty: &Type,
    ) -> Result<(), ()> {
        match &ty.value {
            Type_::Apply(chain, tys) => {
                handle_tys(names, modules, tys)?;
                handle_name_access_chain(names, modules, chain);
            }
            Type_::Ref(_, ty) => {
                handle_ty(names, modules, ty)?;
            }
            Type_::Fun(_, _) => return Err(()),
            Type_::Unit => {}
            Type_::Multiple(tys) => {
                handle_tys(names, modules, tys)?;
            }
        }
        Ok(())
    }
    fn handle_tys(
        names: &mut HashSet<Symbol>,
        modules: &mut HashSet<Symbol>,
        tys: &Vec<Type>,
    ) -> Result<(), ()> {
        for ty in tys.iter() {
            handle_ty(names, modules, ty)?;
        }
        Ok(())
    }
    fn handle_exprs(
        names: &mut HashSet<Symbol>,
        modules: &mut HashSet<Symbol>,
        exprs: &Vec<Exp>,
    ) -> Result<(), ()> {
        for e in exprs.iter() {
            name_and_modules_in_expr(names, modules, e)?;
        }
        Ok(())
    }
    match &e.value {
        Exp_::Value(_) => {}
        Exp_::Move(var) => {
            names.insert(var.0.value);
        }
        Exp_::Copy(var) => {
            names.insert(var.0.value);
        }
        Exp_::Name(name, tys) => {
            handle_name_access_chain(names, modules, name);
            if let Some(tys) = tys {
                handle_tys(names, modules, tys)?;
            };
        }
        Exp_::Call(chain, _, tys, exprs) => {
            handle_name_access_chain(names, modules, chain);
            if let Some(tys) = tys {
                handle_tys(names, modules, tys)?;
            };
            handle_exprs(names, modules, &exprs.value)?;
        }
        Exp_::Pack(chain, tys, exprs) => {
            handle_name_access_chain(names, modules, chain);
            if let Some(tys) = tys {
                handle_tys(names, modules, tys)?;
            };
            for (_, e) in exprs.iter() {
                name_and_modules_in_expr(names, modules, e)?;
            }
        }
        Exp_::Vector(_, tys, exprs) => {
            if let Some(tys) = tys {
                handle_tys(names, modules, tys)?;
            };
            handle_exprs(names, modules, &exprs.value)?;
        }
        Exp_::IfElse(con, then_, else_) => {
            name_and_modules_in_expr(names, modules, con.as_ref())?;
            name_and_modules_in_expr(names, modules, then_.as_ref())?;
            if let Some(else_) = else_ {
                name_and_modules_in_expr(names, modules, else_.as_ref())?;
            }
        }
        Exp_::While(_, _) => {}
        Exp_::Loop(_) => {}
        Exp_::Block(_b) => {
            return Err(());
        }
        Exp_::Lambda(_, _) => {}
        Exp_::Quant(_, _, _, _, _) => {}
        Exp_::ExpList(exprs) => {
            handle_exprs(names, modules, exprs)?;
        }
        Exp_::Unit => {}
        Exp_::Assign(_, _) => {}
        Exp_::Return(_) => {}
        Exp_::Abort(_) => {}
        Exp_::Break => {}
        Exp_::Continue => {}
        Exp_::Dereference(e) => {
            name_and_modules_in_expr(names, modules, e.as_ref())?;
        }
        Exp_::UnaryExp(_, e) => {
            name_and_modules_in_expr(names, modules, e.as_ref())?;
        }
        Exp_::BinopExp(l, _, r) => {
            name_and_modules_in_expr(names, modules, l.as_ref())?;
            name_and_modules_in_expr(names, modules, r.as_ref())?;
        }
        Exp_::Borrow(_, e) => {
            name_and_modules_in_expr(names, modules, e.as_ref())?;
        }
        Exp_::Dot(a, _) => {
            name_and_modules_in_expr(names, modules, a.as_ref())?;
        }
        Exp_::Index(a, b) => {
            name_and_modules_in_expr(names, modules, a.as_ref())?;
            name_and_modules_in_expr(names, modules, b.as_ref())?;
        }
        Exp_::Cast(a, _) => {
            name_and_modules_in_expr(names, modules, a.as_ref())?;
        }
        Exp_::Annotate(a, _) => {
            name_and_modules_in_expr(names, modules, a.as_ref())?;
        }
        Exp_::Spec(_) => return Err(()),
        Exp_::UnresolvedError => return Err(()),
    };
    Ok(())
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ShadowItemUseItem {
    lead: LeadingNameAccess_,
    module: Symbol,
    item: Symbol,
    alias: Option<Symbol>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
struct ShadowItemUseModule {
    lead: LeadingNameAccess_,
    module: Symbol,
    alias: Option<Symbol>,
    has_self: bool,
}

#[derive(Clone, Copy)]
struct ShadowItemLocal {
    index: usize,
}

#[derive(Clone, PartialEq, Eq, Hash)]
enum ShadowItemUse {
    Module(ShadowItemUseModule),
    Item(ShadowItemUseItem),
}

#[derive(Default)]
struct GroupShadowItemUse {
    items: HashMap<(LeadingNameAccess_, Symbol), Vec<ShadowItemUse>>,
}

impl GroupShadowItemUse {
    fn new() -> Self {
        Self::default()
    }
    fn insert(&mut self, x: ShadowItemUse) {
        let k = match &x {
            ShadowItemUse::Module(x) => (x.lead.clone(), x.module),
            ShadowItemUse::Item(x) => (x.lead.clone(), x.module),
        };
        if let Some(xxx) = self.items.get_mut(&k) {
            xxx.push(x);
        } else {
            self.items.insert(k, vec![x]);
        }
    }
    fn to_string(&self, indent_size: usize) -> String {
        let mut ret = String::new();
        for (k, v) in self.items.iter() {
            let mut v_str = String::new();
            if v.len() > 1 {
                v_str.push('{');
            }
            let v_len = v.len();
            for (index, vv) in v.iter().enumerate() {
                v_str.push_str(
                    match vv {
                        ShadowItemUse::Module(x) => match &x.alias {
                            Some(alias) => format!(
                                "{} as {}",
                                x.module.as_str().to_string(),
                                alias.as_str().to_string()
                            ),
                            None => x.module.as_str().to_string(),
                        },
                        ShadowItemUse::Item(item) => {
                            if item.alias.is_some() {
                                format!(
                                    "{} as {}",
                                    item.item.as_str(),
                                    item.alias.unwrap().as_str()
                                )
                            } else {
                                item.item.as_str().to_string()
                            }
                        }
                    }
                    .as_str(),
                );
                if index + 1 < v_len {
                    v_str.push(',');
                }
            }
            if v.len() > 1 {
                v_str.push('}');
            }
            ret.push_str(
                format!(
                    "{}use {}::{}::{};\n",
                    indent(indent_size),
                    match &k.0 {
                        LeadingNameAccess_::AnonymousAddress(_) => todo!(),
                        LeadingNameAccess_::Name(name) => name.value.as_str().to_string(),
                    },
                    k.1.as_str(),
                    v_str
                )
                .as_str(),
            );
        }
        ret
    }
}

#[derive(Clone)]
enum ShadowItem {
    Use(ShadowItemUse),
    Local(ShadowItemLocal),
}

fn use_2_shadow_items(u: &Use) -> HashMap<Symbol, Vec<ShadowItem>> {
    let mut ret: HashMap<Symbol, Vec<ShadowItem>> = HashMap::new();
    match u {
        Use::Module(addr_module, alias) => {
            let name = if let Some(alias) = alias {
                alias.0.value
            } else {
                addr_module.value.module.0.value
            };
            let item = ShadowItem::Use(ShadowItemUse::Module(ShadowItemUseModule {
                lead: addr_module.value.address.value.clone(),
                module: addr_module.value.module.value(),
                alias: alias.map(|x| x.0.value),
                has_self: false,
            }));
            if let Some(xxx) = ret.get_mut(&name) {
                xxx.push(item);
            } else {
                ret.insert(name, vec![item]);
            }
        }
        Use::Members(addr_module, imports) => {
            for (item, alias) in imports.iter() {
                let name = if let Some(alias) = alias {
                    alias.value
                } else {
                    item.value
                };
                let item = if item.value.as_str() != "Self" {
                    ShadowItem::Use(ShadowItemUse::Item(ShadowItemUseItem {
                        lead: addr_module.value.address.value.clone(),
                        module: addr_module.value.module.value(),
                        item: item.value,
                        alias: alias.map(|x| x.value),
                    }))
                } else {
                    ShadowItem::Use(ShadowItemUse::Module(ShadowItemUseModule {
                        lead: addr_module.value.address.value.clone(),
                        module: addr_module.value.module.value(),
                        alias: alias.map(|x| x.value),
                        has_self: true,
                    }))
                };
                if let Some(xxx) = ret.get_mut(&name) {
                    xxx.push(item);
                } else {
                    ret.insert(name, vec![item]);
                }
            }
        }
    };
    ret
}

#[derive(Default)]
pub struct ShadowItems {
    items: HashMap<Symbol, Vec<ShadowItem>>,
}

impl ShadowItems {
    fn new() -> Self {
        Self::default()
    }
    fn insert(&mut self, name: Symbol, item: ShadowItem) {
        if let Some(x) = self.items.get_mut(&name) {
            x.push(item);
        } else {
            self.items.insert(name, vec![item]);
        }
    }
    fn insert_use(&mut self, u: &Use) {
        self.insert2(use_2_shadow_items(u));
    }

    fn insert2(&mut self, item: HashMap<Symbol, Vec<ShadowItem>>) {
        for (name, v) in item.into_iter() {
            if let Some(x) = self.items.get_mut(&name) {
                x.extend(v);
            } else {
                self.items.insert(name, v);
            }
        }
    }
    fn query(&self, name: Symbol) -> Option<&ShadowItem> {
        self.items.get(&name).map(|x| x.last()).flatten()
    }
}
