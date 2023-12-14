use super::move_generate_spec_chen::*;
use crate::item_ori::MacroCall;
use crate::project_ori::Project;
use crate::types_ori::ResolvedType;
use crate::ast_debug::*;
use crate::utils::get_target_module;
use move_compiler::shared::Identifier;
// use move_compiler::{parser::ast::*, shared::ast_debug::AstDebug};
use move_compiler::parser::ast::*;
use move_ir_types::location::Loc;
use move_symbol_pool::Symbol;
use std::borrow::BorrowMut;
use std::collections::{HashMap, HashSet};
use std::fmt::format;
use std::hash::Hash;
use std::path::PathBuf;

// -----------------
use move_model::{
    model::{GlobalEnv, FunctionEnv, StructEnv, ModuleId, NodeId},
    ty::{TypeDisplayContext, Type as MoveModelType},
    ast::{
        Exp as MoveModelExp, ExpData as MoveModelExpData, Value as MoveModelValue,
        Operation as MoveModelOperation
    },
};

use crate::move_generate_spec_zx::{
    ShadowItems as ShadowItemsZX,
    ShadowItem as ShadowItemZX,
    SpecExpItem as SpecExpItemZX,
    BinOPReason as BinOPReasonZX,
};
use std::ops::Deref;
use crate::move_generate_spec_zx::get_shadows;

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
    pub(crate) fn generate(&mut self, x: &StructEnv) {
        self.result
            .push_str(format!("{}spec {}", indent(1), 
                                    x.get_name().display(x.symbol_pool()).to_string()).as_str()
        );
        self.result.push_str("{\n");
        self.result.push_str("\n");
        self.result.push_str(format!("{}}}\n", indent(1)).as_str())
    }
}

#[derive(Default)]
pub struct FunSpecGenerator {
    result: String,
}

pub fn generate_fun_spec(f: &Function, get_exp_ty: &dyn GetExprType) -> String {
    let mut g = FunSpecGenerator::new();
    g.generate(f, get_exp_ty);
    let r = g.to_string();
    r
}


pub fn generate_fun_spec_zx(global_env: &GlobalEnv, f: &FunctionEnv, fpath: &PathBuf) -> String {
    let mut g = FunSpecGenerator::new();
    g.generate_zx(global_env, f, fpath);
    let r = g.to_string();
    r
}

pub fn genrate_struct_spec(s: &StructEnv) -> String {
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
    pub(crate) fn generate(&mut self, f: &Function, get_expr_ty: &dyn GetExprType) {
        self.result
            .push_str(format!("{}spec {}", indent(1), f.name.0.value.as_str()).as_str());
        let para_len = f.signature.parameters.len();
        self.result.push_str("(");
        if para_len > 0 {
            for (index, (var, ty)) in f.signature.parameters.iter().enumerate() {
                self.result.push_str(var.0.value.as_str());
                self.result.push_str(": ");
                self.result.push_str(format_xxx(ty, false).as_str());
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
                self.result
                    .push_str(&format_xxx(&f.signature.return_type, false));
            }
        }
        self.result.push_str("{\n");
        self.result.push_str("\n");
        let assert = Self::generate_body(f, get_expr_ty);
        self.result.push_str(assert.as_str());
        self.result.push_str(format!("{}}}\n", indent(1)).as_str())
    }

    pub(crate) fn generate_zx(&mut self, global_env: &GlobalEnv, f: &FunctionEnv, fpath: &PathBuf) {
        let display_context = &TypeDisplayContext::new(global_env);
        
        self.result
            .push_str(format!("{}spec {}", indent(1), f.get_name_str()).as_str());
        self.result.push_str("(");

        let para_len = f.get_parameter_count();
        if para_len > 0 {
            for (index, para) in f.get_parameters().iter().enumerate() {
                self.result.push_str(para.0.display(f.symbol_pool()).to_string().as_str());
                self.result.push_str(": ");
    
                let para_type_display = para.1.display(&display_context);
                let mut para_type_string = para_type_display.to_string();
                if let Some(position) = para_type_string.rfind("::") {
                    para_type_string = para_type_string.get(position+2..).unwrap_or("").to_string();
                } 

                self.result.push_str(para_type_string.as_str());
                if (index + 1) < para_len {
                    self.result.push_str(", ");
                }
            }
        }
        self.result.push_str(")");
        
        let return_type = f.get_result_type();
        let return_type_display = return_type.display(&display_context);
        let mut return_type_string = String::from(": ");
        return_type_string.push_str(&return_type_display.to_string());
        match return_type {
            MoveModelType::Tuple(_) => {
                // ": ()" len is 4
                if return_type_string.len() <= 4 {
                    return_type_string = String::new();
                }
            },
            _ => {}
        }
        self.result.push_str(return_type_string.as_str());
        self.result.push_str("{\n");
        self.result.push_str("\n");
        let assert = Self::generate_body_zx(self, f, global_env, fpath);
        self.result.push_str(assert.as_str());
        self.result.push_str(format!("{}}}\n", indent(1)).as_str())
    }

    fn generate_body_zx(&self, f: &FunctionEnv, global_env: &GlobalEnv, fpath: &PathBuf) -> String {        

        eprintln!("generate_body_zx-----------");

        let mut statements = String::new();
        let mut shadows = ShadowItemsZX::new(f.get_loc());
        let mut local_emited = HashSet::new();
    
        if let Some(exp) = f.get_def() {
            get_shadows(exp, global_env, &mut shadows);
            FunSpecGenerator::try_emit_exp_zx(
                self, 
                &shadows,
                &mut statements,
                &exp,
                &mut local_emited,
                &exp,
                global_env,
                f
            );
        } else {
            eprint!("body is none");
            return statements;
        }

        


        return statements;
    }


    fn generate_body(f: &Function, get_expr_type: &dyn GetExprType) -> String {
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
        for (index, seq) in body.1.iter().enumerate() {
            match &seq.value {
                SequenceItem_::Declare(b, _) => {
                    insert_bind_list(&mut shadow, b, index);
                }
                SequenceItem_::Bind(b, _, e) => {
                    insert_bind_list(&mut shadow, b, index);
                    FunSpecGenerator::try_emit_exp(
                        &shadow,
                        &mut statements,
                        e,
                        &mut imports,
                        &mut local_emited,
                        body,
                        get_expr_type,
                    )
                }
                SequenceItem_::Seq(e) => FunSpecGenerator::try_emit_exp(
                    &shadow,
                    &mut statements,
                    e,
                    &mut imports,
                    &mut local_emited,
                    body,
                    get_expr_type,
                ),
            }
        }
        if let Some(e) = body.3.as_ref() {
            FunSpecGenerator::try_emit_exp(
                &shadow,
                &mut statements,
                e,
                &mut imports,
                &mut local_emited,
                body,
                get_expr_type,
            );
        }
        {
            let mut result = imports.to_string(2);
            result.push_str(statements.as_str());
            result
        }
    }
}




impl FunSpecGenerator {
    fn try_emit_exp(
        shadow: &ShadowItems,
        statements: &mut String,
        e: &Exp,
        imports: &mut GroupShadowItemUse,
        local_emited: &mut HashSet<usize>,
        body: &Sequence,
        get_expr_type: &dyn GetExprType,
    ) {
        match &e.value {
            Exp_::Call(_call, is_macro, should_be_none, es) => {
                if MacroCall::from_chain(_call).is_some()
                    && *is_macro
                    && should_be_none.is_none()
                    && es.value.len() > 0
                    && FunSpecGenerator::expr_has_spec_unsupprted(es.value.get(0).unwrap()) == false
                {
                    match FunSpecGenerator::inverse_expression(es.value.get(0).unwrap()) {
                        std::result::Result::Ok(e) => {
                            if false
                                == FunSpecGenerator::emit_local_and_imports(
                                    shadow,
                                    statements,
                                    &e,
                                    imports,
                                    local_emited,
                                    body,
                                )
                            {
                                return;
                            }

                            if let Some(e) = es.value.get(1) {
                                if false
                                    == FunSpecGenerator::emit_local_and_imports(
                                        shadow,
                                        statements,
                                        &e,
                                        imports,
                                        local_emited,
                                        body,
                                    )
                                {
                                    return;
                                }
                            }
                            statements.push_str(
                                format!(
                                    "{}aborts_if {}{};\n",
                                    indent(2),
                                    format_xxx(&e, true),
                                    match es.value.get(1) {
                                        Some(e) => {
                                            format!(" with {}", format_xxx(e, true))
                                        }
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
            _ => {
                let items = FunSpecGenerator::collect_spec_exp(e);
                for item in items.iter() {
                    match item {
                        SpecExpItem::BinOP {
                            reason,
                            left,
                            right,
                        } => {
                            let ty = get_expr_type
                                .get_expr_type(left)
                                .map(|x| match x {
                                    ResolvedType::BuildInType(x) => Some(x),
                                    _ => None,
                                })
                                .flatten()
                                .map(|x| match x {
                                    crate::types_ori::BuildInType::Bool => None,
                                    crate::types_ori::BuildInType::NumType => None,
                                    crate::types_ori::BuildInType::String => None,
                                    crate::types_ori::BuildInType::Signer => None,
                                    _ => Some(x),
                                })
                                .flatten()
                                .map(|x| *x)
                                .unwrap_or(crate::types_ori::BuildInType::U64);
                            if *reason != BinOPReason::DivByZero {
                                if false
                                    == FunSpecGenerator::emit_local_and_imports(
                                        shadow,
                                        statements,
                                        left,
                                        imports,
                                        local_emited,
                                        body,
                                    )
                                {
                                    return;
                                }
                            }
                            if false
                                == FunSpecGenerator::emit_local_and_imports(
                                    shadow,
                                    statements,
                                    right,
                                    imports,
                                    local_emited,
                                    body,
                                )
                            {
                                return;
                            }
                            match reason {
                                BinOPReason::OverFlowADD
                                | BinOPReason::OverFlowMUL
                                | BinOPReason::OverFlowSHL => {
                                    statements.push_str(
                                        format!(
                                            "{}aborts_if {} {} {} > {};\n",
                                            indent(2),
                                            format_xxx(left, true),
                                            match reason {
                                                BinOPReason::OverFlowADD => "+",
                                                BinOPReason::OverFlowMUL => "*",
                                                BinOPReason::OverFlowSHL => "<<",
                                                _ => unreachable!(),
                                            },
                                            format_xxx(right, true),
                                            format!(
                                                "MAX_{}",
                                                ty.to_static_str().to_ascii_uppercase()
                                            ),
                                        )
                                        .as_str(),
                                    );
                                }
                                BinOPReason::DivByZero => {
                                    statements.push_str(
                                        format!(
                                            "{}aborts_if {} = 0;\n",
                                            indent(2),
                                            format_xxx(right, true)
                                        )
                                        .as_str(),
                                    );
                                }
                                BinOPReason::UnderFlow => {
                                    statements.push_str(
                                        format!(
                                            "{}aborts_if {} - {} <= 0;\n",
                                            indent(2),
                                            format_xxx(left, true),
                                            format_xxx(right, true),
                                        )
                                        .as_str(),
                                    );
                                }
                            }
                        }
                        SpecExpItem::TypeOf { ty: _ty } => {}
                        SpecExpItem::TypeName { ty: _ty } => {}
                        SpecExpItem::BorrowGlobalMut {
                            ty: _ty,
                            addr: _addr,
                        } => {}
                    }
                }
            }
        }
    }

    fn try_emit_exp_zx(
        &self, 
        shadows: &ShadowItemsZX,
        statements: &mut String,
        exp: &MoveModelExp,
        local_emited: &mut HashSet<NodeId>,
        body: &MoveModelExp,
        env: &GlobalEnv,
        func_env: &FunctionEnv
    ) {
        
        let items = FunSpecGenerator::collect_spec_exp_zx(self, exp, env);
        let display_context = &TypeDisplayContext::new(env);
        for item in items.iter() {
            match item {
                SpecExpItemZX::BinOP { reason, left, right } => {
                    let left_node_id = left.as_ref().node_id();
                    let right_node_id = right.as_ref().node_id();
                    let left_node_loc = env.get_node_loc(left_node_id);
                    let right_node_loc = env.get_node_loc(right_node_id);
                    let left_node_type = env.get_node_type(left_node_id);
                    let right_node_type = env.get_node_type(right_node_id);
                    

                    let left_exp_str = match env.get_source(&left_node_loc) {
                        Ok(x) => FunSpecGenerator::remove_parentheses(x),
                        Err(_) => continue,
                    };

                    let right_exp_str = match env.get_source(&right_node_loc) {
                        Ok(x) => FunSpecGenerator::remove_parentheses(x),
                        Err(_) => continue,
                    };

                    eprintln!("left exp");
                    FunSpecGenerator::helper_exp_print(left, env,1);
                    eprintln!("right exp");
                    FunSpecGenerator::helper_exp_print(right, env,1);
                    eprintln!("--------------- ");
                    eprintln!("--------------- ");
                    eprintln!("--------------- ");
                    eprintln!("--------------- ");

                    if *reason != BinOPReasonZX::DivByZero 
                        && !FunSpecGenerator::is_support_exp(left, env) 
                    {
                            continue;
                    }

                    if !FunSpecGenerator::is_support_exp(right , env) {
                        continue;
                    }

                    match reason {
                        BinOPReasonZX::OverFlowADD 
                        | BinOPReasonZX::OverFlowMUL 
                        | BinOPReasonZX::OverFlowSHL => {
                            let statements_abort_if = format!(
                                                                "{}aborts_if {} {} {} > {};\n",
                                                                indent(2),
                                                                left_exp_str,
                                                                match reason {
                                                                    BinOPReasonZX::OverFlowADD => "+",
                                                                    BinOPReasonZX::OverFlowMUL => "*",
                                                                    BinOPReasonZX::OverFlowSHL => "<<",
                                                                    _ => unreachable!(),
                                                                },
                                                                right_exp_str,
                                                                format!(
                                                                    "MAX_{}",
                                                                    left_node_type.display(display_context)
                                                                                    .to_string()
                                                                                    .to_uppercase(),
                                                                ),
                                                            );
                            statements.push_str(statements_abort_if.as_str());
                        },
                        BinOPReasonZX::DivByZero => {
                            statements.push_str(
                                format!(
                                    "{}aborts_if {} == 0;\n",
                                    indent(2),
                                    right_exp_str,
                                )
                                .as_str(),
                            );
                        },
                        BinOPReasonZX::UnderFlow => {
                            statements.push_str(
                                format!(
                                    "{}aborts_if {} - {} <= 0;\n",
                                    indent(2),
                                    left_exp_str,
                                    right_exp_str,
                                )
                                .as_str(),
                            );
                        },
                    };
                },
                SpecExpItemZX::PatternLet { left, right } => {
                    let right_node_id = right.as_ref().node_id();
                    let right_node_loc = env.get_node_loc(right_node_id);
                    let right_node_type = env.get_node_type(right_node_id);
                    let right_exp_str = match env.get_source(&right_node_loc) {
                        Ok(x) => FunSpecGenerator::remove_parentheses(x),
                        Err(_) => continue,
                    };

                    statements.push_str(
                        format!(
                            "{}let {} = {};\n",
                            indent(2),
                            left.display(func_env.symbol_pool()).to_string(),
                            right_exp_str,
                        )
                        .as_str(),
                    );
                },
                SpecExpItemZX::BorrowGlobalMut { ty, addr } => {},
                SpecExpItemZX::TypeName { ty } => {},
                SpecExpItemZX::TypeOf { ty } => {}
            }
        }
    }

    fn remove_parentheses(input: &str) -> &str {
        // 检查字符串是否以 '(' 开始且以 ')' 结尾
        if input.starts_with('(') && input.ends_with(')') {
            // 如果是，使用字符串切片去掉左右两边的括号
            &input[1..input.len() - 1]
        } else {
            // 如果不是，返回原始字符串
            input
        }
    }

    fn helper_exp_print(e: &MoveModelExp, env: &GlobalEnv, depth: u32) {
        let display_context = &TypeDisplayContext::new(env);
        let node_id = e.as_ref().node_id();
        let node_loc = env.get_node_loc(node_id);
        let node_type = env.get_node_type(node_id);
        let exp_str = match env.get_source(&node_loc) {
            Ok(x) => FunSpecGenerator::remove_parentheses(x),
            Err(_) => {""},
        };

        eprintln!("{:?}{:?} {:?}",
                indent(depth.try_into().unwrap()),
                exp_str,
                node_type.display(display_context).to_string()
            );
        let exp_data = e.as_ref();
        match exp_data {
            MoveModelExpData::Invalid(_) => {eprintln!("body is Invalid")},
            MoveModelExpData::Value(_, v) => {
                eprintln!("{:?}body is value", indent(depth.try_into().unwrap()));
            },
            MoveModelExpData::LocalVar(_, sym) => {
                eprintln!("{:?}LocalVar = {:?}", indent(depth.try_into().unwrap()), sym.display(env.symbol_pool()).to_string());
            },
            MoveModelExpData::Temporary(_, _) => {
                eprintln!("{:?}body is Temporary", indent(depth.try_into().unwrap()));
            },
            MoveModelExpData::Call(_, op, args) => {
                eprintln!("{:?}body is Call", indent(depth.try_into().unwrap()));
                // FunSpecGenerator::helper_op_print();
                for a in args.iter() {
                    FunSpecGenerator::helper_exp_print(a, env, depth + 1);
                }
            },
            MoveModelExpData::Invoke(_, _, _) => {eprintln!("{:?}body is Invalid", indent(depth.try_into().unwrap()))},
            MoveModelExpData::Lambda(_, _, _) => {eprintln!("{:?}body is Lambda", indent(depth.try_into().unwrap()))},
            MoveModelExpData::Quant(_,_,_,_,_,_) => {eprintln!("{:?}body is Quant", indent(depth.try_into().unwrap()))},
            MoveModelExpData::Block(_,p,s,exp) => {
                eprintln!("{:?}body is Block", indent(depth.try_into().unwrap()));
                // handle_expdata_block_parren(p, env, shadows);
                if let Some(op_exp) = s {
                    FunSpecGenerator::helper_exp_print(op_exp, env, depth + 1);
                }
                FunSpecGenerator::helper_exp_print(exp, env, depth + 1);
            },
            MoveModelExpData::IfElse(_,_,_,_) => {eprintln!("body is IfElse")},
            MoveModelExpData::Return(_,_) => {eprintln!("body is Return")},
            MoveModelExpData::Sequence(_,vec_exp) => {
                    eprintln!("{:?}body is Sequence",indent(depth.try_into().unwrap()));
                    for a in vec_exp.iter() {
                        FunSpecGenerator::helper_exp_print(a, env, depth + 1);
                    }
            },
            MoveModelExpData::Loop(_,_) => {eprintln!("body is Loop")},
            MoveModelExpData::LoopCont(_,_) => {eprintln!("body is LoopCont")},
            MoveModelExpData::Assign(_,_,_) => {eprintln!("body is Assign")},
            MoveModelExpData::Mutate(_,_,_) => {eprintln!("body is Mutate")},
            MoveModelExpData::SpecBlock(_,_) => {eprintln!("body is SpecBlock")},
        }
    }
    
}
impl FunSpecGenerator {
    fn expr_has_spec_unsupprted(e: &Exp) -> bool {
        fn exprs_has_spec_unsupprted(es: &Vec<Exp>) -> bool {
            es.iter()
                .any(|e| FunSpecGenerator::expr_has_spec_unsupprted(e))
        }
        match &e.value {
            Exp_::Value(_) => false,
            Exp_::Move(_) => false,
            Exp_::Copy(_) => false,
            Exp_::Name(_, _) => false,
            Exp_::Call(_, _, _, es) => exprs_has_spec_unsupprted(&es.value),
            Exp_::Pack(_, _, es) => es
                .iter()
                .any(|e| FunSpecGenerator::expr_has_spec_unsupprted(&e.1)),
            Exp_::Vector(_, _, es) => exprs_has_spec_unsupprted(&es.value),
            Exp_::IfElse(e, then_, else_) => {
                FunSpecGenerator::expr_has_spec_unsupprted(e.as_ref())
                    || FunSpecGenerator::expr_has_spec_unsupprted(then_.as_ref())
                    || if let Some(else_) = else_ {
                        FunSpecGenerator::expr_has_spec_unsupprted(else_.as_ref())
                    } else {
                        false
                    }
            }
            Exp_::While(e, b) => {
                FunSpecGenerator::expr_has_spec_unsupprted(e.as_ref())
                    || FunSpecGenerator::expr_has_spec_unsupprted(b.as_ref())
            }
            Exp_::Loop(e) => FunSpecGenerator::expr_has_spec_unsupprted(e),
            Exp_::Block(_) => {
                // TODO
                false
            }
            Exp_::Lambda(_, _) => false,
            Exp_::Quant(_, _, _, _, _) => false,
            Exp_::ExpList(es) => exprs_has_spec_unsupprted(es),
            Exp_::Unit => false,
            Exp_::Assign(l, r) => {
                FunSpecGenerator::expr_has_spec_unsupprted(l.as_ref())
                    || FunSpecGenerator::expr_has_spec_unsupprted(r.as_ref())
            }
            Exp_::Return(_) => false,
            Exp_::Abort(_) => false,
            Exp_::Break => false,
            Exp_::Continue => false,
            Exp_::Dereference(_) => true,
            Exp_::UnaryExp(_, e) => FunSpecGenerator::expr_has_spec_unsupprted(e),
            Exp_::BinopExp(l, _, r) => {
                FunSpecGenerator::expr_has_spec_unsupprted(l.as_ref())
                    || FunSpecGenerator::expr_has_spec_unsupprted(r.as_ref())
            }
            Exp_::Borrow(_, _) => true,
            Exp_::Dot(l, _) => FunSpecGenerator::expr_has_spec_unsupprted(l.as_ref()),
            Exp_::Index(l, r) => {
                FunSpecGenerator::expr_has_spec_unsupprted(l.as_ref())
                    || FunSpecGenerator::expr_has_spec_unsupprted(r.as_ref())
            }
            Exp_::Cast(e, _) => FunSpecGenerator::expr_has_spec_unsupprted(e.as_ref()),
            Exp_::Annotate(_, _) => false,
            Exp_::Spec(_) => false,
            _ => false,
        }
    }
}

impl FunSpecGenerator {
    fn emit_local_and_imports(
        shadow: &ShadowItems,
        statements: &mut String,
        e: &Exp,
        imports: &mut GroupShadowItemUse,
        local_emited: &mut HashSet<usize>,
        body: &Sequence,
    ) -> bool // emit ok ???
    {
        let (names, modules) = names_and_modules_in_expr(e);
        for (name, is_module) in {
            let mut x: Vec<_> = names.iter().map(|x| (x.clone(), false)).collect();
            x.extend(
                modules
                    .iter()
                    .map(|x| (x.clone(), true))
                    .collect::<Vec<_>>()
                    .into_iter(),
            );
            x
        } {
            if let Some(x) = shadow.query(name, is_module) {
                match x {
                    ShadowItem::Use(x) => {
                        imports.insert(x.clone());
                    }
                    ShadowItem::Local(index) => {
                        if local_emited.contains(&index.index) == false {
                            let seq = body.1.get(index.index).unwrap().clone();
                            match &seq.value {
                                SequenceItem_::Seq(_) => {
                                    // TODO looks emitable.
                                    return false;
                                }
                                SequenceItem_::Declare(_, _) => {
                                    return false;
                                }
                                SequenceItem_::Bind(_, _, e) => {
                                    if FunSpecGenerator::expr_has_spec_unsupprted(e) {
                                        return false;
                                    }
                                    if false
                                        == FunSpecGenerator::emit_local_and_imports(
                                            shadow,
                                            statements,
                                            e.as_ref(),
                                            imports,
                                            local_emited,
                                            body,
                                        )
                                    {
                                        return false;
                                    }
                                }
                            }
                            // emit right here,right now.
                            statements.push_str(
                                format!("{}{};\n", indent(2), format_xxx(&seq, true)).as_str(),
                            );
                            local_emited.insert(index.index);
                        }
                    }
                }
            }
        }

        true
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
            _ => Err(()),
        }
    }
}

pub(crate) fn format_xxx<T>(
    e: &T,
    replace_not: bool, // ast_debug print `!a` as `! a`.
) -> String
where
    T: MyAstDebug,
{
    // use move_compiler::shared::ast_debug::AstWriter;
    let mut w = AstWriter::new(false);
    e.my_ast_debug(&mut w);
    let x = w.to_string();
    // TOTO better way to do this.
    let mut x = x.trim_end().to_string();
    if replace_not {
        x = x.replacen("! ", "!", usize::MAX);
    }
    x
}

pub(crate) fn indent(num: usize) -> String {
    "    ".to_string().repeat(num)
}

fn names_and_modules_in_expr(
    e: &Exp,
) -> (
    HashSet<Symbol>, // names
    HashSet<Symbol>, // modules
) {
    let mut names = Default::default();
    let mut modules = Default::default();
    names_and_modules_in_expr_(&mut names, &mut modules, e);
    return (names, modules);

    fn names_and_modules_in_expr_(
        names: &mut HashSet<Symbol>,
        modules: &mut HashSet<Symbol>,
        e: &Exp,
    ) {
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
        fn handle_ty(names: &mut HashSet<Symbol>, modules: &mut HashSet<Symbol>, ty: &Type) {
            match &ty.value {
                Type_::Apply(chain, tys) => {
                    handle_tys(names, modules, tys);
                    handle_name_access_chain(names, modules, chain);
                }
                Type_::Ref(_, ty) => {
                    handle_ty(names, modules, ty);
                }
                Type_::Fun(_, _) => {}
                Type_::Unit => {}
                Type_::Multiple(tys) => {
                    handle_tys(names, modules, tys);
                }
            }
        }
        fn handle_tys(names: &mut HashSet<Symbol>, modules: &mut HashSet<Symbol>, tys: &Vec<Type>) {
            for ty in tys.iter() {
                handle_ty(names, modules, ty);
            }
        }
        fn handle_exprs(
            names: &mut HashSet<Symbol>,
            modules: &mut HashSet<Symbol>,
            exprs: &Vec<Exp>,
        ) {
            for e in exprs.iter() {
                names_and_modules_in_expr_(names, modules, e);
            }
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
                    handle_tys(names, modules, tys);
                };
            }
            Exp_::Call(chain, _, tys, exprs) => {
                handle_name_access_chain(names, modules, chain);
                if let Some(tys) = tys {
                    handle_tys(names, modules, tys);
                };
                handle_exprs(names, modules, &exprs.value);
            }
            Exp_::Pack(chain, tys, exprs) => {
                handle_name_access_chain(names, modules, chain);
                if let Some(tys) = tys {
                    handle_tys(names, modules, tys);
                };
                for (_, e) in exprs.iter() {
                    names_and_modules_in_expr_(names, modules, e);
                }
            }
            Exp_::Vector(_, tys, exprs) => {
                if let Some(tys) = tys {
                    handle_tys(names, modules, tys);
                };
                handle_exprs(names, modules, &exprs.value);
            }
            Exp_::IfElse(con, then_, else_) => {
                names_and_modules_in_expr_(names, modules, con.as_ref());
                names_and_modules_in_expr_(names, modules, then_.as_ref());
                if let Some(else_) = else_ {
                    names_and_modules_in_expr_(names, modules, else_.as_ref());
                }
            }
            Exp_::While(_, _) => {}
            Exp_::Loop(_) => {}
            Exp_::Block(_b) => {}
            Exp_::Lambda(_, _) => {}
            Exp_::Quant(_, _, _, _, _) => {}
            Exp_::ExpList(exprs) => {
                handle_exprs(names, modules, exprs);
            }
            Exp_::Unit => {}
            Exp_::Assign(_, _) => {}
            Exp_::Return(_) => {}
            Exp_::Abort(_) => {}
            Exp_::Break => {}
            Exp_::Continue => {}
            Exp_::Dereference(e) => {
                names_and_modules_in_expr_(names, modules, e.as_ref());
            }
            Exp_::UnaryExp(_, e) => {
                names_and_modules_in_expr_(names, modules, e.as_ref());
            }
            Exp_::BinopExp(l, _, r) => {
                names_and_modules_in_expr_(names, modules, l.as_ref());
                names_and_modules_in_expr_(names, modules, r.as_ref());
            }
            Exp_::Borrow(_, e) => {
                names_and_modules_in_expr_(names, modules, e.as_ref());
            }
            Exp_::Dot(a, _) => {
                names_and_modules_in_expr_(names, modules, a.as_ref());
            }
            Exp_::Index(a, b) => {
                names_and_modules_in_expr_(names, modules, a.as_ref());
                names_and_modules_in_expr_(names, modules, b.as_ref());
            }
            Exp_::Cast(a, _) => {
                names_and_modules_in_expr_(names, modules, a.as_ref());
            }
            Exp_::Annotate(a, _) => {
                names_and_modules_in_expr_(names, modules, a.as_ref());
            }
            Exp_::Spec(_) => {}
            _ => {}
        };
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct ShadowItemUseItem {
    lead: LeadingNameAccess_,
    module: Symbol,
    item: Symbol,
    alias: Option<Symbol>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
struct ShadowItemUseModule {
    lead: LeadingNameAccess_,
    module: Symbol,
    alias: Option<Symbol>,
}

#[derive(Clone, Copy, Debug)]
struct ShadowItemLocal {
    index: usize,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
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
            v_str.push('{');
            let v_len = v.len();
            for (index, vv) in v.iter().enumerate() {
                v_str.push_str(
                    match vv {
                        ShadowItemUse::Module(x) => match &x.alias {
                            Some(alias) => format!("{} as {}", "Self", alias.as_str()),
                            None => "Self".to_string(),
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
            v_str.push('}');
            ret.push_str(
                format!(
                    "{}use {}::{}::{};\n",
                    indent(indent_size),
                    match &k.0 {
                        LeadingNameAccess_::AnonymousAddress(x) =>
                            format!("0x{}", x.into_inner().short_str_lossless()),
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

#[derive(Clone, Debug)]
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
                    }))
                };
                if let Some(xxx) = ret.get_mut(&name) {
                    xxx.push(item);
                } else {
                    ret.insert(name, vec![item]);
                }
            }
        }
        // _ => {}
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
    fn query(&self, name: Symbol, module_name: bool) -> Option<&ShadowItem> {
        if module_name {
            for i in self.items.get(&name)?.iter().rev() {
                match i {
                    ShadowItem::Use(x) => match x {
                        ShadowItemUse::Module(_) => return Some(i),
                        ShadowItemUse::Item(_) => {}
                    },
                    ShadowItem::Local(_) => {}
                }
            }
            None
        } else {
            self.items.get(&name).map(|x| x.last()).flatten()
        }
    }
}

pub trait GetExprType {
    fn get_expr_type(&self, e: &Exp) -> Option<&ResolvedType>;
}

#[derive(Default)]
pub struct GetExprTypeImpl {
    types: HashMap<Loc, ResolvedType>,
}

impl GetExprType for GetExprTypeImpl {
    fn get_expr_type(&self, e: &Exp) -> Option<&ResolvedType> {
        self.types.get(&e.loc)
    }
}

impl GetExprTypeImpl {
    pub(crate) fn new(filepath: &PathBuf, p: &Project) -> Self {
        let mut x = Self::default();
        let _ = p.run_visitor_for_file(&mut x, filepath, false);
        x
    }
}

impl crate::project_ori::ItemOrAccessHandler for GetExprTypeImpl {
    fn need_expr_type(&self) -> bool {
        true
    }
    fn handle_expr_typ(&mut self, exp: &Exp, ty: ResolvedType) {
        self.types.insert(exp.loc, ty);
    }

    fn function_or_spec_body_should_visit(&self, _range: &crate::utils_ori::FileRange) -> bool {
        true
    }

    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }

    fn finished(&self) -> bool {
        false
    }
}

impl std::fmt::Display for GetExprTypeImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", "visit for generate spec.")
    }
}
