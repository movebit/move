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
    model::{GlobalEnv, FunctionEnv, StructEnv, ModuleId, NodeId, StructId},
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
use crate::project::Project as ProjectZX;

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

pub fn generate_fun_spec_zx(project: &ProjectZX, global_env: &GlobalEnv, f: &FunctionEnv, fpath: &PathBuf) -> String {
    let mut g = FunSpecGenerator::new();
    g.generate_zx(project, global_env, f, fpath);
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

    pub(crate) fn generate_zx(&mut self, project: &ProjectZX, global_env: &GlobalEnv, f: &FunctionEnv, fpath: &PathBuf) {
        use crate::type_display_zx::TypeDisplayZX;
        let mut addr_2_addrname = HashMap::new();
        for helper1 in project.targets.iter() {
            for (addr_name, addr) in helper1.named_address_map.iter() {
                addr_2_addrname.insert(addr.to_string(), addr_name.clone());
            }
        }
        
        let display_context = f.get_type_display_ctx();
        self.result
            .push_str(format!("{}spec {}", indent(1), f.get_name_str()).as_str());

        self.result.push_str("(");

        let para_len = f.get_parameter_count();
        if para_len > 0 {
            for (index, para) in f.get_parameters().iter().enumerate() {
                self.result.push_str(para.0.display(f.symbol_pool()).to_string().as_str());
                self.result.push_str(": ");
                let display_context_para = TypeDisplayZX {
                    type_: &para.1,
                    context: &display_context,
                    addr_2_addrname: &addr_2_addrname,
                };
                let para_type_string = display_context_zx.to_string();
                self.result.push_str(para_type_string.as_str());
                if (index + 1) < para_len {
                    self.result.push_str(", ");
                }
            }
        }
        self.result.push_str(")");
        
        let return_type = f.get_result_type();
        let display_context_return = TypeDisplayZX {
            type_: &return_type,
            context: &display_context,
            addr_2_addrname: &addr_2_addrname,
        };
        let mut return_type_string = display_context_return.to_string();
        
        return_type_string.insert_str(0, ": ");
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
        self.result.push_str(format!("{}}}\n", indent(1)).as_str());
    }

    fn generate_body_zx(&self, f: &FunctionEnv, global_env: &GlobalEnv, fpath: &PathBuf) -> String {        

        eprintln!("generate_body_zx-----------");

        let mut statements = String::new();
        let mut shadows = ShadowItemsZX::new(f.get_loc());
        let mut local_emited = HashSet::new();
    
        if let Some(exp) = f.get_def() {
            // get_shadows(exp, global_env, &mut shadows);
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

    fn struct_str(mid: ModuleId, sid: StructId, context: &TypeDisplayContext, project: &ProjectZX) -> String {
        let env = context.env;
        if let Some(builder_table) = context.builder_struct_table {
            let qsym = builder_table.get(&(mid, sid)).expect("type known");
            qsym.display(context.env).to_string()
        } else {
            let mut addr_2_addrname = HashMap::new();
            
            for helper1 in project.targets.iter() {
                for (addr_name, addr) in helper1.named_address_map.iter() {
                    addr_2_addrname.insert(addr.to_string(), addr_name.clone());
                }
            }

            let struct_module_env = env.get_module(mid);
            let struct_module_env_full_name = struct_module_env.get_full_name_str();
            let addr_end = struct_module_env_full_name.find("::").unwrap_or_default();
            let addr = struct_module_env_full_name[0..addr_end].to_string();
        
            let struct_env = struct_module_env.clone().into_struct(sid);
            format!(
                "{}::{}::{}",
                addr_2_addrname.get(&addr).unwrap_or(&String::from("0x0")),
                struct_module_env.get_name().display(env).to_string(),
                struct_env.get_name().display(env.symbol_pool()),
            )
        }
    }
}

impl FunSpecGenerator {
   
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
        
        let mut items = FunSpecGenerator::collect_spec_exp_zx(self, exp, env);        
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
                    // let temps = left.used_temporaries(func_env);
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
                SpecExpItemZX::MarcoAbort{if_exp, abort_exp} => {
                    match if_exp.as_ref() {
                        MoveModelExpData::Call(_, op, _) => match op {
                            MoveModelOperation::Eq | MoveModelOperation::Neq | 
                            MoveModelOperation::Lt | MoveModelOperation::Gt | 
                            MoveModelOperation::Le | MoveModelOperation::Ge => {
                                FunSpecGenerator::handle_binop_exp(statements, if_exp, op, abort_exp, env);
                            }
                            MoveModelOperation::MoveFunction(_, _) => {
                                FunSpecGenerator::handle_funcop_exp(statements, if_exp, abort_exp, env);
                            }
                            _ => {}
                        },
                        _ => {}
                    }
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

    fn handle_binop_exp(statements: &mut String, if_exp: &MoveModelExp, op: &MoveModelOperation, abort_exp: &MoveModelExp, env: &GlobalEnv) {
        fn inverse_binop_zx(op: &MoveModelOperation) -> (String, String) {
            match op {
                MoveModelOperation::Eq => (String::from("=="), String::from("!=")),
                MoveModelOperation::Neq => (String::from("!="), String::from("==")),
                MoveModelOperation::Lt => (String::from("<"), String::from(">=")),
                MoveModelOperation::Gt => (String::from(">"), String::from("<=")),
                MoveModelOperation::Le => (String::from("<="), String::from(">")),
                MoveModelOperation::Ge => (String::from(">="), String::from("<")),
                _ => (String::from("_"), String::from("_")),
            }
        }
        
        let op2 = inverse_binop_zx(op);
                            
        let if_exp_node_id = if_exp.as_ref().node_id();
        let mut if_exp_node_loc = env.get_node_loc(if_exp_node_id);
        let if_exp_str = match env.get_source(&if_exp_node_loc) {
            Ok(x) => FunSpecGenerator::remove_parentheses(x),
            Err(_) => return,
        };
        
        let mut if_exp_inverse_str = String::new();
        if let Some(_) = if_exp_str.find(op2.0.as_str()) {
            eprintln!("before {:?}" , if_exp_str);
            if_exp_inverse_str = if_exp_str.replace(op2.0.as_str(), op2.1.as_str());
            eprintln!("after {:?}" , if_exp_str);
        } else {
            return;
        }

        let abort_exp_node_id = abort_exp.as_ref().node_id();
        let abort_exp_node_loc = env.get_node_loc(abort_exp_node_id);
        let abort_exp_str = match env.get_source(&abort_exp_node_loc) {
            Ok(x) => FunSpecGenerator::remove_parentheses(x),
            Err(_) => return,
        };

        statements.push_str(
            format!(
                "{}aborts_if {} with {};\n",
                indent(2),
                if_exp_inverse_str,
                abort_exp_str,
            )
            .as_str(),
        );
    }

    fn handle_funcop_exp(statements: &mut String, if_exp: &MoveModelExp, abort_exp: &MoveModelExp, env: &GlobalEnv) {                  
        let if_exp_node_id = if_exp.as_ref().node_id();
        let mut if_exp_node_loc = env.get_node_loc(if_exp_node_id);
        let if_exp_str = match env.get_source(&if_exp_node_loc) {
            Ok(x) => FunSpecGenerator::remove_parentheses(x),
            Err(_) => return,
        };

        let abort_exp_node_id = abort_exp.as_ref().node_id();
        let abort_exp_node_loc = env.get_node_loc(abort_exp_node_id);
        let abort_exp_str = match env.get_source(&abort_exp_node_loc) {
            Ok(x) => FunSpecGenerator::remove_parentheses(x),
            Err(_) => return,
        };

        statements.push_str(
            format!(
                "{}aborts_if !{} with {};\n",
                indent(2),
                if_exp_str,
                abort_exp_str,
            )
            .as_str(),
        );
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

pub(crate) fn indent(num: usize) -> String {
    "    ".to_string().repeat(num)
}



