use std::{
    collections::HashMap,
    path::PathBuf, 
};

use im::HashSet;
use move_model::{
    symbol::Symbol,
    ast::{ModuleName, Operation},
    model::{GlobalEnv, Loc, NodeId, FunctionEnv},
    ast::{Exp as MoveModelExp, ExpData as MoveModelExpData, Value as MoveModelValue,
        Address as MoveModelAddress, Pattern as MoveModelPattern},
    ty::Type as MoveModelType,
};

use super::move_generate_spec::FunSpecGenerator;
use crate::utils::get_modules_by_fpath_in_target_modules;



#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ShadowItemUseItem {
    module: ModuleName,
    item: Symbol,
    alias: Option<Symbol>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct ShadowItemUseModule {
    module: ModuleName,
    alias: Option<Symbol>,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum ShadowItemUse {
    Module(ShadowItemUseModule),
    Item(ShadowItemUseItem),
}

#[derive(Clone, Copy, Debug)]
pub struct ShadowItemLocal {
    pub index: NodeId,
}


pub enum ShadowItem {
    Use(ShadowItemUse),
    Local(ShadowItemLocal),
}

pub struct ShadowItems {
    pub items: HashMap<Symbol, Vec<ShadowItem>>,
    function_loc: Loc,
}

impl ShadowItems {
    pub fn new(loc: Loc) -> Self {
        ShadowItems {
            items: HashMap::new(),
            function_loc: Loc::new(loc.file_id(), loc.span()),
        }
    }

    /// Given a file path, this function retrieves all modules within the file,
    /// then extracts all `UseDecl` declarations within each module. 
    /// Finally, it identifies `UseDecl` declarations within functions.
    pub fn get_target_use_decl(
        &self, 
        env: &GlobalEnv, 
        fpath: &PathBuf
    ) {
        eprintln!("getting use decl, fpath = {:?}", fpath.as_path());
        let vec_modules = get_modules_by_fpath_in_target_modules(env, fpath);
        for module_env in vec_modules.iter() {
            eprintln!("modlue env name = {:?}", module_env.get_full_name_str());
            let vec_use_decls = module_env.get_use_decls();
            for use_decl in vec_use_decls {
                if !is_loc_in_loc(&use_decl.loc, &self.function_loc) {
                    continue;
                }
                eprintln!("right use decl, {:?}", use_decl.module_name.display(env).to_string());
            }
        }
    }

    pub fn insert(&mut self, name: Symbol, item: ShadowItem) {
        if let Some(x) = self.items.get_mut(&name) {
            x.push(item);
        } else {
            self.items.insert(name, vec![item]);
        }
    }
}

fn is_loc_in_loc(loc: &Loc, loc_range: &Loc) -> bool {
    if loc.span().start() < loc_range.span().start() 
    || loc.span().start() > loc_range.span().end()
    || loc.span().end() < loc_range.span().start()
    || loc.span().end() > loc_range.span().end() {
        return false;
    }
    return true;
}

pub fn get_shadows(exp: &MoveModelExp, env: &GlobalEnv, shadows: &mut ShadowItems) {
    let exp_data = exp.as_ref();
    match exp_data {
        MoveModelExpData::Invalid(_) => {eprintln!("body is Invalid")},
        MoveModelExpData::Value(_, v) => {
            handle_expdata_value(v, env);
        },
        MoveModelExpData::LocalVar(_, sym) => {
            eprintln!("LocalVar = {:?}", sym.display(env.symbol_pool()).to_string());
        },
        MoveModelExpData::Temporary(_, _) => {eprintln!("body is Temporary")},
        MoveModelExpData::Call(node_id, oper, args) => {
            eprintln!("call op is {:?}", oper.display(env, *node_id).to_string());
            for arg in args.iter() {
                eprint!("    ");
                get_shadows(arg, env, shadows);
            }
        },
        MoveModelExpData::Invoke(_, _, _) => {eprintln!("body is Invalid")},
        MoveModelExpData::Lambda(_, _, _) => {eprintln!("body is Lambda")},
        MoveModelExpData::Quant(_,_,_,_,_,_) => {eprintln!("body is Quant")},
        MoveModelExpData::Block(_,p,s,exp) => {
            eprint!("body is Block");
            handle_expdata_block_parren(p, env, shadows);
            if let Some(op_exp) = s {
                get_shadows(op_exp, env, shadows);
            }
            eprint!("body is Block ->");
            get_shadows(exp, env, shadows)
        },
        MoveModelExpData::IfElse(_,if_exp,if_do_exp,else_do_exp) => {
            eprintln!("body is IfElse ->");
            eprint!("if exp: ");
            get_shadows(if_exp, env, shadows);
            eprint!("if do exp: ");
            get_shadows(if_do_exp, env, shadows);
            eprint!("if exp: ");
            get_shadows(else_do_exp, env, shadows);
        },
        MoveModelExpData::Return(_,_) => {eprintln!("body is Return")},
        MoveModelExpData::Sequence(_,vec_exp) => {
            
            for exp in vec_exp.iter() {
                eprint!("body is Sequence -> ");
                get_shadows(exp, env, shadows);
            }
        },
        MoveModelExpData::Loop(_,_) => {eprintln!("body is Loop")},
        MoveModelExpData::LoopCont(_,_) => {eprintln!("body is LoopCont")},
        MoveModelExpData::Assign(_,_,_) => {eprintln!("body is Assign")},
        MoveModelExpData::Mutate(_,_,_) => {eprintln!("body is Mutate")},
        MoveModelExpData::SpecBlock(_,_) => {eprintln!("body is SpecBlock")},
    }
}

pub fn handle_expdata_value(v: &MoveModelValue, env: &GlobalEnv) {
    eprint!("Value ");
    match v {
        MoveModelValue::Address(x) => {
            handle_expdata_value_address(x, env);
        },
        MoveModelValue::Number(x) => {eprintln!("Number = {:?}", x)},
        MoveModelValue::Bool(x) => {eprintln!("Bool = {:?}", x)},
        MoveModelValue::ByteArray(x) => {
            eprint!("ByteArray vec<u8>: ");
            for u in x.iter() {
                eprint!("{:?} ", u);
            }
            eprintln!();
        },
        MoveModelValue::AddressArray(x) => {
            for y in x.iter() {
                handle_expdata_value_address(y, env);
            }
        },
        MoveModelValue::Vector(x) => {
            for y in x.iter() {
                handle_expdata_value(y, env);
            }
        },
    }
}

pub fn handle_expdata_value_address(addr: &MoveModelAddress, env: &GlobalEnv) {
    match addr {
        MoveModelAddress::Numerical(x) => {
            eprintln!("Numerical = {:?}", x);
        },
        MoveModelAddress::Symbolic(x) => {
            eprintln!("Symbolic = {:?}", x.display(env.symbol_pool()).to_string());
        },
    }
}

pub fn handle_expdata_block_parren(
    p: &MoveModelPattern, 
    env: &GlobalEnv, 
    shadows: &mut ShadowItems
) {
    let vec_sym = p.vars();
    for (node_id, sym) in vec_sym.iter() {
        shadows.insert(
            sym.clone(), 
            ShadowItem::Local(ShadowItemLocal { index:*node_id })
        );
    }
}

#[derive(Clone)]
pub(crate) enum SpecExpItem {
    BinOP {
        reason: BinOPReason,
        left: MoveModelExp,
        right: MoveModelExp,
    },
    TypeOf {
        ty: MoveModelType,
    },
    TypeName {
        ty: MoveModelType,
    },
    BorrowGlobalMut {
        ty: MoveModelType,
        addr: MoveModelExp,
    },
    PatternLet {
        left: Symbol,
        right: MoveModelExp,
    },
    MarcoAbort {
        if_exp:MoveModelExp,
        abort_exp: MoveModelExp,
    }
}


/// 这个枚举代表操作符错误类型
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum BinOPReason {
    OverFlowADD,
    OverFlowMUL,
    OverFlowSHL,
    DivByZero,
    UnderFlow,
}

impl FunSpecGenerator {
    fn collect_spec_exp_op_movefunc(
        &self, 
        ret: &mut Vec<SpecExpItem>,
        func_env: &FunctionEnv, 
        vec_exp: &Vec<MoveModelExp>,
    ) {
        const TYPE_OF: &str = "type_of";
        const TYPE_NAME: &str = "type_name";
        const TYPE_INFO: &str = "type_info";
        let para = func_env.get_parameters();
        match func_env.get_name_str().as_str() {
            "borrow_global_mut" if !para.is_empty() && !vec_exp.is_empty() => {
                ret.push(SpecExpItem::BorrowGlobalMut {
                        ty: para.get(0).unwrap().1.clone(),
                        addr: vec_exp.get(0).unwrap().clone(),
                    }
                )
            },
            TYPE_OF if !para.is_empty() => {
                ret.push(SpecExpItem::TypeOf {
                        ty: para.get(0).unwrap().1.clone(),
                    }
                )
            },
            TYPE_NAME if !para.is_empty() => {
                ret.push(SpecExpItem::TypeOf {
                        ty: para.get(0).unwrap().1.clone(),
                }
            )
            },
            _ => {}
        }
    }

    fn collect_spec_exp_op(
        &self, 
        ret: &mut Vec<SpecExpItem>, 
        op: &Operation,
        vec_exp: &Vec<MoveModelExp>,
        env: &GlobalEnv
    ) {
        match op {
            Operation::Add => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::OverFlowADD,
                        left: vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Sub => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::UnderFlow,
                        left: vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Mul => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::OverFlowMUL,
                        left: vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Mod => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::DivByZero,
                        left:vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Div => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::DivByZero,
                        left:vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Shl => {
                eprintln!("collect spec exp Call Shl");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
                ret.push(SpecExpItem::BinOP {
                        reason: BinOPReason::OverFlowSHL,
                        left: vec_exp[0].clone(),
                        right: vec_exp[1].clone(),
                    }
                );
            },
            Operation::Cast => {
                eprintln!("collect spec exp Call Cast");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::Not => {
                eprintln!("collect spec exp Call Not");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            }
            Operation::Pack(_, _) => {
                eprintln!("collect spec exp Call Pack");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            }
            Operation::Vector => {
                eprintln!("collect spec exp Call Vector");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::Abort => {
                eprintln!("collect spec exp Call Abort");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::Deref => {
                eprintln!("collect spec exp Call Deref");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::Borrow(_) => {
                eprintln!("collect spec exp Call Borrow");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::Index => {
                eprintln!("collect spec exp Call Index");
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            Operation::MoveFunction(module_id, func_id) => {        
                self.collect_spec_exp_op_movefunc(
                    ret, 
                    &env.get_function(module_id.qualified(*func_id)),
                    vec_exp
                );
            }
            Operation::Tuple => {
                eprintln!("Operation Tuple is empty = {:?}", vec_exp.is_empty());
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                } 
            }
            _ => {
                eprintln!("other Operation");
            },
        }
    }

    fn collect_spec_exp_(&self, ret: &mut Vec<SpecExpItem>, e: &MoveModelExp, env: &GlobalEnv) {
        match e.as_ref() {
            MoveModelExpData::Block(_, p, assign_exp, exp) => {
                if let Some(op) = assign_exp {
                    if FunSpecGenerator::is_support_exp(op, env) {
                        self.handle_pattern(ret, p, op, env);
                    }
                }
                self.collect_spec_exp_(ret, exp, env);
            },
            MoveModelExpData::Sequence(_, vec_exp) => {
                for exp in vec_exp.iter() {
                    self.collect_spec_exp_(ret, exp, env);
                }
            },
            MoveModelExpData::Assign(_, _, exp) => {
                self.collect_spec_exp_(ret, exp, env);
            },
            MoveModelExpData::Mutate(_, exp_left, exp_right) => {
                self.collect_spec_exp_(ret, exp_left, env);
                self.collect_spec_exp_(ret, exp_right, env);
            },
            MoveModelExpData::Call(id, op, vec_exp) => {
                self.collect_spec_exp_op(ret, op, vec_exp, env);
            },
            MoveModelExpData::IfElse(_, if_exp, if_do_exp, else_do_exp) => {
                // 如果 if_do_exp为空且else_do_exp调用了 abort num, 则源代码应该调用了宏函数
                match if_do_exp.as_ref() {
                    MoveModelExpData::Call(_, _, if_do_args) => {
                        if !if_do_args.is_empty() {return ;}
                        match else_do_exp.as_ref() {
                            MoveModelExpData::Call(_, oper, abort_exp) => match oper {
                                Operation::Abort => {
                                    if abort_exp.len() != 1 {return ;}
                                    ret.push(
                                        SpecExpItem::MarcoAbort  {
                                            if_exp: if_exp.clone(),
                                            abort_exp: abort_exp.get(0).unwrap().clone(),
                                        }
                                    );
                                }
                                _ => {}
                            } ,
                            _ => {}
                        }
                    },
                    _ => {}
                }
            }
            _ => {}
        }
    }

    pub(crate) fn collect_spec_exp_zx(&self, exp: &MoveModelExp, env: &GlobalEnv) -> Vec<SpecExpItem> {
        let mut ret: Vec<SpecExpItem> = Vec::new();
        self.collect_spec_exp_(&mut ret, exp, env);
        let ret = FunSpecGenerator::handle_unused_pattern(&mut ret, env);
        return ret;
    }

    pub fn is_support_exp(e: &MoveModelExp, env: &GlobalEnv) -> bool {
        let exp_data = e.as_ref();
        match exp_data {
            MoveModelExpData::Invalid(_) => {return false},
            MoveModelExpData::Call(_, op, args) => {
                if !FunSpecGenerator::is_support_operation(op) {
                    return false;
                }
                for a in args.iter() {
                    if !FunSpecGenerator::is_support_exp(a, env) {
                        return false;
                    }
                }
            },
            MoveModelExpData::Block(_,p,s,exp) => {
                // handle_expdata_block_parren(p, env, shadows);
                if let Some(op_exp) = s {
                    if !FunSpecGenerator::is_support_exp(op_exp, env) {
                        return false;
                    }
                }
                if !FunSpecGenerator::is_support_exp(exp, env) {
                    return false;
                }
                
            },
            MoveModelExpData::IfElse(_,_,_,_) => {},
            MoveModelExpData::Return(_,_) => {},
            MoveModelExpData::Sequence(_,vec_exp) => {
                    for a in vec_exp.iter() {
                        if !FunSpecGenerator::is_support_exp(a, env) {
                            return false;
                        }
                    }
            },
            _ => {}
        }
        return true;
    }

    fn handle_pattern(&self, ret: &mut Vec<SpecExpItem>, p: &MoveModelPattern, assign_exp:&MoveModelExp, env: &GlobalEnv) {
        match p {
            MoveModelPattern::Var(_, v) => {
                self.collect_spec_exp_(ret, assign_exp, env);
                    ret.push(
                        SpecExpItem::PatternLet {
                            left: v.clone(),
                            right: assign_exp.clone(),
                        }
                    );
            },
            MoveModelPattern::Tuple(_, vec_pattern) => {
                match assign_exp.as_ref() {
                    MoveModelExpData::Call(call_node_id, oper, args) => {
                        if vec_pattern.len() != args.len() {
                            return ;
                        }

                        for index in 0..vec_pattern.len() {
                            let pat = vec_pattern.get(index).unwrap();
                            let arg_exp = args.get(index).unwrap();
                            self.handle_pattern(ret, pat, arg_exp, env);
                        }
                        
                    }
                    _ => return ,
                }  
                eprintln!("handle tuple pattern start ---------");
                for (index, pat) in vec_pattern.iter().enumerate() {
                    // self.handle_pattern(ret, pat, op, env);
                               
                }
                eprintln!("handle tuple pattern end ---------");
            }
            _ => {}
        }
    }

    fn handle_unused_pattern(items: &Vec<SpecExpItem>, env: &GlobalEnv) -> Vec<SpecExpItem>{
        let mut ret = items.clone();
        let mut used_local_var:HashSet<Symbol> = HashSet::new();
        for item in items.iter().rev() {
            match item {
                SpecExpItem::BinOP { reason, left, right } => {
                    let left_vars = left.free_vars(env);
                    let right_vars = right.free_vars(env);

                    left_vars.iter().for_each(|(sym, _)| { used_local_var.insert(sym.clone()); });
                    right_vars.iter().for_each(|(sym, _)| { used_local_var.insert(sym.clone()); });
                },
                SpecExpItem::PatternLet { left, right } => {
                    if used_local_var.contains(left) {
                        let right_vars = right.free_vars(env);
                        right_vars.iter().for_each(|(sym, _)| { used_local_var.insert(sym.clone()); });
                    } else {
                        ret.retain(|x| {
                            match x {
                                SpecExpItem::PatternLet { left: l_iter, right: r_iter } =>
                                    !(*left == *l_iter && *right == *r_iter),
                                _ => true,
                            }
                        });
                    }
                },
                _ => {}
            }
        }

        return ret;
    }

    fn is_support_operation(op:&Operation) -> bool {
        match op {
            Operation::BorrowGlobal(_)
            | Operation::Borrow(_)
            | Operation::Deref
            | Operation::MoveTo
            | Operation::MoveFrom
            | Operation::Freeze
            | Operation::Vector => return false,
            _ => return true,
        }
    }


}


