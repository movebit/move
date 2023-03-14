use super::move_generate_spec::FunSpecGenerator;
use move_compiler::parser::ast::*;

/*
     { let c = 0;
    let a = (b + 1) /  c ; }
    spec {
        aborts_if a + 1 < a;
        aborts_if c == 0;
    }

    a + 1
    (a +1) / c

*/
impl FunSpecGenerator {
    // 针对加法 减法 移位等运算可能会参数溢出等异常
    // 这个函数收集 e 中所有的加法减法等操作
    pub(crate) fn collect_spec_exp(e: &Exp) -> Vec<SpecExpItem> {
        let mut ret = Vec::new();
        fn collect_spec_exp_(ret: &mut Vec<SpecExpItem>, e: &Exp) {
            match &e.value {
                Exp_::Call(n, _, tys, es) => {
                    let first_ty = tys.as_ref().map(|x| x.get(0)).flatten();
                    let first_e = es.value.get(0);
                    match &n.value {
                        NameAccessChain_::One(name) => match name.value.as_str() {
                            "borrow_global_mut" if first_ty.is_some() && first_e.is_some() => {
                                let ty = first_ty.clone().unwrap().clone();
                                ret.push(SpecExpItem::BorrowGlobal {
                                    ty,
                                    addr: first_e.clone().unwrap().clone(),
                                });
                            }
                            "type_of" if first_ty.is_some() => {
                                let ty = first_ty.clone().unwrap().clone();
                                ret.push(SpecExpItem::TypeOf { ty });
                            }
                            "type_name" if first_ty.is_some() => {
                                let ty = first_ty.clone().unwrap().clone();
                                ret.push(SpecExpItem::TypeName { ty });
                            }
                            _ => {}
                        },
                        _ => {}
                    }

                    for e in es.value.iter() {
                        collect_spec_exp_(ret, e)
                    }
                }
                Exp_::Pack(_, _, e_exp) => {
                    for e in e_exp.iter() {
                        collect_spec_exp_(ret, &e.1)
                    }
                }
                Exp_::Vector(_, _, e_exp) => {
                    for e in e_exp.value.iter() {
                        collect_spec_exp_(ret, &e)
                    }
                }
                Exp_::IfElse(e_exp, then_, else_) => {
                    collect_spec_exp_(ret, &e_exp.as_ref());
                    collect_spec_exp_(ret, &then_.as_ref());
                    if let Some(else_) = else_ {
                        collect_spec_exp_(ret, &else_.as_ref());
                    } else {
                    }
                }
                Exp_::While(a, b) => {
                    collect_spec_exp_(ret, &a.as_ref());
                    collect_spec_exp_(ret, &b.as_ref())
                }
                Exp_::Loop(_) => {}
                Exp_::Block(_) => {}
                Exp_::Lambda(_, e_exp) => collect_spec_exp_(ret, &e_exp),
                Exp_::Quant(_, _, _, _, _) => {}
                Exp_::ExpList(es) => {
                    for e in es.iter() {
                        collect_spec_exp_(ret, &e)
                    }
                }
                Exp_::Assign(a, b) => {
                    collect_spec_exp_(ret, &a.as_ref());
                    collect_spec_exp_(ret, &b.as_ref())
                }
                Exp_::Abort(e_exp) => collect_spec_exp_(ret, &e_exp.as_ref()),
                Exp_::Dereference(e_exp) => collect_spec_exp_(ret, &e_exp.as_ref()),
                Exp_::UnaryExp(_, e_exp) => collect_spec_exp_(ret, &e_exp.as_ref()),
                Exp_::BinopExp(l, op, r) => {
                    if let Some(reason) = BinOPReason::cause_exception(op.value.clone()) {
                        ret.push(SpecExpItem::BinOP {
                            reason,
                            left: l.as_ref().clone(),
                            right: r.as_ref().clone(),
                        });
                    }
                    collect_spec_exp_(ret, l.as_ref());
                    collect_spec_exp_(ret, r.as_ref());
                }

                Exp_::Borrow(_, e) => collect_spec_exp_(ret, &e.as_ref()),
                Exp_::Dot(e, _) => collect_spec_exp_(ret, &e.as_ref()),
                Exp_::Index(a, b) => {
                    collect_spec_exp_(ret, &a.as_ref());
                    collect_spec_exp_(ret, &b.as_ref())
                }
                Exp_::Cast(e, t) => {
                    collect_spec_exp_(ret, &e.as_ref())
                    //TODO TYPE
                }
                Exp_::Annotate(e_exp, t) => {}
                _ => {}
            }
        }

        collect_spec_exp_(&mut ret, e);
        ret
    }
}

#[derive(Clone, Debug)]
pub(crate) enum SpecExpItem {
    BinOP {
        reason: BinOPReason,
        left: Exp,
        right: Exp,
    },
    TypeOf {
        ty: Type,
    },
    TypeName {
        ty: Type,
    },
    BorrowGlobal {
        ty: Type,
        addr: Exp,
    },
}

/// 这个枚举代表操作符错误类型
#[derive(Clone, Copy, Debug)]
pub(crate) enum BinOPReason {
    OverFlowADD,
    OverFlowMUL,
    OverFlowSHL,
    DivByZero,
    UnderFlow,
}

impl BinOPReason {
    /// 匹配可能有问题的错误类型
    fn cause_exception(op: BinOp_) -> Option<Self> {
        match op {
            BinOp_::Add => Some(Self::OverFlowADD),
            BinOp_::Sub => Some(Self::UnderFlow),
            BinOp_::Mul => Some(Self::OverFlowMUL),
            BinOp_::Mod => Some(Self::DivByZero),
            BinOp_::Div => Some(Self::DivByZero),
            BinOp_::Shl => Some(Self::OverFlowSHL),
            _ => None,
        }
    }
}
