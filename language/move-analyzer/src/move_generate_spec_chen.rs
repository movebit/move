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
        fn collect_add_sub_etc_(ret: &mut Vec<SpecExpItem>, e: &Exp) {
            match &e.value {
                Exp_::Call(n, _, _, e_exp) => {
                    match &n.value {
                        NameAccessChain_::One(n_1) => {}
                        _ => {}
                    }

                    for e in e_exp.value.iter() {
                        collect_add_sub_etc_(ret, e)
                    }
                }
                Exp_::Pack(_, _, e_exp) => {
                    for e in e_exp.iter() {
                        collect_add_sub_etc_(ret, &e.1)
                    }
                }
                Exp_::Vector(_, _, e_exp) => {
                    for e in e_exp.value.iter() {
                        collect_add_sub_etc_(ret, &e)
                    }
                }
                Exp_::IfElse(e_exp, then_, else_) => {
                    collect_add_sub_etc_(ret, &e_exp.as_ref());
                    collect_add_sub_etc_(ret, &then_.as_ref());
                    if let Some(else_) = else_ {
                        collect_add_sub_etc_(ret, &else_.as_ref());
                    } else {
                    }
                }
                Exp_::While(a, b) => {
                    collect_add_sub_etc_(ret, &a.as_ref());
                    collect_add_sub_etc_(ret, &b.as_ref())
                }
                Exp_::Loop(e_exp) => collect_add_sub_etc_(ret, &e_exp),
                Exp_::Block(s) => {
                    //TODO
                    // for e1 in s.1.to_vec().iter()
                    // {
                    //     match &e1.value
                    //     {
                    //         SequenceItem_::Bind(_,_,e_t) =>
                    //         {
                    //             collect_add_sub_etc_(ret,&e_t.as_ref())
                    //         }
                    //         SequenceItem_::Seq(e_t) =>
                    //         {
                    //             collect_add_sub_etc_(ret,&e_t.as_ref())
                    //         }
                    //     }
                    // }
                }
                Exp_::Lambda(_, e_exp) => collect_add_sub_etc_(ret, &e_exp),
                Exp_::Quant(_, _, a, _, b) => {
                    for e_1 in a.iter() {
                        for e_2 in e_1.iter() {
                            collect_add_sub_etc_(ret, &e_2)
                        }
                    }
                    collect_add_sub_etc_(ret, &b.as_ref())
                }
                Exp_::ExpList(e_exp) => {
                    for e in e_exp.iter() {
                        collect_add_sub_etc_(ret, &e)
                    }
                }
                Exp_::Assign(a, b) => {
                    collect_add_sub_etc_(ret, &a.as_ref());
                    collect_add_sub_etc_(ret, &b.as_ref())
                }
                Exp_::Abort(e_exp) => collect_add_sub_etc_(ret, &e_exp.as_ref()),
                Exp_::Dereference(e_exp) => collect_add_sub_etc_(ret, &e_exp.as_ref()),
                Exp_::UnaryExp(_, e_exp) => collect_add_sub_etc_(ret, &e_exp.as_ref()),
                Exp_::BinopExp(l, op, r) => {
                    if let Some(reason) = BinOPReason::cause_exception(op.value.clone()) {
                        ret.push(SpecExpItem::BinOP(
                            reason,
                            l.as_ref().clone(),
                            r.as_ref().clone(),
                        ));
                    }
                    collect_add_sub_etc_(ret, l.as_ref());
                    collect_add_sub_etc_(ret, r.as_ref());
                }

                Exp_::Borrow(_, e_exp) => collect_add_sub_etc_(ret, &e_exp.as_ref()),
                Exp_::Dot(e_exp, _) => collect_add_sub_etc_(ret, &e_exp.as_ref()),
                Exp_::Index(a, b) => {
                    collect_add_sub_etc_(ret, &a.as_ref());
                    collect_add_sub_etc_(ret, &b.as_ref())
                }
                Exp_::Cast(e_exp, t) => {
                    collect_add_sub_etc_(ret, &e_exp.as_ref())
                    //TODO TYPE
                }
                Exp_::Annotate(e_exp, t) => {
                    collect_add_sub_etc_(ret, &e_exp.as_ref())
                    //TODO TYPE
                }
                _ => {}
            }
        }

        collect_add_sub_etc_(&mut ret, e);
        ret
    }
}

pub(crate) enum SpecExpItem {
    BinOP(BinOPReason, Exp, Exp),
    TypeOf(Type),
}

/// 这个枚举代表操作符错误类型
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
