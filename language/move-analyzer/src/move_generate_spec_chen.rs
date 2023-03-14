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
    pub(crate) fn collect_add_sub_etc(e: &Exp) -> Vec<ExpceptionItem> {
        let mut ret = Vec::new();
        fn collect_add_sub_etc_(ret: &mut Vec<ExpceptionItem>, e: &Exp) {
            match &e.value {
                Exp_::Value(_) => todo!(),
                Exp_::Move(_) => todo!(),
                Exp_::Copy(_) => todo!(),
                Exp_::Name(_, _) => todo!(),
                Exp_::Call(_, _, _, _) => todo!(),
                Exp_::Pack(_, _, _) => todo!(),
                Exp_::Vector(_, _, _) => todo!(),
                Exp_::IfElse(_, _, _) => todo!(),
                Exp_::While(_, _) => todo!(),
                Exp_::Loop(_) => todo!(),
                Exp_::Block(_) => todo!(),
                Exp_::Lambda(_, _) => todo!(),
                Exp_::Quant(_, _, _, _, _) => todo!(),
                Exp_::ExpList(_) => todo!(),
                Exp_::Unit => todo!(),
                Exp_::Assign(_, _) => todo!(),
                Exp_::Return(_) => todo!(),
                Exp_::Abort(_) => todo!(),
                Exp_::Break => todo!(),
                Exp_::Continue => todo!(),
                Exp_::Dereference(_) => todo!(),
                Exp_::UnaryExp(_, _) => todo!(),
                Exp_::BinopExp(l, op, r) => {
                    if let Some(reason) = BinOPReasion::cause_exception(op.value.clone()) {
                        ret.push(ExpceptionItem::BinOP(
                            reason,
                            l.as_ref().clone(),
                            r.as_ref().clone(),
                        ));
                    }
                }

                Exp_::Borrow(_, _) => todo!(),
                Exp_::Dot(_, _) => todo!(),
                Exp_::Index(_, _) => todo!(),
                Exp_::Cast(_, _) => todo!(),
                Exp_::Annotate(_, _) => todo!(),
                Exp_::Spec(_) => todo!(),
                Exp_::UnresolvedError => todo!(),
            }
        }
        
        collect_add_sub_etc_(&mut ret, e);
        ret
    }
}

pub(crate) enum ExpceptionItem {
    BinOP(BinOPReasion, Exp, Exp),
    TypeOf(Type),
}

pub(crate) enum BinOPReasion {
    OverFlow,
    DivByZero,
    UnderFlow,
}

impl BinOPReasion {
    fn cause_exception(op: BinOp_) -> Option<Self> {
        match op {
            BinOp_::Add => todo!(),
            BinOp_::Sub => todo!(),
            BinOp_::Mul => todo!(),
            BinOp_::Mod => todo!(),
            BinOp_::Div => todo!(),
            BinOp_::BitOr => todo!(),
            BinOp_::BitAnd => todo!(),
            BinOp_::Xor => todo!(),
            BinOp_::Shl => todo!(),
            BinOp_::Shr => todo!(),
            BinOp_::Range => todo!(),
            BinOp_::Implies => todo!(),
            BinOp_::Iff => todo!(),
            BinOp_::And => todo!(),
            BinOp_::Or => todo!(),
            BinOp_::Eq => todo!(),
            BinOp_::Neq => todo!(),
            BinOp_::Lt => todo!(),
            BinOp_::Gt => todo!(),
            BinOp_::Le => todo!(),
            BinOp_::Ge => todo!(),
        }
    }
}
