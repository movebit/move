use std::f32::consts::E;

use super::move_generate_spec::FunSpecGenerator;
use move_compiler::parser::ast::*;


impl FunSpecGenerator {
    // 针对加法 减法 移位等运算可能会参数溢出等异常
    // 这个函数收集 e 中所有的加法减法等操作
    pub(crate) fn collect_add_sub_etc(e: &Exp) -> Vec<&Exp> {
        let mut v_temp:Vec<&Exp> = Vec::new();
        if FunSpecGenerator::is_binop_specful(e){
            match &e.value {
                Exp_::Call(_, _, _, e_exp) => {
                    for e in e_exp.value.iter()
                    {
                       v_temp.push(&e)
                    }
                    return v_temp
                }
                Exp_::Pack(_, _, e_exp) => {
                    for e in e_exp.iter()
                    {
                       v_temp.push(&e.1)
                    }
                    return v_temp
                }
                    
                Exp_::Vector(_, _, e_exp) => {
                    for e in e_exp.value.iter()
                    {
                       v_temp.push(&e)
                    }
                    return v_temp
                }
                        
                Exp_::IfElse(e_exp, then_, else_) => {
                    v_temp.push(&e_exp);
                    v_temp.push(&then_);
                    if let Some(else_) = else_ {
                        v_temp.push(&else_)
                    } else{}
                    
                    return v_temp
                }
                Exp_::While(a, b) => {
                    v_temp.push(&a);
                    v_temp.push(&b);
                    return v_temp
                }
                Exp_::Loop(e_exp) => {
                    v_temp.push(&e_exp);
                    return v_temp
                }
                Exp_::Block(_) => {
                    // TODO
                    return v_temp
                }
                Exp_::ExpList(e_exp) => {
                    for e in e_exp.iter()
                    {
                       v_temp.push(&e)
                    }
                    return v_temp
                }
                
                Exp_::Assign(a, b) => {
                    v_temp.push(&a);
                    v_temp.push(&a);
                    return v_temp
                }
    
                Exp_::UnaryExp(_, e_exp) => {
                    v_temp.push(&e_exp);
                    return v_temp
                }
    
              
                Exp_::Dot(e_exp, _) => {
                    v_temp.push(&e_exp);
                    return v_temp
                }
                Exp_::Index(a, b) => {
                    v_temp.push(&a);
                    v_temp.push(&b);
                    return v_temp
                }
                Exp_::Cast(a, _) =>    
                {
                    v_temp.push(&a);
                    return v_temp
                }                 
                
                Exp_::BinopExp(l, op, r) => {
                    v_temp.push(e);
                    return v_temp
                }
                _ => return v_temp
            }
        }
        else
        {
            return v_temp
        }
        }
    
    fn is_binop_specful(e:&Exp) -> bool{

        //有没有表达式？如果有，继续迭代
        //如果是Binop，看看有没有符合条件的运算，有就返回true，反之false
        //如果不是Binop，返回false

        let mut b_temp:bool = false;

        match &e.value {
            Exp_::Call(_, _, _, e_exp) => {
                for e in e_exp.value.iter()
                {
                    b_temp = b_temp || FunSpecGenerator::is_binop_specful(&e);
                }
                b_temp
            },
            Exp_::Pack(_, _, e_exp) => 
                e_exp.iter()
                    .any(|e| FunSpecGenerator::is_binop_specful(&e.1)),
            Exp_::Vector(_, _, e_exp) => 
                e_exp.value.iter()
                    .any(|e| FunSpecGenerator::is_binop_specful(&e)),
            Exp_::IfElse(e_exp, then_, else_) => {
                FunSpecGenerator::is_binop_specful(&e_exp.as_ref())||
                FunSpecGenerator::is_binop_specful(&then_.as_ref())||
                if let Some(else_) = else_ {
                    FunSpecGenerator::is_binop_specful(&else_.as_ref())
                } else {
                    false
                }              
            }
            Exp_::While(a, b) => {
                FunSpecGenerator::is_binop_specful(&a.as_ref())||
                FunSpecGenerator::is_binop_specful(&b.as_ref())
            }
            Exp_::Loop(e_exp) => FunSpecGenerator::is_binop_specful(&e_exp.as_ref()),
            Exp_::Block(_) => {
                // TODO
                false
            }
            Exp_::ExpList(e_exp) => 
                e_exp.iter()
                .any(|e| FunSpecGenerator::is_binop_specful(&e)),
            
            Exp_::Assign(l, r) => {
                FunSpecGenerator::is_binop_specful(&l.as_ref())||
                FunSpecGenerator::is_binop_specful(&r.as_ref())
            }

            Exp_::UnaryExp(_, e_exp) => FunSpecGenerator::is_binop_specful(&e_exp.as_ref()),

          
            Exp_::Dot(e_exp, _) => FunSpecGenerator::is_binop_specful(&e_exp.as_ref()),
            Exp_::Index(l, r) => {
                FunSpecGenerator::is_binop_specful(&l.as_ref())||
                FunSpecGenerator::is_binop_specful(&r.as_ref())
            }
            Exp_::Cast(e_exp, _) => FunSpecGenerator::is_binop_specful(&e_exp.as_ref()),
            Exp_::BinopExp(l, op, r) => {
                match &op.value{
                    BinOp_::Add => true,
                    BinOp_::BitAnd => true,
                    BinOp_::BitOr => true,
                    BinOp_::Div => true,
                    BinOp_::Mul => true,
                    BinOp_::Xor => true,
                    BinOp_::Sub => true,
                    _ => false
                }
            }
            _ => false
        }
    }

}


