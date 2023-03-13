use super::move_generate_spec::FunSpecGenerator;
use move_compiler::parser::ast::*;
impl FunSpecGenerator {
    // 针对加法 减法 移位等运算可能会参数溢出等异常
    // 这个函数收集 e种所有的加法减法等操作
    pub(crate) fn collect_add_sub_etc(e: &Exp) -> Vec<&Exp> {
        unimplemented!()
    }
}
