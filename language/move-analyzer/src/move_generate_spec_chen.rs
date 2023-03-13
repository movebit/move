use super::move_generate_spec::FunSpecGenerator;
use move_compiler::parser::ast::*;
impl FunSpecGenerator {
    //  针对加法 减法 移位等运算自动生成 aborts_if
    pub(crate) fn collect_add_sub_etc(e: &Exp) -> Vec<&Exp> {
        unimplemented!()
    }
}
