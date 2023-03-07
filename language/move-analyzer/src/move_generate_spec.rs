use move_compiler::parser::ast::*;
use move_compiler::shared::Identifier;

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
            .push_str(format!("    spec {}", x.name.0.value.as_str()).as_str());
        self.result.push_str("{\n");
        self.result.push_str("\n");
        self.result.push_str("    }\n\n")
    }
}

#[derive(Default)]
pub struct FunSpecGenerator {
    result: String,
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
            .push_str(format!("    spec {}", f.name.0.value.as_str()).as_str());
        let para_len = f.signature.parameters.len();
        self.result.push_str("(");
        if para_len > 0 {
            for (index, (var, ty)) in f.signature.parameters.iter().enumerate() {
                self.result.push_str(var.0.value.as_str());
                self.result.push_str(": ");
                self.result.push_str(format_type(ty).as_str());
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
                self.result.push_str(&format_type(&f.signature.return_type));
            }
        }
        self.result.push_str("{\n");
        self.result.push_str("\n");
        self.result.push_str("    }\n\n")
    }
}

pub(crate) fn format_type(ty: &Type) -> String {
    use move_compiler::shared::ast_debug::{AstDebug, AstWriter};
    let mut w = AstWriter::new(false);
    ty.ast_debug(&mut w);
    let x = w.to_string();
    // TOTO better way to do this.
    x.trim_end().to_string()
}
