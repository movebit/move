use std::{
    cell::{Cell, RefCell},
    rc::Rc,
};

use crate::diagnostics::Diagnostics;

use super::{ast::Definition, syntax::parse_file_string};
use crate::parser::ast::*;

pub struct CommentExtractor {
    comment: Vec<Comment>,
}

impl CommentExtractor {
    pub fn new(input: &str) -> Self {
        unimplemented!();
    }
}

pub enum Comment {
    Line(CommentPosition, String),
    Block(CommentPosition, String),
}
pub struct CommentPosition {
    start: u32,
    end: u32,
}

pub enum FormatErr {
    ParseFailed(Diagnostics),
}

impl From<Diagnostics> for FormatErr {
    fn from(value: Diagnostics) -> Self {
        Self::ParseFailed(value)
    }
}

pub fn format(input: &str, config: FormatConfig) -> std::result::Result<String, FormatErr> {
    let c = CommentExtractor::new(input);
    let c = c.comment.into_iter();
    let flags = crate::shared::Flags::testing();
    let mut env = crate::shared::CompilationEnv::new(flags);
    let file_hash = move_command_line_common::files::FileHash::empty();
    let defs = parse_file_string(&mut env, file_hash, input)?;
    let defs = defs.0;
    let mut f = Format::new(config, c);
    let mut ret = String::new();
    for d in defs.iter() {
        match d {
            Definition::Module(x) => ret.push_str(f.format_modules(x).as_str()),
            Definition::Address(x) => ret.push_str(f.format_address(x).as_str()),
            Definition::Script(x) => ret.push_str(f.format_script(x).as_str()),
        }
    }
    std::result::Result::Ok(ret)
}

struct Format {
    config: FormatConfig,
    depth: Rc<RefCell<usize>>,
}

impl Format {
    fn new(config: FormatConfig) -> Self {
        Self {
            config,
            comments,
            depth: Default::default(),
        }
    }
}

impl Format {
    fn format_modules(&mut self, d: &ModuleDefinition) -> String {
        let mut ret = String::default();
        let _d = self.increment_depth();
        for m in d.members.iter() {
            ret.push_str(self.format_module_member(m).as_str());
        }
        ret
    }
    fn format_module_member(&mut self, d: &ModuleMember) -> String {
        let mut ret = String::default();

        match d {
            ModuleMember::Function(x) => ret.push_str(&self.format_function(x).as_str()),
            ModuleMember::Struct(x) => ret.push_str(&self.format_struct(x).as_str()),
            ModuleMember::Use(x) => ret.push_str(&self.format_use(x).as_str()),
            ModuleMember::Friend(x) => ret.push_str(&self.format_friend(x).as_str()),
            ModuleMember::Constant(x) => ret.push_str(&self.format_const(x).as_str()),
            ModuleMember::Spec(x) => ret.push_str(&self.format_spec(x).as_str()),
        }
        ret
    }
    fn format_attributes(&mut self, atts: &Vec<Attributes>) -> String {
        unimplemented!()
    }
    fn format_spec(&mut self, f: &SpecBlock) -> String {
        unimplemented!()
    }
    fn format_const(&mut self, f: &Constant) -> String {
        let mut ret = String::default();
        ret.push_str(self.format_attributes(&f.attributes).as_str());
        ret.push_str("const ");
        ret.push_str(f.name.0.value.as_str());
        ret.push_str(":");
        ret.push_str(self.format_type(&f.signature).as_str());
        ret.push_str("=");
        ret.push_str(&self.format_expr(&f.value).as_str());
        ret.push_str(";");
        ret
    }
    fn format_type(&mut self, ty: &Type) -> String {
        let ret = String::default();
        ret
    }
    fn format_friend(&mut self, f: &FriendDecl) -> String {
        unimplemented!()
    }
    fn format_use(&mut self, f: &UseDecl) -> String {
        unimplemented!()
    }
    fn format_struct(&mut self, f: &StructDefinition) -> String {
        unimplemented!()
    }
    fn format_function(&mut self, f: &Function) -> String {
        let mut ret = String::default();
        ret.push_str(self.format_attributes(&f.attributes).as_str());
        ret.push_str("fun ");
        ret.push_str(f.name.0.value.as_str());
        ret.push_str(self.format_signature(&f.signature).as_str());
        ret.push_str(self.format_acquires(&f.acquires).as_str());
        let _guard = self.increment_depth();

        ret
    }
    fn format_acquires(&mut self, a: &Vec<NameAccessChain>) -> String {
        unimplemented!()
    }
    fn format_signature(&mut self, signature: &FunctionSignature) -> String {
        unimplemented!()
    }

    fn format_address(&mut self, d: &AddressDefinition) -> String {
        unimplemented!()
    }

    fn format_script(&mut self, d: &Script) -> String {
        unimplemented!()
    }

    fn increment_depth(&self) -> DepthGuard {
        let old = *self.depth.as_ref().borrow();
        *self.depth.as_ref().borrow_mut() = old + 1;
        DepthGuard(self.depth.clone())
    }
    fn format_var(&mut self, var: &Var) -> String {
        let ret = String::default();

        ret
    }
    fn format_name(&mut self, name: &NameAccessChain) -> String {
        let ret = String::default();
        ret
    }
    fn format_expr(&mut self, expr: &Exp) -> String {
        let mut ret = String::default();
        match &expr.value {
            Exp_::Value(x) => match &x.value {
                Value_::Address(_) => todo!(),
                Value_::Num(_) => todo!(),
                Value_::Bool(_) => todo!(),
                Value_::HexString(_) => todo!(),
                Value_::ByteString(_) => todo!(),
            },
            Exp_::Move(var) => ret.push_str(format!("move {}", self.format_var(var)).as_str()),
            Exp_::Copy(var) => ret.push_str(format!("move {}", self.format_var(var)).as_str()),
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
            Exp_::BinopExp(_, _, _) => todo!(),
            Exp_::Borrow(_, _) => todo!(),
            Exp_::Dot(_, _) => todo!(),
            Exp_::Index(_, _) => todo!(),
            Exp_::Cast(_, _) => todo!(),
            Exp_::Annotate(_, _) => todo!(),
            Exp_::Spec(_) => todo!(),
            Exp_::UnresolvedError => todo!(),
        }
        ret
    }
}

pub struct FormatConfig {
    pub ident_size: usize,
}

struct DepthGuard(Rc<RefCell<usize>>);

impl Drop for DepthGuard {
    fn drop(&mut self) {
        let old = *self.0.as_ref().borrow();
        *self.0.as_ref().borrow_mut() = old - 1;
    }
}
