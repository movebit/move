//! This file primarily modifies the `TypeDisplay` structure and its `fmt::Display` trait
//! within the `move-model`. The objective is to adjust the `struct_str` method in the
//! `TypeDisplay` structure to print the structural information in the form
//! "address::module_name::struct_name" instead of "module_name::struct_name" or
//! "0x12::module_name::struct_name".

use move_model::{
    model:: {
        StructId, ModuleId
    },
    ty::{Type, TypeDisplayContext, ReferenceKind}   
};

use std::{
    collections::HashMap,
    fmt,
    fmt::Formatter,
};
pub struct TypeDisplayZX<'a> {
    pub type_: &'a Type,
    pub context: &'a TypeDisplayContext<'a>,
    pub addr_2_addrname:  &'a HashMap<String, String>,
}

impl<'a> TypeDisplayZX<'a> {
    fn new(&self, ty: &'a Type) -> TypeDisplayZX<'a> {
        TypeDisplayZX {
            type_: ty,
            context: self.context,
            addr_2_addrname: self.addr_2_addrname
        }
    }
}  

impl<'a> fmt::Display for TypeDisplayZX<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Type::*;
        let comma_list = |f: &mut Formatter<'_>, ts: &[Type]| -> fmt::Result {
            let mut first = true;
            for t in ts {
                if first {
                    first = false
                } else {
                    f.write_str(", ")?;
                }
                write!(f, "{}", self.new(t))?;
            }
            Ok(())
        };
        match self.type_ {
            Primitive(p) => write!(f, "{}", p),
            Tuple(ts) => {
                f.write_str("(")?;
                comma_list(f, ts)?;
                f.write_str(")")
            },
            Vector(t) => {
                write!(f, "vector<{}>", self.new(t))
            },
            TypeDomain(t) => write!(f, "domain<{}>", self.new(t)),
            ResourceDomain(mid, sid, inst_opt) => {
                write!(f, "resources<{}", self.struct_str(*mid, *sid))?;
                if let Some(inst) = inst_opt {
                    f.write_str("<")?;
                    comma_list(f, inst)?;
                    f.write_str(">")?;
                }
                f.write_str(">")
            },
            Fun(a, t) => {
                f.write_str("|")?;
                write!(f, "{}", self.new(a))?;
                f.write_str("|")?;
                write!(f, "{}", self.new(t))
            },
            Struct(mid, sid, ts) => {
                write!(f, "{}", self.struct_str(*mid, *sid))?;
                if !ts.is_empty() {
                    f.write_str("<")?;
                    comma_list(f, ts)?;
                    f.write_str(">")?;
                }
                Ok(())
            },
            Reference(kind, t) => {
                f.write_str("&")?;
                let modifier = match kind {
                    ReferenceKind::Immutable => "",
                    ReferenceKind::Mutable => "mut ",
                };
                f.write_str(modifier)?;
                write!(f, "{}", self.new(t))
            },
            TypeParameter(idx) => {
                if let Some(names) = &self.context.type_param_names {
                    let idx = *idx as usize;
                    if idx < names.len() {
                        write!(f, "{}", names[idx].display(self.context.env.symbol_pool()))
                    } else {
                        write!(f, "#{}", idx)
                    }
                } else {
                    write!(f, "#{}", idx)
                }
            },
            Var(idx) => {
                write!(f, "")
            },
            Error => f.write_str("*error*"),
        }
    }
}

impl<'a> TypeDisplayZX<'a> {
    fn struct_str(&self, mid: ModuleId, sid: StructId) -> String {
        let env = self.context.env;
        if let Some(builder_table) = self.context.builder_struct_table {
            eprintln!("stryct 00000000000000000000");
            let qsym = builder_table.get(&(mid, sid)).expect("type known");
            qsym.display(self.context.env).to_string()
        } else {
            eprintln!("stryct 11111111111111111111");
            let struct_module_env = env.get_module(mid);
            let struct_module_env_full_name = struct_module_env.get_full_name_str();
            let addr_end = struct_module_env_full_name.find("::").unwrap_or_default();
            let addr = struct_module_env_full_name[0..addr_end].to_string();
        
            let struct_env = struct_module_env.clone().into_struct(sid);
            format!(
                "{}::{}::{}",
                self.addr_2_addrname.get(&addr).unwrap_or(&String::from("0x0")),
                struct_module_env.get_name().display(env).to_string(),
                struct_env.get_name().display(env.symbol_pool()),
            )
        }
    }
}

