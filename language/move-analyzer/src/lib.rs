// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

#[macro_use]
extern crate lazy_static;

#[macro_export]
macro_rules! impl_convert_loc {
    ($struct_name : ident) => {
        impl ConvertLoc for $struct_name {
            fn convert_file_hash_filepath(&self, hash: &FileHash) -> Option<PathBuf> {
                self.hash_file
                    .as_ref()
                    .borrow()
                    .get_path(hash)
                    .map(|x| x.clone())
            }
            fn convert_loc_range(&self, loc: &Loc) -> Option<FileRange> {
                self.convert_file_hash_filepath(&loc.file_hash())
                    .map(|file| {
                        self.file_line_mapping.as_ref().borrow().translate(
                            &file,
                            loc.start(),
                            loc.end(),
                        )
                    })
                    .flatten()
            }
        }
    };
}
extern crate move_ir_types;

pub mod completion;
#[macro_use]
pub mod context;
pub mod call;
pub mod code_lens;
pub mod document_symbol;
pub mod fmt;
#[cfg(test)]
mod fmt_test;
pub mod goto_definition;
pub mod hover;
#[cfg(test)]
mod ide_test;
pub mod inlay_hitnt;
pub mod item;
pub mod move_generate_spec;
pub mod move_generate_spec_chen;
pub mod move_generate_spec_file;
pub mod move_generate_spec_sel;
pub mod project;
pub mod project_context;
pub mod project_visitor;
pub mod references;
pub mod scope;
pub mod syntax;
pub mod token_tree;
pub mod types;
pub mod utils;
