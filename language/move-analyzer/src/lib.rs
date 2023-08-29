// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

extern crate move_ir_types;

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

pub mod context;
pub mod diagnostics;
pub mod goto_definition;
pub mod item;
pub mod project;
pub mod project_manager;
pub mod project_visitor_by_move_model;
pub mod multiproject;
pub mod project_context;
pub mod analyzer_handler;
pub mod scope;
pub mod syntax;
pub mod types;
pub mod utils;