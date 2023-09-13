// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{analyzer_handler::*, project::Project};
use std::path::PathBuf;

impl Project {
    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ItemOrAccessHandler,
        filepath: &PathBuf,
    ) {
        log::info!("run visitor part for {} ", visitor);
        visitor.handle_project_env(self, &self.global_env, &filepath);
    }
}
