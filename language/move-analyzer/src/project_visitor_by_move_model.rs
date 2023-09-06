// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{analyzer_handler::*, project::Project};
use std::path::PathBuf;

impl Project {
    /// Entrance for `ItemOrAccessHandler` base on analyze.
    pub fn run_full_visitor_by_move_model(&self, visitor: &mut dyn ItemOrAccessHandler) {
        log::info!("lll >> run_full_visitor_by_move_model {} ", visitor);
        log::info!(
            "lll >> after load project, self.manifest_paths.len = {:?}",
            self.manifest_paths.len()
        );
        // for module in self.global_env.get_target_modules() {
        // }
    }

    pub fn run_visitor_for_file(
        &self,
        visitor: &mut dyn ItemOrAccessHandler,
        filepath: &PathBuf,
    ) {
        log::info!("run visitor part for {} ", visitor);
        visitor.handle_project_env(self, &self.global_env, &filepath);
    }
}
