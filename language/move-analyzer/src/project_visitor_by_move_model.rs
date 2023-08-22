// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use crate::{project::Project, project_context::*, analyzer_handler::*};

impl Project {
    /// Entrance for `ItemOrAccessHandler` base on analyze.
    pub fn run_full_visitor_by_move_model(&self, visitor: &mut dyn ItemOrAccessHandler) {
        log::info!("lll >> run_full_visitor_by_move_model {} ", visitor);
        self.project_context.clear_scopes_and_addresses();

        // visit should `rev`.
        let manifests: Vec<_> = self.manifest_paths.iter().rev().cloned().collect();
        for m in manifests.iter() {
            self.visit_by_move_model(
                &self.project_context,
                visitor,
                true,
            );
            if visitor.finished() {
                return;
            }
        }
    }

    pub fn visit_by_move_model(
        &self,
        project_context: &ProjectContext,
        visitor: &mut dyn ItemOrAccessHandler,
        enter_import: bool,
    ) {

    }
}
