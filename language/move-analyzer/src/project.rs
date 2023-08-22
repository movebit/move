// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use super::{project_context::*, utils::*};
use crate::analyzer_handler::*;
use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    time::SystemTime,
    path::PathBuf, 
    rc::Rc
};

/// Project
pub struct Project {
    pub(crate) modules: HashMap<
        PathBuf, // manifest path.
        Rc<RefCell<SourceDefs>>,
    >, // This modules is consistent with the asts in MultiProject
    pub(crate) manifests: Vec<move_package::source_package::parsed_manifest::SourceManifest>,
    pub(crate) hash_file: Rc<RefCell<PathBufHashMap>>,
    pub(crate) file_line_mapping: Rc<RefCell<FileLineMapping>>,
    pub(crate) manifest_paths: Vec<PathBuf>,
    pub(crate) project_context: ProjectContext,
    pub(crate) manifest_not_exists: HashSet<PathBuf>,
    pub(crate) manifest_load_failures: HashSet<PathBuf>,
    pub(crate) manifest_mod_time: HashMap<PathBuf, Option<SystemTime>>,
}
