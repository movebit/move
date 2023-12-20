// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

//! This module is responsible for building symbolication information on top of compiler's typed
//! AST, in particular identifier definitions to be used for implementing go-to-def and
//! go-to-references language server commands.
//!
//! There are two main structs that are used at different phases of the process, the Symbolicator
//! struct is used when building symbolication information and the Symbols struct is summarizes the
//! symbolication results and is used by the language server find definitions and references.
//!
//! Here is a brief description of how the symbolication information is encoded. Each identifier is
//! in the source code of a given module is represented by its location (UseLoc struct): line
//! number, starting and ending column, and hash of the source file where this identifier is
//! located). A definition for each identifier (if any - e.g., built-in type definitions are
//! excluded as there is no place in source code where they are defined) is also represented by its
//! location in the source code (DefLoc struct): line, starting column and a hash of the source
//! file where it's located. The symbolication process maps each identifier with its definition - a
//! per module map is keyed on the line number where the identifier is located, and the map entry
//! contains a list of identifier/definition pairs ordered by the column where the identifier starts.
//!
//! For example consider the following code fragment (0-based line numbers on the left and 0-based
//! column numbers at the bottom):
//!
//! 7: const SOME_CONST: u64 = 42;
//! 8:
//! 9: SOME_CONST + SOME_CONST
//!    |     |  |   | |      |
//!    0     6  9  13 15    22
//!
//! Symbolication information for this code fragment would look as follows assuming that this code
//! is stored in a file with hash FHASH (note that identifier in the definition of the constant maps
//! to itself):
//!
//! [7] -> [UseLoc(7:6-13, FHASH), DefLoc(7:6, FHASH)]
//! [9] -> [UseLoc(9:0-9 , FHASH), DefLoc((7:6, FHASH)], [UseLoc(9:13-22, FHASH), DefLoc((7:6, FHASH)]
//!
//! Including line number (and file hash) with the (use) identifier location may appear redundant,
//! but it's needed to allow accumulating uses with each definition to support
//! go-to-references. This is done in a global map from an identifier location (DefLoc) to a set of
//! use locations (UseLoc) - we find a all references of a given identifier by first finding its
//! definition and then using this definition as a key to the global map.
//!
//! Symbolication algorithm first analyzes all top-level definitions from all modules and then
//! processes function bodies and struct definitions to match uses to definitions. For local
//! definitions, the symbolicator builds a scope stack, entering encountered definitions and
//! matching uses to a definition in the innermost scope.

use crate::{
    context::Context,
    utils::{get_modules_by_fpath_in_target_modules}, project::Project,
};
use lsp_server::Request;
use lsp_types::{DocumentSymbol, DocumentSymbolParams, SymbolKind};
use move_model::{
    model::{ModuleEnv, StructEnv}
};


/// Handles document symbol request of the language server
#[allow(deprecated)]
pub fn on_document_symbol_request(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<DocumentSymbolParams>(request.params.clone())
        .expect("could not deserialize document symbol request");

    let fpath = parameters.text_document.uri.to_file_path().unwrap();
    eprintln!("on_document_symbol_request: {:?}", fpath);
    let project = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return ;
        }
    };

    let mut result_vec_document_symbols: Vec<DocumentSymbol> = vec![];
    for module_env in get_modules_by_fpath_in_target_modules(&project.global_env, &fpath) {
        eprintln!("start handle module env name: {:?}", module_env.get_full_name_str());
        let module_range = project.loc_to_range(&module_env.get_loc());
        let module_name = module_env.get_name().display(&project.global_env).to_string().clone();
        let module_detail = Some(module_name.clone());
        let module_kind = SymbolKind::MODULE;

        let mut children = vec![];

        handle_document_symbols_function(&project, &module_env, &mut children);
        handle_document_symbols_const(&project, &module_env, &mut children);
        handle_document_symbols_struct(project, &module_env, &mut children);

        result_vec_document_symbols.push( DocumentSymbol{
            name: module_name,
            detail: module_detail,
            kind: module_kind,
            range: module_range,
            selection_range: module_range,
            children: Some(children),
            tags: Some(vec![]),
            deprecated: Some(false),
        });
        eprintln!("end handle module env name: {:?}", module_env.get_full_name_str());
    }
    
    let response = lsp_server::Response::new_ok(
        request.id.clone(), 
        result_vec_document_symbols
    );
    if let Err(err) = context
        .connection
        .sender
        .send(lsp_server::Message::Response(response))
    {
        eprintln!("could not send use response: {:?}", err);
    }
    
}

/// Helper function to handle function in the document symbols
#[allow(deprecated)]
fn handle_document_symbols_function(project: &Project, module_env :&ModuleEnv, children: &mut Vec<DocumentSymbol>) {
    for function_env in module_env.get_functions() {
        let func_range = project.loc_to_range(&function_env.get_loc());
                            
        children.push(DocumentSymbol {
            name: function_env.get_name_str().to_string(),
            detail:None,
            kind: SymbolKind::FUNCTION,
            range: func_range,
            selection_range: func_range,
            children: None,
            tags: Some(vec![]),
            deprecated: Some(false),
        });  
    }
}

/// Helper function to handle constants in the document symbols
#[allow(deprecated)]
fn handle_document_symbols_const(project: &Project, 
    module_env :&ModuleEnv, 
    children: &mut Vec<DocumentSymbol>
) {
    for const_env in module_env.get_named_constants() {
        let const_range = project.loc_to_range(&const_env.get_loc());
                            
        children.push(DocumentSymbol {
            name: const_env.get_name().display(&project.global_env.symbol_pool()).to_string(),
            detail:None,
            kind: SymbolKind::CONSTANT,
            range: const_range,
            selection_range: const_range,
            children: None,
            tags: Some(vec![]),
            deprecated: Some(false),
        });  
    }
}

/// Helper function to handle sturct in the document symbols
#[allow(deprecated)]
fn handle_document_symbols_struct(project: &Project, 
    module_env :&ModuleEnv, 
    children: &mut Vec<DocumentSymbol>
) {
    for struct_env in module_env.get_structs() {
        let struct_range = project.loc_to_range(&struct_env.get_loc());
        let mut fields: Vec<DocumentSymbol> = vec![];

        handle_document_symbols_struct_fields(&project, &struct_env, &mut fields);

        children.push(DocumentSymbol {
            name: struct_env.get_name().display(&struct_env.symbol_pool()).to_string(),
            detail:None,
            kind: SymbolKind::CONSTANT,
            range: struct_range,
            selection_range: struct_range,
            children: Some(fields),
            tags: Some(vec![]),
            deprecated: Some(false),
        });  
    }
}

/// Helper function to handle fields of sturct in the document symbols
#[allow(deprecated)]
fn handle_document_symbols_struct_fields(
    project: &Project,
    struct_env :&StructEnv, 
    children: &mut Vec<DocumentSymbol>
) {
    for field_env in struct_env.get_fields() {
        let field_range = project.loc_to_range(&field_env.get_loc());

        children.push(DocumentSymbol {
            name: field_env.get_name().display(&struct_env.symbol_pool()).to_string(),
            detail:None,
            kind: SymbolKind::FIELD,
            range: field_range,
            selection_range: field_range,
            children: None,
            tags: Some(vec![]),
            deprecated: Some(false),
        });  
    }
}