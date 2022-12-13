#![no_main]

use move_binary_format::file_format::{
    empty_module, AbilitySet, Bytecode, CodeUnit, CompiledModule, Constant, FieldDefinition,
    FunctionDefinition, FunctionHandle, FunctionHandleIndex, IdentifierIndex, ModuleHandleIndex,
    Signature, SignatureIndex, SignatureToken,
    SignatureToken::{Address, Bool},
    StructDefinition, StructFieldInformation, StructHandle, StructHandleIndex, TypeSignature,
    Visibility,
};
use move_core_types::{account_address::AccountAddress, identifier::Identifier};
use std::str::FromStr;

use arbitrary::Arbitrary;

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    // fuzzed code goes here
});

#[derive(Debug, Arbitrary)]
struct Functionbody {
    code: Vec<Bytecode>,
    abilities: AbilitySet,
    param_types: Vec<SignatureToken>,
    return_type: Option<SignatureToken>,
}

#[derive(Debug, Arbitrary)]
struct Struct {
    fields: Vec<(String, SignatureToken)>,
}

#[derive(Debug, Arbitrary)]
struct Module {
    structs: Vec<(String, Struct)>,
    Funcs: Vec<(String, Functionbody)>,
    constants: Vec<(String, Constant)>,
}

impl Module {
    fn to_compile_module(self) -> CompiledModule {
        CompiledModule {
            version: (),
            self_module_handle_idx: (),
            module_handles: (),
            struct_handles: (),
            function_handles: (),
            field_handles: (),
            friend_decls: (),
            struct_def_instantiations: (),
            function_instantiations: (),
            field_instantiations: (),
            signatures: (),
            identifiers: (),
            address_identifiers: (),
            constant_pool: (),
            metadata: (),
            struct_defs: (),
            function_defs: (),
        }
    }
}
