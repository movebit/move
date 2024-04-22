use anyhow::Result;
use clap::Parser;
use crossbeam::channel::{bounded, select, Sender};
use log::{Level, Metadata, Record};
use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::{
    notification::Notification as _, request::Request as _, CompletionOptions, Diagnostic,
    HoverProviderCapability, OneOf, SaveOptions, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, TypeDefinitionProviderCapability, WorkDoneProgressOptions,
};
use move_command_line_common::files::FileHash;
use move_compiler::{diagnostics::Diagnostics, shared::*, PASS_TYPING};
use std::{
    collections::{BTreeMap, HashMap},
    path::{Path, PathBuf},
    sync::{Arc, Mutex},
    thread,
};

use beta_2024::{context::{Context as Context_beta_2024, FileDiags, MultiProject}, inlay_hints::InlayHintsConfig, symbols, vfs::VirtualFileSystem};


use move_symbol_pool::Symbol;
use url::Url;


use sui_move_analyzer::sui_move_analyzer_beta_2024::{
    send_diag as send_diag_beta_2024,
    try_reload_projects as try_reload_projects_beta_2024,
    on_request as on_request_beta_2024,
    on_notification as on_notification_beta_2024,
    on_response as on_response_beta_2024
};

// use sui_move_analyzer::sui_move_analyzer_alpha_2024::{
//     send_diag as send_diag_alpha_2024,
//     try_reload_projects as try_reload_projects_alpha_2024,
//     on_request as on_request_alpha_2024,
//     on_notification as on_notification_alpha_2024,
//     on_response as on_response_alpha_2024
// };

#[derive(Parser)]
#[clap(author, version, about)]
struct Options {}

struct SimpleLogger;
impl log::Log for SimpleLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        metadata.level() <= Level::Info
    }
    fn log(&self, record: &Record) {
        if self.enabled(record.metadata()) {
            eprintln!("{} - {}", record.level(), record.args());
        }
    }
    fn flush(&self) {}
}
const LOGGER: SimpleLogger = SimpleLogger;

pub fn init_log() {
    log::set_logger(&LOGGER)
        .map(|()| log::set_max_level(log::LevelFilter::Warn))
        .unwrap()
}


fn main() {
    #[cfg(feature = "pprof")]
    cpu_pprof(20);

    // For now, sui-move-analyzer only responds to options built-in to clap,
    // such as `--help` or `--version`.
    Options::parse();
    init_log();
    // stdio is used to communicate Language Server Protocol requests and responses.
    // stderr is used for logging (and, when Visual Studio Code is used to communicate with this
    // server, it captures this output in a dedicated "output channel").
    let exe = std::env::current_exe()
        .unwrap()
        .to_string_lossy()
        .to_string();
    eprintln!(
        "Starting language server '{}' communicating via stdio...",
        exe
    );

    let (connection, io_threads) = Connection::stdio();
    let symbols = Arc::new(Mutex::new(symbols::Symbolicator::empty_symbols()));
    let mut context = Context_beta_2024 {
        projects: MultiProject::new(),
        connection,
        files: VirtualFileSystem::default(),
        symbols: symbols.clone(),
        ref_caches: Default::default(),
        diag_version: FileDiags::new(),
    };

    let (id, _client_response) = context
        .connection
        .initialize_start()
        .expect("could not start connection initialization");

    let capabilities = serde_json::to_value(lsp_types::ServerCapabilities {
        // The server receives notifications from the client as users open, close,
        // and modify documents.
        text_document_sync: Some(TextDocumentSyncCapability::Options(
            TextDocumentSyncOptions {
                open_close: Some(true),
                // TODO: We request that the language server client send us the entire text of any
                // files that are modified. We ought to use the "incremental" sync kind, which would
                // have clients only send us what has changed and where, thereby requiring far less
                // data be sent "over the wire." However, to do so, our language server would need
                // to be capable of applying deltas to its view of the client's open files. See the
                // 'sui_move_analyzer::vfs' module for details.
                change: Some(TextDocumentSyncKind::FULL),
                will_save: None,
                will_save_wait_until: None,
                save: Some(
                    SaveOptions {
                        include_text: Some(true),
                    }
                    .into(),
                ),
            },
        )),
        selection_range_provider: None,
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        // The server provides completions as a user is typing.
        completion_provider: Some(CompletionOptions {
            resolve_provider: None,
            // In Move, `foo::` and `foo.` should trigger completion suggestions for after
            // the `:` or `.`
            // (Trigger characters are just that: characters, such as `:`, and not sequences of
            // characters, such as `::`. So when the language server encounters a completion
            // request, it checks whether completions are being requested for `foo:`, and returns no
            // completions in that case.)
            trigger_characters: Some(vec![":".to_string(), ".".to_string()]),
            all_commit_characters: None,
            work_done_progress_options: WorkDoneProgressOptions {
                work_done_progress: None,
            },
            completion_item: None,
        }),
        definition_provider: Some(OneOf::Left(symbols::DEFS_AND_REFS_SUPPORT)),
        type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(
            symbols::DEFS_AND_REFS_SUPPORT,
        )),
        references_provider: Some(OneOf::Left(symbols::DEFS_AND_REFS_SUPPORT)),
        document_symbol_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .expect("could not serialize server capabilities");

    let (diag_sender_symbol, diag_receiver_symbol) =
        bounded::<Result<BTreeMap<Symbol, Vec<Diagnostic>>>>(0);
    let mut symbolicator_runner = symbols::SymbolicatorRunner::idle();
    if symbols::DEFS_AND_REFS_SUPPORT {
        let initialize_params: lsp_types::InitializeParams =
            serde_json::from_value(_client_response)
                .expect("could not deserialize client capabilities");

        symbolicator_runner = symbols::SymbolicatorRunner::new(symbols.clone(), diag_sender_symbol);

        // If initialization information from the client contains a path to the directory being
        // opened, try to initialize symbols before sending response to the client. Do not bother
        // with diagnostics as they will be recomputed whenever the first source file is opened. The
        // main reason for this is to enable unit tests that rely on the symbolication information
        // to be available right after the client is initialized.
        if let Some(uri) = initialize_params.root_uri {
            if let Some(p) = symbols::SymbolicatorRunner::root_dir(&uri.to_file_path().unwrap()) {
                // need to evaluate in a separate thread to allow for a larger stack size (needed on
                // Windows)
                thread::Builder::new()
                    .stack_size(symbols::STACK_SIZE_BYTES)
                    .spawn(move || {
                        if let Ok((Some(new_symbols), _)) =
                            symbols::Symbolicator::get_symbols(p.as_path())
                        {
                            let mut old_symbols = symbols.lock().unwrap();
                            (*old_symbols).merge(new_symbols);
                        }
                    })
                    .unwrap()
                    .join()
                    .unwrap();
            }
        }
    };

    context
        .connection
        .initialize_finish(
            id,
            serde_json::json!({
                "capabilities": capabilities,
            }),
        )
        .expect("could not finish connection initialization");
    let (diag_sender, diag_receiver) = bounded::<(PathBuf, Diagnostics)>(1);
    let diag_sender = Arc::new(Mutex::new(diag_sender));
    let mut inlay_hints_config = InlayHintsConfig::default();

    loop {
        select! {
            recv(diag_receiver) -> message => {
                match message {
                    Ok ((mani ,x)) => {
                        send_diag_beta_2024(&mut context,mani,x);
                    }
                    Err(error) => log::error!("IDE diag message error: {:?}", error),
                }
            },
            recv(diag_receiver_symbol) -> message => {
                match message {
                    Ok(result) => {
                        match result {
                            Ok(diags) => {
                                for (k, v) in diags {
                                    let url = Url::from_file_path(Path::new(&k.to_string())).unwrap();
                                    let params = lsp_types::PublishDiagnosticsParams::new(url, v, None);
                                    let notification = Notification::new(lsp_types::notification::PublishDiagnostics::METHOD.to_string(), params);
                                    if let Err(err) = context
                                        .connection
                                        .sender
                                        .send(lsp_server::Message::Notification(notification)) {
                                            eprintln!("could not send diagnostics response: {:?}", err);
                                        };
                                }
                            },
                            Err(err) => {
                                let typ = lsp_types::MessageType::ERROR;
                                let message = format!("{err}");
                                    // report missing manifest only once to avoid re-generating
                                    // user-visible error in cases when the developer decides to
                                    // keep editing a file that does not belong to a packages
                                let params = lsp_types::ShowMessageParams { typ, message };
                                let notification = Notification::new(lsp_types::notification::ShowMessage::METHOD.to_string(), params);
                                if let Err(err) = context
                                    .connection
                                    .sender
                                    .send(lsp_server::Message::Notification(notification)) {
                                        eprintln!("could not send compiler error response: {:?}", err);
                                    };
                            },
                        }
                    },
                    Err(error) => eprintln!("symbolicator message error: {:?}", error),
                }
            },
            recv(context.connection.receiver) -> message => {
                try_reload_projects_beta_2024(&mut context);
                match message {
                    Ok(Message::Request(request)) => on_request_beta_2024(&mut context, &request , &mut inlay_hints_config),
                    Ok(Message::Response(response)) => on_response_beta_2024(&context, &response),
                    Ok(Message::Notification(notification)) => {
                        match notification.method.as_str() {
                            lsp_types::notification::Exit::METHOD => break,
                            lsp_types::notification::Cancel::METHOD => {
                                // TODO: Currently the server does not implement request cancellation.
                                // It ought to, especially once it begins processing requests that may
                                // take a long time to respond to.
                            }
                            _ => on_notification_beta_2024(&mut context, &notification, diag_sender.clone()),
                        }
                    }
                    Err(error) => eprintln!("IDE message error: {:?}", error),
                }
            }
        };
    }

    io_threads.join().expect("I/O threads could not finish");
    symbolicator_runner.quit();
    eprintln!("Shut down language server '{}'.", exe);
}