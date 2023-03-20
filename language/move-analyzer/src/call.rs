use super::context::*;
use super::project::*;
use lsp_server::Message;
use lsp_server::Request;
use lsp_server::Response;
use std::path::PathBuf;
use std::str::FromStr;

pub fn on_get_call_tree(context: &Context, request: &Request) {
    let parameters = serde_json::from_value::<ReqParameters>(request.params.clone())
        .expect("could not deserialize go-to-def request");
    let fpath = PathBuf::from_str(parameters.fpath.as_str()).unwrap();
    let mut handler = Handler::new();
    let _ = match context.projects.get_project(&fpath) {
        Some(x) => x,
        None => {
            log::error!("project not found:{:?}", fpath.as_path());
            return;
        }
    }
    .run_full_visitor(&mut handler);
    let results = handler.to_results();
    let r = Response::new_ok(request.id.clone(), serde_json::to_value(results).unwrap());
    context
        .connection
        .sender
        .send(Message::Response(r))
        .unwrap();
}

struct Handler {
    results: Vec<Result>,
}

impl Handler {
    fn new() -> Self {
        Self { results: vec![] }
    }
    fn to_results(self) -> Vec<Result> {
        self.results
    }
}

impl super::project::ItemOrAccessHandler for Handler {
    fn need_call_tree(&self) -> bool {
        true
    }
    fn visit_fun_or_spec_body(&self) -> bool {
        true
    }
    fn finished(&self) -> bool {
        false
    }
    fn function_or_spec_body_should_visit(&self, _range: &crate::utils::FileRange) -> bool {
        true
    }
    fn handle_call_pair(&mut self, from: FunID, to: FunID) {
        self.results.push(Result { from, to });
    }
}
#[derive(Clone, serde:: Deserialize)]
pub struct ReqParameters {
    fpath: String,
}

impl std::fmt::Display for Handler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "run visit for get call tree.")
    }
}

#[derive(Debug, serde::Serialize)]
struct Result {
    from: FunID,
    to: FunID,
}
