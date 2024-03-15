use std::path::PathBuf;

use dashmap::DashMap;
use i18n_embed_fl::fl;
use icy_ppe::ast::{AstVisitor, Program};
use icy_ppe::parser::parse_program;
use icy_ppe::tables::OpCode;
use ppl_language_server::semantic_token::{semantic_token_from_ast, LEGEND_TYPE};
use ppl_language_server::{ImCompleteSemanticToken, LANGUAGE_LOADER};
use ropey::Rope;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::notification::Notification;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

#[derive(Debug)]
struct Backend {
    client: Client,
    ast_map: DashMap<String, Program>,
    document_map: DashMap<String, Rope>,
    semantic_token_map: DashMap<String, Vec<ImCompleteSemanticToken>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: None,
            offset_encoding: None,
            capabilities: ServerCapabilities {
                inlay_hint_provider: Some(OneOf::Left(true)),
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                /*
                                completion_provider: Some(CompletionOptions {
                                    resolve_provider: Some(false),
                                    trigger_characters: Some(vec![".".to_string()]),
                                    work_done_progress_options: Default::default(),
                                    all_commit_characters: None,
                                    completion_item: None,
                                }),
                                execute_command_provider: Some(ExecuteCommandOptions {
                                    commands: vec!["dummy.do_something".to_string()],
                                    work_done_progress_options: Default::default(),
                                }),
                */
                workspace: Some(WorkspaceServerCapabilities {
                    workspace_folders: Some(WorkspaceFoldersServerCapabilities {
                        supported: Some(true),
                        change_notifications: Some(OneOf::Left(true)),
                    }),
                    file_operations: None,
                }),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensRegistrationOptions(
                        SemanticTokensRegistrationOptions {
                            text_document_registration_options: {
                                TextDocumentRegistrationOptions {
                                    document_selector: Some(vec![DocumentFilter {
                                        language: Some("ppl".to_string()),
                                        scheme: Some("file".to_string()),
                                        pattern: None,
                                    }]),
                                }
                            },
                            semantic_tokens_options: SemanticTokensOptions {
                                work_done_progress_options: WorkDoneProgressOptions::default(),
                                legend: SemanticTokensLegend {
                                    token_types: LEGEND_TYPE.into(),
                                    token_modifiers: vec![],
                                },
                                range: Some(true),
                                full: Some(SemanticTokensFullOptions::Bool(true)),
                            },
                            static_registration_options: StaticRegistrationOptions::default(),
                        },
                    ),
                ),
                // definition: Some(GotoCapability::default()),
                // definition_provider: Some(OneOf::Left(true)),
                // references_provider: Some(OneOf::Left(true)),
                // rename_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
        })
    }
    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file opened!")
            .await;
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: params.text_document.text,
            version: params.text_document.version,
        })
        .await
    }

    async fn did_change(&self, mut params: DidChangeTextDocumentParams) {
        self.on_change(TextDocumentItem {
            uri: params.text_document.uri,
            text: std::mem::take(&mut params.content_changes[0].text),
            version: params.text_document.version,
        })
        .await
    }

    async fn did_save(&self, _: DidSaveTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file saved!")
            .await;
    }
    async fn did_close(&self, _: DidCloseTextDocumentParams) {
        self.client
            .log_message(MessageType::INFO, "file closed!")
            .await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let _ = params;
        let result = async {
            let uri = params.text_document_position_params.text_document.uri;
            let ast = self.ast_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position_params.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;

            get_tooltip(&ast, offset)
        }
        .await;
        Ok(result)
    }

    async fn goto_definition(
        &self,
        _params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(None)
        /*
        let definition = async {
            let uri = params.text_document_position_params.text_document.uri;
            let ast = self.ast_map.get(uri.as_str())?;
            let rope = self.document_map.get(uri.as_str())?;

            let position = params.text_document_position_params.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            // self.client.log_message(MessageType::INFO, &format!("{:#?}, {}", ast.value(), offset)).await;
            let span = get_definition(&ast, offset);
            self.client
                .log_message(MessageType::INFO, &format!("{:?}, ", span))
                .await;
            span.and_then(|(_, range)| {
                let start_position = offset_to_position(range.start, &rope)?;
                let end_position = offset_to_position(range.end, &rope)?;

                let range = Range::new(start_position, end_position);

                Some(GotoDefinitionResponse::Scalar(Location::new(uri, range)))
            })
        }
        .await;
        Ok(definition)*/
    }

    async fn references(&self, _params: ReferenceParams) -> Result<Option<Vec<Location>>> {
        Ok(None)
        /*
        let reference_list = || -> Option<Vec<Location>> {
            let uri = params.text_document_position.text_document.uri;
            let ast = self.ast_map.get(&uri.to_string())?;
            let rope = self.document_map.get(&uri.to_string())?;

            let position = params.text_document_position.position;
            let char = rope.try_line_to_char(position.line as usize).ok()?;
            let offset = char + position.character as usize;
            let reference_list = get_reference(&ast, offset, false);
            let ret = reference_list
                .into_iter()
                .filter_map(|(_, range)| {
                    let start_position = offset_to_position(range.start, &rope)?;
                    let end_position = offset_to_position(range.end, &rope)?;

                    let range = Range::new(start_position, end_position);

                    Some(Location::new(uri.clone(), range))
                })
                .collect::<Vec<_>>();
            Some(ret)
        }();
        Ok(reference_list)*/
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri.to_string();
        self.client
            .log_message(MessageType::LOG, "semantic_token_full")
            .await;
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let mut im_complete_tokens = self.semantic_token_map.get_mut(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let ast = self.ast_map.get(&uri)?;
            let extends_tokens = semantic_token_from_ast(&ast);
            im_complete_tokens.extend(extends_tokens);
            im_complete_tokens.sort_by(|a, b| a.start.cmp(&b.start));
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let delta_line = line - pre_line;
                    let delta_start = if delta_line == 0 {
                        start - pre_start
                    } else {
                        start
                    };
                    let ret = Some(SemanticToken {
                        delta_line,
                        delta_start,
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn semantic_tokens_range(
        &self,
        params: SemanticTokensRangeParams,
    ) -> Result<Option<SemanticTokensRangeResult>> {
        let uri = params.text_document.uri.to_string();
        let semantic_tokens = || -> Option<Vec<SemanticToken>> {
            let im_complete_tokens = self.semantic_token_map.get(&uri)?;
            let rope = self.document_map.get(&uri)?;
            let mut pre_line = 0;
            let mut pre_start = 0;
            let semantic_tokens = im_complete_tokens
                .iter()
                .filter_map(|token| {
                    let line = rope.try_byte_to_line(token.start).ok()? as u32;
                    let first = rope.try_line_to_char(line as usize).ok()? as u32;
                    let start = rope.try_byte_to_char(token.start).ok()? as u32 - first;
                    let ret = Some(SemanticToken {
                        delta_line: line.saturating_sub(pre_line),
                        delta_start: if start >= pre_start {
                            start - pre_start
                        } else {
                            start
                        },
                        length: token.length as u32,
                        token_type: token.token_type as u32,
                        token_modifiers_bitset: 0,
                    });
                    pre_line = line;
                    pre_start = start;
                    ret
                })
                .collect::<Vec<_>>();
            Some(semantic_tokens)
        }();
        if let Some(semantic_token) = semantic_tokens {
            return Ok(Some(SemanticTokensRangeResult::Tokens(SemanticTokens {
                result_id: None,
                data: semantic_token,
            })));
        }
        Ok(None)
    }

    async fn inlay_hint(
        &self,
        params: tower_lsp::lsp_types::InlayHintParams,
    ) -> Result<Option<Vec<InlayHint>>> {
        self.client
            .log_message(MessageType::INFO, "inlay hint")
            .await;
        let uri = &params.text_document.uri;
        if let Some(_program) = self.ast_map.get(uri.as_str()) {}
        let inlay_hint_list = Vec::new();
        /*
                let document = match self.document_map.get(uri.as_str()) {
                    Some(rope) => rope,
                    None => return Ok(None),
                };
                let inlay_hint_list = hashmap
                    .into_iter()
                    .map(|(k, v)| {
                        (
                            k.start,
                            k.end,
                            match v {
                                ppl_language_server::chumsky::Value::Null => "null".to_string(),
                                ppl_language_server::chumsky::Value::Bool(_) => "bool".to_string(),
                                ppl_language_server::chumsky::Value::Num(_) => "number".to_string(),
                                ppl_language_server::chumsky::Value::Str(_) => "string".to_string(),
                                ppl_language_server::chumsky::Value::List(_) => "[]".to_string(),
                                ppl_language_server::chumsky::Value::Func(_) => v.to_string(),
                            },
                        )
                    })
                    .filter_map(|item| {
                        // let start_position = offset_to_position(item.0, document)?;
                        let end_position = offset_to_position(item.1, &document)?;
                        let inlay_hint = InlayHint {
                            text_edits: None,
                            tooltip: None,
                            kind: Some(InlayHintKind::TYPE),
                            padding_left: None,
                            padding_right: None,
                            data: None,
                            position: end_position,
                            label: InlayHintLabel::LabelParts(vec![InlayHintLabelPart {
                                value: item.2,
                                tooltip: None,
                                location: Some(Location {
                                    uri: params.text_document.uri.clone(),
                                    range: Range {
                                        start: Position::new(0, 4),
                                        end: Position::new(0, 5),
                                    },
                                }),
                                command: None,
                            }]),
                        };
                        Some(inlay_hint)
                    })
                    .collect::<Vec<_>>();
        */
        Ok(Some(inlay_hint_list))
    }

    /*


        async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
            let uri = params.text_document_position.text_document.uri;
            let position = params.text_document_position.position;
            let completions = || -> Option<Vec<CompletionItem>> {
                let rope = self.document_map.get(&uri.to_string())?;
                let ast = self.ast_map.get(&uri.to_string())?;
                let char = rope.try_line_to_char(position.line as usize).ok()?;
                let offset = char + position.character as usize;
                let completions = completion(&ast, offset);
                let mut ret = Vec::with_capacity(completions.len());
                for (_, item) in completions {
                    match item {
                        ppl_language_server::completion::ImCompleteCompletionItem::Variable(var) => {
                            ret.push(CompletionItem {
                                label: var.clone(),
                                insert_text: Some(var.clone()),
                                kind: Some(CompletionItemKind::VARIABLE),
                                detail: Some(var),
                                ..Default::default()
                            });
                        }
                        ppl_language_server::completion::ImCompleteCompletionItem::Function(
                            name,
                            args,
                        ) => {
                            ret.push(CompletionItem {
                                label: name.clone(),
                                kind: Some(CompletionItemKind::FUNCTION),
                                detail: Some(name.clone()),
                                insert_text: Some(format!(
                                    "{}({})",
                                    name,
                                    args.iter()
                                        .enumerate()
                                        .map(|(index, item)| { format!("${{{}:{}}}", index + 1, item) })
                                        .collect::<Vec<_>>()
                                        .join(",")
                                )),
                                insert_text_format: Some(InsertTextFormat::SNIPPET),
                                ..Default::default()
                            });
                        }
                    }
                }
                Some(ret)
            }();
            Ok(completions.map(CompletionResponse::Array))
        }

        async fn rename(&self, params: RenameParams) -> Result<Option<WorkspaceEdit>> {
            let workspace_edit = || -> Option<WorkspaceEdit> {
                let uri = params.text_document_position.text_document.uri;
                let ast = self.ast_map.get(&uri.to_string())?;
                let rope = self.document_map.get(&uri.to_string())?;

                let position = params.text_document_position.position;
                let char = rope.try_line_to_char(position.line as usize).ok()?;
                let offset = char + position.character as usize;
                let reference_list = get_reference(&ast, offset, true);
                let new_name = params.new_name;
                if !reference_list.is_empty() {
                    let edit_list = reference_list
                        .into_iter()
                        .filter_map(|(_, range)| {
                            let start_position = offset_to_position(range.start, &rope)?;
                            let end_position = offset_to_position(range.end, &rope)?;
                            Some(TextEdit::new(
                                Range::new(start_position, end_position),
                                new_name.clone(),
                            ))
                        })
                        .collect::<Vec<_>>();
                    let mut map = HashMap::new();
                    map.insert(uri, edit_list);
                    let workspace_edit = WorkspaceEdit::new(map);
                    Some(workspace_edit)
                } else {
                    None
                }
            }();
            Ok(workspace_edit)
        }
    */
    async fn did_change_configuration(&self, _: DidChangeConfigurationParams) {
        self.client
            .log_message(MessageType::INFO, "configuration changed!")
            .await;
    }

    async fn did_change_workspace_folders(&self, _: DidChangeWorkspaceFoldersParams) {
        self.client
            .log_message(MessageType::INFO, "workspace folders changed!")
            .await;
    }

    async fn did_change_watched_files(&self, _: DidChangeWatchedFilesParams) {
        self.client
            .log_message(MessageType::INFO, "watched files have changed!")
            .await;
    }

    async fn execute_command(&self, _: ExecuteCommandParams) -> Result<Option<Value>> {
        self.client
            .log_message(MessageType::INFO, "command executed!")
            .await;

        match self.client.apply_edit(WorkspaceEdit::default()).await {
            Ok(res) if res.applied => self.client.log_message(MessageType::INFO, "applied").await,
            Ok(_) => self.client.log_message(MessageType::INFO, "rejected").await,
            Err(err) => self.client.log_message(MessageType::ERROR, err).await,
        }

        Ok(None)
    }
}

#[derive(Debug, Deserialize, Serialize)]
struct InlayHintParams {
    path: String,
}

enum CustomNotification {}
impl Notification for CustomNotification {
    type Params = InlayHintParams;
    const METHOD: &'static str = "custom/notification";
}
struct TextDocumentItem {
    uri: Url,
    text: String,
    version: i32,
}

impl Backend {
    async fn on_change(&self, params: TextDocumentItem) {
        let rope = ropey::Rope::from_str(&params.text);
        let uri = params.uri.to_string();
        self.document_map.insert(uri.clone(), rope.clone());
        let prg = parse_program(PathBuf::from(uri), &params.text);
        let semantic_tokens = semantic_token_from_ast(&prg);

        let diagnostics = prg
            .errors
            .iter()
            .filter_map(|e| match e {
                icy_ppe::parser::Error::ParserError(err) => {
                    let start_position = offset_to_position(err.range.start, &rope)?;
                    let end_position = offset_to_position(err.range.end, &rope)?;
                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        format!("{}", err.error),
                    ))
                }
                icy_ppe::parser::Error::TokenizerError(err) => {
                    let start_position = offset_to_position(err.range.start, &rope)?;
                    let end_position = offset_to_position(err.range.end, &rope)?;
                    Some(Diagnostic::new_simple(
                        Range::new(start_position, end_position),
                        format!("{}", err.error),
                    ))
                }
            })
            .collect::<Vec<_>>();

        self.client
            .publish_diagnostics(params.uri.clone(), diagnostics, Some(params.version))
            .await;

        self.ast_map.insert(params.uri.to_string(), prg);
        // self.client
        //     .log_message(MessageType::INFO, &format!("{:?}", semantic_tokens))
        //     .await;
        self.semantic_token_map
            .insert(params.uri.to_string(), semantic_tokens);
    }
}

#[tokio::main]
async fn main() {
    env_logger::init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(|client| Backend {
        client,
        ast_map: DashMap::new(),
        document_map: DashMap::new(),
        semantic_token_map: DashMap::new(),
    })
    .finish();

    serde_json::json!({"test": 20});
    Server::new(stdin, stdout, socket).serve(service).await;
}

struct TooltipVisitor {
    pub tooltip: Option<Hover>,
    pub offset: usize,
}

impl AstVisitor<()> for TooltipVisitor {
    fn visit_predefined_call_statement(&mut self, call: &icy_ppe::ast::PredefinedCallStatement) {
        if call.get_identifier_token().span.contains(&self.offset) {
            self.tooltip = get_statement_hover(call.get_func().opcode);
        }
    }
}

fn offset_to_position(offset: usize, rope: &Rope) -> Option<Position> {
    let line = rope.try_char_to_line(offset).ok()?;
    let first_char_of_line = rope.try_line_to_char(line).ok()?;
    let column = offset - first_char_of_line;
    Some(Position::new(line as u32, column as u32))
}

fn get_tooltip(ast: &Program, offset: usize) -> Option<Hover> {
    let mut visitor = TooltipVisitor {
        tooltip: None,
        offset,
    };
    ast.visit(&mut visitor);
    visitor.tooltip
}

fn get_statement_hover(opcode: icy_ppe::tables::OpCode) -> Option<Hover> {
    match opcode {
        OpCode::WHILE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-while")),
        OpCode::END => get_hint(fl!(LANGUAGE_LOADER, "hint-func-end")),
        OpCode::CLS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-cls")),
        OpCode::CLREOL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-clreol")),
        OpCode::MORE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-more")),
        OpCode::WAIT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-wait")),
        OpCode::COLOR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-color")),
        OpCode::GOTO => get_hint(fl!(LANGUAGE_LOADER, "hint-func-goto")),
        OpCode::LET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-let")),
        OpCode::PRINT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-print")),
        OpCode::PRINTLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-println")),
        OpCode::CONFFLAG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-confflag")),
        OpCode::CONFUNFLAG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-confunflag")),
        OpCode::DISPFILE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dispfile")),
        OpCode::INPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-input")),
        OpCode::FCREATE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fcreate")),
        OpCode::FOPEN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fopen")),
        OpCode::FAPPEND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fappend")),
        OpCode::FCLOSE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fclose")),
        OpCode::FGET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fget")),
        OpCode::FPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fput")),
        OpCode::FPUTLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fputln")),
        OpCode::RESETDISP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-resetdisp")),
        OpCode::STARTDISP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-startdisp")),
        OpCode::FPUTPAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fputpad")),
        OpCode::HANGUP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-hangup")),
        OpCode::GETUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-getuser")),
        OpCode::PUTUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-putuser")),
        OpCode::DEFCOLOR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-defcolor")),
        OpCode::DELETE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-delete")),
        OpCode::DELUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-deluser")),
        OpCode::ADJTIME => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjtime")),
        OpCode::LOG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-log")),
        OpCode::INPUTSTR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputstr")),
        OpCode::INPUTYN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputyn")),
        OpCode::INPUTMONEY => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputmoney")),
        OpCode::INPUTINT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputint")),
        OpCode::INPUTCC => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputcc")),
        OpCode::INPUTDATE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputdate")),
        OpCode::INPUTTIME => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputtime")),
        OpCode::GOSUB => get_hint(fl!(LANGUAGE_LOADER, "hint-func-gosub")),
        OpCode::RETURN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-return")),
        OpCode::PROMPTSTR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-promptstr")),
        OpCode::DTRON => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dtron")),
        OpCode::DTROFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dtroff")),
        OpCode::CDCHKON => get_hint(fl!(LANGUAGE_LOADER, "hint-func-cdchkon")),
        OpCode::CDCHKOFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-cdchkoff")),
        OpCode::DELAY => get_hint(fl!(LANGUAGE_LOADER, "hint-func-delay")),
        OpCode::SENDMODEM => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sendmodem")),
        OpCode::INC => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inc")),
        OpCode::DEC => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dec")),
        OpCode::NEWLINE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-newline")),
        OpCode::NEWLINES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-newlines")),
        OpCode::TOKENIZE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tokenize")),
        OpCode::GETTOKEN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-gettoken")),
        OpCode::SHELL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-shell")),
        OpCode::DISPTEXT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-disptext")),
        OpCode::STOP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-stop")),
        OpCode::INPUTTEXT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-inputtext")),
        OpCode::BEEP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-beep")),
        OpCode::PUSH => get_hint(fl!(LANGUAGE_LOADER, "hint-func-push")),
        OpCode::POP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pop")),
        OpCode::KBDSTUFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdstuff")),
        OpCode::CALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-call")),
        OpCode::JOIN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-join")),
        OpCode::QUEST => get_hint(fl!(LANGUAGE_LOADER, "hint-func-quest")),
        OpCode::BLT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-blt")),
        OpCode::DIR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dir")),
        OpCode::KBDFILE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdfile")),
        OpCode::BYE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-bye")),
        OpCode::GOODBYE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-goodbye")),
        OpCode::BROADCAST => get_hint(fl!(LANGUAGE_LOADER, "hint-func-broadcast")),
        OpCode::WAITFOR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-waitfor")),
        OpCode::KBDCHKON => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdchkon")),
        OpCode::KBDCHKOFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdchkoff")),
        OpCode::OPTEXT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-optext")),
        OpCode::DISPSTR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dispstr")),
        OpCode::RDUNET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-rdunet")),
        OpCode::WRUNET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-wrunet")),
        OpCode::DOINTR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dointr")),
        OpCode::VARSEG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-varseg")),
        OpCode::VAROFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-varoff")),
        OpCode::POKEB => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pokeb")),
        OpCode::POKEW => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pokew")),
        OpCode::VARADDR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-varaddr")),
        OpCode::ANSIPOS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-ansipos")),
        OpCode::BACKUP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-backup")),
        OpCode::FORWARD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-forward")),
        OpCode::FRESHLINE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-freshline")),
        OpCode::WRUSYS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-wrusys")),
        OpCode::RDUSYS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-rdusys")),
        OpCode::NEWPWD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-newpwd")),
        OpCode::OPENCAP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-opencap")),
        OpCode::CLOSECAP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-closecap")),
        OpCode::MESSAGE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-message")),
        OpCode::SAVESCRN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-savescrn")),
        OpCode::RESTSCRN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-restscrn")),
        OpCode::SOUND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sound")),
        OpCode::CHAT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-chat")),
        OpCode::SPRINT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sprint")),
        OpCode::SPRINTLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sprintln")),
        OpCode::MPRINT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-mprint")),
        OpCode::MPRINTLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-mprintln")),
        OpCode::RENAME => get_hint(fl!(LANGUAGE_LOADER, "hint-func-rename")),
        OpCode::FREWIND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-frewind")),
        OpCode::POKEDW => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pokedw")),
        OpCode::DBGLEVEL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dbglevel")),
        OpCode::SHOWON => get_hint(fl!(LANGUAGE_LOADER, "hint-func-showon")),
        OpCode::SHOWOFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-showoff")),
        OpCode::PAGEON => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pageon")),
        OpCode::PAGEOFF => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pageoff")),
        OpCode::FSEEK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fseek")),
        OpCode::FFLUSH => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fflush")),
        OpCode::FREAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fread")),
        OpCode::FWRITE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fwrite")),
        OpCode::FDEFIN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdefin")),
        OpCode::FDEFOUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdefout")),
        OpCode::FDGET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdget")),
        OpCode::FDPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdput")),
        OpCode::FDPUTLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdputln")),
        OpCode::FDPUTPAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdputpad")),
        OpCode::FDREAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdread")),
        OpCode::FDWRITE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdwrite")),
        OpCode::ADJBYTES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjbytes")),
        OpCode::KBDSTRING => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdstring")),
        OpCode::ALIAS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-alias")),
        OpCode::REDIM => get_hint(fl!(LANGUAGE_LOADER, "hint-func-redim")),
        OpCode::APPEND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-append")),
        OpCode::COPY => get_hint(fl!(LANGUAGE_LOADER, "hint-func-copy")),
        OpCode::KBDFLUSH => get_hint(fl!(LANGUAGE_LOADER, "hint-func-kbdflush")),
        OpCode::MDMFLUSH => get_hint(fl!(LANGUAGE_LOADER, "hint-func-mdmflush")),
        OpCode::KEYFLUSH => get_hint(fl!(LANGUAGE_LOADER, "hint-func-keyflush")),
        OpCode::LASTIN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-lastin")),
        OpCode::FLAG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-flag")),
        OpCode::DOWNLOAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-download")),
        OpCode::WRUSYSDOOR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-wrusysdoor")),
        OpCode::GETALTUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-getaltuser")),
        OpCode::ADJDBYTES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjdbytes")),
        OpCode::ADJTBYTES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjtbytes")),
        OpCode::ADJTFILES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjtfiles")),
        OpCode::LANG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-lang")),
        OpCode::SORT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sort")),
        OpCode::MOUSEREG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-mousereg")),
        OpCode::SCRFILE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-scrfile")),
        OpCode::SEARCHINIT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-searchinit")),
        OpCode::SEARCHFIND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-searchfind")),
        OpCode::SEARCHSTOP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-searchstop")),
        OpCode::PRFOUND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-prfound")),
        OpCode::PRFOUNDLN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-prfoundln")),
        OpCode::TPAGET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpaget")),
        OpCode::TPAPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpaput")),
        OpCode::TPACGET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpacget")),
        OpCode::TPACPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpacput")),
        OpCode::TPAREAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tparead")),
        OpCode::TPAWRITE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpawrite")),
        OpCode::TPACREAD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpacread")),
        OpCode::TPACWRITE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-tpacwrite")),
        OpCode::BITSET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-bitset")),
        OpCode::BITCLEAR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-bitclear")),
        OpCode::BRAG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-brag")),
        OpCode::FREALTUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-frealtuser")),
        OpCode::SETLMR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-setlmr")),
        OpCode::SETENV => get_hint(fl!(LANGUAGE_LOADER, "hint-func-setenv")),
        OpCode::FCLOSEALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fcloseall")),
        OpCode::DECLARE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-declare")),
        OpCode::FUNCTION => get_hint(fl!(LANGUAGE_LOADER, "hint-func-function")),
        OpCode::PROCEDURE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-procedure")),
        OpCode::PCALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-pcall")),
        OpCode::FPCLR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fpclr")),
        OpCode::BEGIN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-begin")),
        OpCode::FEND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fend")),
        OpCode::STATIC => get_hint(fl!(LANGUAGE_LOADER, "hint-func-static")),
        OpCode::STACKABORT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-stackabort")),
        OpCode::DCREATE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dcreate")),
        OpCode::DOPEN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dopen")),
        OpCode::DCLOSE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dclose")),
        OpCode::DSETALIAS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dsetalias")),
        OpCode::DPACK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dpack")),
        OpCode::DCLOSEALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dcloseall")),
        OpCode::DLOCK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dlock")),
        OpCode::DLOCKR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dlockr")),
        OpCode::DLOCKG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dlockg")),
        OpCode::DUNLOCK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dunlock")),
        OpCode::DNCREATE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dncreate")),
        OpCode::DNOPEN => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dnopen")),
        OpCode::DNCLOSE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dnclose")),
        OpCode::DNCLOSEALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dncloseall")),
        OpCode::DNEW => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dnew")),
        OpCode::DADD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dadd")),
        OpCode::DAPPEND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dappend")),
        OpCode::DTOP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dtop")),
        OpCode::DGO => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dgo")),
        OpCode::DBOTTOM => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dbottom")),
        OpCode::DSKIP => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dskip")),
        OpCode::DBLANK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dblank")),
        OpCode::DDELETE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-ddelete")),
        OpCode::DRECALL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-drecall")),
        OpCode::DTAG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dtag")),
        OpCode::DSEEK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dseek")),
        OpCode::DFBLANK => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dfblank")),
        OpCode::DGET => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dget")),
        OpCode::DPUT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dput")),
        OpCode::DFCOPY => get_hint(fl!(LANGUAGE_LOADER, "hint-func-dfcopy")),
        OpCode::EVAL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-eval")),
        OpCode::ACCOUNT => get_hint(fl!(LANGUAGE_LOADER, "hint-func-account")),
        OpCode::RECORDUSAGE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-recordusage")),
        OpCode::MSGTOFILE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-msgtofile")),
        OpCode::QWKLIMITS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-qwklimits")),
        OpCode::COMMAND => get_hint(fl!(LANGUAGE_LOADER, "hint-func-command")),
        OpCode::USELMRS => get_hint(fl!(LANGUAGE_LOADER, "hint-func-uselmrs")),
        OpCode::CONFINFO => get_hint(fl!(LANGUAGE_LOADER, "hint-func-confinfo")),
        OpCode::ADJTUBYTES => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adjtubytes")),
        OpCode::GRAFMODE => get_hint(fl!(LANGUAGE_LOADER, "hint-func-grafmode")),
        OpCode::ADDUSER => get_hint(fl!(LANGUAGE_LOADER, "hint-func-adduser")),
        OpCode::KILLMSG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-killmsg")),
        OpCode::CHDIR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-chdir")),
        OpCode::MKDIR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-mkdir")),
        OpCode::REDIR => get_hint(fl!(LANGUAGE_LOADER, "hint-func-redir")),
        OpCode::FDOWRAKA => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdowraka")),
        OpCode::FDOADDAKA => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdoaddaka")),
        OpCode::FDOWRORG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdowrorg")),
        OpCode::FDOADDORG => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdoaddorg")),
        OpCode::FDOQMOD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdoqmod")),
        OpCode::FDOQADD => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdoqadd")),
        OpCode::FDOQDEL => get_hint(fl!(LANGUAGE_LOADER, "hint-func-fdoqdel")),
        OpCode::SOUNDDELAY => get_hint(fl!(LANGUAGE_LOADER, "hint-func-sounddelay")),
        OpCode::ShortDesc => get_hint(fl!(LANGUAGE_LOADER, "hint-func-shortdesc")),
        OpCode::MoveMsg => get_hint(fl!(LANGUAGE_LOADER, "hint-func-movemsg")),
        OpCode::SetBankBal => get_hint(fl!(LANGUAGE_LOADER, "hint-func-setbankbal")),
        _ => None,
    }
}

fn get_hint(arg: String) -> Option<Hover> {
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: arg,
        }),
        range: None,
    })
}
