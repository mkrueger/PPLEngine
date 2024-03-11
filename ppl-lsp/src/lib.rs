pub mod completion;
pub mod jump_definition;
pub mod reference;
pub mod semantic_token;

#[derive(Debug)]
pub struct ImCompleteSemanticToken {
    pub start: usize,
    pub length: usize,
    pub token_type: usize,
}
