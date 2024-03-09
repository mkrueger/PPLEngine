use thiserror::Error;

#[derive(Error, Debug)]
pub enum IcyError {
    #[error("Parameter {0} should be from type String")]
    ParameterStringExpected(u32),

    #[error("Parameter {0} should be from type Integer")]
    ParameterIntegerExpected(u32),

    #[error("File channel should be 0 <= 7 it was: {0}")]
    FileChannelOutOfBounds(i32),

    #[error("{0} should be from type Integer")]
    IntegerExpected(String),

    #[error("not supported")]
    NotSupported,

    #[error("Variable {0} not found.")]
    VariableNotFound(String),

    #[error("User {0} not found.")]
    UserNotFound(String),
}
