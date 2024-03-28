use thiserror::Error;

use self::{user_inf::UserInf, users::UserRecord};

pub mod data;
pub mod state;
pub mod text_messages;
pub mod user_inf;
pub mod users;

#[derive(Error, Debug)]
pub enum IcyBoardError {
    #[error("invalid user.inf record size: '{0}' expected {1} got {2}")]
    InvalidUserInfRecordSize(&'static str, usize, usize),
}

#[derive(Clone)]
pub struct User {
    pub user: UserRecord,
    pub inf: UserInf,
}
