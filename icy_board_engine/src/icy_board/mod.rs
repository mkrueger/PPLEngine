use thiserror::Error;

use self::{user_inf::UserInf, users::UserRecord};

pub mod conferences;
pub mod data;
pub mod messages;
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

impl User {
    pub fn get_first_name(&self) -> String {
        if let Some(idx) = self.user.name.find(' ') {
            self.user.name[..idx].to_string()
        } else {
            self.user.name.clone()
        }
    }
    pub fn get_last_name(&self) -> String {
        if let Some(idx) = self.user.name.find(' ') {
            self.user.name[idx + 1..].to_string()
        } else {
            String::new()
        }
    }
}
