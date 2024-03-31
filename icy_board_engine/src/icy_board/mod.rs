use std::{collections::HashMap, fs, path::PathBuf};

use icy_ppe::Res;
use qfile::{QFilePath, QTraitSync};
use thiserror::Error;

use self::{
    conferences::ConferenceHeader, data::IcyBoardData, text_messages::DisplayText,
    user_inf::UserInf, users::UserRecord,
};

pub mod conferences;
pub mod data;
pub mod messages;
pub mod output;
pub mod state;
pub mod text_messages;
pub mod user_inf;
pub mod users;

#[derive(Error, Debug)]
pub enum IcyBoardError {
    #[error("Error: {0}")]
    Error(String),

    #[error("invalid user.inf record size: '{0}' expected {1} got {2}")]
    InvalidUserInfRecordSize(&'static str, usize, usize),

    #[error("Can't run action ({0})")]
    UnknownAction(String),

    #[error("Thread crashed ({0})")]
    ThreadCrashed(String),
}

pub struct IcyBoard {
    pub users: Vec<User>,
    pub data: IcyBoardData,
    pub conferences: Vec<ConferenceHeader>,
    pub display_text: DisplayText,

    paths: HashMap<String, String>,
}

impl IcyBoard {
    pub fn new() -> Self {
        let users = Vec::new();
        let conferences = Vec::new();
        let data = IcyBoardData::default();
        let display_text = DisplayText::default();
        let paths = HashMap::new();
        IcyBoard {
            users,
            data,
            conferences,
            display_text,
            paths,
        }
    }
    pub fn resolve_file(&self, file: &str) -> String {
        let mut s: String = file
            .chars()
            .map(|x| match x {
                '\\' => '/',
                _ => x,
            })
            .collect();

        for (k, v) in &self.paths {
            if s.starts_with(k) {
                s = v.clone() + &s[k.len()..];
            }
        }

        if let Ok(mut file_path) = QFilePath::add_path(s.clone()) {
            if let Ok(file) = file_path.get_path_buf() {
                return file.to_string_lossy().to_string();
            }
        }
        s
    }

    pub fn load(file: &str) -> Res<IcyBoard> {
        let data = IcyBoardData::deserialize(file)?;
        let users = Vec::new();
        let conferences = Vec::new();
        let mut paths = HashMap::new();

        let file_path = PathBuf::from(file);
        let mut help = data.path.help_loc.clone();
        if help.ends_with('\\') {
            help.pop();
        }

        help = help.replace('\\', "/");

        let help_loc = PathBuf::from(&help);
        let mut path = file_path.parent().unwrap().to_path_buf();
        path.push(help_loc.file_name().unwrap());
        if !path.exists() {
            return Err(Box::new(IcyBoardError::Error(
                "Can't resolve C: file".to_string(),
            )));
        }

        //let len = to_str().unwrap().len();
        let k = help_loc.parent().unwrap().to_str().unwrap().to_string();
        let v = file_path
            .parent()
            .unwrap()
            .to_path_buf()
            .to_str()
            .unwrap()
            .to_string();
        paths.insert(k, v);
        let mut res = IcyBoard {
            users,
            data,
            conferences,
            display_text: DisplayText::default(),
            paths,
        };

        let r = res.resolve_file(&res.data.path.usr_file);
        let users = UserRecord::read_users(&PathBuf::from(&r))?;

        let r = res.resolve_file(&res.data.path.inf_file);
        let user_inf = UserInf::read_users(&PathBuf::from(&r))?;
        for user in users {
            let inf = user_inf[user.rec_num - 1].clone();
            res.users.push(User { user, inf });
        }

        let r = res.resolve_file(&res.data.path.conference_file);
        let max_conferences = res.data.num_conf as usize;
        let conferences = ConferenceHeader::load(&r, max_conferences)?;
        res.conferences = conferences;
        let txt = fs::read(res.resolve_file(&res.data.path.text_loc) + "/PCBTEXT")?;
        res.display_text = DisplayText::parse_file(&txt)?;
        Ok(res)
    }
}

impl Default for IcyBoard {
    fn default() -> Self {
        Self::new()
    }
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
