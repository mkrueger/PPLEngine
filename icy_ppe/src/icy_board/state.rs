use std::{collections::HashMap, fs, path::Path};

use crate::Res;

use super::{
    data::{IcyBoardData, Node},
    text_messages::DisplayText,
    users::UserRecord,
};

#[derive(Clone, Debug)]
pub struct IcyBoardState {
    pub users: Vec<UserRecord>,
    pub nodes: Vec<Node>,
    pub data: IcyBoardData,

    pub icy_display_text: DisplayText,

    pub yes_char: char,
    pub no_char: char,

    /// If true, the keyboard timer is checked.
    /// After it's elapsed logoff the user for inactivity.
    pub keyboard_check: bool,
    /// The text displayed by the @OPTEXT@ macro
    pub op_text: String,

    /// 0 = no debug, 1 - errors, 2 - errors and warnings, 3 - all
    pub debug_level: i32,
    pub use_alias: bool,
    pub display_text: bool,
    pub env_vars: HashMap<String, String>,
}

impl Default for IcyBoardState {
    fn default() -> Self {
        Self {
            users: Vec::new(),
            nodes: Vec::new(),
            data: IcyBoardData::default(),
            icy_display_text: DisplayText::default(),
            yes_char: 'Y',
            no_char: 'N',
            keyboard_check: false,
            op_text: String::new(),
            debug_level: 0,
            use_alias: false,
            display_text: true,
            env_vars: HashMap::new(),
        }
    }
}

impl IcyBoardState {
    /// Returns the load data of this [`IcyBoardData`].
    /// # Errors
    pub fn load_data(&mut self) -> Res<()> {
        let pcb_text = Path::new(&self.data.path.text_loc).join("PCBTEXT");

        let text_data = fs::read(pcb_text)?;
        self.icy_display_text = DisplayText::parse_file(&text_data)?;

        Ok(())
    }

    /// Turns on keyboard check & resets the keyboard check timer.
    pub fn reset_keyboard_check_timer(&mut self) {
        self.keyboard_check = true;
    }

    pub fn get_env(&self, key: &str) -> Option<&String> {
        self.env_vars.get(key)
    }

    pub fn set_env(&mut self, key: &str, value: &str) {
        self.env_vars.insert(key.to_string(), value.to_string());
    }

    pub fn remove_env(&mut self, env: &str) {
        self.env_vars.remove(env);
    }
}
