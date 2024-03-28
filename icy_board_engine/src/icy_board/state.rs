use std::{collections::HashMap, fs, path::Path};

use icy_ppe::Res;

use super::{
    data::{IcyBoardData, Node},
    text_messages::DisplayText,
    User,
};

#[derive(Clone, Default)]
pub struct DisplayOptions {
    /// If true, the more prompt is automatically answered after 10 seconds.
    pub auto_more: bool,

    /// If true, the @ color codes are disabled.
    pub disable_color: bool,
}

#[derive(Clone, Default)]
pub struct TransferStatistics {
    pub downloaded_files: usize,
    pub downloaded_bytes: usize,
    pub uploaded_files: usize,
    pub uploaded_bytes: usize,

    pub dl_transfer_time: usize,
    pub ul_transfer_time: usize,
}

impl TransferStatistics {
    pub fn get_cps_download(&self) -> usize {
        if self.dl_transfer_time == 0 {
            return 0;
        }
        self.downloaded_bytes / self.dl_transfer_time
    }

    pub fn get_cps_upload(&self) -> usize {
        if self.ul_transfer_time == 0 {
            return 0;
        }
        self.uploaded_bytes / self.ul_transfer_time
    }

    pub fn get_cps_both(&self) -> usize {
        let total_time = self.dl_transfer_time + self.ul_transfer_time;
        if total_time == 0 {
            return 0;
        }
        // actually correct - it's not the average, but the accumlated csp
        (self.downloaded_bytes + self.uploaded_bytes) / total_time
    }
}

#[derive(Default, Clone)]
pub struct ConferenceType {
    pub name: String,
    pub number: u16,
}

#[derive(Clone)]
pub struct IcyBoardState {
    pub users: Vec<User>,
    pub nodes: Vec<Node>,
    pub data: IcyBoardData,

    pub icy_display_text: DisplayText,

    pub disp_options: DisplayOptions,
    pub transfer_statistics: TransferStatistics,
    pub current_conference: ConferenceType,

    pub login_date: u16,

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
            login_date: 0,
            op_text: String::new(),
            disp_options: DisplayOptions::default(),
            transfer_statistics: TransferStatistics::default(),
            current_conference: ConferenceType {
                name: String::new(),
                number: 0,
            },
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

    pub fn country_date(&self, date: u16) -> String {
        // todo date format.
        icy_ppe::datetime::juilian_to_date(date)
    }
}
