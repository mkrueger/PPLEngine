use std::{
    collections::HashMap,
    fs::{self, File},
    io::{BufRead, BufReader, Cursor, Read},
    path::Path,
};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::Res;

use super::text_messages::DisplayText;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct PcbDataType {
    // Software version
    pub version: String,

    // sysop display name
    pub sysop: String,

    pub sysop_security: SysopSecurity,

    pub path: PcbPaths,

    // sysop local password
    pub password: String,

    // true if use sysop real name instead of 'SYSOP'
    pub use_real_name: bool,

    pub use_local_graphics: bool,

    /// node number of this node
    pub node_number: usize,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct SysopSecurity {
    pub sysop: i32,
    pub read_all_comments: i32,
    pub read_all_mail: i32,
    pub copy_move_messages: i32,
    pub enter_at_vars_in_messages: i32,
    pub edit_any_message: i32,
    pub not_update_msg_read_status: i32,
    pub use_broadcast_command: i32,
    pub view_private_uploads: i32,
    pub enter_generic_message: i32,
    pub edit_message_headers: i32,
    pub protect_unprotect_messages: i32,
    pub overwrite_uploads: i32,
    pub set_pack_out_date_on_messages: i32,
    pub see_all_return_receipt_messages: i32,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct PcbPaths {
    /// location of help files
    help_loc: String,
    /// location of security message files
    sec_loc: String,
    /// location of chat files
    chat_loc: String,
    /// location of pcbtext files
    text_loc: String,
    /// location of index files
    index_loc: String,
    /// location of temporary files
    tmp_loc: String,
    /// name of users file
    usr_file: String,
    /// name of users info file
    inf_file: String,
    /// name and location of callers file
    clr_file: String,
    /// name and location of conference data file
    conference_file: String,
    /// name and location of pwrd file
    pwd_file: String,
    /// name and location of fsec file
    fsec_file: String,
    /// name and location of upsec file
    upsec_file: String,
    /// name and location of tcan file
    tcan_file: String,
    /// name and location of welcome file
    welcome_file: String,
    /// name and location of newuser file
    newuser_file: String,
    /// name and location of closed file
    closed_file: String,
    /// name and location of warning file
    warning_file: String,
    /// name and location of expired file
    expired_file: String,
    /// name and location of USERNET.DAT file
    usernet_file: String,
    /// name and location of conference join menu
    conf_menu: String,
    /// name and location of newreg file questions
    newreg_file: String,
    /// name and location of non-reg user's answer file
    answer_file: String,
    /// name and location of protocol data file
    protecol_data_file: String,
    /// name and location of download summary file
    download_file: String,
    /// name and loc of logoff script questionnaire
    logoff_script: String,
    /// name and loc of logoff script answers
    logoff_answer: String,
    /// name and loc of pcbml.dat file
    pcml_dat_file: String,
    /// name and loc of group chat topic/intro file
    group_chat: String,
    /// name and loc of PCBFILER.DEF file
    color_file: String,
}

const SYSOP_LEVEL_LINE: usize = 7;
const READ_ALL_COMMENTS_LINE: usize = 5;
const READ_ALL_MAIL_LINE: usize = 6;
const COPY_MOVE_MESSAGES_LINE: usize = 8;
const ENTER_AT_VARS_IN_MESSAGES_LINE: usize = 163;
const EDIT_ANY_MESSAGE_LINE: usize = 172;
const NOT_UPDATE_MSG_READ_STATUS_LINE: usize = 176;
const USE_BROADCAST_COMMAND_LINE: usize = 234;
const VIEW_PRIVATE_UPLOADS_LINE: usize = 235;
const ENTER_GENERIC_MESSAGE_LINE: usize = 236;
const EDIT_MESSAGE_HEADERS_LINE: usize = 237;
const PROTECT_UNPROTECT_MESSAGES_LINE: usize = 238;
const OVERWRITE_UPLOADS_LINE: usize = 239;
const SET_PACK_OUT_DATE_ON_MESSAGES_LINE: usize = 252;
const SEE_ALL_RETURN_RECEIPT_MESSAGES_LINE: usize = 253;

impl PcbDataType {
    /// .
    ///
    /// # Errors
    /// # Panics
    ///
    /// Panics if .
    pub fn load(filename: &str, c_drive: &str) -> Res<Self> {
        let mut lines = Vec::new();
        let reader = BufReader::new(File::open(filename)?);

        for (i, line) in reader.lines().enumerate() {
            if let Ok(line) = line {
                lines.push(line);
            } else {
                log::warn!("Error reading line {}", i);
                lines.push(String::new());
            }
        }

        let ret = Self {
            version: lines[0].clone(),
            sysop: lines[1].clone(),
            password: lines[2].clone(),
            use_real_name: lines[3] != "0",
            use_local_graphics: lines[4] != "0",
            node_number: 0,

            sysop_security: SysopSecurity {
                sysop: lines[SYSOP_LEVEL_LINE].parse().unwrap(),
                read_all_comments: lines[READ_ALL_COMMENTS_LINE].parse().unwrap(),
                read_all_mail: lines[READ_ALL_MAIL_LINE].parse().unwrap(),
                copy_move_messages: lines[COPY_MOVE_MESSAGES_LINE].parse().unwrap(),
                enter_at_vars_in_messages: lines[ENTER_AT_VARS_IN_MESSAGES_LINE].parse().unwrap(),
                edit_any_message: lines[EDIT_ANY_MESSAGE_LINE].parse().unwrap(),
                not_update_msg_read_status: lines[NOT_UPDATE_MSG_READ_STATUS_LINE].parse().unwrap(),
                use_broadcast_command: lines[USE_BROADCAST_COMMAND_LINE].parse().unwrap(),
                view_private_uploads: lines[VIEW_PRIVATE_UPLOADS_LINE].parse().unwrap(),
                enter_generic_message: lines[ENTER_GENERIC_MESSAGE_LINE].parse().unwrap(),
                edit_message_headers: lines[EDIT_MESSAGE_HEADERS_LINE].parse().unwrap(),
                protect_unprotect_messages: lines[PROTECT_UNPROTECT_MESSAGES_LINE].parse().unwrap(),
                overwrite_uploads: lines[OVERWRITE_UPLOADS_LINE].parse().unwrap(),
                set_pack_out_date_on_messages: lines[SET_PACK_OUT_DATE_ON_MESSAGES_LINE]
                    .parse()
                    .unwrap(),
                see_all_return_receipt_messages: lines[SEE_ALL_RETURN_RECEIPT_MESSAGES_LINE]
                    .parse()
                    .unwrap(),
            },
            path: PcbPaths {
                help_loc: convert_path(c_drive, &lines[23]),
                sec_loc: convert_path(c_drive, &lines[24]),
                chat_loc: convert_path(c_drive, &lines[25]),
                text_loc: convert_path(c_drive, &lines[26]),
                index_loc: convert_path(c_drive, &lines[27]),
                tmp_loc: convert_path(c_drive, &lines[178]),
                usr_file: convert_path(c_drive, &lines[28]),
                inf_file: convert_path(c_drive, &lines[179]),
                clr_file: convert_path(c_drive, &lines[29]),
                conference_file: convert_path(c_drive, &lines[30]),
                pwd_file: convert_path(c_drive, &lines[31]),
                fsec_file: convert_path(c_drive, &lines[32]),
                upsec_file: convert_path(c_drive, &lines[33]),
                tcan_file: convert_path(c_drive, &lines[34]),
                welcome_file: convert_path(c_drive, &lines[35]),
                newuser_file: convert_path(c_drive, &lines[36]),
                closed_file: convert_path(c_drive, &lines[37]),
                warning_file: convert_path(c_drive, &lines[38]),
                expired_file: convert_path(c_drive, &lines[39]),
                usernet_file: convert_path(c_drive, &lines[40]),
                conf_menu: convert_path(c_drive, &lines[41]),
                newreg_file: convert_path(c_drive, &lines[42]),
                answer_file: convert_path(c_drive, &lines[43]),
                protecol_data_file: convert_path(c_drive, &lines[44]),
                download_file: convert_path(c_drive, &lines[45]),
                logoff_script: convert_path(c_drive, &lines[46]),
                logoff_answer: convert_path(c_drive, &lines[47]),
                pcml_dat_file: convert_path(c_drive, &lines[48]),
                group_chat: convert_path(c_drive, &lines[49]),
                color_file: convert_path(c_drive, &lines[153]),
            },
        };
        Ok(ret)
    }

    /// Returns the load users of this [`PcbDataType`].
    /// # Errors
    /// If no file can be read.
    pub fn load_users(&self) -> Res<Vec<UserRecord>> {
        UserRecord::read_users(Path::new(&self.path.usr_file))
    }
}

fn convert_path(c_drive: &str, path: &str) -> String {
    let mut path = path;
    let mut res = if path.starts_with("C:") {
        path = &path[2..];
        c_drive.to_string()
    } else {
        String::new()
    };

    for c in path.chars() {
        if c == '\\' {
            res.push('/');
        } else {
            res.push(c);
        }
    }
    res
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct UserRecord {
    pub name: String,
    pub password: String,

    pub bus_data_phone: String,
    pub home_voice_phone: String,

    pub last_date_on: u16,
    pub last_time_on: String,

    pub expert_mode: bool,

    /// Protocol (A->Z)
    pub protocol: char,

    pub msg_clear: bool,
    pub has_mail: bool,
    pub dont_ask_fse: bool,
    pub use_fsedefault: bool,
    pub scroll_msg_body: bool,
    pub short_header: bool,
    pub wide_editor: bool,

    // datetype       DateLastDirRead;    /* Date for Last DIR Scan (most recent file) */
    pub security_level: i32,
    /// Expired security level
    pub exp_security_level: i32,

    /// Number of times the caller has connected
    pub num_times_on: usize,

    /// Page length when display data on the screen
    pub page_len: i32,

    pub num_uploads: i32,
    pub num_downloads: i32,

    pub daily_downloaded_bytes: usize,

    pub user_comment: String,
    pub sysop_comment: String,

    /// Number of minutes online
    pub elapsed_time_on: i32,

    pub scroll_flag: bool,

    /// Julian date for Registration Expiration Date
    pub reg_exp_date: i32,
    // unsigned short LastConference;     /* Number of the conference the caller was in */
    pub total_dl_bytes: usize,
    pub total_ul_bytes: usize,
    // bool           DeleteFlag;         /* 1=delete this record, 0=keep */
    // long           RecNum;             /* Record Number in USERS.INF file */
    // packedbyte2    Flags;
    // char           Reserved[8];        /* Bytes 390-397 from the USERS file */
    // unsigned long  MsgsRead;           /* Number of messages the user has read in PCB */
    // unsigned long  MsgsLeft;           /* Number of messages the user has left in PCB */
    pub alias_support: bool,
    pub alias: String,

    pub address_support: bool,

    pub street1: String,
    pub street2: String,
    pub city: String,
    pub state: String,
    pub zip: String,
    pub country: String,

    pub notes: [String; 5],

    // bool           PasswordSupport;
    pub pwd_prev_pwd: [String; 3],
    pub pwd_last_change: i32,
    pub pwd_times_changed: usize,
    pub pwd_expire_date: i32,

    pub verify_support: bool,
    pub verify: String,
    // bool           StatsSupport;
    // callerstattype Stats;
    // bool           NotesSupport;
    // notestypez     Notes;
    // bool           AccountSupport;
    // accounttype    Account;
    // bool           QwkSupport;
    // qwkconfigtype  QwkConfig;

    // 3.40 vars
    pub short_descr: bool,
    pub gender: String,
    pub birth_date: String,
    pub email: String,
    pub web: String,
}

impl UserRecord {
    /// # Errors
    /// evelv
    pub fn read_users(path: &Path) -> Res<Vec<UserRecord>> {
        const RECORD_SIZE: u64 = 0x190;

        let mut users = Vec::new();

        let data = fs::read(path)?;

        let mut cursor = Cursor::new(data);
        while cursor.position() + RECORD_SIZE <= cursor.get_ref().len() as u64 {
            let mut name = [0u8; 25];
            cursor.read_exact(&mut name)?;

            let mut city = [0u8; 24];
            cursor.read_exact(&mut city)?;

            let mut password = [0u8; 12];
            cursor.read_exact(&mut password)?;

            let mut data_phone = [0u8; 13];
            cursor.read_exact(&mut data_phone)?;

            let mut voice_phone = [0u8; 13];
            cursor.read_exact(&mut voice_phone)?;

            let mut last_date_on = [0u8; 6];
            cursor.read_exact(&mut last_date_on)?;

            let mut last_time_on = [0u8; 5];
            cursor.read_exact(&mut last_time_on)?;

            let expert_mode = cursor.read_u8()?;
            let protocol = cursor.read_u8()?;

            // reserved byte
            cursor.read_u8()?;

            let mut date_last_dir_read = [0u8; 6];
            cursor.read_exact(&mut date_last_dir_read)?;

            let security_level = cursor.read_u8()?;
            let num_times_on = cursor.read_u16::<LittleEndian>()?;
            let page_len = cursor.read_u8()?;
            let num_uploads = cursor.read_u16::<LittleEndian>()?;
            let num_downloads = cursor.read_u16::<LittleEndian>()?;

            // unknown
            cursor.read_u8()?;
            cursor.read_u8()?;
            cursor.read_u8()?;
            let daily_downloaded_bytes = cursor.read_u32::<LittleEndian>()?;

            // unknown
            cursor.read_u8()?;

            let mut cmt1 = [0u8; 31];
            cursor.read_exact(&mut cmt1)?;
            let mut cmt2 = [0u8; 31];
            cursor.read_exact(&mut cmt2)?;

            let elapsed_time_on = cursor.read_u16::<LittleEndian>()? as i32;

            let mut reg_exp_date = [0u8; 6];
            cursor.read_exact(&mut reg_exp_date)?;

            // unknown data
            for _ in 0..0xCF {
                cursor.read_u8()?;
            }

            let user = UserRecord {
                name: String::from_utf8_lossy(&name).trim().to_string(),
                city: String::from_utf8_lossy(&city).trim().to_string(),
                password: String::from_utf8_lossy(&password).trim().to_string(),
                bus_data_phone: String::from_utf8_lossy(&data_phone).trim().to_string(),
                home_voice_phone: String::from_utf8_lossy(&voice_phone).trim().to_string(),
                expert_mode: expert_mode == b'Y',
                protocol: protocol as char,
                security_level: security_level as i32,
                num_times_on: num_times_on as usize,
                page_len: page_len as i32,
                num_uploads: num_uploads as i32,
                num_downloads: num_downloads as i32,
                daily_downloaded_bytes: daily_downloaded_bytes as usize,
                user_comment: String::from_utf8_lossy(&cmt1).trim().to_string(),
                sysop_comment: String::from_utf8_lossy(&cmt2).trim().to_string(),
                elapsed_time_on,

                ..Default::default()
            };

            users.push(user);
        }

        Ok(users)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Node {
    pub status: char,
    pub mail_waiting: bool,
    pub pager: u32,
    pub name: String,
    pub city: String,
    pub operation: String,
    pub message: String,
    pub channel: u8,
    pub last_update: String,
}

#[derive(Clone, Debug, Default)]
pub struct IcyBoardData {
    pub users: Vec<UserRecord>,
    pub nodes: Vec<Node>,
    pub pcb_data: PcbDataType,

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

impl IcyBoardData {
    /// Returns the load data of this [`IcyBoardData`].
    /// # Errors
    pub fn load_data(&mut self) -> Res<()> {
        let pcb_text = Path::new(&self.pcb_data.path.text_loc).join("PCBTEXT");

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
