use std::{
    fs,
    io::{Cursor, Read},
    path::Path,
};

use byteorder::{LittleEndian, ReadBytesExt};

use crate::Res;

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

    // datetype       DateLastDirRead;    ///  Date for Last DIR Scan (most recent file)
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
    // unsigned short LastConference;     ///  Number of the conference the caller was in
    pub total_dl_bytes: usize,
    pub total_ul_bytes: usize,
    // bool           DeleteFlag;         ///  1=delete this record, 0=keep
    // long           RecNum;             ///  Record Number in USERS.INF file
    // packedbyte2    Flags;
    // char           Reserved[8];        ///  Bytes 390-397 from the USERS file
    // unsigned long  MsgsRead;           ///  Number of messages the user has read in PCB
    // unsigned long  MsgsLeft;           ///  Number of messages the user has left in PCB
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
    pub birth_date: i32,
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
