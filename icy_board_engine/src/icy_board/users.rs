use std::{
    fs,
    io::{Cursor, Read},
    path::Path,
};

use byteorder::{LittleEndian, ReadBytesExt};
use icy_ppe::Res;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct UserRecord {
    pub name: String,
    pub city: String,
    pub password: String,

    pub bus_data_phone: String,
    pub home_voice_phone: String,

    pub last_date_on: u16,
    pub last_time_on: String,

    pub expert_mode: bool,

    /// Protocol (A->Z)
    pub protocol: char,

    pub is_dirty: bool,
    pub msg_clear: bool,
    pub has_mail: bool,
    pub dont_ask_fse: bool,
    pub use_fsedefault: bool,
    pub scroll_msg_body: bool,
    pub short_header: bool,
    pub wide_editor: bool,

    ///  Date for Last DIR Scan (most recent file)
    pub date_last_dir_read: u16,
    pub security_level: u8,

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

    /// Julian date for Registration Expiration Date
    pub reg_exp_date: u16,
    // unsigned short LastConference;     ///  Number of the conference the caller was in
    pub delete_flag: bool,
    pub rec_num: usize,

    pub last_conference: u16,
    pub ul_tot_dnld_bytes: u32,
    pub ul_tot_upld_bytes: u32,
}

impl UserRecord {
    /// # Errors
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

            let last_date_on = cursor.read_u16::<LittleEndian>()?;

            let mut last_time_on = [0u8; 6];
            cursor.read_exact(&mut last_time_on)?;

            let expert_mode = cursor.read_u8()?;
            let protocol = cursor.read_u8()?;

            let packet_flags = cursor.read_u8()?;

            let is_dirty = (packet_flags & (1 << 0)) != 0;
            let msg_clear = (packet_flags & (1 << 1)) != 0;
            let has_mail = (packet_flags & (1 << 2)) != 0;
            let dont_ask_fse = (packet_flags & (1 << 3)) != 0;
            let use_fsedefault = (packet_flags & (1 << 4)) != 0;
            let scroll_msg_body = (packet_flags & (1 << 5)) != 0;
            let short_header = (packet_flags & (1 << 6)) != 0;
            let wide_editor = (packet_flags & (1 << 7)) != 0;

            let date_last_dir_read = cursor.read_u16::<LittleEndian>()?;

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

            let reg_exp_date = cursor.read_u16::<LittleEndian>()?;
            let exp_security_level = cursor.read_u16::<LittleEndian>()? as i32;
            let last_conference = cursor.read_u16::<LittleEndian>()?;
            let ul_tot_dnld_bytes = cursor.read_u32::<LittleEndian>()?;
            let ul_tot_upld_bytes = cursor.read_u32::<LittleEndian>()?;
            let delete_flag = cursor.read_u8()? != 0;
            let rec_num = cursor.read_u32::<LittleEndian>()? as usize;

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
                last_date_on,
                last_time_on: String::from_utf8_lossy(&last_time_on).trim().to_string(),
                expert_mode: expert_mode == b'Y',
                protocol: protocol as char,

                is_dirty,
                msg_clear,
                has_mail,
                dont_ask_fse,
                use_fsedefault,
                scroll_msg_body,
                short_header,
                wide_editor,

                date_last_dir_read,
                security_level,
                num_times_on: num_times_on as usize,
                page_len: page_len as i32,
                num_uploads: num_uploads as i32,
                num_downloads: num_downloads as i32,
                daily_downloaded_bytes: daily_downloaded_bytes as usize,
                user_comment: String::from_utf8_lossy(&cmt1).trim().to_string(),
                sysop_comment: String::from_utf8_lossy(&cmt2).trim().to_string(),
                elapsed_time_on,
                reg_exp_date,
                exp_security_level,
                last_conference,
                ul_tot_dnld_bytes,
                ul_tot_upld_bytes,
                delete_flag,
                rec_num,
            };

            users.push(user);
        }
        Ok(users)
    }
}
