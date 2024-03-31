use std::{fs::File, io::BufReader};

use icy_ppe::{parser::Encoding, Res};

use super::data::{append_bool, append_int, append_line, read_bool, read_int, read_line};

#[derive(Default, Clone, Debug)]
pub struct ConferenceHeader {
    pub name: String,
    pub auto_rejoin: bool,
    pub view_members: bool,
    pub private_uploads: bool,
    pub private_msgs: bool,
    pub echo_mail: bool,
    pub add_conference_security: i32,
    pub add_conference_time: i32,
    pub message_blocks: i32,
    pub message_file: String,
    pub users_menu: String,
    pub sysop_menu: String,
    pub news_file: String,

    /// Sort type for public upload DIR file
    pub pub_upload_sort: i32,
    pub pub_upload_directory: String,
    pub pub_upload_location: String,

    pub private_upload_sort: i32,
    pub private_upload_directory: String,
    pub private_upload_location: String,

    pub public_conference: bool,

    pub doors_menu: String,
    pub doors_file: String,
    pub doors_security: i32,

    pub blt_menu: String,
    pub blt_file: String,
    // empty string
    pub script_menu: String,
    pub script_file: String,
    // empty string
    pub dir_menu: String,
    pub dir_file: String,
    // empty string
    pub dlpth_list_file: String,
}

impl ConferenceHeader {
    #[allow(clippy::field_reassign_with_default)]
    pub fn deserialize(reader: &mut BufReader<File>) -> Res<Self> {
        let encoding = Encoding::CP437;
        let mut ret = Self::default();
        ret.name = read_line(reader, encoding)?;
        ret.auto_rejoin = read_bool(reader, encoding)?;
        ret.view_members = read_bool(reader, encoding)?;
        ret.private_uploads = read_bool(reader, encoding)?;
        ret.private_msgs = read_bool(reader, encoding)?;
        ret.echo_mail = read_bool(reader, encoding)?;
        ret.add_conference_security = read_int(reader, encoding)?;
        ret.add_conference_time = read_int(reader, encoding)?;
        ret.message_blocks = read_int(reader, encoding)?;
        ret.message_file = read_line(reader, encoding)?;
        ret.users_menu = read_line(reader, encoding)?;
        ret.sysop_menu = read_line(reader, encoding)?;
        ret.news_file = read_line(reader, encoding)?;
        ret.pub_upload_sort = read_int(reader, encoding)?;
        ret.pub_upload_directory = read_line(reader, encoding)?;
        ret.pub_upload_location = read_line(reader, encoding)?;
        ret.private_upload_sort = read_int(reader, encoding)?;
        ret.private_upload_directory = read_line(reader, encoding)?;
        ret.private_upload_location = read_line(reader, encoding)?;
        ret.public_conference = read_bool(reader, encoding)?;
        ret.doors_menu = read_line(reader, encoding)?;
        ret.doors_file = read_line(reader, encoding)?;
        ret.doors_security = read_int(reader, encoding)?;
        ret.blt_menu = read_line(reader, encoding)?;
        ret.blt_file = read_line(reader, encoding)?;
        read_line(reader, encoding)?;
        ret.script_menu = read_line(reader, encoding)?;
        ret.script_file = read_line(reader, encoding)?;
        read_line(reader, encoding)?;
        ret.dir_menu = read_line(reader, encoding)?;
        ret.dir_file = read_line(reader, encoding)?;
        read_line(reader, encoding)?;
        ret.dlpth_list_file = read_line(reader, encoding)?;
        Ok(ret)
    }

    pub fn load(file_name: &String, len: usize) -> Res<Vec<Self>> {
        let mut reader = BufReader::new(File::open(file_name)?);
        let mut ret = Vec::new();
        for _ in 0..=len {
            ret.push(Self::deserialize(&mut reader)?);
        }

        Ok(ret)
    }

    pub fn serialize(&self, _encoding: Encoding) -> Vec<u8> {
        let mut res = Vec::new();
        let encoding = Encoding::CP437;
        append_line(&mut res, encoding, &self.name);
        append_bool(&mut res, encoding, self.auto_rejoin);
        append_bool(&mut res, encoding, self.view_members);
        append_bool(&mut res, encoding, self.private_uploads);
        append_bool(&mut res, encoding, self.private_msgs);
        append_bool(&mut res, encoding, self.echo_mail);
        append_line(&mut res, encoding, &self.message_file);
        append_line(&mut res, encoding, &self.users_menu);
        append_line(&mut res, encoding, &self.sysop_menu);
        append_line(&mut res, encoding, &self.news_file);
        append_int(&mut res, encoding, self.add_conference_security);
        append_int(&mut res, encoding, self.add_conference_time);
        append_int(&mut res, encoding, self.message_blocks);
        append_line(&mut res, encoding, &self.pub_upload_directory);
        append_line(&mut res, encoding, &self.pub_upload_location);
        append_int(&mut res, encoding, self.pub_upload_sort);
        append_line(&mut res, encoding, &self.private_upload_directory);
        append_line(&mut res, encoding, &self.private_upload_location);
        append_int(&mut res, encoding, self.private_upload_sort);
        append_bool(&mut res, encoding, self.public_conference);
        append_line(&mut res, encoding, &self.doors_menu);
        append_line(&mut res, encoding, &self.doors_file);
        append_int(&mut res, encoding, self.doors_security);
        append_line(&mut res, encoding, &self.blt_menu);
        append_line(&mut res, encoding, &self.blt_file);
        res.extend(b"\r\n");
        append_line(&mut res, encoding, &self.script_menu);
        append_line(&mut res, encoding, &self.script_file);
        res.extend(b"\r\n");
        append_line(&mut res, encoding, &self.dir_menu);
        append_line(&mut res, encoding, &self.dir_file);
        res.extend(b"\r\n");
        append_line(&mut res, encoding, &self.dlpth_list_file);
        res
    }
}
