use std::{
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use serde::{Deserialize, Serialize};

use super::{
    commands::Command,
    is_false, is_null_8, is_null_i32,
    pcbconferences::{PcbAdditionalConferenceHeader, PcbConferenceHeader},
    user_base::Password,
    IcyBoardSerializer,
};

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct Conference {
    pub name: String,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub is_public: bool,

    #[serde(default)]
    #[serde(skip_serializing_if = "Password::is_empty")]
    pub password: Password,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub required_security: u8,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub sec_attachments: u8,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub sec_write_message: u8,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub auto_rejoin: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub view_members: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub private_uploads: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub private_msgs: bool,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub allow_aliases: bool,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_i32")]
    pub add_conference_security: i32,
    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_i32")]
    pub add_conference_time: i32,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_false")]
    pub use_main_commands: bool,

    pub users_menu: PathBuf,
    pub sysop_menu: PathBuf,
    pub news_file: PathBuf,
    pub attachment_location: PathBuf,

    /// Sort type for public upload DIR file
    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub pub_upload_sort: u8,
    pub pub_upload_dir_file: PathBuf,
    pub pub_upload_location: PathBuf,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub private_upload_sort: u8,
    pub private_upload_dir_file: PathBuf,
    pub private_upload_location: PathBuf,

    pub command_file: PathBuf,
    pub intro_file: PathBuf,
    pub doors_menu: PathBuf,
    pub doors_file: PathBuf,

    pub blt_menu: PathBuf,
    pub blt_file: PathBuf,

    pub script_menu: PathBuf,
    pub script_file: PathBuf,

    pub file_area_menu: PathBuf,
    pub file_area_file: PathBuf,

    pub message_area_menu: PathBuf,
    pub message_area_file: PathBuf,

    #[serde(skip)]
    pub commands: Vec<Command>,
}

impl Conference {}

#[derive(Default, Clone, Serialize, Deserialize)]
pub struct ConferenceBase {
    #[serde(default)]
    #[serde(skip_serializing_if = "Vec::is_empty")]
    #[serde(rename = "conference")]
    entries: Vec<Conference>,
}

impl ConferenceBase {
    pub fn len(&self) -> usize {
        self.entries.len()
    }
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn import_pcboard(
        conferences: &[PcbConferenceHeader],
        add_conferences: &[PcbAdditionalConferenceHeader],
    ) -> ConferenceBase {
        let mut confs = Vec::new();
        for (i, c) in conferences.iter().enumerate() {
            let d = &add_conferences[i];

            let new = Conference {
                name: c.name.clone(),
                is_public: c.public_conference,
                use_main_commands: true,
                commands: Vec::new(),
                password: Password::PlainText(d.password.clone()),
                required_security: c.required_security,
                sec_attachments: d.attach_level,
                sec_write_message: d.req_level_to_enter,
                auto_rejoin: c.auto_rejoin,
                view_members: c.view_members,
                private_uploads: c.private_uploads,
                private_msgs: c.private_msgs,
                allow_aliases: d.allow_aliases,
                add_conference_security: c.add_conference_security,
                add_conference_time: c.add_conference_time,
                users_menu: PathBuf::from(&c.users_menu),
                sysop_menu: PathBuf::from(&c.sysop_menu),
                news_file: PathBuf::from(&c.news_file),
                attachment_location: PathBuf::from(&d.attach_loc),
                pub_upload_sort: c.pub_upload_sort,
                pub_upload_dir_file: PathBuf::from(&c.pub_upload_dirfile),
                pub_upload_location: PathBuf::from(&c.pub_upload_location),
                private_upload_sort: c.private_upload_sort,
                private_upload_dir_file: PathBuf::from(&c.private_upload_dirfile),
                private_upload_location: PathBuf::from(&c.private_upload_location),
                command_file: PathBuf::from(&d.cmd_lst),
                intro_file: PathBuf::from(&d.intro),
                doors_menu: PathBuf::from(&c.doors_menu),
                doors_file: PathBuf::from(&c.doors_file),
                blt_menu: PathBuf::from(&c.blt_menu),
                blt_file: PathBuf::from(&c.blt_file),
                script_menu: PathBuf::from(&c.script_menu),
                script_file: PathBuf::from(&c.script_file),
                file_area_menu: PathBuf::from(&c.dir_menu),
                file_area_file: PathBuf::from(&c.dir_menu),
                message_area_menu: PathBuf::new(),
                message_area_file: PathBuf::from(&c.message_file),
            };
            confs.push(new);
        }
        Self { entries: confs }
    }

    pub fn get(&self, index: usize) -> Option<&Conference> {
        self.entries.get(index)
    }

    pub fn get_mut(&mut self, index: usize) -> Option<&mut Conference> {
        self.entries.get_mut(index)
    }
}

impl Deref for ConferenceBase {
    type Target = Vec<Conference>;

    fn deref(&self) -> &Self::Target {
        &self.entries
    }
}

impl DerefMut for ConferenceBase {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.entries
    }
}

/*
impl Index<usize> for ConferenceBase {
    type Output = Conference;
    fn index(&self, i: usize) -> &Conference {
        &self.entries[i]
    }
}

impl IndexMut<usize> for ConferenceBase {
    fn index_mut(&mut self, i: usize) -> &mut Conference {
        &mut self.entries[i]
    }
}
*/

impl IcyBoardSerializer for ConferenceBase {
    const FILE_TYPE: &'static str = "conferences";
}
