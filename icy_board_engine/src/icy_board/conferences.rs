use std::{
    ops::{Index, IndexMut},
    path::PathBuf,
};

use serde::{Deserialize, Serialize};


use super::{commands::Command, is_false, is_null_8, is_null_i32, user_base::Password, IcyBoardSerializer};

use super::PcbBoard;

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
    #[serde(skip_serializing_if = "is_null_i32")]
    pub pub_upload_sort: i32,
    pub pub_upload_directory: PathBuf,
    pub pub_upload_location: PathBuf,

    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_i32")]
    pub private_upload_sort: i32,
    pub private_upload_directory: PathBuf,
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

    pub fn import_pcboard(pcboard: &PcbBoard) -> ConferenceBase {
        let mut confs = Vec::new();
        for (i, c) in pcboard.conferences.iter().enumerate() {
            let d = &pcboard.add_conferences[i];

            let new = Conference {
                name: c.name.clone(),
                is_public: c.public_conference,
                use_main_commands: true,
                commands: Vec::new(),
                password: Password::PlainText(d.password.clone()),
                required_security: c.required_security as u8,
                sec_attachments: d.attach_level,
                sec_write_message: d.req_level_to_enter,
                auto_rejoin: c.auto_rejoin,
                view_members: c.view_members,
                private_uploads: c.private_uploads,
                private_msgs: c.private_msgs,
                allow_aliases: d.allow_aliases,
                add_conference_security: c.add_conference_security,
                add_conference_time: c.add_conference_time,
                users_menu: PathBuf::from(&pcboard.resolve_file(&c.users_menu)),
                sysop_menu: PathBuf::from(&pcboard.resolve_file(&c.sysop_menu)),
                news_file: PathBuf::from(&pcboard.resolve_file(&c.news_file)),
                attachment_location: PathBuf::from(&pcboard.resolve_file(&d.attach_loc)),
                pub_upload_sort: c.pub_upload_sort,
                pub_upload_directory: PathBuf::from(&pcboard.resolve_file(&c.pub_upload_directory)),
                pub_upload_location: PathBuf::from(&pcboard.resolve_file(&c.pub_upload_location)),
                private_upload_sort: c.private_upload_sort,
                private_upload_directory: PathBuf::from(
                    &pcboard.resolve_file(&c.private_upload_directory),
                ),
                private_upload_location: PathBuf::from(
                    &pcboard.resolve_file(&c.private_upload_location),
                ),
                command_file: PathBuf::from(&pcboard.resolve_file(&d.cmd_lst)),
                intro_file: PathBuf::from(&pcboard.resolve_file(&d.intro)),
                doors_menu: PathBuf::from(&pcboard.resolve_file(&c.doors_menu)),
                doors_file: PathBuf::from(&pcboard.resolve_file(&c.doors_file)),
                blt_menu: PathBuf::from(&pcboard.resolve_file(&c.blt_menu)),
                blt_file: PathBuf::from(&pcboard.resolve_file(&c.blt_file)),
                script_menu: PathBuf::from(&pcboard.resolve_file(&c.script_menu)),
                script_file: PathBuf::from(&pcboard.resolve_file(&c.script_file)),
                file_area_menu: PathBuf::from(&pcboard.resolve_file(&c.dir_menu)),
                file_area_file: PathBuf::from(&pcboard.resolve_file(&c.dir_menu)),
                message_area_menu: PathBuf::new(),
                message_area_file: PathBuf::from(&pcboard.resolve_file(&c.message_file)),
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


impl IcyBoardSerializer for ConferenceBase {
    const FILE_TYPE: &'static str = "conferences";

}