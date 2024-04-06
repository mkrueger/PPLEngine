/*
use std::{
    ops::{Deref, DerefMut},
    path::PathBuf,
};

use icy_ppe::Res;
use serde::{Deserialize, Serialize};

use super::{is_null_8, IcyBoardSerializer, PCBoardRecordImporter};

/// A survey is a question and answer pair.
/// PCBoard calles them "Questionnairies" but we call them surveys.
#[derive(Serialize, Deserialize, Default)]
pub struct FileArea {
    pub name: String,
    pub path: PathBuf,

    pub has_new_files: bool,
    pub is_readonly: bool,
    pub is_free: bool,
    pub allow_ul_pwd: bool,
    pub req_level_to_list: u8,
    pub req_level_to_download: u8,
    pub req_level_to_upload: u8,


    #[serde(default)]
    #[serde(skip_serializing_if = "is_null_8")]
    pub required_security: u8,
}

#[derive(Serialize, Deserialize, Default)]
pub struct FileAreaList {
    #[serde(rename = "survey")]
    surveys: Vec<FileArea>,
}

impl Deref for FileAreaList {
    type Target = Vec<FileArea>;
    fn deref(&self) -> &Self::Target {
        &self.surveys
    }
}

impl DerefMut for FileAreaList {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.surveys
    }
}

impl PCBoardRecordImporter<FileArea> for FileAreaList {
    const RECORD_SIZE: usize = 60;

    fn push(&mut self, value: FileArea) {
        self.surveys.push(value);
    }

    fn load_pcboard_record(data: &[u8]) -> Res<FileArea> {
        let question_file = PathBuf::from(icy_ppe::tables::import_cp437_string(
            &data[..Self::RECORD_SIZE / 2],
            true,
        ));
        let answer_file = PathBuf::from(icy_ppe::tables::import_cp437_string(
            &data[Self::RECORD_SIZE / 2..],
            true,
        ));
        Ok(FileArea {
            question_file,
            answer_file,
            required_security: 0,
            name: todo!(),
            path: todo!(),
            has_new_files: todo!(),
            is_readonly: todo!(),
            is_free: todo!(),
            allow_ul_pwd: todo!(),
            req_level_to_list: todo!(),
            req_level_to_download: todo!(),
            req_level_to_upload: todo!(),
        })
    }
}

impl IcyBoardSerializer for FileAreaList {
    const FILE_TYPE: &'static str = "surveys";
}
*/
