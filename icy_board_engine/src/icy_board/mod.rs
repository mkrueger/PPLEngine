use std::{
    collections::HashMap,
    fs,
    io::Write,
    path::{Path, PathBuf},
};

use icy_ppe::Res;
use qfile::{QFilePath, QTraitSync};
use relative_path::RelativePath;
use thiserror::Error;

use crate::vm::errors::IcyError;

use self::{
    commands::CommandList, conferences::ConferenceBase, icb_config::IcbConfig,
    icb_text::IcbTextFile, language::SupportedLanguages, sec_levels::SecurityLevelDefinitions,
    statistics::Statistics, user_base::UserBase, xfer_protocols::SupportedProtocols,
};

pub mod bulletins;
pub mod commands;
pub mod conferences;
pub mod icb_config;
pub mod icb_text;
pub mod language;
pub mod menu;
pub mod output;
pub mod pcb;
pub mod sec_levels;
pub mod state;
pub mod statistics;
pub mod user_base;
pub mod xfer_protocols;

pub use pcb::*;

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

    #[error("Can't read file {0} ({1})")]
    FileError(PathBuf, String),

    #[error("Can't write file {0} ({1})")]
    ErrorCreatingFile(String, String),

    #[error("Loading file {0} invalid record size ({1}:{2})")]
    InvalidRecordSize(String, usize, usize),

    #[error("Importing file {0} parsing record error ({1})")]
    ImportRecordErorr(String, String),
}

pub struct PcbBoardLayer {}

impl PcbBoardLayer {
    pub(crate) fn get_sl_path(&self) -> String {
        todo!()
    }

    pub(crate) fn get_pcbdat(&self) -> String {
        todo!()
    }
}

pub struct IcyBoard {
    pub file_name: PathBuf,
    pub users: UserBase,
    pub config: IcbConfig,
    pub conferences: ConferenceBase,
    pub display_text: IcbTextFile,

    // TODO: proper board statistics.
    pub num_callers: usize,

    pub pcb: PcbBoardLayer,
    pub languages: SupportedLanguages,
    pub protocols: SupportedProtocols,
    pub sec_levels: SecurityLevelDefinitions,
    pub statistics: Statistics,
    pub commands: CommandList,
    paths: HashMap<String, String>,
}

impl IcyBoard {
    pub fn new() -> Self {
        let display_text = IcbTextFile::default();

        IcyBoard {
            display_text,
            file_name: PathBuf::new(),
            users: UserBase::default(),
            config: IcbConfig::new(),
            conferences: ConferenceBase::default(),
            pcb: PcbBoardLayer {},
            num_callers: 0,
            paths: HashMap::new(),
            languages: SupportedLanguages::default(),
            protocols: SupportedProtocols::default(),
            sec_levels: SecurityLevelDefinitions::default(),
            commands: CommandList::default(),
            statistics: Statistics::default(),
        }
    }

    pub fn resolve_file<P: AsRef<Path>>(&self, file: &P) -> String {
        let mut s: String = file
            .as_ref()
            .to_string_lossy()
            .to_string()
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
                if !file.exists() {
                    log::warn!("File not found: {}", file.to_string_lossy());
                }
                return file.to_string_lossy().to_string();
            }
        }
        s
    }

    pub fn load<P: AsRef<Path>>(path: &P) -> Res<Self> {
        let config = IcbConfig::load(path)?;

        let parent_path = path.as_ref().parent().unwrap();
        let users = UserBase::load(
            &RelativePath::from_path(&config.paths.user_base)
                .unwrap()
                .to_path(parent_path),
        )?;
        let conferences = ConferenceBase::load(
            &RelativePath::from_path(&config.paths.conferences)
                .unwrap()
                .to_path(parent_path),
        )?;
        let display_text = IcbTextFile::load(
            &RelativePath::from_path(&config.paths.icbtext)
                .unwrap()
                .to_path(parent_path),
        )?;
        let languages = SupportedLanguages::load(
            &RelativePath::from_path(&config.paths.language_file)
                .unwrap()
                .to_path(parent_path),
        )?;

        let protocols = SupportedProtocols::load(
            &RelativePath::from_path(&config.paths.protocol_data_file)
                .unwrap()
                .to_path(parent_path),
        )?;

        let sec_levels = SecurityLevelDefinitions::load(
            &RelativePath::from_path(&config.paths.security_level_file)
                .unwrap()
                .to_path(parent_path),
        )?;

        let commands = CommandList::load(
            &RelativePath::from_path(&config.paths.command_file)
                .unwrap()
                .to_path(parent_path),
        )?;

        let statistics = Statistics::load(
            &RelativePath::from_path(&config.paths.statistics_file)
                .unwrap()
                .to_path(parent_path),
        )?;

        Ok(IcyBoard {
            file_name: path.as_ref().to_path_buf(),
            num_callers: 0,
            users,
            config,
            conferences,
            display_text,
            languages,
            protocols,
            sec_levels,
            commands,
            statistics,
            paths: HashMap::new(),
            pcb: PcbBoardLayer {},
        })
    }
}

impl Default for IcyBoard {
    fn default() -> Self {
        Self::new()
    }
}

pub fn is_false(b: impl std::borrow::Borrow<bool>) -> bool {
    !b.borrow()
}

pub fn is_true(b: impl std::borrow::Borrow<bool>) -> bool {
    *b.borrow()
}

pub fn path_is_empty(b: impl std::borrow::Borrow<PathBuf>) -> bool {
    (*b.borrow()).as_os_str().is_empty()
}

pub fn set_true() -> bool {
    true
}

pub fn is_null_8(b: impl std::borrow::Borrow<u8>) -> bool {
    *b.borrow() == 0
}

pub fn is_null_64(b: impl std::borrow::Borrow<u64>) -> bool {
    *b.borrow() == 0
}
pub fn is_null_32(b: impl std::borrow::Borrow<u32>) -> bool {
    *b.borrow() == 0
}

pub fn is_null_16(b: impl std::borrow::Borrow<u16>) -> bool {
    *b.borrow() == 0
}

pub fn is_null_i32(b: impl std::borrow::Borrow<i32>) -> bool {
    *b.borrow() == 0
}

const UTF8_BOM: [u8; 3] = [0xEF, 0xBB, 0xBF];

pub fn read_cp437<P: AsRef<Path>>(path: &P) -> Res<String> {
    match fs::read(path) {
        Ok(data) => {
            let import = if data.starts_with(&UTF8_BOM) {
                String::from_utf8(data)?
            } else {
                icy_ppe::tables::import_cp437_string(&data, false)
            };
            Ok(import)
        }
        Err(e) => Err(IcyBoardError::FileError(path.as_ref().to_path_buf(), e.to_string()).into()),
    }
}

pub fn write_with_bom<P: AsRef<Path>>(path: &P, buf: &str) -> Res<()> {
    match fs::File::create(path) {
        Ok(mut file) => {
            file.write_all(&UTF8_BOM)?;
            file.write_all(buf.as_bytes())?;
        }
        Err(e) => {
            return Err(IcyBoardError::ErrorCreatingFile(
                path.as_ref().to_string_lossy().to_string(),
                e.to_string(),
            )
            .into())
        }
    }
    Ok(())
}

pub fn convert_to_utf8<P: AsRef<Path>, Q: AsRef<Path>>(from: &P, to: &Q) -> Res<()> {
    let import = read_cp437(from)?;
    write_with_bom(to, &import)?;
    Ok(())
}

pub trait IcyBoardSerializer: serde::de::DeserializeOwned + serde::ser::Serialize {
    const FILE_TYPE: &'static str;

    fn load<P: AsRef<Path>>(path: &P) -> Res<Self> {
        match fs::read_to_string(path) {
            Ok(txt) => match toml::from_str(&txt) {
                Ok(result) => Ok(result),
                Err(e) => {
                    log::error!(
                        "Loading {} toml file '{}': {}",
                        Self::FILE_TYPE,
                        path.as_ref().display(),
                        e
                    );
                    Err(IcyError::ErrorParsingConfig(
                        path.as_ref().to_string_lossy().to_string(),
                        e.to_string(),
                    )
                    .into())
                }
            },
            Err(e) => {
                if e.kind() == std::io::ErrorKind::NotFound {
                    Err(IcyError::FileNotFound(path.as_ref().to_string_lossy().to_string()).into())
                } else {
                    log::error!(
                        "Loading {} file '{}': {}",
                        Self::FILE_TYPE,
                        path.as_ref().display(),
                        e
                    );
                    Err(IcyError::ErrorLoadingFile(
                        path.as_ref().to_string_lossy().to_string(),
                        e.to_string(),
                    )
                    .into())
                }
            }
        }
    }

    fn save<P: AsRef<Path>>(&self, path: &P) -> Res<()> {
        match toml::to_string(self) {
            Ok(txt) => match fs::write(path, txt) {
                Ok(_) => Ok(()),
                Err(e) => {
                    log::error!(
                        "Error writing {} file '{}': {}",
                        Self::FILE_TYPE,
                        path.as_ref().display(),
                        e
                    );
                    Err(IcyError::ErrorGeneratingToml(
                        path.as_ref().to_string_lossy().to_string(),
                        e.to_string(),
                    )
                    .into())
                }
            },
            Err(e) => {
                log::error!(
                    "Error generating {} toml file '{}': {}",
                    Self::FILE_TYPE,
                    path.as_ref().display(),
                    e
                );
                Err(IcyError::ErrorGeneratingToml(
                    path.as_ref().to_string_lossy().to_string(),
                    e.to_string(),
                )
                .into())
            }
        }
    }
}

pub trait PCBoardImport: Sized + Default + IcyBoardSerializer {
    fn import_pcboard<P: AsRef<Path>>(path: &P) -> Res<Self>;
}

pub trait PCBoardRecordImporter<T>: Sized + Default {
    const RECORD_SIZE: usize;

    fn push(&mut self, value: T);

    fn load_pcboard_record(record: &[u8]) -> Res<T>;

    fn import_pcboard<P: AsRef<Path>>(path: &P) -> Res<Self> {
        let mut res = Self::default();
        match &std::fs::read(path) {
            Ok(data) => {
                let mut data = &data[..];
                while !data.is_empty() {
                    if data.len() < Self::RECORD_SIZE {
                        log::error!(
                            "Importing file '{}' from pcboard binary file ended prematurely",
                            path.as_ref().display(),
                        );
                        return Err(IcyBoardError::InvalidRecordSize(
                            path.as_ref().display().to_string(),
                            Self::RECORD_SIZE,
                            data.len(),
                        )
                        .into());
                    }
                    match Self::load_pcboard_record(&data[..Self::RECORD_SIZE]) {
                        Ok(value) => {
                            res.push(value);
                        }
                        Err(e) => {
                            return Err(IcyBoardError::ImportRecordErorr(
                                path.as_ref().display().to_string(),
                                e.to_string(),
                            )
                            .into());
                        }
                    }

                    data = &data[Self::RECORD_SIZE..];
                }
                Ok(res)
            }
            Err(err) => {
                log::error!(
                    "Importing file '{}' from pcboard binary file: {}",
                    path.as_ref().display(),
                    err
                );
                Err(IcyError::ErrorLoadingFile(
                    path.as_ref().to_string_lossy().to_string(),
                    err.to_string(),
                )
                .into())
            }
        }
    }
}

pub trait PCBoardBinImporter: Sized + Default {
    const SIZE: usize;

    fn import_data(data: &[u8]) -> Res<Self>;

    fn import_pcboard<P: AsRef<Path>>(path: &P) -> Res<Self> {
        match &std::fs::read(path) {
            Ok(data) => {
                if data.len() < Self::SIZE {
                    log::error!(
                        "Importing file '{}' from pcboard binary file ended prematurely",
                        path.as_ref().display(),
                    );
                    return Err(IcyBoardError::InvalidRecordSize(
                        path.as_ref().display().to_string(),
                        Self::SIZE,
                        data.len(),
                    )
                    .into());
                }
                Self::import_data(data)
            }
            Err(err) => {
                log::error!(
                    "Importing file '{}' from pcboard binary file: {}",
                    path.as_ref().display(),
                    err
                );
                Err(IcyError::ErrorLoadingFile(
                    path.as_ref().to_string_lossy().to_string(),
                    err.to_string(),
                )
                .into())
            }
        }
    }
}

pub trait PCBoardTextImport: PCBoardImport {
    fn import_data(data: String) -> Res<Self>;

    fn import_pcboard<P: AsRef<Path>>(path: &P) -> Res<Self> {
        match read_cp437(path) {
            Ok(data) => Self::import_data(data),
            Err(err) => {
                log::error!(
                    "Importing file '{}' from pcboard binary file: {}",
                    path.as_ref().display(),
                    err
                );
                Err(IcyError::ErrorLoadingFile(
                    path.as_ref().to_string_lossy().to_string(),
                    err.to_string(),
                )
                .into())
            }
        }
    }
}
