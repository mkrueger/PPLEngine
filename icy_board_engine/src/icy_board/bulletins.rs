use std::{
    fs,
    path::{Path, PathBuf},
};

use icy_ppe::{tables::CP437_TO_UNICODE, Res};

use super::IcyBoardError;

pub struct Bullettin {
    pub file_name: PathBuf,
}
pub const MASK_BULLETINS: &str = "0123456789ADGHLNRS";

impl Bullettin {
    pub fn load_file<P: AsRef<Path>>(file_name: P) -> Res<Vec<Self>> {
        match fs::read(&file_name) {
            Ok(data) => {
                let mut result = Vec::new();
                const BLT_LEN: usize = 30;
                let mut cursor = &data[0..];
                while BLT_LEN <= cursor.len() {
                    let mut file_name = String::new();
                    for b in &cursor[0..BLT_LEN] {
                        if *b == 0 {
                            break;
                        }
                        file_name.push(CP437_TO_UNICODE[*b as usize]);
                    }
                    result.push(Bullettin {
                        file_name: PathBuf::from(file_name),
                    });
                    cursor = &cursor[BLT_LEN..];
                }
                Ok(result)
            }
            Err(err) => Err(Box::new(IcyBoardError::FileError(
                file_name.as_ref().to_path_buf(),
                err.to_string(),
            ))),
        }
    }
}
