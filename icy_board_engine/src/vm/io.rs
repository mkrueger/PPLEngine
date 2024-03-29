use std::{
    collections::HashMap,
    fs::{self, File, OpenOptions},
    io::{BufRead, BufReader, Result, Seek, SeekFrom, Write},
    time::SystemTime,
};

use icy_ppe::Res;
use qfile::{QFilePath, QTraitSync};
use substring::Substring;

const O_RD: i32 = 0;
const O_RW: i32 = 2;
const O_WR: i32 = 1;
const O_APPEND: i32 = 4;

pub trait PCBoardIO {
    /// Open a file for append access
    /// channel - integer expression with the channel to use for the file
    /// file - file name to open
    /// am - desired access mode for the file
    /// sm - desired share mode for the file
    fn fappend(&mut self, channel: usize, file: &str, am: i32, sm: i32);

    /// Creates a new file
    /// channel - integer expression with the channel to use for the file
    /// file - file name to open
    /// am - desired access mode for the file
    /// sm - desired share mode for the file
    fn fcreate(&mut self, channel: usize, file: &str, am: i32, sm: i32);

    /// Opens a new file
    /// channel - integer expression with the channel to use for the file
    /// file - file name to open
    /// am - desired access mode for the file
    /// sm - desired share mode for the file
    /// # Errors
    fn fopen(&mut self, channel: usize, file: &str, am: i32, sm: i32) -> Res<()>;

    /// Dermine if a file error has occured on a channel since last check
    /// channel - integer expression with the channel to use for the file
    /// Returns
    /// True, if an error occured on the specified channel, False otherwise
    fn ferr(&self, channel: usize) -> bool;

    fn fput(&mut self, channel: usize, text: String);

    /// Read a line from an open file
    /// channel - integer expression with the channel to use for the file
    /// # Returns
    /// The line read or "", on error
    ///
    /// # Example
    /// INTEGER i
    /// STRING s
    /// FOPEN 1,"FILE.DAT",ORD,S DW
    /// IF (FERR(1)) THEN
    ///   PRINTLN "Error on opening..."
    ///   END
    /// ENDIF
    ///
    /// FGET 1, s
    /// WHILE (!FERR(1)) DO
    ///   INC i
    ///   PRINTLN "Line ", RIGHT(i, 3), ": ", s
    ///   FGET 1, s
    /// ENDWHILE
    /// FCLOSE 1
    fn fget(&mut self, channel: usize) -> String;

    /// channel - integer expression with the channel to use for the file
    /// #Example
    /// STRING s
    /// FAPPEND `1,"C:\PCB\MAIN\PPE.LOG",O_RW,S_DN`
    /// FPUTLN 1, `U_NAME`()
    /// FREWIND 1
    /// WHILE (!FERR(1)) DO
    /// FGET 1,s
    /// PRINTLN s
    /// ENDWHILE
    /// FCLOSE 1
    fn frewind(&mut self, channel: usize);

    fn fclose(&mut self, channel: usize);

    fn file_exists(&self, file: &str) -> bool;

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    fn delete(&mut self, file: &str) -> std::io::Result<()>;

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    fn rename(&mut self, old: &str, new: &str) -> std::io::Result<()>;

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    fn copy(&mut self, from: &str, to: &str) -> std::io::Result<()>;

    fn resolve_file(&self, file: &str) -> String {
        file.to_string()
    }

    /// .
    ///
    /// # Examples
    ///
    /// ```
    /// // Example template not implemented for trait functions
    /// ```
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    fn get_file_date(&self, file: &str) -> Result<SystemTime>;
    fn get_file_size(&self, file: &str) -> u64;
}

struct FileChannel {
    file: Option<Box<File>>,
    reader: Option<BufReader<File>>,
    _content: Vec<u8>,
    err: bool,
}

impl FileChannel {
    fn new() -> Self {
        FileChannel {
            file: None,
            reader: None,
            _content: Vec::new(),
            err: false,
        }
    }
}

pub struct DiskIO {
    path: String, // use that as root
    channels: [FileChannel; 8],
}

impl DiskIO {
    #[must_use]
    pub fn new(path: &str) -> Self {
        DiskIO {
            path: path.to_string(),
            channels: [
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
                FileChannel::new(),
            ],
        }
    }
}

impl PCBoardIO for DiskIO {
    fn resolve_file(&self, file: &str) -> String {
        let s: String = file
            .chars()
            .map(|x| match x {
                '\\' => '/',
                _ => x,
            })
            .collect();

        let s = if file.starts_with("C:\\") {
            let mut result = self.path.to_string();
            result.push('/');
            result.push_str(s.substring(2, file.len() - 2));
            result
        } else {
            s
        };

        if let Ok(mut file_path) = QFilePath::add_path(s.clone()) {
            if let Ok(file) = file_path.get_path_buf() {
                return file.to_string_lossy().to_string();
            }
        }
        s
    }

    fn fappend(&mut self, channel: usize, file: &str, _am: i32, sm: i32) {
        let _ = self.fopen(channel, file, O_APPEND, sm);
    }

    fn fcreate(&mut self, channel: usize, file: &str, _am: i32, sm: i32) {
        let _ = self.fopen(channel, file, O_WR, sm);
    }

    fn delete(&mut self, file: &str) -> std::io::Result<()> {
        let file = self.resolve_file(file);
        fs::remove_file(file)
    }

    fn rename(&mut self, old: &str, new: &str) -> std::io::Result<()> {
        let old = self.resolve_file(old);
        let new = self.resolve_file(new);
        fs::rename(old, new)
    }
    fn copy(&mut self, from: &str, to: &str) -> std::io::Result<()> {
        let old = self.resolve_file(from);
        let new = self.resolve_file(to);
        fs::copy(old, new)?;
        Ok(())
    }

    fn fopen(&mut self, channel: usize, file: &str, mode: i32, _sm: i32) -> Res<()> {
        let file = self.resolve_file(file);

        let file = match mode {
            O_RD => File::open(file),
            O_WR => File::create(file),
            O_RW => OpenOptions::new().read(true).write(true).open(file),
            O_APPEND => OpenOptions::new().append(true).open(file),
            _ => panic!("unsupported mode {mode}"),
        };
        match file {
            Ok(handle) => {
                self.channels[channel] = FileChannel {
                    file: Some(Box::new(handle)),
                    reader: None,
                    _content: Vec::new(),
                    err: false,
                };
            }
            Err(err) => {
                log::error!("error opening file: {}", err);
                self.channels[channel] = FileChannel {
                    file: None,
                    reader: None,
                    _content: Vec::new(),
                    err: true,
                };
            }
        }

        Ok(())
    }

    fn ferr(&self, channel: usize) -> bool {
        self.channels[channel].err
    }

    fn fput(&mut self, channel: usize, text: String) {
        if let Some(f) = &mut self.channels[channel].file {
            let _ = f.write(text.as_bytes());
            self.channels[channel].err = false;
        } else {
            log::error!("channel {} not found", channel);
            self.channels[channel].err = true;
        }
    }

    fn fget(&mut self, channel: usize) -> String {
        if let Some(f) = self.channels[channel].file.take() {
            self.channels[channel].reader = Some(BufReader::new(*f));
        }
        if let Some(reader) = &mut self.channels[channel].reader {
            let mut line = String::new();
            if reader.read_line(&mut line).is_err() {
                self.channels[channel].err = true;
                String::new()
            } else {
                self.channels[channel].err = false;
                line.trim_end_matches(|c| c == '\r' || c == '\n')
                    .to_string()
            }
        } else {
            log::error!("no file!");
            self.channels[channel].err = true;
            String::new()
        }
    }

    fn frewind(&mut self, channel: usize) {
        match &mut self.channels[channel].file {
            Some(f) => {
                f.seek(SeekFrom::Start(0)).expect("seek error");
                self.channels[channel].err = false;
            }
            _ => {
                self.channels[channel].err = true;
            }
        }
    }

    fn fclose(&mut self, channel: usize) {
        match &mut self.channels[channel].file {
            Some(_) => {
                self.channels[channel] = FileChannel {
                    file: None,
                    reader: None,
                    _content: Vec::new(),
                    err: false,
                };
            }
            _ => {
                self.channels[channel].err = true;
            }
        }
    }

    fn file_exists(&self, file: &str) -> bool {
        let file = self.resolve_file(file);
        fs::metadata(file).is_ok()
    }

    fn get_file_date(&self, file: &str) -> Result<SystemTime> {
        let metadata = fs::metadata(self.resolve_file(file))?;
        metadata.accessed()
    }

    fn get_file_size(&self, file: &str) -> u64 {
        if let Ok(metadata) = fs::metadata(self.resolve_file(file)) {
            metadata.len()
        } else {
            0
        }
    }
}

struct SimulatedFileChannel {
    file: Option<String>,
    filepos: i32,
    err: bool,
}

impl SimulatedFileChannel {
    fn new() -> Self {
        SimulatedFileChannel {
            file: None,
            filepos: 0,
            err: false,
        }
    }
}

pub struct MemoryIO {
    channels: [SimulatedFileChannel; 8],
    pub files: HashMap<String, String>,
}

impl MemoryIO {
    #[must_use]
    pub fn new() -> Self {
        MemoryIO {
            files: HashMap::new(),
            channels: [
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
                SimulatedFileChannel::new(),
            ],
        }
    }
}

impl Default for MemoryIO {
    fn default() -> Self {
        Self::new()
    }
}

impl PCBoardIO for MemoryIO {
    fn fappend(&mut self, channel: usize, file: &str, _am: i32, _sm: i32) {
        let f = file.to_string();
        let mut filepos = 0;

        if let Some(v) = self.files.get(&f) {
            filepos = v.len();
        } else {
            self.files.insert(f.clone(), String::new());
        }

        self.channels[channel] = SimulatedFileChannel {
            file: Some(f),
            filepos: filepos as i32,
            err: false,
        };
    }

    fn fcreate(&mut self, channel: usize, file: &str, _am: i32, _sm: i32) {
        let f = file.to_string();
        self.files.insert(f.clone(), String::new());

        self.channels[channel] = SimulatedFileChannel {
            file: Some(f),
            filepos: 0,
            err: false,
        };
    }

    fn fopen(&mut self, channel: usize, file: &str, _am: i32, _sm: i32) -> Res<()> {
        let f = file.to_string();

        if self.files.get(&f).is_none() {
            self.channels[channel] = SimulatedFileChannel {
                file: None,
                filepos: 0,
                err: true,
            };
            return Ok(());
        }

        self.channels[channel] = SimulatedFileChannel {
            file: Some(f),
            filepos: 0,
            err: false,
        };
        Ok(())
    }

    fn delete(&mut self, file: &str) -> std::io::Result<()> {
        let f = file.to_string();
        self.files.remove(&f);
        Ok(())
    }

    fn rename(&mut self, old: &str, new: &str) -> std::io::Result<()> {
        if let Some(removed) = self.files.remove(&old.to_string()) {
            self.files.insert(new.to_string(), removed);
        }
        Ok(())
    }

    fn copy(&mut self, from: &str, to: &str) -> std::io::Result<()> {
        if let Some(removed) = self.files.get(&from.to_string()) {
            self.files.insert(to.to_string(), removed.clone());
        }
        Ok(())
    }

    fn ferr(&self, channel: usize) -> bool {
        self.channels[channel].err
    }

    fn fput(&mut self, channel: usize, text: String) {
        if let Some(v) = &self.channels[channel].file {
            let content = self.files.get_mut(v).unwrap();
            content.insert_str(self.channels[channel].filepos as usize, &text);
            self.channels[channel].filepos += text.len() as i32;
        } else {
            self.channels[channel] = SimulatedFileChannel {
                file: None,
                filepos: 0,
                err: true,
            };
        }
    }

    fn fget(&mut self, channel: usize) -> String {
        if let Some(v) = &mut self.channels[channel].file {
            let f = self.files.get(v).unwrap();
            if self.channels[channel].filepos as usize >= f.len() {
                self.channels[channel].err = true;
                return String::new();
            }
            let result: String = f
                .chars()
                .skip(self.channels[channel].filepos as usize)
                .take_while(|c| *c != '\n')
                .collect();
            self.channels[channel].filepos += result.len() as i32 + 1;
            result
        } else {
            self.channels[channel].err = true;
            String::new()
        }
    }

    fn frewind(&mut self, channel: usize) {
        if self.channels[channel].file.is_some() {
            self.channels[channel].filepos = 0;
        } else {
            self.channels[channel] = SimulatedFileChannel {
                file: None,
                filepos: 0,
                err: true,
            };
        }
    }

    fn fclose(&mut self, channel: usize) {
        self.channels[channel] = SimulatedFileChannel {
            file: None,
            filepos: 0,
            err: false,
        };
    }

    fn file_exists(&self, file: &str) -> bool {
        self.files.contains_key(file)
    }

    fn get_file_date(&self, _file: &str) -> Result<SystemTime> {
        Ok(SystemTime::UNIX_EPOCH)
    }

    fn get_file_size(&self, file: &str) -> u64 {
        if let Some(v) = self.files.get(file) {
            v.len() as u64
        } else {
            0
        }
    }
}
