use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
    time::Instant,
};

use chrono::{Datelike, Local, Timelike};
use icy_engine::ansi::constants::COLOR_OFFSETS;
use icy_engine::TextAttribute;
use icy_engine::{ansi, Buffer, BufferParser, Caret};
use icy_ppe::{datetime::IcbDate, Res};

use crate::vm::{BoardIO, HangupType, TerminalTarget};
pub mod functions;

use self::functions::display_flags;

use super::{
    data::Node,
    output::Output,
    text_messages::{MOREPROMPT, THANKSFORCALLING, UNLIMITED},
    IcyBoard,
};

#[derive(Clone)]
pub struct DisplayOptions {
    /// If true, the more prompt is automatically answered after 10 seconds.
    pub auto_more: bool,

    /// If true, the @ color codes are disabled.
    pub disable_color: bool,

    pub display_text: bool,
}

impl Default for DisplayOptions {
    fn default() -> Self {
        Self {
            auto_more: false,
            disable_color: false,
            display_text: true,
        }
    }
}

#[derive(Clone, Default)]
pub struct TransferStatistics {
    pub downloaded_files: usize,
    pub downloaded_bytes: usize,
    pub uploaded_files: usize,
    pub uploaded_bytes: usize,

    pub dl_transfer_time: usize,
    pub ul_transfer_time: usize,
}

impl TransferStatistics {
    pub fn get_cps_download(&self) -> usize {
        if self.dl_transfer_time == 0 {
            return 0;
        }
        self.downloaded_bytes / self.dl_transfer_time
    }

    pub fn get_cps_upload(&self) -> usize {
        if self.ul_transfer_time == 0 {
            return 0;
        }
        self.uploaded_bytes / self.ul_transfer_time
    }

    pub fn get_cps_both(&self) -> usize {
        let total_time = self.dl_transfer_time + self.ul_transfer_time;
        if total_time == 0 {
            return 0;
        }
        // actually correct - it's not the average, but the accumlated csp
        (self.downloaded_bytes + self.uploaded_bytes) / total_time
    }
}

#[derive(Default, Clone)]
pub struct ConferenceType {
    pub name: String,
    pub number: i32,
    pub conf_security: i32,
}

#[derive(Clone)]
pub struct Session {
    pub disp_options: DisplayOptions,
    pub current_conference: ConferenceType,
    pub login_date: Instant,

    pub cur_user: i32,
    pub cur_security: u8,

    pub is_sysop: bool,
    pub op_text: String,
    pub use_alias: bool,

    /// If true, the keyboard timer is checked.
    /// After it's elapsed logoff the user for inactivity.
    pub keyboard_timer_check: bool,
}

impl Session {
    pub fn new() -> Self {
        Self {
            disp_options: DisplayOptions::default(),
            current_conference: ConferenceType::default(),
            login_date: Instant::now(),
            cur_user: -1,
            cur_security: 0,
            is_sysop: false,
            op_text: String::new(),
            use_alias: false,
            keyboard_timer_check: false,
        }
    }
}

impl Default for Session {
    fn default() -> Self {
        Self::new()
    }
}

pub struct IcyBoardState {
    pub ctx: Arc<Mutex<dyn BoardIO>>,
    pub board: Arc<Mutex<IcyBoard>>,

    pub nodes: Vec<Node>,

    pub transfer_statistics: TransferStatistics,

    pub yes_char: char,
    pub no_char: char,

    pub session: Session,

    /// 0 = no debug, 1 - errors, 2 - errors and warnings, 3 - all
    pub debug_level: i32,
    pub env_vars: HashMap<String, String>,

    parser: ansi::Parser,
    caret: Caret,
    buffer: Buffer,
}

impl Default for IcyBoardState {
    fn default() -> Self {
        let buffer = Buffer::new((80, 25));
        let caret = Caret::default();
        let ctx = Arc::new(Mutex::new(Output::default()));
        Self {
            board: Arc::new(Mutex::new(IcyBoard::new())),
            ctx,
            nodes: Vec::new(),
            yes_char: 'Y',
            no_char: 'N',
            debug_level: 0,
            env_vars: HashMap::new(),
            session: Session::new(),
            transfer_statistics: TransferStatistics::default(),
            parser: ansi::Parser::default(),
            buffer,
            caret,
        }
    }
}

impl IcyBoardState {
    pub fn new(board: Arc<Mutex<IcyBoard>>, ctx: Arc<Mutex<dyn BoardIO>>) -> Self {
        Self {
            board,
            ctx,
            ..Default::default()
        }
    }

    /// Turns on keyboard check & resets the keyboard check timer.
    pub fn reset_keyboard_check_timer(&mut self) {
        self.session.keyboard_timer_check = true;
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

    fn use_graphics(&self) -> bool {
        !self.session.disp_options.disable_color
    }

    fn check_time_left(&self) {
        // TODO: Check time left.
    }

    pub fn reset_color(&mut self) -> Res<()> {
        self.set_color(7)
    }

    pub fn clear_screen(&mut self) -> Res<()> {
        if self.use_ansi() {
            self.print(TerminalTarget::Both, "\x1B[2J\x1B[H")
        } else {
            // form feed character
            self.print(TerminalTarget::Both, "\x0C")
        }
    }

    pub fn clear_eol(&mut self) -> Res<()> {
        if self.use_ansi() {
            self.print(TerminalTarget::Both, "\x1B[K")
        } else {
            Ok(())
        }
    }

    pub fn set_current_user(&mut self, user: i32) {
        self.session.cur_user = user;
        if user >= 0 {
            let last_conference = if let Ok(board) = self.board.lock() {
                self.session.cur_security = board.users[user as usize].user.security_level;
                board.users[user as usize].user.last_conference as i32
            } else {
                return;
            };
            self.switch_conference(last_conference);
        }
    }

    fn switch_conference(&mut self, last_conference: i32) {
        if last_conference > 0
            && last_conference < self.board.lock().unwrap().conferences.len() as i32
        {
            self.session.current_conference.number = last_conference;
            if let Ok(board) = self.board.lock() {
                self.session.current_conference.name =
                    board.conferences[last_conference as usize].name.clone();
                self.session.current_conference.conf_security =
                    board.conferences[last_conference as usize].add_conference_security;
            }
        } else {
            return;
        }
    }
}

enum PcbState {
    Default,
    GotAt,
    ReadColor1,
    ReadColor2(char),
    ReadAtSequence(String),
}

impl IcyBoardState {
    pub fn use_ansi(&self) -> bool {
        true
    }

    pub fn is_sysop(&self) -> bool {
        self.session.is_sysop
    }

    pub fn get_bps(&self) -> i32 {
        115_200
    }

    /// # Errors
    pub fn gotoxy(&mut self, target: TerminalTarget, x: i32, y: i32) -> Res<()> {
        if self.use_ansi() {
            self.write_raw(
                target,
                format!("\x1B[{};{}H", y, x)
                    .chars()
                    .collect::<Vec<char>>()
                    .as_slice(),
            )
        } else {
            Ok(())
        }
    }

    /// # Errors
    pub fn print(&mut self, target: TerminalTarget, str: &str) -> Res<()> {
        self.write_raw(target, str.chars().collect::<Vec<char>>().as_slice())
    }

    fn no_terminal(&self, terminal_target: TerminalTarget) -> bool {
        match terminal_target {
            TerminalTarget::Sysop => !self.is_sysop(),
            _ => false,
        }
    }

    fn write_char(&mut self, c: char) -> Res<()> {
        self.parser
            .print_char(&mut self.buffer, 0, &mut self.caret, c)?;
        self.ctx.lock().unwrap().write_raw(&[c])
    }

    fn write_string(&mut self, data: &[char]) -> Res<()> {
        for c in data {
            self.parser
                .print_char(&mut self.buffer, 0, &mut self.caret, *c)?;
        }
        self.ctx.lock().unwrap().write_raw(data)
    }

    /// # Errors
    pub fn write_raw(&mut self, target: TerminalTarget, data: &[char]) -> Res<()> {
        if self.no_terminal(target) {
            return Ok(());
        }
        if self.session.disp_options.display_text {
            let mut state = PcbState::Default;

            for c in data {
                if *c == '\x1A' {
                    break;
                }
                match state {
                    PcbState::Default => {
                        if *c == '@' {
                            state = PcbState::GotAt;
                        } else {
                            self.write_char(*c)?;
                        }
                    }
                    PcbState::GotAt => {
                        if *c == 'X' || *c == 'x' {
                            state = PcbState::ReadColor1;
                        } else {
                            state = PcbState::ReadAtSequence(c.to_string());
                        }
                    }
                    PcbState::ReadAtSequence(s) => {
                        if *c == '@' {
                            state = PcbState::Default;
                            if let Some(s) = self.translate_variable(&s) {
                                self.write_string(s.chars().collect::<Vec<char>>().as_slice())?;
                            }
                        } else {
                            state = PcbState::ReadAtSequence(s + &c.to_string());
                        }
                    }
                    PcbState::ReadColor1 => {
                        if c.is_ascii_hexdigit() {
                            state = PcbState::ReadColor2(*c);
                        } else {
                            self.write_char('@')?;
                            self.write_char(*c)?;
                            state = PcbState::Default;
                        }
                    }
                    PcbState::ReadColor2(ch1) => {
                        state = PcbState::Default;
                        if !c.is_ascii_hexdigit() {
                            self.write_char('@')?;
                            self.write_char(ch1)?;
                            self.write_char(*c)?;
                        } else {
                            let color =
                                (c.to_digit(16).unwrap() | (ch1.to_digit(16).unwrap() << 4)) as u8;
                            self.set_color(color)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn translate_variable(&mut self, s: &str) -> Option<String> {
        match s {
            "ALIAS" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = &self.board.lock().unwrap().users.get(cu) {
                    if let Some(alias) = &user.inf.alias {
                        return Some(alias.alias.clone());
                    }
                }
                None
            }
            "AUTOMORE" => {
                self.session.disp_options.auto_more = true;
                None
            }
            "BEEP" => Some("\x07".to_string()),
            "BICPS" => Some(self.transfer_statistics.get_cps_both().to_string()),
            "BOARDNAME" => Some(self.board.lock().unwrap().data.board_name.to_string()),
            "BPS" => Some(self.get_bps().to_string()),

            // TODO
            "BYTECREDIT" => None,
            "BYTELIMIT" => None,
            "BYTERATIO" => None,
            "BYTESLEFT" => None,
            "CARRIER" => None,

            "CITY" => {
                let cu = self.session.cur_user as usize;
                self.board
                    .lock()
                    .unwrap()
                    .users
                    .get(cu)
                    .map(|user| user.user.city.clone())
            }
            "CLREOL" => {
                let _ = self.clear_eol();
                None
            }
            "CLS" => {
                let _ = self.clear_screen();
                None
            }
            "CONFNAME" => Some(self.session.current_conference.name.clone()),
            "CONFNUM" => Some(self.session.current_conference.number.to_string()),

            // TODO
            "CREDLEFT" => None,
            "CREDNOW" => None,
            "CREDSTART" => None,
            "CREDUSED" => None,
            "CURMSGNUM" => None,

            "DATAPHONE" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.bus_data_phone.clone());
                }
                None
            }
            "DAYBYTES" => None,
            "DELAY" => None,
            "DIRNAME" => None,
            "DIRNUM" => None,
            "DLBYTES" => None,
            "DLFILES" => None,
            "EVENT" => None,
            "EXPDATE" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.reg_exp_date.to_string());
                }
                Some(IcbDate::default().to_country_date())
            }
            "EXPDAYS" => {
                if self.board.lock().unwrap().data.subscript_mode {
                    let cu = self.session.cur_user as usize;
                    if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                        if user.user.reg_exp_date.get_year() != 0 {
                            return Some(
                                0.to_string(), // TODO
                                               /*
                                               (self.session.login_date.to_julian_date()
                                                   - user.user.reg_exp_date.to_julian_date())
                                               .to_string(),*/
                            );
                        }
                    }
                }
                let entry = self
                    .board
                    .lock()
                    .unwrap()
                    .display_text
                    .get_display_text(UNLIMITED)
                    .unwrap();
                let _ = self.set_color(entry.color);
                Some(entry.text)
            }
            "FBYTES" => None,
            "FFILES" => None,
            "FILECREDIT" => None,
            "FILERATIO" => None,
            "FIRSTU" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.get_first_name().to_uppercase());
                }
                None
            }
            "FNUM" => None,
            "FREESPACE" => None,
            "HOMEPHONE" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.home_voice_phone.clone());
                }
                None
            }
            "HIGHMSGNUM" => None,
            "INAME" => None,
            "INCONF" => None,
            "KBLEFT" => None,
            "KBLIMIT" => None,
            "LASTCALLERNODE" => None,
            "LASTCALLERSYSTEM" => None,
            "LASTDATEON" => None,
            "LASTTIMEON" => None,
            "LMR" => None,
            "LOGDATE" => None,
            "LOGTIME" => None,
            "LOWMSGNUM" => None,
            "MAXBYTES" => None,
            "MAXFILES" => None,
            "MINLEFT" => Some("1000".to_string()),
            "MORE" => {
                let _ = self.more_promt(MOREPROMPT);
                None
            }
            "MSGLEFT" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.inf.messages_left.to_string());
                }
                None
            }
            "MSGREAD" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.inf.messages_read.to_string());
                }
                None
            }
            "NOCHAR" => Some(self.no_char.to_string()),
            "NODE" => Some(self.board.lock().unwrap().data.node_num.to_string()),
            "NUMBLT" => None,
            "NUMCALLS" => None,
            "NUMCONF" => Some(self.board.lock().unwrap().data.num_conf.to_string()),
            "NUMDIR" => None,
            "NUMTIMESON" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.num_times_on.to_string());
                }
                None
            }
            "OFFHOURS" => None,
            "OPTEXT" => Some(self.session.op_text.to_string()),
            "PAUSE" => {
                self.session.disp_options.auto_more = true;
                let _ = self.more_promt(MOREPROMPT);
                self.session.disp_options.auto_more = false;
                None
            }
            "POFF" => None,
            "PON" => None,
            "POS" => None,
            "PROLTR" => None,
            "PRODESC" => None,
            "PWXDATE" => None,
            "PWXDAYS" => None,
            "QOFF" => None,
            "QON" => None,
            "RATIOBYTES" => None,
            "RATIOFILES" => None,
            "RCPS" => Some(self.transfer_statistics.get_cps_upload().to_string()),
            "RBYTES" => Some(self.transfer_statistics.uploaded_bytes.to_string()),
            "RFILES" => Some(self.transfer_statistics.uploaded_files.to_string()),
            "REAL" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.name.to_string());
                }
                None
            }
            "SECURITY" => None,
            "SCPS" => Some(self.transfer_statistics.get_cps_download().to_string()),
            "SBYTES" => Some(self.transfer_statistics.downloaded_bytes.to_string()),
            "SFILES" => Some(self.transfer_statistics.downloaded_files.to_string()),
            "SYSDATE" => {
                let now = Local::now();
                let d = now.date_naive();
                Some(format!(
                    "{:02}/{:02}/{:02}",
                    d.month0() + 1,
                    d.day(),
                    d.year_ce().1 % 100
                ))
            }
            "SYSOPIN" => Some(self.board.lock().unwrap().data.sysop_start.clone()),
            "SYSOPOUT" => Some(self.board.lock().unwrap().data.sysop_stop.clone()),
            "SYSTIME" => {
                let now = Local::now();
                let t = now.time();
                Some(format!("{:02}:{:02}", t.hour(), t.minute()))
            }
            "TIMELIMIT" => None,
            "TIMELEFT" => None,
            "TIMEUSED" => None,
            "TOTALTIME" => None,
            "UPBYTES" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.ul_tot_upld_bytes.to_string());
                }
                None
            }
            "UPFILES" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    return Some(user.user.num_uploads.to_string());
                }
                None
            }
            "USER" => {
                let cu = self.session.cur_user as usize;
                if let Some(user) = self.board.lock().unwrap().users.get(cu) {
                    if self.session.use_alias {
                        if let Some(alias) = &user.inf.alias {
                            return Some(alias.alias.clone());
                        }
                    }
                    Some(user.user.name.clone())
                } else {
                    Some("0".to_string())
                }
            }
            "WAIT" => None,
            "WHO" => None,
            "XOFF" => {
                self.session.disp_options.disable_color = true;
                None
            }
            "XON" => {
                self.session.disp_options.disable_color = false;
                None
            }
            "YESCHAR" => Some(self.yes_char.to_string()),
            _ => {
                if s.to_ascii_uppercase().starts_with("ENV=") {
                    let key = &s[4..];
                    if let Some(value) = self.get_env(key) {
                        return Some(value.clone());
                    }
                }
                None
            }
        }
    }

    /// # Errors
    pub fn read(&mut self) -> Res<String> {
        self.ctx.lock().unwrap().read()
    }

    /// # Errors
    pub fn get_char(&mut self) -> Res<Option<char>> {
        self.ctx.lock().unwrap().get_char()
    }

    pub fn inbytes(&mut self) -> i32 {
        self.ctx.lock().unwrap().inbytes()
    }

    pub fn set_color(&mut self, color: u8) -> Res<()> {
        if !self.use_graphics() {
            return Ok(());
        }
        if self.caret.get_attribute().as_u8(icy_engine::IceMode::Blink) == color {
            return Ok(());
        }

        let mut color_change = "\x1B[".to_string();

        let new_color = TextAttribute::from_u8(color, icy_engine::IceMode::Blink);

        //  let was_bold = self.caret.get_attribute().get_foreground() > 7;
        let new_bold = new_color.get_foreground() > 7;

        /*/
        if was_bold != new_bold  {
            if new_bold {
                color_change += "1;";
            } else  {
                color_change += "0;";
            }

            }*/

        color_change += "0;";

        if new_bold {
            color_change += "1;";
        }

        if
        /* !self.caret.get_attribute().is_blinking() && */
        new_color.is_blinking() {
            color_change += "5;";
        }
        //if self.caret.get_attribute().get_foreground() != new_color.get_foreground() {
        color_change += format!(
            "{};",
            COLOR_OFFSETS[new_color.get_foreground() as usize % 8] + 30
        )
        .as_str();
        //}

        //        if self.caret.get_attribute().get_background() != new_color.get_background() {
        color_change += format!(
            "{};",
            COLOR_OFFSETS[new_color.get_background() as usize % 8] + 40
        )
        .as_str();
        //      }
        color_change.pop();
        color_change += "m";
        // println!("color_change: {}", &color_change[1..]);
        self.write_raw(
            TerminalTarget::Both,
            color_change.chars().collect::<Vec<char>>().as_slice(),
        )
    }

    pub fn get_caret_position(&mut self) -> (i32, i32) {
        (self.caret.get_position().x, self.caret.get_position().y)
    }

    /// # Errors
    pub fn hangup(&mut self, hangup_type: HangupType) -> Res<()> {
        if HangupType::Hangup != hangup_type {
            if HangupType::Goodbye == hangup_type {
                let logoff_script = self
                    .board
                    .lock()
                    .as_ref()
                    .unwrap()
                    .data
                    .path
                    .logoff_script
                    .clone();
                self.display_file(&logoff_script)?;
            }
            self.display_text(
                THANKSFORCALLING,
                display_flags::LFBEFORE | display_flags::NEWLINE,
            )?;
        }
        self.reset_color()?;
        self.ctx.lock().unwrap().hangup()
    }

    pub fn bell(&mut self) -> Res<()> {
        self.write_raw(TerminalTarget::Both, &['\x07'])
    }

    pub fn more_promt(&mut self, moreprompt: usize) -> Res<()> {
        let txt = self
            .board
            .lock()
            .unwrap()
            .display_text
            .get_display_text(moreprompt)?;

        self.print(TerminalTarget::Both, &txt.text)?;
        loop {
            if let Some(ch) = self.get_char()? {
                let ch = ch.to_uppercase().to_string();

                if ch == self.yes_char.to_string() || ch == self.no_char.to_string() {
                    break;
                }
            }
        }

        Ok(())
    }

    pub fn new_line(&mut self) -> Res<()> {
        self.write_raw(TerminalTarget::Both, &['\r', '\n'])
    }
}
