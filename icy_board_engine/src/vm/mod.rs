use icy_engine::ansi;
use icy_engine::ansi::constants::COLOR_OFFSETS;
use icy_engine::Buffer;
use icy_engine::BufferParser;
use icy_engine::Caret;
use icy_engine::Position;
use icy_engine::TextAttribute;
use icy_ppe::ast::BinOp;
use icy_ppe::ast::Statement;
use icy_ppe::ast::UnaryOp;
use icy_ppe::datetime::IcbDate;
use icy_ppe::executable::Executable;
use icy_ppe::executable::PPECommand;
use icy_ppe::executable::PPEExpr;
use icy_ppe::executable::PPEScript;
use icy_ppe::executable::PPEStatement;
use icy_ppe::executable::VariableTable;
use icy_ppe::executable::VariableType;
use icy_ppe::executable::VariableValue;
use icy_ppe::Res;
use std::collections::HashMap;
use std::path::PathBuf;
use std::string::String;
use thiserror::Error;

pub mod expressions;

pub mod statements;
use crate::icy_board::data::Node;
use crate::icy_board::state::IcyBoardState;
use crate::icy_board::text_messages::MOREPROMPT;
use crate::icy_board::text_messages::UNLIMITED;
use crate::icy_board::User;

use self::expressions::FUNCTION_TABLE;
pub use self::statements::*;

pub mod io;
pub use self::io::*;

pub mod errors;
mod tests;

#[derive(Error, Debug, Clone, Copy)]
pub enum VMError {
    #[error("Internal VM error")]
    InternalVMError,

    #[error("Label not found ({0})")]
    LabelNotFound(usize),

    #[error("Tried to pop from empty value stack.")]
    PushPopStackEmpty,
}

#[derive(Clone, Copy)]
pub enum TerminalTarget {
    Both,
    User,
    Sysop,
}

#[derive(Clone, Copy)]
pub enum HangupType {
    Hangup,
    Bye,
    Goodbye,
}

pub trait ExecutionContext {
    fn get_bps(&self) -> i32 {
        115_200
    }

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn write_raw(&mut self, data: &[char]) -> Res<()>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn read(&mut self) -> Res<String>;

    /// .
    ///
    /// # Errors
    /// Errors if the variable is not found.
    fn get_char(&mut self) -> Res<Option<char>>;

    fn inbytes(&mut self) -> i32;

    /// .
    /// # Errors
    /// Errors if the variable is not found.
    fn hangup(&mut self, hangup_type: HangupType) -> Res<()>;
}

pub struct StackFrame {
    pub values: HashMap<unicase::Ascii<String>, VariableValue>,
    pub cur_ptr: usize,
    pub label_table: HashMap<unicase::Ascii<String>, usize>,
}

pub fn calc_stmt_table(blk: &[Statement]) -> HashMap<unicase::Ascii<String>, usize> {
    let mut res = HashMap::new();
    for (i, stmt) in blk.iter().enumerate() {
        if let Statement::Label(label) = stmt {
            res.insert(label.get_label().clone(), i);
        }
    }
    res
}

pub struct ReturnAddress {
    ptr: usize,
    id: usize,
}

impl ReturnAddress {
    pub fn gosub(cur_ptr: usize) -> ReturnAddress {
        ReturnAddress {
            ptr: cur_ptr,
            id: 0,
        }
    }
    fn func_call(cur_ptr: usize, proc_id: usize) -> ReturnAddress {
        ReturnAddress {
            ptr: cur_ptr,
            id: proc_id,
        }
    }

    pub fn get_ptr(&self) -> usize {
        self.ptr
    }

    pub fn get_id(&self) -> usize {
        self.id
    }

    pub fn is_gosub(&self) -> bool {
        self.id == 0
    }
}

pub struct VirtualMachine<'a> {
    ctx2: &'a mut dyn ExecutionContext,
    io: &'a mut dyn PCBoardIO,
    pub file_name: PathBuf,
    pub variable_table: VariableTable,

    pub script: PPEScript,
    pub cur_ptr: usize,
    pub is_running: bool,
    pub fpclear: bool,
    pub is_sysop: bool,

    pub icy_board_data: IcyBoardState,
    pub cur_user: usize,
    pub current_user: Option<User>,
    pub pcb_node: Option<Node>,

    pub cur_tokens: Vec<String>, //  stack_frames: Vec<StackFrame>

    pub return_addresses: Vec<ReturnAddress>,
    call_local_value_stack: Vec<VariableValue>,
    write_back_stack: Vec<PPEExpr>,

    pub label_table: HashMap<usize, usize>,
    pub push_pop_stack: Vec<VariableValue>,

    parser: ansi::Parser,
    caret: Caret,
    buffer: Buffer,
}

enum PcbState {
    Default,
    GotAt,
    ReadColor1,
    ReadColor2(char),
    ReadAtSequence(String),
}

impl<'a> VirtualMachine<'a> {
    pub fn use_ansi(&self) -> bool {
        true
    }

    pub fn has_sysop(&self) -> bool {
        self.is_sysop
    }

    pub fn get_bps(&self) -> i32 {
        115_200
    }

    /// # Errors
    pub fn gotoxy(&mut self, target: TerminalTarget, x: i32, y: i32) -> Res<()> {
        if self.icy_board_data.display_text {
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
            TerminalTarget::Sysop => !self.has_sysop(),
            _ => false,
        }
    }

    fn write_char(&mut self, c: char) -> Res<()> {
        self.parser
            .print_char(&mut self.buffer, 0, &mut self.caret, c)?;
        self.ctx2.write_raw(&[c])
        //   Ok(())
    }
    fn write_string(&mut self, data: &[char]) -> Res<()> {
        for c in data {
            self.parser
                .print_char(&mut self.buffer, 0, &mut self.caret, *c)?;
        }
        self.ctx2.write_raw(data)
        //    Ok(())
    }

    /// # Errors
    pub fn write_raw(&mut self, target: TerminalTarget, data: &[char]) -> Res<()> {
        if self.no_terminal(target) {
            return Ok(());
        }
        if self.icy_board_data.display_text {
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
                            match s.as_str() {
                                "CLS" => {
                                    self.write_string(
                                        "\x1B[2J".chars().collect::<Vec<char>>().as_slice(),
                                    )?;
                                }
                                str => {
                                    if let Some(s) = self.translate_variable(str) {
                                        self.write_string(
                                            s.chars().collect::<Vec<char>>().as_slice(),
                                        )?;
                                    }
                                }
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
                if let Some(user) = &self.current_user {
                    if let Some(alias) = &user.inf.alias {
                        return Some(alias.alias.clone());
                    }
                }
                None
            }
            "AUTOMORE" => {
                self.icy_board_data.disp_options.auto_more = true;
                None
            }
            "BEEP" => Some("\x07".to_string()),
            "BICPS" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .get_cps_both()
                    .to_string(),
            ),
            "BOARDNAME" => Some(self.icy_board_data.data.board_name.to_string()),
            "BPS" => Some(self.get_bps().to_string()),

            // TODO
            "BYTECREDIT" => None,
            "BYTELIMIT" => None,
            "BYTERATIO" => None,
            "BYTESLEFT" => None,
            "CARRIER" => None,

            "CITY" => self
                .current_user
                .as_ref()
                .map(|user| user.user.city.clone()),
            "CLREOL" => {
                if self.use_ansi() {
                    Some("\x1B[K".to_string())
                } else {
                    None
                }
            }
            "CLS" => {
                if self.use_ansi() {
                    Some("\x1B[2J\x1B[H".to_string())
                } else {
                    // form feed character
                    Some("\x0C".to_string())
                }
            }
            "CONFNAME" => Some(self.icy_board_data.current_conference.name.clone()),
            "CONFNUM" => Some(self.icy_board_data.current_conference.number.to_string()),

            // TODO
            "CREDLEFT" => None,
            "CREDNOW" => None,
            "CREDSTART" => None,
            "CREDUSED" => None,
            "CURMSGNUM" => None,

            "DATAPHONE" => self
                .current_user
                .as_ref()
                .map(|user| user.user.bus_data_phone.clone()),
            "DAYBYTES" => None,
            "DELAY" => None,
            "DIRNAME" => None,
            "DIRNUM" => None,
            "DLBYTES" => None,
            "DLFILES" => None,
            "EVENT" => None,
            "EXPDATE" => {
                if self.icy_board_data.data.subscript_mode {
                    if let Some(user) = &self.current_user {
                        return Some(user.user.reg_exp_date.to_country_date());
                    }
                }
                Some(IcbDate::default().to_country_date())
            }
            "EXPDAYS" => {
                if self.icy_board_data.data.subscript_mode {
                    if let Some(user) = &self.current_user {
                        if user.user.reg_exp_date.get_year() != 0 {
                            return Some(
                                (self.icy_board_data.login_date.to_julian_date()
                                    - user.user.reg_exp_date.to_julian_date())
                                .to_string(),
                            );
                        }
                    }
                }
                let color = self
                    .icy_board_data
                    .icy_display_text
                    .get_display_color(UNLIMITED)
                    .unwrap();
                let _ = self.set_color(color);
                Some(
                    self.icy_board_data
                        .icy_display_text
                        .get_display_text(UNLIMITED)
                        .unwrap(),
                )
            }
            "FBYTES" => None,
            "FFILES" => None,
            "FILECREDIT" => None,
            "FILERATIO" => None,
            "FIRSTU" => self
                .current_user
                .as_ref()
                .map(|user| user.get_first_name().to_uppercase()),
            "FNUM" => None,
            "FREESPACE" => None,
            "HOMEPHONE" => self
                .current_user
                .as_ref()
                .map(|user| user.user.home_voice_phone.clone()),
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
                if let Some(user) = &self.current_user {
                    Some(user.inf.messages_left.to_string())
                } else {
                    Some("0".to_string())
                }
            }
            "MSGREAD" => {
                if let Some(user) = &self.current_user {
                    Some(user.inf.messages_read.to_string())
                } else {
                    Some("0".to_string())
                }
            }
            "NOCHAR" => Some(self.icy_board_data.no_char.to_string()),
            "NODE" => Some(self.icy_board_data.data.node_num.to_string()),
            "NUMBLT" => None,
            "NUMCALLS" => None,
            "NUMCONF" => Some(self.icy_board_data.data.num_conf.to_string()),
            "NUMDIR" => None,
            "NUMTIMESON" => {
                if let Some(user) = &self.current_user {
                    Some(user.user.num_times_on.to_string())
                } else {
                    Some("0".to_string())
                }
            }
            "OFFHOURS" => None,
            "OPTEXT" => Some(self.icy_board_data.op_text.to_string()),
            "PAUSE" => {
                self.icy_board_data.disp_options.auto_more = true;
                let _ = self.more_promt(MOREPROMPT);
                self.icy_board_data.disp_options.auto_more = false;
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
            "RCPS" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .get_cps_upload()
                    .to_string(),
            ),
            "RBYTES" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .uploaded_bytes
                    .to_string(),
            ),
            "RFILES" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .uploaded_files
                    .to_string(),
            ),
            "REAL" => {
                if let Some(user) = &self.current_user {
                    Some(user.user.name.clone())
                } else {
                    Some("0".to_string())
                }
            }
            "SECURITY" => None,
            "SCPS" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .get_cps_download()
                    .to_string(),
            ),
            "SBYTES" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .downloaded_bytes
                    .to_string(),
            ),
            "SFILES" => Some(
                self.icy_board_data
                    .transfer_statistics
                    .downloaded_files
                    .to_string(),
            ),
            "SYSDATE" => None,
            "SYSOPIN" => Some(self.icy_board_data.data.sysop_start.clone()),
            "SYSOPOUT" => Some(self.icy_board_data.data.sysop_stop.clone()),
            "SYSTIME" => None,
            "TIMELIMIT" => None,
            "TIMELEFT" => None,
            "TIMEUSED" => None,
            "TOTALTIME" => None,
            "UPBYTES" => {
                if let Some(user) = &self.current_user {
                    Some(user.user.ul_tot_upld_bytes.to_string())
                } else {
                    Some("0".to_string())
                }
            }
            "UPFILES" => {
                if let Some(user) = &self.current_user {
                    Some(user.user.num_uploads.to_string())
                } else {
                    Some("0".to_string())
                }
            }
            "USER" => {
                if let Some(user) = &self.current_user {
                    if self.icy_board_data.use_alias {
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
                self.icy_board_data.disp_options.disable_color = true;
                None
            }
            "XON" => {
                self.icy_board_data.disp_options.disable_color = false;
                None
            }
            "YESCHAR" => Some(self.icy_board_data.yes_char.to_string()),
            _ => {
                if s.to_ascii_uppercase().starts_with("ENV=") {
                    let key = &s[4..];
                    if let Some(value) = self.icy_board_data.get_env(key) {
                        return Some(value.clone());
                    }
                }
                None
            }
        }
    }

    /// # Errors
    pub fn read(&mut self) -> Res<String> {
        self.ctx2.read()
    }

    /// # Errors
    pub fn get_char(&mut self) -> Res<Option<char>> {
        self.ctx2.get_char()
    }

    pub fn inbytes(&mut self) -> i32 {
        self.ctx2.inbytes()
    }

    pub fn set_color(&mut self, color: u8) -> Res<()> {
        if self.icy_board_data.disp_options.disable_color {
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
        self.ctx2.hangup(hangup_type)
    }

    pub fn bell(&mut self) -> Res<()> {
        self.write_raw(TerminalTarget::Both, &['\x07'])
    }

    fn more_promt(&mut self, moreprompt: usize) -> Res<()> {
        self.print(
            TerminalTarget::Both,
            &self
                .icy_board_data
                .icy_display_text
                .get_display_text(moreprompt)?,
        )?;
        loop {
            if let Some(ch) = self.get_char()? {
                let ch = ch.to_uppercase().to_string();

                if ch == self.icy_board_data.yes_char.to_string()
                    || ch == self.icy_board_data.no_char.to_string()
                {
                    break;
                }
            }
        }

        Ok(())
    }
}

impl<'a> VirtualMachine<'a> {
    fn set_user_variables(&mut self, cur_user: &User) {
        self.variable_table
            .set_value(U_EXPERT, VariableValue::new_bool(cur_user.user.expert_mode));
        self.variable_table
            .set_value(U_FSE, VariableValue::new_bool(cur_user.user.use_fsedefault));
        self.variable_table
            .set_value(U_FSEP, VariableValue::new_bool(!cur_user.user.dont_ask_fse));
        self.variable_table
            .set_value(U_CLS, VariableValue::new_bool(cur_user.user.msg_clear));

        self.variable_table.set_value(
            U_EXPDATE,
            VariableValue::new_date(cur_user.user.reg_exp_date.to_pcboard_date()),
        );
        self.variable_table.set_value(
            U_SEC,
            VariableValue::new_int(cur_user.user.security_level as i32),
        );
        self.variable_table
            .set_value(U_PAGELEN, VariableValue::new_int(cur_user.user.page_len));
        self.variable_table.set_value(
            U_EXPSEC,
            VariableValue::new_int(cur_user.user.exp_security_level),
        );
        self.variable_table.set_value(
            U_CITY,
            VariableValue::new_string(cur_user.user.city.clone()),
        );
        self.variable_table.set_value(
            U_BDPHONE,
            VariableValue::new_string(cur_user.user.bus_data_phone.clone()),
        );
        self.variable_table.set_value(
            U_HVPHONE,
            VariableValue::new_string(cur_user.user.home_voice_phone.clone()),
        );
        self.variable_table.set_value(
            U_TRANS,
            VariableValue::new_string(cur_user.user.protocol.to_string()),
        );
        self.variable_table.set_value(
            U_CMNT1,
            VariableValue::new_string(cur_user.user.user_comment.clone()),
        );
        self.variable_table.set_value(
            U_CMNT2,
            VariableValue::new_string(cur_user.user.sysop_comment.clone()),
        );
        self.variable_table.set_value(
            U_PWD,
            VariableValue::new_string(cur_user.user.password.clone()),
        );

        self.variable_table.set_value(
            U_SCROLL,
            VariableValue::new_bool(cur_user.user.scroll_msg_body),
        );
        self.variable_table.set_value(
            U_LONGHDR,
            VariableValue::new_bool(!cur_user.user.short_header),
        );
        self.variable_table
            .set_value(U_DEF79, VariableValue::new_bool(cur_user.user.wide_editor));
        let alias = if let Some(alias) = &cur_user.inf.alias {
            alias.alias.clone()
        } else {
            String::new()
        };
        self.variable_table
            .set_value(U_ALIAS, VariableValue::new_string(alias));

        let verify = if let Some(verify) = &cur_user.inf.verify {
            verify.verify.clone()
        } else {
            String::new()
        };
        self.variable_table
            .set_value(U_VER, VariableValue::new_string(verify));

        self.variable_table
            .get_var_entry_mut(U_ADDR)
            .value
            .set_array_value(
                2,
                0,
                0,
                VariableValue::new_string(cur_user.user.city.clone()),
            );

        if let Some(address) = &cur_user.inf.address {
            self.variable_table
                .get_var_entry_mut(U_ADDR)
                .value
                .set_array_value(0, 0, 0, VariableValue::new_string(address.street1.clone()));
            self.variable_table
                .get_var_entry_mut(U_ADDR)
                .value
                .set_array_value(1, 0, 0, VariableValue::new_string(address.street2.clone()));

            self.variable_table
                .get_var_entry_mut(U_ADDR)
                .value
                .set_array_value(3, 0, 0, VariableValue::new_string(address.state.clone()));
            self.variable_table
                .get_var_entry_mut(U_ADDR)
                .value
                .set_array_value(4, 0, 0, VariableValue::new_string(address.zip.clone()));
            self.variable_table
                .get_var_entry_mut(U_ADDR)
                .value
                .set_array_value(5, 0, 0, VariableValue::new_string(address.country.clone()));
        }

        if let Some(notes) = &cur_user.inf.notes {
            for i in 0..5 {
                self.variable_table
                    .get_var_entry_mut(U_NOTES)
                    .value
                    .set_array_value(i, 0, 0, VariableValue::new_string(notes.notes[i].clone()));
            }
        }
        if let Some(pwd) = &cur_user.inf.password {
            self.variable_table.set_value(
                U_PWDEXP,
                VariableValue::new_date(pwd.expire_date.to_pcboard_date()),
            );
        }
        if self.variable_table.get_version() >= 300 {
            // PCBoard seems not to set this variable ever.
            // U_ACCOUNT
        }

        if self.variable_table.get_version() >= 340 {
            self.variable_table.set_value(
                U_SHORTDESC,
                VariableValue::new_bool(cur_user.user.short_header),
            );
            if let Some(pers_info) = &cur_user.inf.personal {
                self.variable_table.set_value(
                    U_GENDER,
                    VariableValue::new_string(pers_info.gender.clone()),
                );
                self.variable_table.set_value(
                    U_BIRTHDATE,
                    VariableValue::new_string(pers_info.birth_date.to_string()),
                );
                self.variable_table
                    .set_value(U_EMAIL, VariableValue::new_string(pers_info.email.clone()));
                self.variable_table
                    .set_value(U_WEB, VariableValue::new_string(pers_info.web.clone()));
            }
        }
    }

    pub fn put_user_variables(&self, cur_user: &mut User) {
        cur_user.user.expert_mode = self.variable_table.get_value(U_EXPERT).as_bool();
        cur_user.user.use_fsedefault = self.variable_table.get_value(U_FSE).as_bool();
        cur_user.user.dont_ask_fse = self.variable_table.get_value(U_FSEP).as_bool();
        cur_user.user.msg_clear = self.variable_table.get_value(U_CLS).as_bool();

        cur_user.user.reg_exp_date =
            IcbDate::from_pcboard(self.variable_table.get_value(U_EXPDATE).as_int());
        cur_user.user.security_level = self.variable_table.get_value(U_SEC).as_int() as u8;
        cur_user.user.page_len = self.variable_table.get_value(U_PAGELEN).as_int();
        cur_user.user.exp_security_level = self.variable_table.get_value(U_EXPSEC).as_int();

        cur_user.user.city = self.variable_table.get_value(U_CITY).as_string();
        cur_user.user.bus_data_phone = self.variable_table.get_value(U_BDPHONE).as_string();
        cur_user.user.home_voice_phone = self.variable_table.get_value(U_HVPHONE).as_string();
        cur_user.user.protocol = self
            .variable_table
            .get_value(U_TRANS)
            .as_string()
            .chars()
            .next()
            .unwrap_or('Z');
        cur_user.user.user_comment = self.variable_table.get_value(U_CMNT1).as_string();
        cur_user.user.sysop_comment = self.variable_table.get_value(U_CMNT2).as_string();
        cur_user.user.password = self.variable_table.get_value(U_PWD).as_string();

        cur_user.user.scroll_msg_body = self.variable_table.get_value(U_SCROLL).as_bool();
        cur_user.user.short_header = self.variable_table.get_value(U_LONGHDR).as_bool();
        cur_user.user.wide_editor = self.variable_table.get_value(U_DEF79).as_bool();
        if let Some(alias) = &mut cur_user.inf.alias {
            alias.alias = self.variable_table.get_value(U_ALIAS).as_string();
        }
        if let Some(verify) = &mut cur_user.inf.verify {
            verify.verify = self.variable_table.get_value(U_VER).as_string();
        }
        if let Some(address) = &mut cur_user.inf.address {
            address.street1 = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(0, 0, 0)
                .as_string();
            address.street2 = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(1, 0, 0)
                .as_string();
            address.city = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(2, 0, 0)
                .as_string();
            address.state = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(3, 0, 0)
                .as_string();
            address.zip = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(4, 0, 0)
                .as_string();
            address.country = self
                .variable_table
                .get_value(U_ADDR)
                .get_array_value(6, 0, 0)
                .as_string();
        }
        if let Some(notes) = &mut cur_user.inf.notes {
            notes.notes.clear();
            for i in 0..5 {
                let v = self
                    .variable_table
                    .get_value(U_NOTES)
                    .get_array_value(i, 0, 0)
                    .as_string();
                notes.notes.push(v);
            }
        }
        if let Some(pwd) = &mut cur_user.inf.password {
            pwd.expire_date =
                IcbDate::from_pcboard(self.variable_table.get_value(U_PWDEXP).as_int());
        }

        if self.variable_table.get_version() >= 300 {
            // PCBoard seems not to set this variable ever.
            // U_ACCOUNT
        }

        if self.variable_table.get_version() >= 340 {
            cur_user.user.short_header = self.variable_table.get_value(U_SHORTDESC).as_bool();

            if let Some(pers_info) = &mut cur_user.inf.personal {
                pers_info.gender = self.variable_table.get_value(U_GENDER).as_string();
                pers_info.birth_date =
                    IcbDate::parse(&self.variable_table.get_value(U_BIRTHDATE).as_string());
                pers_info.email = self.variable_table.get_value(U_EMAIL).as_string();
                pers_info.web = self.variable_table.get_value(U_WEB).as_string();
            }
        }
    }

    fn eval_expr(&mut self, expr: &PPEExpr) -> Result<VariableValue, VMError> {
        match expr {
            PPEExpr::Invalid => Err(VMError::InternalVMError),
            PPEExpr::Value(id) => Ok(self.variable_table.get_value(*id).clone()),
            PPEExpr::UnaryExpression(op, expr) => {
                let val = self.eval_expr(expr)?;
                match op {
                    UnaryOp::Not => Ok(val.not()),
                    UnaryOp::Minus => Ok(-val),
                    UnaryOp::Plus => Ok(val),
                }
            }
            PPEExpr::BinaryExpression(op, left, right) => {
                let left_value = self.eval_expr(left)?;
                let right_value = self.eval_expr(right)?;
                match op {
                    BinOp::Add => Ok(left_value + right_value),
                    BinOp::Sub => Ok(left_value - right_value),
                    BinOp::Mul => Ok(left_value * right_value),
                    BinOp::Div => Ok(left_value / right_value),
                    BinOp::Mod => Ok(left_value % right_value),
                    BinOp::PoW => Ok(left_value.pow(right_value)),
                    BinOp::Eq => Ok(VariableValue::new_bool(left_value == right_value)),
                    BinOp::NotEq => Ok(VariableValue::new_bool(left_value != right_value)),
                    BinOp::Or => Ok(VariableValue::new_bool(
                        left_value.as_bool() || right_value.as_bool(),
                    )),
                    BinOp::And => Ok(VariableValue::new_bool(
                        left_value.as_bool() && right_value.as_bool(),
                    )),
                    BinOp::Lower => Ok(VariableValue::new_bool(left_value < right_value)),
                    BinOp::LowerEq => Ok(VariableValue::new_bool(left_value <= right_value)),
                    BinOp::Greater => Ok(VariableValue::new_bool(left_value > right_value)),
                    BinOp::GreaterEq => Ok(VariableValue::new_bool(left_value >= right_value)),
                }
            }
            PPEExpr::Dim(id, dims) => {
                let dim_1 = self.eval_expr(&dims[0])?.as_int() as usize;
                let dim_2 = if dims.len() >= 2 {
                    self.eval_expr(&dims[1])?.as_int() as usize
                } else {
                    0
                };
                let dim_3 = if dims.len() >= 3 {
                    self.eval_expr(&dims[2])?.as_int() as usize
                } else {
                    0
                };

                Ok(self
                    .variable_table
                    .get_value(*id)
                    .get_array_value(dim_1, dim_2, dim_3))
            }
            PPEExpr::PredefinedFunctionCall(func, arguments) => Ok((FUNCTION_TABLE
                [(func.opcode as i16).unsigned_abs() as usize])(
                self, arguments
            )
            .unwrap()),

            PPEExpr::FunctionCall(func_id, arguments) => {
                let proc_offset;
                let locals;
                let parameters;
                let first;
                let return_var_id;
                unsafe {
                    let proc = &self.variable_table.get_var_entry(*func_id);
                    proc_offset = proc.value.data.function_value.start_offset as usize;
                    first = (proc.value.data.function_value.first_var_id + 1) as usize;
                    locals = proc.value.data.function_value.local_variables as usize;
                    parameters = proc.value.data.function_value.parameters as usize;
                    return_var_id = proc.value.data.function_value.return_var as usize;
                }

                self.prepare_call(locals, parameters, first, arguments, 0)?;

                self.return_addresses
                    .push(ReturnAddress::func_call(self.cur_ptr, *func_id));
                self.goto(proc_offset)?;
                self.run()?;
                self.fpclear = false;
                Ok(self.variable_table.get_value(return_var_id).clone())
            }
        }
    }

    fn run(&mut self) -> Result<(), VMError> {
        let max_ptr = self.script.statements.len();
        while !self.fpclear && self.is_running && self.cur_ptr < max_ptr {
            let p = self.cur_ptr;
            self.cur_ptr += 1;
            let c = self.script.statements[p].command.clone();
            // println!("{}: {:?}", p, &c);
            self.execute_statement(&c)?;
        }
        Ok(())
    }

    fn set_variable(&mut self, variable: &PPEExpr, value: VariableValue) -> Result<(), VMError> {
        match variable {
            PPEExpr::Value(id) => {
                self.variable_table.set_value(*id, value);
            }
            PPEExpr::Dim(id, dims) => {
                let dim_1 = self.eval_expr(&dims[0])?.as_int() as usize;
                let dim_2 = if dims.len() >= 2 {
                    self.eval_expr(&dims[1])?.as_int() as usize
                } else {
                    0
                };
                let dim_3 = if dims.len() >= 3 {
                    self.eval_expr(&dims[2])?.as_int() as usize
                } else {
                    0
                };

                self.variable_table
                    .get_var_entry_mut(*id)
                    .value
                    .set_array_value(dim_1, dim_2, dim_3, value);
            }
            _ => {
                return Err(VMError::InternalVMError);
            }
        }
        Ok(())
    }

    fn execute_statement(&mut self, stmt: &PPECommand) -> Result<(), VMError> {
        match stmt {
            PPECommand::End | PPECommand::Stop => {
                self.is_running = false;
            }

            PPECommand::EndFunc | PPECommand::EndProc | PPECommand::Return => {
                if let Some(addr) = self.return_addresses.pop() {
                    self.cur_ptr = addr.get_ptr();
                    let proc_id = addr.get_id();
                    if proc_id > 0 {
                        let locals;
                        let first;
                        let parameters;
                        let return_var_id;
                        let pass_flags;
                        unsafe {
                            let proc = &self.variable_table.get_var_entry(proc_id);
                            first = (proc.value.data.procedure_value.first_var_id + 1) as usize;
                            locals = proc.value.data.procedure_value.local_variables as usize;
                            parameters = proc.value.data.procedure_value.parameters as usize;
                            if proc.header.variable_type == VariableType::Function {
                                return_var_id = proc.value.data.function_value.return_var as usize;
                                pass_flags = 0;
                            } else {
                                return_var_id = 0;
                                pass_flags = proc.value.data.procedure_value.pass_flags;
                            }
                        }

                        // get write back values
                        let mut pass_values = Vec::new();
                        if pass_flags > 0 {
                            for i in 0..parameters {
                                if (1 << i) & pass_flags != 0 {
                                    let id = first + i;
                                    let val = self.variable_table.get_value(id).clone();
                                    // println!("push pass value {}", val);
                                    pass_values.push(val);
                                }
                            }
                        }

                        // write back locals + parameters
                        for i in (0..(locals + parameters)).rev() {
                            let id = first + i;
                            if self.variable_table.get_var_entry(id).header.flags & 0x1 == 0x0 {
                                let value = self.call_local_value_stack.pop().unwrap();
                                if id != return_var_id {
                                    self.variable_table.set_value(id, value);
                                }
                            }
                        }

                        if pass_flags > 0 {
                            for i in (0..parameters).rev() {
                                if (1 << i) & pass_flags != 0 {
                                    let val = pass_values.pop().unwrap();
                                    let argument_expr = self.write_back_stack.pop().unwrap();
                                    //println!("pop pass value {}", val);

                                    self.set_variable(&argument_expr, val)?;
                                }
                            }
                        }

                        if stmt == &PPECommand::EndFunc {
                            self.fpclear = true;
                        }
                    }
                } else {
                    self.is_running = false;
                }
            }

            PPECommand::IfNot(expr, label) => {
                let value = self.eval_expr(expr)?.as_bool();
                if !value {
                    self.goto(*label)?;
                }
            }

            PPECommand::ProcedureCall(proc_id, arguments) => {
                let proc_offset;
                let locals;
                let parameters;
                let first;
                let pass_flags;

                unsafe {
                    let proc = &self.variable_table.get_var_entry(*proc_id);
                    proc_offset = proc.value.data.procedure_value.start_offset as usize;
                    first = (proc.value.data.procedure_value.first_var_id + 1) as usize;
                    locals = proc.value.data.procedure_value.local_variables as usize;
                    parameters = proc.value.data.procedure_value.parameters as usize;
                    pass_flags = proc.value.data.procedure_value.pass_flags;
                }
                self.prepare_call(locals, parameters, first, arguments, pass_flags)?;

                self.return_addresses
                    .push(ReturnAddress::func_call(self.cur_ptr, *proc_id));
                self.goto(proc_offset)?;
            }

            PPECommand::PredefinedCall(proc, arguments) => {
                (STATEMENT_TABLE[proc.opcode as usize])(self, arguments).unwrap();
            }

            PPECommand::Goto(label) => {
                self.goto(*label)?;
            }
            PPECommand::Gosub(label) => {
                self.return_addresses
                    .push(ReturnAddress::gosub(self.cur_ptr));
                self.goto(*label)?;
            }
            PPECommand::Let(variable, expr) => {
                let val = self.eval_expr(expr)?;
                self.set_variable(variable, val)?;
            }
        }

        Ok(())
    }

    #[allow(clippy::needless_range_loop)]
    fn prepare_call(
        &mut self,
        locals: usize,
        parameters: usize,
        first: usize,
        arguments: &[PPEExpr],
        pass_flags: u16,
    ) -> Result<(), VMError> {
        // store locals + parameters
        for i in 0..(locals + parameters) {
            let id = first + i;
            if self.variable_table.get_var_entry(id).header.flags & 0x1 == 0x0 {
                let val = self.variable_table.get_value(id).clone();
                //println!("store {:02X} to {}", id, val);
                self.call_local_value_stack.push(val);
            }
        }
        for i in 0..parameters {
            let id = first + i;
            let value = self.eval_expr(&arguments[i])?;
            //  println!("set parameter {:02X} to {}", id, value);
            self.variable_table.set_value(id, value);

            if (1 << i) & pass_flags != 0 {
                self.write_back_stack.push(arguments[i].clone());
            }
        }
        for i in 0..locals {
            let id = first + parameters + i;
            let (flags, vtype) = {
                let header = &self.variable_table.get_var_entry(id).header;
                (header.flags, header.variable_type)
            };
            if (flags & 0x1) == 0x0 {
                // println!("reset local {:02X}", id);
                self.variable_table
                    .set_value(id, vtype.create_empty_value());
            }
        }

        Ok(())
    }

    fn goto(&mut self, label: usize) -> Result<(), VMError> {
        if let Some(label) = self.label_table.get(&label) {
            self.cur_ptr = *label;
            Ok(())
        } else {
            Err(VMError::LabelNotFound(label))
        }
    }
}

/// .
/// # Errors
pub fn run(
    file_name: PathBuf,
    prg: &Executable,
    ctx: &mut dyn ExecutionContext,
    io: &mut dyn PCBoardIO,
    icy_board_data: IcyBoardState,
    is_sysop: bool,
) -> Res<bool> {
    let Ok(script) = PPEScript::from_ppe_file(prg) else {
        return Ok(false);
    };

    let mut label_table = calc_labe_table(&script.statements);
    for (i, stmt) in script.statements.iter().enumerate() {
        label_table.insert(stmt.span.start * 2, i);
    }

    let buffer = Buffer::new((80, 25));
    let caret = Caret::new(Position::default());
    let mut vm = VirtualMachine {
        file_name,
        ctx2: ctx,
        return_addresses: Vec::new(),
        script,
        io,
        is_running: true,
        fpclear: false,
        cur_tokens: Vec::new(),
        icy_board_data,
        cur_user: 0,
        current_user: None,
        pcb_node: None,
        variable_table: prg.variable_table.clone(),
        cur_ptr: 0,
        label_table,
        call_local_value_stack: Vec::new(),
        write_back_stack: Vec::new(),
        push_pop_stack: Vec::new(),
        is_sysop,
        parser: ansi::Parser::default(),
        buffer,
        caret,
    };

    vm.run()?;
    Ok(true)
}

fn calc_labe_table(statements: &[PPEStatement]) -> HashMap<usize, usize> {
    let mut res = HashMap::new();
    let mut offset = 0;
    for (i, stmt) in statements.iter().enumerate() {
        res.insert(offset, i);
        offset += stmt.command.get_size();
    }
    res
}

pub const U_EXPERT: usize = 1;
pub const U_FSE: usize = 2;
pub const U_FSEP: usize = 3;
pub const U_CLS: usize = 4;
pub const U_EXPDATE: usize = 5;
pub const U_SEC: usize = 6;
pub const U_PAGELEN: usize = 7;
pub const U_EXPSEC: usize = 8;
pub const U_CITY: usize = 9;
pub const U_BDPHONE: usize = 10;
pub const U_HVPHONE: usize = 11;
pub const U_TRANS: usize = 12;
pub const U_CMNT1: usize = 13;
pub const U_CMNT2: usize = 14;
pub const U_PWD: usize = 15;
pub const U_SCROLL: usize = 16;
pub const U_LONGHDR: usize = 17;
pub const U_DEF79: usize = 18;
pub const U_ALIAS: usize = 19;
pub const U_VER: usize = 20;
pub const U_ADDR: usize = 21;
pub const U_NOTES: usize = 22;
pub const U_PWDEXP: usize = 23;
// 3.00
pub const U_ACCOUNT: usize = 24;

// 3.40
pub const U_SHORTDESC: usize = 25;
pub const U_GENDER: usize = 26;
pub const U_BIRTHDATE: usize = 27;
pub const U_EMAIL: usize = 28;
pub const U_WEB: usize = 29;
