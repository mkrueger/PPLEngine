use std::{
    fs,
    path::{Path, PathBuf},
};

use icy_engine::IceMode;
use icy_ppe::Res;

use crate::{
    icy_board::icb_text::{IcbTextStyle, IceText},
    vm::TerminalTarget,
};

use super::IcyBoardState;

pub mod display_flags {
    pub const DEFAULT: i32 = 0b0000_0000_0000_0000;
    pub const ECHODOTS: i32 = 0b0000_0000_0000_0001;
    pub const FIELDLEN: i32 = 0b0000_0000_0000_0010;
    pub const UPCASE: i32 = 0b0000_0000_0000_0100;
    pub const STACKED: i32 = 0b0000_0000_0000_1000;
    pub const ERASELINE: i32 = 0b0000_0000_0001_0000;
    pub const NEWLINE: i32 = 0b0000_0000_0010_0000;
    pub const LFBEFORE: i32 = 0b0000_0000_0100_0000;
    pub const LFAFTER: i32 = 0b0000_0000_1000_0000;
    pub const LOGIT: i32 = 0b0000_0001_0000_0000;
    pub const LOGITLEFT: i32 = 0b0000_0010_0000_0000;
    pub const GUIDE: i32 = 0b0000_0100_0000_0000;
    pub const WORDWRAP: i32 = 0b0000_1000_0000_0000;
    pub const YESNO: i32 = 0b0000_1000_0000_0000; // same as 'WORDWRAP'
    pub const NOCLEAR: i32 = 0b0001_0000_0000_0000;
    pub const BELL: i32 = 0b0010_0000_0000_0000;
    pub const HIGHASCII: i32 = 0b0100_0000_0000_0000;
    pub const AUTO: i32 = 0b1000_0000_0000_0000;
    pub const NOTBLANK: i32 = 0b1000_0000_0000_0000; // same as 'AUTO'
}

pub mod pcb_colors {
    pub const BLUE: u8 = 9;
    pub const GREEN: u8 = 10;
    pub const CYAN: u8 = 11;
    pub const RED: u8 = 12;
    pub const MAGENTA: u8 = 13;
    pub const YELLOW: u8 = 14;
    pub const WHITE: u8 = 15;
}
const TXT_STOPCHAR: char = '_';

pub enum PPECallType {
    PPE,
    Menu,
    File,
}
pub struct PPECall {
    pub call_type: PPECallType,
    pub file: String,
    pub arguments: Vec<String>,
}

impl PPECall {
    pub fn try_parse_line(line: &str) -> Option<PPECall> {
        if line.is_empty() {
            return None;
        }
        let mut iter = line.chars();
        let first_ch = iter.next().unwrap();

        if first_ch == '!' || first_ch == '%' || first_ch == '$' {
            let call_type = match first_ch {
                '!' => PPECallType::PPE,
                '%' => PPECallType::File,
                '$' => PPECallType::Menu,
                _ => unreachable!(),
            };
            let mut arguments = Vec::new();
            let mut arg = String::new();

            for ch in iter {
                if ch == ' ' || ch == '_' {
                    if !arg.is_empty() {
                        arguments.push(arg);
                        arg = String::new();
                    }
                    if ch == '_' {
                        break;
                    }
                    continue;
                }
                arg.push(ch);
            }
            Some(Self {
                call_type,
                file: arguments[0].clone(),
                arguments: arguments[1..].to_vec(),
            })
        } else {
            None
        }
    }
}

impl IcyBoardState {
    pub fn display_text(&mut self, message_number: IceText, display_flags: i32) -> Res<()> {
        let txt_entry = self
            .board
            .lock()
            .unwrap()
            .display_text
            .get_display_text(message_number)?;
        let color = if txt_entry.style == IcbTextStyle::Plain {
            self.caret.get_attribute().as_u8(IceMode::Blink)
        } else {
            txt_entry.style.to_color()
        };
        self.display_string(&txt_entry.text, color, display_flags)
    }

    pub fn display_string(&mut self, txt: &str, color: u8, display_flags: i32) -> Res<()> {
        if display_flags & display_flags::NOTBLANK != 0 && txt.is_empty() {
            return Ok(());
        }

        if display_flags & display_flags::LOGIT != 0 {
            log::info!("{}", txt);
        }

        let old_color = self.caret.get_attribute().as_u8(icy_engine::IceMode::Blink);
        if display_flags & display_flags::LFBEFORE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::BELL != 0 {
            self.bell()?;
        }
        if self.use_graphics() {
            self.set_color(color)?;
        }

        self.display_line(txt)?;

        // up to 2 new lines are correct
        if display_flags & display_flags::NEWLINE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::LFAFTER != 0 {
            self.new_line()?;
        }
        if self.use_graphics() {
            self.set_color(old_color)?;
        }
        Ok(())
    }

    fn display_line(&mut self, txt: &str) -> Res<()> {
        if !txt.is_empty() {
            if let Some(call) = PPECall::try_parse_line(txt) {
                for sc in call.arguments {
                    self.session.tokens.push_back(sc.to_string());
                }
                match call.call_type {
                    PPECallType::PPE => {
                        let file = self.board.lock().unwrap().resolve_file(&call.file);
                        self.run_ppe(&file)?;
                    }
                    PPECallType::Menu => {
                        self.display_menu(&call.file)?;
                    }
                    PPECallType::File => {
                        let file = self.board.lock().unwrap().resolve_file(&call.file);
                        self.display_file(&file)?;
                    }
                }
                return Ok(());
            } else {
                // display text
                self.print(TerminalTarget::Both, txt)?;
            }
        }
        Ok(())
    }

    pub fn display_menu<P: AsRef<Path>>(&mut self, file_name: &P) -> Res<bool> {
        let resolved_name_ppe = self
            .board
            .lock()
            .unwrap()
            .resolve_file(&(file_name.as_ref().with_extension("PPE")));
        let path = PathBuf::from(resolved_name_ppe);
        if path.exists() {
            self.run_ppe(&path)?;
            return Ok(true);
        }
        self.display_file(&file_name)
    }

    pub fn display_file<P: AsRef<Path>>(&mut self, file_name: &P) -> Res<bool> {
        let resolved_name = self.board.lock().unwrap().resolve_file(file_name);

        let Ok(content) = fs::read(resolved_name) else {
            self.bell()?;
            self.set_color(pcb_colors::RED)?;
            self.print(
                TerminalTarget::Both,
                &format!("\r\n({}) is missing!\r\n\r\n", file_name.as_ref().display()),
            )?;
            return Ok(true);
        };
        let mut converted_content = String::new();
        for byte in content {
            converted_content.push(icy_ppe::tables::CP437_TO_UNICODE[byte as usize]);
        }
        self.session.disp_options.non_stop = true;
        for line in converted_content.lines() {
            self.display_line(line)?;
            self.write_raw(TerminalTarget::Both, &['\r', '\n'])?;
            self.session.disp_options.non_stop = false;
            let next = self.next_line()?;
            self.session.disp_options.non_stop = true;
            if !next {
                return Ok(false);
            }
        }
        self.session.disp_options.non_stop = false;
        Ok(true)
    }

    pub fn input_field(
        &mut self,
        message_number: IceText,
        len: i32,
        valid: &str,
        display_flags: i32,
    ) -> Res<String> {
        let txt_entry = self
            .board
            .lock()
            .unwrap()
            .display_text
            .get_display_text(message_number)?;

        self.input_string(
            txt_entry.style as i32,
            txt_entry.text,
            len,
            valid,
            display_flags,
        )
    }

    pub fn input_string(
        &mut self,
        color: i32,
        prompt: String,
        len: i32,
        valid: &str,
        display_flags: i32,
    ) -> Res<String> {
        self.session.num_lines_printed = 0;

        let mut prompt = prompt;
        let display_question = if prompt.ends_with(TXT_STOPCHAR) {
            prompt.pop();
            false
        } else {
            true
        };
        self.check_time_left();
        if display_flags & display_flags::LFBEFORE != 0 {
            self.new_line()?;
        }

        let old_color = self.caret.get_attribute().as_u8(icy_engine::IceMode::Blink);
        if display_flags & display_flags::LFBEFORE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::BELL != 0 {
            self.bell()?;
        }
        if self.use_graphics() {
            self.set_color(color as u8)?;
        }

        self.display_line(&prompt)?;

        if display_question {
            self.print(TerminalTarget::Both, "? ")?;
        }
        if display_flags & display_flags::FIELDLEN != 0 {
            self.print(TerminalTarget::Both, " (")?;
            self.forward(len)?;
            self.print(TerminalTarget::Both, ")")?;
            self.backward(len + 1)?;
        }
        if self.use_graphics() {
            self.set_color(old_color)?;
        }

        let mut output = String::new();
        loop {
            let Some((echo, mut ch)) = self.get_char()? else {
                continue;
            };
            if display_flags & display_flags::UPCASE != 0 {
                ch = ch.to_ascii_uppercase();
            }

            if ch == '\n' || ch == '\r' {
                if display_flags & display_flags::ERASELINE != 0 {
                    self.clear_line()?;
                }
                break;
            }
            if ch == '\x08' && !output.is_empty() {
                output.pop();
                if echo {
                    self.print(TerminalTarget::Both, "\x08 \x08")?;
                }
                continue;
            }

            if (output.len() as i32) < len && valid.contains(ch) {
                output.push(ch);
                if echo {
                    self.print(TerminalTarget::Both, &ch.to_string())?;
                }
            }
        }
        if display_flags & display_flags::NEWLINE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::LFAFTER != 0 {
            self.new_line()?;
        }
        self.session.num_lines_printed = 0;
        Ok(output)
    }
}
