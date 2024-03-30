use std::fs;

use icy_ppe::Res;

use crate::vm::TerminalTarget;

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

impl IcyBoardState {
    pub fn display_text(&mut self, message_number: usize, display_flags: i32) -> Res<()> {
        let txt_entry = self
            .board
            .lock()
            .unwrap()
            .display_text
            .get_display_text(message_number)?;

        if display_flags & display_flags::NOTBLANK != 0 && txt_entry.text.is_empty() {
            return Ok(());
        }

        if display_flags & display_flags::LFBEFORE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::BELL != 0 {
            self.bell()?;
        }
        if self.use_graphics() {
            self.set_color(txt_entry.color)?;
        }
        // TODO '!' start script
        // TODO '%' display file
        // TODO '$' display menu ???
        self.print(TerminalTarget::Both, &txt_entry.text)?;

        // up to 2 new lines are correct
        if display_flags & display_flags::NEWLINE != 0 {
            self.new_line()?;
        }
        if display_flags & display_flags::LFAFTER != 0 {
            self.new_line()?;
        }

        Ok(())
    }

    pub fn display_file(&mut self, file_name: &str) -> Res<()> {
        let resolved_name = self.board.lock().unwrap().resolve_file(file_name);

        let Ok(content) = fs::read(resolved_name) else {
            self.bell()?;
            self.set_color(pcb_colors::RED)?;
            self.print(
                TerminalTarget::Both,
                &format!("File not found: {}", file_name),
            )?;
            return Ok(());
        };
        let mut converted_content = Vec::new();
        for byte in content {
            converted_content.push(icy_ppe::tables::CP437_TO_UNICODE[byte as usize]);
        }
        self.write_raw(TerminalTarget::Both, &converted_content)?;

        Ok(())
    }

    pub fn input_field(
        &mut self,
        message_number: usize,
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
            txt_entry.color as i32,
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
        let mut prompt = prompt;
        if prompt.ends_with(TXT_STOPCHAR) {
            prompt.pop();
        }
        self.check_time_left();

        loop {
            if display_flags & display_flags::LFBEFORE != 0 {
                self.new_line()?;
            }
            self.set_color(color as u8)?;
            self.print(TerminalTarget::Both, &prompt)?;

            let mut output = String::new();
            loop {
                let Some(ch) = self.get_char()? else {
                    continue;
                };
                if ch == '\n' || ch == '\r' {
                    self.new_line()?;
                    break;
                }
                if ch == '\x08' && !output.is_empty() {
                    output.pop();
                    self.print(TerminalTarget::Both, "\x08 \x08")?;
                    continue;
                }

                if (output.len() as i32) < len && valid.contains(ch) {
                    output.push(ch);
                    self.print(TerminalTarget::Both, &ch.to_string())?;
                }
            }
            if !output.is_empty() {
                return Ok(output);
            }
        }
    }
}
