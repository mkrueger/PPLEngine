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
        self.display_string(&txt_entry.text, txt_entry.color, display_flags)
    }

    pub fn display_string(&mut self, txt: &str, color: u8, display_flags: i32) -> Res<()> {
        if display_flags & display_flags::NOTBLANK != 0 && txt.is_empty() {
            return Ok(());
        }

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

        Ok(())
    }

    fn display_line(&mut self, txt: &str) -> Res<()> {
        if !txt.is_empty() {
            match txt.chars().next().unwrap() {
                '!' => {
                    let mut script = String::new();
                    for ch in txt.chars().skip(1) {
                        if ch == '_' {
                            break;
                        }
                        script.push(ch);
                    }
                    let splitted_cmd: Vec<&str> = script.split(' ').collect();
                    if !splitted_cmd.is_empty() {
                        let ppe = splitted_cmd[0];
                        let file = self.board.lock().unwrap().resolve_file(ppe);
                        self.run_ppe(file, &splitted_cmd[1..])?;
                    }
                }
                '%' => {
                    let mut file_name = String::new();
                    for ch in txt.chars().skip(1) {
                        if ch == '_' {
                            break;
                        }
                        file_name.push(ch);
                    }
                    let file = self.board.lock().unwrap().resolve_file(&file_name);
                    self.display_file(&file)?;
                    return Ok(());
                }

                '$' => {
                    // TODO: Menu ?
                }
                _ => {
                    // display text
                    self.print(TerminalTarget::Both, txt)?;
                }
            }
        }
        Ok(())
    }

    pub fn display_file(&mut self, file_name: &str) -> Res<bool> {
        let resolved_name = self.board.lock().unwrap().resolve_file(file_name);

        let Ok(content) = fs::read(resolved_name) else {
            self.bell()?;
            self.set_color(pcb_colors::RED)?;
            self.print(
                TerminalTarget::Both,
                &format!("File not found: {}", file_name),
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
        self.session.num_lines_printed = 0;

        let mut prompt = prompt;
        if prompt.ends_with(TXT_STOPCHAR) {
            prompt.pop();
        }
        self.check_time_left();
        if display_flags & display_flags::LFBEFORE != 0 {
            self.new_line()?;
        }
        self.display_string(&prompt, color as u8, display_flags::DEFAULT)?;
        let mut output = String::new();
        loop {
            let Some((echo, ch)) = self.get_char()? else {
                continue;
            };
            if ch == '\n' || ch == '\r' {
                if display_flags & display_flags::ERASELINE != 0 {
                    self.clear_line()?;
                }
                break;
            }
            if ch == '\x08' && !output.is_empty() {
                output.pop();
                self.print(TerminalTarget::Both, "\x08 \x08")?;
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
        if display_flags & display_flags::UPCASE != 0 {
            return Ok(output.to_uppercase());
        }
        Ok(output)
    }
}
