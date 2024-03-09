use std::{
    collections::VecDeque,
    io::{stdout, Write},
    time::Duration,
};

use crossterm::{
    cursor::{position, DisableBlinking, EnableBlinking, MoveTo},
    event::{poll, read, Event, KeyCode, KeyModifiers},
    style::{Color, SetBackgroundColor, SetForegroundColor},
    terminal::{disable_raw_mode, enable_raw_mode, Clear},
    ExecutableCommand,
};
use icy_ppe::{
    interpreter::{ExecutionContext, TerminalTarget},
    tables::CP437_TO_UNICODE,
    Res,
};

#[derive(Default)]
pub struct Output {
    char_buffer: VecDeque<char>,
    pub is_sysop: bool,
}

fn get_color(n: usize) -> Color {
    match n {
        0 => Color::Black,
        1 => Color::DarkRed,
        2 => Color::DarkGreen,
        3 => Color::DarkYellow,
        4 => Color::DarkBlue,
        5 => Color::DarkMagenta,
        6 => Color::DarkCyan,
        7 => Color::Grey,
        8 => Color::DarkGrey,
        9 => Color::Red,
        10 => Color::Green,
        11 => Color::Yellow,
        12 => Color::Blue,
        13 => Color::Magenta,
        14 => Color::Cyan,
        15 => Color::White,
        _ => Color::Black,
    }
}

enum PcbState {
    Default,
    GotAt,
    ReadColor1,
    ReadColor2(u8),
    ReadAtSequence(String),
}

impl Output {
    fn fill_buffer(&mut self) -> Res<()> {
        while let Ok(true) = poll(Duration::from_millis(50)) {
            let r = read()?;
            if let Event::Key(key_evt) = r {
                match key_evt.code {
                    KeyCode::Char(c) => {
                        if (c == 'x' || c == 'c')
                            && key_evt.modifiers.contains(KeyModifiers::CONTROL)
                        {
                            let _ = disable_raw_mode();
                            panic!("Ctrl-X or Ctrl-C pressed");
                        }
                        self.char_buffer.push_back(c)
                    }
                    KeyCode::Enter => self.char_buffer.push_back('\r'),
                    KeyCode::Backspace => self.char_buffer.push_back('\x08'),
                    KeyCode::Esc => self.char_buffer.push_back('\x1B'),
                    KeyCode::Tab => self.char_buffer.push_back('\x09'),
                    KeyCode::Delete => self.char_buffer.push_back('\x7F'),

                    KeyCode::Insert => self.char_buffer.extend("\x1B[2~".chars()),
                    KeyCode::Home => self.char_buffer.extend("\x1B[H".chars()),
                    KeyCode::End => self.char_buffer.extend("\x1B[F".chars()),
                    KeyCode::Up => self.char_buffer.extend("\x1B[A".chars()),
                    KeyCode::Down => self.char_buffer.extend("\x1B[B".chars()),
                    KeyCode::Right => self.char_buffer.extend("\x1B[C".chars()),
                    KeyCode::Left => self.char_buffer.extend("\x1B[D".chars()),
                    KeyCode::PageUp => self.char_buffer.extend("\x1B[5~".chars()),
                    KeyCode::PageDown => self.char_buffer.extend("\x1B[6~".chars()),

                    _ => {}
                }
            }
        }
        Ok(())
    }

    fn no_terminal(&self, terminal_target: TerminalTarget) -> bool {
        match terminal_target {
            TerminalTarget::Sysop => !self.has_sysop(),
            _ => false,
        }
    }
}

impl ExecutionContext for Output {
    fn has_sysop(&self) -> bool {
        self.is_sysop
    }

    fn gotoxy(&mut self, terminal_target: TerminalTarget, x: i32, y: i32) -> Res<()> {
        if self.no_terminal(terminal_target) {
            return Ok(());
        }
        stdout().execute(MoveTo(x as u16, y as u16))?;
        Ok(())
    }

    fn print(&mut self, terminal_target: TerminalTarget, str: &str) -> Res<()> {
        if self.no_terminal(terminal_target) {
            return Ok(());
        }
        let mut state = PcbState::Default;
        for c in str.chars() {
            if c == '\x1A' {
                break;
            }
            match state {
                PcbState::Default => {
                    if c == '@' {
                        state = PcbState::GotAt;
                    } else if c == CP437_TO_UNICODE[b'\x1B' as usize] {
                        disable_raw_mode().unwrap();
                        print!("\x1B");
                        enable_raw_mode().unwrap();
                    } else if c == '\n' || c == '\r' || c == '\x08' || c == '\x1B' {
                        disable_raw_mode().unwrap();
                        print!("{}", c);
                        enable_raw_mode().unwrap();
                    } else {
                        print!("{}", c);
                    }
                }
                PcbState::GotAt => {
                    if c == 'X' {
                        state = PcbState::ReadColor1;
                    } else {
                        state = PcbState::ReadAtSequence(c.to_string());
                    }
                }
                PcbState::ReadAtSequence(s) => {
                    if c == '@' {
                        state = PcbState::Default;
                        match s.as_str() {
                            "CLS" => {
                                stdout()
                                    .execute(Clear(crossterm::terminal::ClearType::All))
                                    .unwrap()
                                    .execute(MoveTo(0, 0))
                                    .unwrap();
                            }
                            _str => {
                                // println!("Unknown pcb sequence: {}", str);
                            }
                        }
                    } else {
                        state = PcbState::ReadAtSequence(s + &(c as char).to_string());
                    }
                }
                PcbState::ReadColor1 => {
                    if c.is_ascii_hexdigit() {
                        state = PcbState::ReadColor2(c as u8);
                    } else {
                        print!("@{}", CP437_TO_UNICODE[c as usize]);
                        state = PcbState::Default;
                    }
                }
                PcbState::ReadColor2(ch1) => {
                    state = PcbState::Default;
                    if !c.is_ascii_hexdigit() {
                        print!(
                            "@{}{}",
                            CP437_TO_UNICODE[ch1 as usize], CP437_TO_UNICODE[c as usize]
                        );
                    } else {
                        let color = ((c as char).to_digit(16).unwrap()
                            | ((ch1 as char).to_digit(16).unwrap() << 4))
                            as u8;
                        self.set_color(color);
                    }
                }
            }
        }
        stdout().flush().unwrap();
        Ok(())
    }

    fn write_raw(&mut self, terminal_target: TerminalTarget, data: &[u8]) -> Res<()> {
        if self.no_terminal(terminal_target) {
            return Ok(());
        }
        let mut state = PcbState::Default;
        for c in data {
            let c = *c;
            if c == 0x1A {
                break;
            }
            match state {
                PcbState::Default => {
                    if c == b'@' {
                        state = PcbState::GotAt;
                    } else if c == b'\n' || c == b'\r' || c == 0x08 || c == b'\x1B' {
                        disable_raw_mode().unwrap();
                        print!("{}", c as char);
                        enable_raw_mode().unwrap();
                    } else {
                        print!("{}", CP437_TO_UNICODE[c as usize]);
                    }
                }
                PcbState::GotAt => {
                    if c == b'X' {
                        state = PcbState::ReadColor1;
                    } else {
                        state = PcbState::ReadAtSequence((c as char).to_string());
                    }
                }
                PcbState::ReadAtSequence(s) => {
                    if c == b'@' {
                        state = PcbState::Default;
                        match s.as_str() {
                            "CLS" => {
                                stdout()
                                    .execute(Clear(crossterm::terminal::ClearType::All))
                                    .unwrap()
                                    .execute(MoveTo(0, 0))
                                    .unwrap();
                            }
                            _str => {
                                // println!("Unknown pcb sequence: {}", str);
                            }
                        }
                    } else {
                        state = PcbState::ReadAtSequence(s + &(c as char).to_string());
                    }
                }
                PcbState::ReadColor1 => {
                    if c.is_ascii_hexdigit() {
                        state = PcbState::ReadColor2(c);
                    } else {
                        print!("@{}", CP437_TO_UNICODE[c as usize]);
                        state = PcbState::Default;
                    }
                }
                PcbState::ReadColor2(ch1) => {
                    state = PcbState::Default;
                    if !c.is_ascii_hexdigit() {
                        print!(
                            "@{}{}",
                            CP437_TO_UNICODE[ch1 as usize], CP437_TO_UNICODE[c as usize]
                        );
                    } else {
                        let color = ((c as char).to_digit(16).unwrap()
                            | ((ch1 as char).to_digit(16).unwrap() << 4))
                            as u8;
                        self.set_color(color);
                    }
                }
            }
        }
        stdout().flush().unwrap();
        Ok(())
    }

    fn read(&mut self) -> Res<String> {
        let mut result = String::new();
        loop {
            let Ok(Some(ch)) = self.get_char() else {
                continue;
            };
            if ch == '\r' || ch == '\n' {
                break;
            }
            result.push(ch);
        }
        Ok(result)
    }

    fn get_char(&mut self) -> Res<Option<char>> {
        self.fill_buffer()?;
        if let Some(c) = self.char_buffer.pop_front() {
            Ok(Some(c))
        } else {
            Ok(None)
        }
    }

    fn inbytes(&mut self) -> i32 {
        let _ = self.fill_buffer();
        self.char_buffer.len() as i32
    }

    fn set_color(&mut self, color: u8) {
        let fg_color = color as usize & 0b1111;
        let bg_color = (color >> 4) as usize & 0b0111;
        //println!("Setting color: {} fg: {} bg: {}", color, fg_color, bg_color);
        stdout()
            .execute(SetForegroundColor(get_color(fg_color)))
            .unwrap()
            .execute(SetBackgroundColor(get_color(bg_color)))
            .unwrap();

        if color & 0x80 != 0 {
            stdout().execute(EnableBlinking).unwrap();
        } else {
            stdout().execute(DisableBlinking).unwrap();
        }
    }

    fn get_caret_position(&mut self) -> (i32, i32) {
        if let Ok((x, y)) = position() {
            (x as i32, y as i32)
        } else {
            (0, 0)
        }
    }

    /// simulate user input for later processing
    fn send_to_com(&mut self, _data: &str) -> Res<()> {
        Ok(())
    }
}
