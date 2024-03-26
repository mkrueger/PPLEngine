use std::{
    collections::VecDeque,
    io::{stdout, Write},
    time::Duration,
};

use crossterm::{
    event::{poll, read, Event, KeyCode, KeyModifiers},
    terminal::{disable_raw_mode, enable_raw_mode},
};
use icy_ppe::Res;

use crate::vm::{ExecutionContext, HangupType};

#[derive(Default)]
pub struct Output {
    char_buffer: VecDeque<char>,
    pub is_sysop: bool,
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
}

impl ExecutionContext for Output {
    fn write_raw(&mut self, data: &[char]) -> Res<()> {
        disable_raw_mode().unwrap();
        for c in data {
            print!("{}", *c);
        }
        stdout().flush().unwrap();
        enable_raw_mode().unwrap();
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

    fn hangup(&mut self, _hangup_type: HangupType) -> Res<()> {
        Ok(())
    }
}
