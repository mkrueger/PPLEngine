use std::io::{stdin, stdout};

use crossterm::{
    cursor::{position, DisableBlinking, EnableBlinking, MoveTo},
    style::{Color, SetBackgroundColor, SetForegroundColor},
    terminal::Clear,
    ExecutableCommand,
};
use icy_ppe::{interpreter::ExecutionContext, Res};

#[derive(Default)]
pub struct Output {}

fn get_color(n: u8) -> Color {
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
pub const CP437_TO_UNICODE: [char; 256] = [
    '\u{0000}', '\u{263a}', '\u{263b}', '\u{2665}', '\u{2666}', '\u{2663}', '\u{2660}', '\u{2022}',
    '\u{25d8}', '\u{25cb}', '\n', '\u{2642}', '\u{2640}', '\u{266a}', '\u{266b}', '\u{263c}',
    '\u{25ba}', '\u{25c4}', '\u{2195}', '\u{203c}', '\u{00b6}', '\u{00a7}', '\u{25ac}', '\u{21a8}',
    '\u{2191}', '\u{2193}', '\u{2192}', '\u{2190}', '\u{221f}', '\u{2194}', '\u{25b2}', '\u{25bc}',
    '\u{0020}', '\u{0021}', '\u{0022}', '\u{0023}', '\u{0024}', '\u{0025}', '\u{0026}', '\u{0027}',
    '\u{0028}', '\u{0029}', '\u{002a}', '\u{002b}', '\u{002c}', '\u{002d}', '\u{002e}', '\u{002f}',
    '\u{0030}', '\u{0031}', '\u{0032}', '\u{0033}', '\u{0034}', '\u{0035}', '\u{0036}', '\u{0037}',
    '\u{0038}', '\u{0039}', '\u{003a}', '\u{003b}', '\u{003c}', '\u{003d}', '\u{003e}', '\u{003f}',
    '\u{0040}', '\u{0041}', '\u{0042}', '\u{0043}', '\u{0044}', '\u{0045}', '\u{0046}', '\u{0047}',
    '\u{0048}', '\u{0049}', '\u{004a}', '\u{004b}', '\u{004c}', '\u{004d}', '\u{004e}', '\u{004f}',
    '\u{0050}', '\u{0051}', '\u{0052}', '\u{0053}', '\u{0054}', '\u{0055}', '\u{0056}', '\u{0057}',
    '\u{0058}', '\u{0059}', '\u{005a}', '\u{005b}', '\u{005c}', '\u{005d}', '\u{005e}', '\u{005f}',
    '\u{0060}', '\u{0061}', '\u{0062}', '\u{0063}', '\u{0064}', '\u{0065}', '\u{0066}', '\u{0067}',
    '\u{0068}', '\u{0069}', '\u{006a}', '\u{006b}', '\u{006c}', '\u{006d}', '\u{006e}', '\u{006f}',
    '\u{0070}', '\u{0071}', '\u{0072}', '\u{0073}', '\u{0074}', '\u{0075}', '\u{0076}', '\u{0077}',
    '\u{0078}', '\u{0079}', '\u{007a}', '\u{007b}', '\u{007c}', '\u{007d}', '\u{007e}', '\u{007f}',
    '\u{00c7}', '\u{00fc}', '\u{00e9}', '\u{00e2}', '\u{00e4}', '\u{00e0}', '\u{00e5}', '\u{00e7}',
    '\u{00ea}', '\u{00eb}', '\u{00e8}', '\u{00ef}', '\u{00ee}', '\u{00ec}', '\u{00c4}', '\u{00c5}',
    '\u{00c9}', '\u{00e6}', '\u{00c6}', '\u{00f4}', '\u{00f6}', '\u{00f2}', '\u{00fb}', '\u{00f9}',
    '\u{00ff}', '\u{00d6}', '\u{00dc}', '\u{00a2}', '\u{00a3}', '\u{00a5}', '\u{20a7}', '\u{0192}',
    '\u{00e1}', '\u{00ed}', '\u{00f3}', '\u{00fa}', '\u{00f1}', '\u{00d1}', '\u{00aa}', '\u{00ba}',
    '\u{00bf}', '\u{2310}', '\u{00ac}', '\u{00bd}', '\u{00bc}', '\u{00a1}', '\u{00ab}', '\u{00bb}',
    '\u{2591}', '\u{2592}', '\u{2593}', '\u{2502}', '\u{2524}', '\u{2561}', '\u{2562}', '\u{2556}',
    '\u{2555}', '\u{2563}', '\u{2551}', '\u{2557}', '\u{255d}', '\u{255c}', '\u{255b}', '\u{2510}',
    '\u{2514}', '\u{2534}', '\u{252c}', '\u{251c}', '\u{2500}', '\u{253c}', '\u{255e}', '\u{255f}',
    '\u{255a}', '\u{2554}', '\u{2569}', '\u{2566}', '\u{2560}', '\u{2550}', '\u{256c}', '\u{2567}',
    '\u{2568}', '\u{2564}', '\u{2565}', '\u{2559}', '\u{2558}', '\u{2552}', '\u{2553}', '\u{256b}',
    '\u{256a}', '\u{2518}', '\u{250c}', '\u{2588}', '\u{2584}', '\u{258c}', '\u{2590}', '\u{2580}',
    '\u{03b1}', '\u{00df}', '\u{0393}', '\u{03c0}', '\u{03a3}', '\u{03c3}', '\u{00b5}', '\u{03c4}',
    '\u{03a6}', '\u{0398}', '\u{03a9}', '\u{03b4}', '\u{221e}', '\u{03c6}', '\u{03b5}', '\u{2229}',
    '\u{2261}', '\u{00b1}', '\u{2265}', '\u{2264}', '\u{2320}', '\u{2321}', '\u{00f7}', '\u{2248}',
    '\u{00b0}', '\u{2219}', '\u{00b7}', '\u{221a}', '\u{207f}', '\u{00b2}', '\u{25a0}', '\u{00a0}',
];

impl ExecutionContext for Output {
    fn gotoxy(&mut self, x: i32, y: i32) -> Res<()> {
        stdout().execute(MoveTo(x as u16, y as u16))?;
        Ok(())
    }

    fn print(&mut self, str: &str) -> Res<()> {
        self.write_raw(str.chars().map(|c| c as u8).collect::<Vec<u8>>().as_slice())
    }

    fn write_raw(&mut self, data: &[u8]) -> Res<()> {
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
                                    .unwrap();
                            }
                            str => {
                                println!("Unknown pcb sequence: {}", str);
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
                        let color = ((c as char).to_digit(16).unwrap() << 4
                            | (ch1 as char).to_digit(16).unwrap())
                            as u8;
                        self.set_color(color);
                    }
                }
            }
        }

        Ok(())
    }

    fn read(&mut self) -> Res<String> {
        let mut res = String::new();
        stdin().read_line(&mut res)?;
        Ok(res)
    }

    fn get_char(&mut self) -> Res<Option<char>> {
        Ok(None)
    }

    fn inbytes(&mut self) -> i32 {
        0
    }

    fn set_color(&mut self, color: u8) {
        stdout()
            .execute(SetForegroundColor(get_color(color % 15)))
            .unwrap()
            .execute(SetBackgroundColor(get_color((color >> 4) % 8)))
            .unwrap();

        if color & 0x80 != 0 {
            stdout().execute(EnableBlinking).unwrap();
        } else {
            stdout().execute(DisableBlinking).unwrap();
        }
    }

    fn get_caret_position(&mut self) -> (i32, i32) {
        let pos = position().unwrap();
        (pos.0 as i32, pos.1 as i32)
    }

    /// simulate user input for later processing
    fn send_to_com(&mut self, _data: &str) -> Res<()> {
        Ok(())
    }
}
