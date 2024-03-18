#![allow(clippy::needless_pass_by_value, clippy::unnecessary_wraps)]
use std::fs::File;
use std::path::PathBuf;
use std::str::FromStr;

use crate::ast::{Variable, VariableData};
use crate::vm::VirtualMachine;
use crate::Res;
use easy_reader::EasyReader;
use radix_fmt::radix;
use rand::Rng; // 0.8.5
use substring::Substring;

/// Returns the length of a string
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::Integer` - the length of `str`
/// # Remarks
/// 0 means empty string
/// According to specs 256 is the maximum returned
pub fn len(str: Variable) -> Variable {
    Variable::new_int(str.as_string().len() as i32)
}

/// Returns the lowercase equivalent of a string
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::String` - lowercase equivalent of `str`
pub fn lower(str: Variable) -> Variable {
    Variable::new_string(str.as_string().to_lowercase())
}

/// Returns the uppercase equivalent of a string
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::String` - uppercase equivalent of `str`
pub fn upper(str: Variable) -> Variable {
    Variable::new_string(str.as_string().to_uppercase())
}

/// Returns a substring
/// # Arguments
///  * `str` - A string value
///  * `pos` - An integer value with a position from str to begin the substring 1 == first char
///  * `chars` - An integer value with the number of chars to take from `str`
/// # Returns
///  the substring of `str`, "" if chars <= 0, Will add padding up to the full length specified
pub fn mid(str: Variable, pos: Variable, chars: Variable) -> Res<Variable> {
    let mut pos = pos.as_int() - 1; // 1 based
    let mut chars = chars.as_int();
    if chars <= 0 {
        return Ok(Variable::new_string(String::new()));
    }

    let mut res = String::new();
    while pos < 0 {
        res.push(' ');
        pos += 1;
        chars -= 1;
    }

    if chars > 0 {
        res.push_str(
            str.as_string()
                .substring(pos as usize, (pos + chars) as usize),
        );
    }
    Ok(Variable::new_string(res))
}

pub fn left(str: Variable, chars: Variable) -> Res<Variable> {
    let mut chars = chars.as_int();
    if chars <= 0 {
        return Ok(Variable::new_string(String::new()));
    }

    let mut res = String::new();
    if chars > 0 {
        let str = str.as_string();
        if chars < str.len() as i32 {
            res.push_str(str.substring(0, chars as usize));
        } else {
            res.push_str(str.as_str());
            chars -= str.len() as i32;
            while chars > 0 {
                res.push(' ');
                chars -= 1;
            }
        }
    }
    Ok(Variable::new_string(res))
}

pub fn right(str: Variable, chars: Variable) -> Res<Variable> {
    let chars = chars.as_int();
    if chars <= 0 {
        return Ok(Variable::new_string(String::new()));
    }
    let mut chars = chars as usize;

    let mut res = String::new();
    if chars > 0 {
        let str = str.as_string();
        while chars > str.len() {
            res.push(' ');
            chars -= 1;
        }
        res.push_str(str.substring(str.len() - chars, str.len()));
    }
    Ok(Variable::new_string(res))
}

pub fn space(chars: Variable) -> Res<Variable> {
    let mut chars = chars.as_int();
    if chars <= 0 {
        return Ok(Variable::new_string(String::new()));
    }
    let mut res = String::new();
    while chars > 0 {
        res.push(' ');
        chars -= 1;
    }
    Ok(Variable::new_string(res))
}

pub fn ferr(interpreter: &mut VirtualMachine, channel: Variable) -> Res<Variable> {
    let channel = channel.as_int();
    Ok(Variable::new_bool(interpreter.io.ferr(channel as usize)))
}

pub fn chr(chr: Variable) -> Res<Variable> {
    let c = chr.as_int();
    if c <= 0 {
        return Ok(Variable::new_string(String::new()));
    }
    // undocumented: returns a space for c > 255
    if c > 255 {
        return Ok(Variable::new_string(" ".to_string()));
    }
    let mut res = String::new();
    unsafe {
        res.push(char::from_u32_unchecked(c as u32));
    }
    Ok(Variable::new_string(res))
}

pub fn asc(chr: Variable) -> Variable {
    let c = chr.as_string();
    if c.is_empty() {
        return Variable::new_int(0);
    }
    Variable::new_int(c.as_bytes()[0] as i32)
}

/// Returns the position of a substring
/// # Arguments
///  * `str` - A string value
///  * `sub` - A string expression to search for
/// # Returns
///  A 1 based integer of the position of sub or 0 if sub is not found.
pub fn instr(str: Variable, sub: Variable) -> Variable {
    let str = str.as_string();
    let sub = sub.as_string();
    if sub.is_empty() {
        return Variable::new_int(0);
    }

    match str.find(&sub) {
        Some(x) => Variable::new_int(1 + x as i32),
        _ => Variable::new_int(0),
    }
}

/// Returns a flag indicating if the user has aborted the display of information.
pub fn abort(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO ABORT") // TODO
}

/// Trim specified characters from the beginning of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the beginning of `str`
/// # Returns
///  The trimmed `str`
pub fn ltrim(str: Variable, ch: Variable) -> Variable {
    let mut ch = ch.as_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.as_string();
    let pat = ch.remove(0);
    Variable::new_string(res.trim_start_matches(pat).to_string())
}

/// Replaces all occurences of a given character to another character in a string.
/// # Arguments
///  * `str` - A string value
///  * `old` - A string with the old character
///  * `new` - A string with the new character
pub fn replace(str: Variable, old: Variable, new: Variable) -> Variable {
    let str = str.as_string();
    let old = old.as_string();
    let new = new.as_string();
    if old.is_empty() {
        return Variable::new_string(str);
    }

    let mut res = String::new();
    let old = old.chars().next().unwrap();
    let new = new.chars().next().unwrap();
    for c in str.chars() {
        if c == old {
            res.push(new);
        } else {
            res.push(c);
        }
    }
    Variable::new_string(res)
}

/// Remove all occurences of a given character in a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to remove
pub fn strip(str: Variable, ch: Variable) -> Variable {
    let str = str.as_string();
    let ch = ch.as_string();
    let mut res = String::new();
    let ch = ch.chars().next().unwrap();
    for c in str.chars() {
        if c != ch {
            res.push(c);
        }
    }
    Variable::new_string(res)
}

/// Remove @X codes from a string
/// # Arguments
///  * `str` - A string value
/// # Returns
/// A string without any @X codes
pub fn strip_atx(str: Variable) -> Variable {
    let str = str.as_string();
    let mut res = String::new();
    let mut state = 0;
    let mut ch1 = 'A';
    for c in str.chars() {
        match state {
            0 => {
                if c == '@' {
                    state = 1;
                } else {
                    res.push(c);
                }
            }
            1 => {
                if c == 'X' {
                    state = 2;
                } else {
                    res.push('@');
                    res.push(c);
                    state = 0;
                }
            }
            2 => {
                if c.is_ascii_hexdigit() {
                    state = 3;
                } else {
                    res.push('@');
                    res.push(c);
                    ch1 = c;
                    state = 0;
                }
            }
            3 => {
                state = 0;
                if !c.is_ascii_hexdigit() {
                    res.push('@');
                    res.push(ch1);
                    res.push(c);
                }
            }
            _ => {
                state = 0;
            }
        }
    }
    Variable::new_string(res)
}

pub fn replace_string(_str: Variable, _old: Variable, _new: Variable) -> Variable {
    panic!("TODO")
}

pub fn strip_string(_str: Variable, _strip: Variable) -> Variable {
    panic!("TODO")
}

/// Trim specified characters from the end of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the end of `str`
/// # Returns
///  The trimmed `str`
pub fn rtrim(str: Variable, ch: Variable) -> Variable {
    let mut ch = ch.as_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.as_string();
    let pat = ch.remove(0);
    Variable::new_string(res.trim_end_matches(pat).to_string())
}

/// Trim specified characters from the beginning and end of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the beginning and end of `str`
/// # Returns
///  The trimmed `str`
pub fn trim(str: Variable, ch: Variable) -> Variable {
    let mut ch = ch.as_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.as_string();
    let pat = ch.remove(0);
    Variable::new_string(res.trim_matches(pat).to_string())
}

pub fn random(upper: Variable) -> Res<Variable> {
    let upper = upper.as_int();
    if upper <= 0 {
        return Ok(Variable::new_int(0));
    }

    let mut rng = rand::thread_rng();
    Ok(Variable::new_int(rng.gen_range(0..upper)))
}

pub fn date() -> Variable {
    panic!("TODO")
}

pub fn time() -> Variable {
    panic!("TODO")
}

pub fn u_name(interpreter: &VirtualMachine) -> Variable {
    Variable::new_string(
        interpreter.icy_board_data.users[interpreter.cur_user]
            .name
            .clone(),
    )
}

pub fn u_ldate(interpreter: &VirtualMachine) -> Variable {
    // interpreter.pcb_data.users[interpreter.cur_user].last_date_on
    // TODO
    Variable::new(crate::ast::VariableType::Date, VariableData::default())
}

pub fn u_ltime(interpreter: &VirtualMachine) -> Variable {
    // TODO
    Variable::new(crate::ast::VariableType::Time, VariableData::default())
}

pub fn u_ldir(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_lmr(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_logons(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_ful(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_fdl(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_bdlday(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_timeon(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_bdl(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_bul(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_msgrd(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}
pub fn u_msgwr(interpreter: &VirtualMachine) -> Variable {
    panic!("TODO") // TODO
}

pub fn year(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn month(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn day(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dow(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn hour(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn min(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn sec(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn timeap(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ver(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn nochar(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_string(interpreter.icy_board_data.no_char.to_string())
}
pub fn yeschar(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_string(interpreter.icy_board_data.yes_char.to_string())
}

pub fn inkey(interpreter: &mut VirtualMachine) -> Res<Variable> {
    if let Some(ch) = interpreter.ctx.get_char()? {
        if ch as u8 == 127 {
            return Ok(Variable::new_string("DEL".to_string()));
        }
        if ch == '\x1B' {
            if let Some(ch) = interpreter.ctx.get_char()? {
                if ch == '[' {
                    if let Some(ch) = interpreter.ctx.get_char()? {
                        match ch {
                            'A' => return Ok(Variable::new_string("UP".to_string())),
                            'B' => return Ok(Variable::new_string("DOWN".to_string())),
                            'C' => return Ok(Variable::new_string("RIGHT".to_string())),
                            'D' => return Ok(Variable::new_string("LEFT".to_string())),

                            'H' => return Ok(Variable::new_string("HOME".to_string())),
                            'K' => return Ok(Variable::new_string("END".to_string())),

                            'V' => return Ok(Variable::new_string("PGUP".to_string())),
                            'U' => return Ok(Variable::new_string("PGDN".to_string())),

                            '@' => return Ok(Variable::new_string("INS".to_string())),

                            _ => return Ok(Variable::new_string(ch.to_string())),
                        }
                    }
                }
            }
            return Ok(Variable::new_string("\x1B".to_string()));
        }
        Ok(Variable::new_string(ch.to_string()))
    } else {
        Ok(Variable::new_string(String::new()))
    }
}

pub fn tostring(x: Variable) -> Variable {
    Variable::new_string(x.as_string())
}

pub fn mask_pwd() -> Variable {
    Variable::new_string((' '..='~').collect::<String>())
}
pub fn mask_alpha() -> Variable {
    Variable::new_string(('A'..='Z').collect::<String>() + ('a'..='z').collect::<String>().as_str())
}
pub fn mask_num() -> Variable {
    Variable::new_string(('0'..='9').collect::<String>())
}
pub fn mask_alnum() -> Variable {
    Variable::new_string(
        ('A'..='Z').collect::<String>()
            + ('a'..='z').collect::<String>().as_str()
            + ('0'..='9').collect::<String>().as_str(),
    )
}
pub fn mask_file() -> Variable {
    Variable::new_string(
        ('A'..='Z').collect::<String>()
            + ('a'..='z').collect::<String>().as_str()
            + ('0'..='9').collect::<String>().as_str()
            + "!#$%&'()-.:[\\]^_`~",
    )
}
pub fn mask_path() -> Variable {
    Variable::new_string(
        ('A'..='Z').collect::<String>()
            + ('a'..='z').collect::<String>().as_str()
            + ('0'..='9').collect::<String>().as_str()
            + "!#$%&'()-.:[\\]^_`~",
    )
}
pub fn mask_ascii() -> Variable {
    Variable::new_string((' '..='~').collect::<String>())
}

pub fn curconf(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pcbdat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ppepath(interpreter: &VirtualMachine) -> Variable {
    let Some(dir) = interpreter.prg.file_name.parent() else {
        return Variable::new_string(String::new());
    };
    let mut res = dir.to_string_lossy().to_string();
    res.push('/');
    Variable::new_string(res)
}

pub fn valdate(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn valtime(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pcbnode(interpreter: &VirtualMachine) -> Variable {
    Variable::new_int(interpreter.icy_board_data.pcb_data.node_number as i32)
}

pub fn readline(interpreter: &VirtualMachine, file: Variable, line: Variable) -> Res<Variable> {
    let file_name = file.as_string();
    let line = line.as_int();
    let file_name = interpreter.io.resolve_file(&file_name);

    let file = File::open(file_name)?;
    let mut reader = EasyReader::new(file)?;
    for _ in 1..line {
        reader.next_line()?;
    }
    let line_text = reader.next_line()?.unwrap_or_default();
    Ok(Variable::new_string(line_text))
}

pub fn sysopsec(interpreter: &VirtualMachine) -> Variable {
    Variable::new_int(interpreter.icy_board_data.pcb_data.sysop_security.sysop)
}
pub fn onlocal(interpreter: &VirtualMachine) -> Variable {
    // TODO: OnLocal should return true if the user is local, false otherwise
    Variable::new_bool(true)
}

pub fn un_stat(interpreter: &VirtualMachine) -> Variable {
    if let Some(node) = &interpreter.pcb_node {
        Variable::new_int(node.status as i32)
    } else {
        Variable::new_int(0)
    }
}

pub fn un_name(interpreter: &VirtualMachine) -> Variable {
    if let Some(node) = &interpreter.pcb_node {
        Variable::new_string(node.name.clone())
    } else {
        Variable::new_string(String::new())
    }
}
pub fn un_city(interpreter: &VirtualMachine) -> Variable {
    if let Some(node) = &interpreter.pcb_node {
        Variable::new_string(node.city.clone())
    } else {
        Variable::new_string(String::new())
    }
}
pub fn un_oper(interpreter: &VirtualMachine) -> Variable {
    if let Some(node) = &interpreter.pcb_node {
        Variable::new_string(node.operation.clone())
    } else {
        Variable::new_string(String::new())
    }
}
pub fn cursec(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn gettoken(interpreter: &mut VirtualMachine) -> Variable {
    if interpreter.cur_tokens.is_empty() {
        Variable::new_string(String::new())
    } else {
        Variable::new_string(interpreter.cur_tokens.remove(0))
    }
}
pub fn minleft(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn minon(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn getenv(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn callid(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regal(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regah(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regbl(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regbh(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regcl(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regch(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regdl(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regdh(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regax(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regbx(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regcx(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regdx(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regsi(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regdi(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regf(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regcf(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn regds(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn reges(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn b2w(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn peekb(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn peekw(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn mkaddr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn exist(interpreter: &VirtualMachine, file_name: &str) -> Variable {
    Variable::new_bool(interpreter.io.file_exists(file_name))
}

/// Convert an integer to a string in a specified number base.
/// # Arguments
///  * `int` - Any integer to convert to string format.
///  * `base` - The base to use for the conversion. 2 <= base <= 36
/// # Returns
///  A string representation of `int` in the specified base.
pub fn i2s(int: i32, base: i32) -> Variable {
    let s = radix(int, base as u8).to_string();
    Variable::new_string(s)
}

/// Convert a string in a specified number base to an integer.
/// # Arguments
///  * `src` - A string value to convert to an integer.
///  * `base` - The base to use for the conversion. 2 <= base <= 36
/// # Returns
///  An integer representation of `s` in the specified base.
pub fn s2i(src: &str, base: i32) -> Res<Variable> {
    let i = i32::from_str_radix(src, base as u32)?;
    Ok(Variable::new_int(i))
}
pub fn carrier(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_int(interpreter.ctx.get_bps())
}
pub fn tokenstr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn cdon(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn langext(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ansion(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn valcc(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fmtcc(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn cctype(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn getx(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_int(interpreter.ctx.get_caret_position().0 + 1)
}

pub fn gety(interpreter: &mut VirtualMachine) -> Variable {
    let y = interpreter.ctx.get_caret_position().1;
    Variable::new_int(y + 1)
}

pub fn band(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn bor(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn bxor(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn bnot(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn u_pwdhist(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn u_pwdlc(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn u_pwdtc(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn u_stat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn defcolor(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn grafmode(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn psa(_x: Variable) -> Variable {
    panic!("TODO")
}

#[allow(clippy::unnecessary_wraps)]
pub fn fileinf(interpreter: &VirtualMachine, file: &str, item: i32) -> Res<Variable> {
    match item {
        1 => Ok(Variable::new_bool(interpreter.io.file_exists(file))),
        2 => Ok(Variable::new(
            crate::ast::VariableType::Date,
            VariableData::default(),
        )), // TODO: File date
        3 => Ok(Variable::new(
            crate::ast::VariableType::Time,
            VariableData::default(),
        )), // TODO: File time
        4 => Ok(Variable::new_int(interpreter.io.get_file_size(file) as i32)),
        5 => Ok(Variable::new_int(0)), // TODO: File attributes
        6 => Ok(Variable::new_string("C:".to_string())), // Drive
        7 => Ok(Variable::new_string(
            PathBuf::from_str(file)
                .unwrap()
                .parent()
                .unwrap()
                .to_string_lossy()
                .to_string(),
        )),
        8 => Ok(Variable::new_string(
            PathBuf::from_str(file)
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
                .to_string(),
        )),
        9 => Ok(Variable::new_string(
            PathBuf::from_str(file)
                .unwrap()
                .file_stem()
                .unwrap()
                .to_string_lossy()
                .to_string(),
        )),
        _ => {
            log::error!("Unknown fileinf item: {}", item);
            Ok(Variable::new_int(0))
        }
    }
}

pub fn ppename(interpreter: &VirtualMachine) -> Variable {
    let p = interpreter.prg.file_name.with_extension("");
    let Some(dir) = p.file_name() else {
        return Variable::new_string(String::new());
    };
    let res = dir.to_string_lossy().to_string();
    Variable::new_string(res)
}

pub fn mkdate(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn curcolor(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn kinkey(interpreter: &mut VirtualMachine) -> Res<Variable> {
    inkey(interpreter)
}
pub fn minkey(interpreter: &mut VirtualMachine) -> Res<Variable> {
    inkey(interpreter)
}
pub fn maxnode(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_int(interpreter.icy_board_data.nodes.len() as i32)
}
pub fn slpath(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn helppath(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn temppath(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn modem(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn loggedon(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn callnum(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn mgetbyte(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tokcount(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_int(interpreter.cur_tokens.len() as i32)
}

pub fn u_recnum(interpreter: &mut VirtualMachine, user_name: Variable) -> Variable {
    let user_name = user_name.as_string().to_uppercase();
    for (i, user) in interpreter.icy_board_data.users.iter().enumerate() {
        if user.name.to_uppercase() == user_name {
            return Variable::new_int(i as i32);
        }
    }
    Variable::new_int(-1)
}

pub fn u_inconf(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn peekdw(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dbglevel(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn scrtext(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn showstat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pagestat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tobigstr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn toboolean(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tobyte(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn todate(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn todreal(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn toedate(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tointeger(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tomoney(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn toreal(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tosbyte(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tosword(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn totime(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tounsigned(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn toword(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn mixed(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn alias(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confreg(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confexp(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confsel(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confsys(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confmw(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn lprinted(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn isnonstop(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn errcorrect(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confalias(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn useralias(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn curuser(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn chatstat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn defans(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn lastans(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn meganum(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn evttimeadj(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn isbitset(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fmtreal(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn flagcnt(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn kbdbufsize(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pplbufsize(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn kbdfilused(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn lomsgnum(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn himsgnum(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn drivespace(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn outbytes() -> Variable {
    Variable::new_int(0)
}
pub fn hiconfnum(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn inbytes(interpreter: &mut VirtualMachine) -> Variable {
    Variable::new_int(interpreter.ctx.inbytes())
}

pub fn crc32(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pcbmac(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn actmsgnum(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn stackleft(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn stackerr(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn dgetalias(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dbof(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dchanged(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ddecimals(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ddeleted(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn deof(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn derr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dfields(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dlength(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dname(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dreccount(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn drecno(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dtype(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fnext(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dnext(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn toddate(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dcloseall(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dopen(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dclose(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dsetalias(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dpack(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dlockf(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dlock(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dlockr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dunlock(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dnopen(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dnclose(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dncloseall(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dnew(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dadd(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dappend(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dtop(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dgo(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dbottom(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dskip(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dblank(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ddelete(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn drecall(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dtag(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dseek(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dfblank(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dget(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dput(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dfcopy(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dselect(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn dchkstat(_x: Variable) -> Variable {
    panic!("TODO")
}

pub fn pcbaccount(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn pcbaccstat(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn derrmsg(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn account(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn scanmsghdr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn checkrip(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ripver(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn qwklimits(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn findfirst(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn findnext(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn uselmrs(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn confinfo(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn tinkey(interpreter: &mut VirtualMachine) -> Res<Variable> {
    inkey(interpreter)
}
pub fn cwd(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn instrr(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fdordaka(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fdordorg(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fdordarea(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn fdoqrd(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn getdrive(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn setdrive(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn bs2i(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn bd2i(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn i2bs(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn i2bd(_x: Variable) -> Variable {
    panic!("TODO")
}
pub fn ftell(_x: Variable) -> Variable {
    panic!("TODO")
}
