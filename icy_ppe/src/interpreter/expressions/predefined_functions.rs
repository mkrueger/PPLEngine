#![allow(clippy::needless_pass_by_value)]
use std::fs::File;

use super::super::errors::IcyError;
use super::{get_int, get_string};
use crate::ast::{convert_to, VariableType, VariableValue};
use crate::interpreter::Interpreter;
use crate::Res;
use easy_reader::EasyReader;
use radix_fmt::radix;
use rand::Rng; // 0.8.5
use substring::Substring;

/// Returns the length of a strning
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::Integer` - the length of `str`
/// # Remarks
/// 0 means empty string
/// According to specs 256 is the maximum returned
pub fn len(str: VariableValue) -> VariableValue {
    VariableValue::Integer(str.to_string().len() as i32)
}

/// Returns the lowercase equivalent of a string
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::String` - lowercase equivalent of `str`
pub fn lower(str: VariableValue) -> VariableValue {
    VariableValue::String(str.to_string().to_lowercase())
}

/// Returns the uppercase equivalent of a string
/// # Arguments
///  * `str` - A string value
/// # Returns
///  `VariableValue::String` - uppercase equivalent of `str`
pub fn upper(str: VariableValue) -> VariableValue {
    VariableValue::String(str.to_string().to_uppercase())
}

/// Returns a substring
/// # Arguments
///  * `str` - A string value
///  * `pos` - An integer value with a position from str to begin the substring 1 == first char
///  * `chars` - An integer value with the number of chars to take from `str`
/// # Returns
///  the substring of `str`, "" if chars <= 0, Will add padding up to the full length specified
pub fn mid(str: VariableValue, pos: VariableValue, chars: VariableValue) -> Res<VariableValue> {
    let mut pos = get_int(&pos)? - 1; // 1 based
    let mut chars = get_int(&chars)?;
    if chars <= 0 {
        return Ok(VariableValue::String(String::new()));
    }

    let mut res = String::new();
    while pos < 0 {
        res.push(' ');
        pos += 1;
        chars -= 1;
    }

    if chars > 0 {
        res.push_str(
            str.to_string()
                .substring(pos as usize, (pos + chars) as usize),
        );
    }
    Ok(VariableValue::String(res))
}

pub fn left(str: VariableValue, chars: VariableValue) -> Res<VariableValue> {
    let mut chars = get_int(&chars)?;
    if chars <= 0 {
        return Ok(VariableValue::String(String::new()));
    }

    let mut res = String::new();
    if chars > 0 {
        let str = str.to_string();
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
    Ok(VariableValue::String(res))
}

pub fn right(str: VariableValue, chars: VariableValue) -> Res<VariableValue> {
    let chars = get_int(&chars)?;
    if chars <= 0 {
        return Ok(VariableValue::String(String::new()));
    }
    let mut chars = chars as usize;

    let mut res = String::new();
    if chars > 0 {
        let str = str.to_string();
        while chars > str.len() {
            res.push(' ');
            chars -= 1;
        }
        res.push_str(str.substring(str.len() - chars, str.len()));
    }
    Ok(VariableValue::String(res))
}

pub fn space(chars: VariableValue) -> Res<VariableValue> {
    let mut chars = get_int(&chars)?;
    if chars <= 0 {
        return Ok(VariableValue::String(String::new()));
    }
    let mut res = String::new();
    while chars > 0 {
        res.push(' ');
        chars -= 1;
    }
    Ok(VariableValue::String(res))
}

pub fn ferr(interpreter: &mut Interpreter, channel: VariableValue) -> Res<VariableValue> {
    let channel = get_int(&channel)?;
    Ok(VariableValue::Boolean(
        interpreter.io.ferr(channel as usize),
    ))
}

pub fn chr(chr: VariableValue) -> Res<VariableValue> {
    let c = get_int(&chr)?;
    if c <= 0 {
        return Ok(VariableValue::String(String::new()));
    }
    // undocumented: returns a space for c > 255
    if c > 255 {
        return Ok(VariableValue::String(" ".to_string()));
    }
    let mut res = String::new();
    unsafe {
        res.push(char::from_u32_unchecked(c as u32));
    }
    Ok(VariableValue::String(res))
}

pub fn asc(chr: VariableValue) -> VariableValue {
    let c = chr.to_string();
    if c.is_empty() {
        return VariableValue::Integer(0);
    }
    VariableValue::Integer(c.as_bytes()[0] as i32)
}

/// Returns the position of a substring
/// # Arguments
///  * `str` - A string value
///  * `sub` - A string expression to search for
/// # Returns
///  A 1 based integer of the position of sub or 0 if sub is not found.
pub fn instr(str: VariableValue, sub: VariableValue) -> VariableValue {
    let str = str.to_string();
    let sub = sub.to_string();
    if sub.is_empty() {
        return VariableValue::Integer(0);
    }

    match str.find(&sub) {
        Some(x) => VariableValue::Integer(1 + x as i32),
        _ => VariableValue::Integer(0),
    }
}

/// Returns a flag indicating if the user has aborted the display of information.
pub fn abort(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO ABORT") // TODO
}

/// Trim specified characters from the beginning of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the beginning of `str`
/// # Returns
///  The trimmed `str`
pub fn ltrim(str: VariableValue, ch: VariableValue) -> VariableValue {
    let mut ch = ch.to_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.to_string();
    let pat = ch.remove(0);
    VariableValue::String(res.trim_start_matches(pat).to_string())
}

/// Replaces all occurences of a given character to another character in a string.
/// # Arguments
///  * `str` - A string value
///  * `old` - A string with the old character
///  * `new` - A string with the new character
pub fn replace(str: VariableValue, old: VariableValue, new: VariableValue) -> VariableValue {
    let VariableValue::String(str) = str else {
        return VariableValue::String(String::new());
    };
    let VariableValue::String(old) = old else {
        return VariableValue::String(String::new());
    };
    let VariableValue::String(new) = new else {
        return VariableValue::String(String::new());
    };
    if old.is_empty() {
        return VariableValue::String(str);
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
    VariableValue::String(res)
}

/// Remove all occurences of a given character in a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to remove
pub fn strip(str: VariableValue, ch: VariableValue) -> VariableValue {
    let VariableValue::String(str) = str else {
        return VariableValue::String(String::new());
    };
    let VariableValue::String(ch) = ch else {
        return VariableValue::String(String::new());
    };
    let mut res = String::new();
    let ch = ch.chars().next().unwrap();
    for c in str.chars() {
        if c != ch {
            res.push(c);
        }
    }
    VariableValue::String(res)
}

/// Remove @X codes from a string
/// # Arguments
///  * `str` - A string value
/// # Returns
/// A string without any @X codes
pub fn strip_atx(str: VariableValue) -> VariableValue {
    let VariableValue::String(str) = str else {
        return VariableValue::String(String::new());
    };
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
    VariableValue::String(res)
}

pub fn replace_string(
    _str: VariableValue,
    _old: VariableValue,
    _new: VariableValue,
) -> VariableValue {
    panic!("TODO")
}

pub fn strip_string(_str: VariableValue, _strip: VariableValue) -> VariableValue {
    panic!("TODO")
}

/// Trim specified characters from the end of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the end of `str`
/// # Returns
///  The trimmed `str`
pub fn rtrim(str: VariableValue, ch: VariableValue) -> VariableValue {
    let mut ch = ch.to_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.to_string();
    let pat = ch.remove(0);
    VariableValue::String(res.trim_end_matches(pat).to_string())
}

/// Trim specified characters from the beginning and end of a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to strip from the beginning and end of `str`
/// # Returns
///  The trimmed `str`
pub fn trim(str: VariableValue, ch: VariableValue) -> VariableValue {
    let mut ch = ch.to_string();
    if ch.is_empty() {
        return str;
    }

    let res = str.to_string();
    let pat = ch.remove(0);
    VariableValue::String(res.trim_matches(pat).to_string())
}

pub fn random(upper: VariableValue) -> Res<VariableValue> {
    let upper = get_int(&upper)?;
    if upper <= 0 {
        return Ok(VariableValue::Integer(0));
    }

    let mut rng = rand::thread_rng();
    Ok(VariableValue::Integer(rng.gen_range(0..upper)))
}

pub fn date() -> VariableValue {
    panic!("TODO")
}

pub fn time() -> VariableValue {
    panic!("TODO")
}

pub fn u_name(interpreter: &Interpreter) -> VariableValue {
    VariableValue::String(
        interpreter.icy_board_data.users[interpreter.cur_user]
            .name
            .clone(),
    )
}

pub fn u_ldate(interpreter: &Interpreter) -> VariableValue {
    // interpreter.pcb_data.users[interpreter.cur_user].last_date_on
    // TODO
    VariableValue::Date(0)
}

pub fn u_ltime(interpreter: &Interpreter) -> VariableValue {
    // TODO
    VariableValue::Time(0)
}

pub fn u_ldir(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_lmr(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_logons(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_ful(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_fdl(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_bdlday(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_timeon(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_bdl(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_bul(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_msgrd(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_msgwr(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}

pub fn year(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn month(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn day(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dow(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn hour(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn min(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn sec(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn timeap(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ver(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn nochar(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn yeschar(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn inkey(interpreter: &mut Interpreter) -> Res<VariableValue> {
    if let Some(ch) = interpreter.ctx.get_char()? {
        if ch as u8 == 127 {
            return Ok(VariableValue::String("DEL".to_string()));
        }
        if ch == '\x1B' {
            if let Some(ch) = interpreter.ctx.get_char()? {
                if ch == '[' {
                    if let Some(ch) = interpreter.ctx.get_char()? {
                        match ch {
                            'A' => return Ok(VariableValue::String("UP".to_string())),
                            'B' => return Ok(VariableValue::String("DOWN".to_string())),
                            'C' => return Ok(VariableValue::String("RIGHT".to_string())),
                            'D' => return Ok(VariableValue::String("LEFT".to_string())),

                            'H' => return Ok(VariableValue::String("HOME".to_string())),
                            'K' => return Ok(VariableValue::String("END".to_string())),

                            'V' => return Ok(VariableValue::String("PGUP".to_string())),
                            'U' => return Ok(VariableValue::String("PGDN".to_string())),

                            '@' => return Ok(VariableValue::String("INS".to_string())),

                            _ => return Ok(VariableValue::String(ch.to_string())),
                        }
                    }
                }
            }
            return Ok(VariableValue::String("\x1B".to_string()));
        }

        Ok(VariableValue::String(ch.to_string()))
    } else {
        Ok(VariableValue::String(String::new()))
    }
}

pub fn tostring(x: VariableValue) -> VariableValue {
    VariableValue::String(x.to_string())
}

pub fn mask_pwd(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_alpha(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_num(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_alnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_file(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_path(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mask_ascii(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn curconf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pcbdat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ppepath(interpreter: &Interpreter) -> VariableValue {
    let Some(dir) = interpreter.prg.file_name.parent() else {
        return VariableValue::String(String::new());
    };
    let mut res = dir.to_string_lossy().to_string();
    res.push('/');
    VariableValue::String(res)
}

pub fn valdate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn valtime(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pcbnode(interpreter: &Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.icy_board_data.pcb_data.node_number as i32)
}

pub fn readline(
    interpreter: &Interpreter,
    file: VariableValue,
    line: VariableValue,
) -> Res<VariableValue> {
    let VariableValue::String(file_name) = file else {
        return Err(Box::new(IcyError::ParameterStringExpected(0)));
    };
    let VariableValue::Integer(line) = line else {
        return Err(Box::new(IcyError::ParameterIntegerExpected(1)));
    };
    let file_name = interpreter.io.resolve_file(&file_name);

    let file = File::open(file_name)?;
    let mut reader = EasyReader::new(file)?;
    for _ in 1..line {
        reader.next_line()?;
    }
    let line_text = reader.next_line()?.unwrap_or_default();
    Ok(VariableValue::String(line_text))
}

pub fn sysopsec(interpreter: &Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.icy_board_data.pcb_data.sysop_security.sysop)
}
pub fn onlocal(interpreter: &Interpreter) -> VariableValue {
    // TODO: OnLocal should return true if the user is local, false otherwise
    VariableValue::Boolean(true)
}

pub fn un_stat(interpreter: &Interpreter) -> VariableValue {
    if let Some(node) = &interpreter.pcb_node {
        VariableValue::Integer(node.status as i32)
    } else {
        VariableValue::Integer(0)
    }
}

pub fn un_name(interpreter: &Interpreter) -> VariableValue {
    if let Some(node) = &interpreter.pcb_node {
        VariableValue::String(node.name.clone())
    } else {
        VariableValue::String(String::new())
    }
}
pub fn un_city(interpreter: &Interpreter) -> VariableValue {
    if let Some(node) = &interpreter.pcb_node {
        VariableValue::String(node.city.clone())
    } else {
        VariableValue::String(String::new())
    }
}
pub fn un_oper(interpreter: &Interpreter) -> VariableValue {
    if let Some(node) = &interpreter.pcb_node {
        VariableValue::String(node.operation.clone())
    } else {
        VariableValue::String(String::new())
    }
}
pub fn cursec(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn gettoken(interpreter: &mut Interpreter) -> VariableValue {
    if interpreter.cur_tokens.is_empty() {
        VariableValue::String(String::new())
    } else {
        VariableValue::String(interpreter.cur_tokens.remove(0))
    }
}
pub fn minleft(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn minon(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn getenv(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn callid(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regal(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regah(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regbl(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regbh(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regcl(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regch(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regdl(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regdh(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regax(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regbx(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regcx(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regdx(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regsi(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regdi(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regcf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn regds(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn reges(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn b2w(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn peekb(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn peekw(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mkaddr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn exist(interpreter: &Interpreter, file_name: &str) -> VariableValue {
    VariableValue::Boolean(interpreter.io.file_exists(file_name))
}

/// Convert an integer to a string in a specified number base.
/// # Arguments
///  * `int` - Any integer to convert to string format.
///  * `base` - The base to use for the conversion. 2 <= base <= 36
/// # Returns
///  A string representation of `int` in the specified base.
pub fn i2s(int: i32, base: i32) -> VariableValue {
    let s = radix(int, base as u8).to_string();
    VariableValue::String(s)
}

/// Convert a string in a specified number base to an integer.
/// # Arguments
///  * `src` - A string value to convert to an integer.
///  * `base` - The base to use for the conversion. 2 <= base <= 36
/// # Returns
///  An integer representation of `s` in the specified base.
pub fn s2i(src: &str, base: i32) -> Res<VariableValue> {
    let i = i32::from_str_radix(src, base as u32)?;
    Ok(VariableValue::Integer(i))
}
pub fn carrier(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tokenstr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn cdon(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn langext(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ansion(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn valcc(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fmtcc(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn cctype(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn getx(interpreter: &mut Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.ctx.get_caret_position().0 + 1)
}

pub fn gety(interpreter: &mut Interpreter) -> VariableValue {
    let y = interpreter.ctx.get_caret_position().1;
    VariableValue::Integer(y + 1)
}

pub fn band(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn bor(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn bxor(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn bnot(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn u_pwdhist(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn u_pwdlc(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn u_pwdtc(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn u_stat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn defcolor(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn abs(x: VariableValue) -> Res<VariableValue> {
    match x {
        VariableValue::Boolean(_)
        | VariableValue::Integer(_)
        | VariableValue::Date(_)
        | VariableValue::Money(_)
        | VariableValue::EDate(_)
        | VariableValue::Time(_) => Ok(VariableValue::Integer(get_int(&x)?.abs())),
        VariableValue::Word(_) | VariableValue::Unsigned(_) | VariableValue::Byte(_) => Ok(x),

        VariableValue::SByte(i) => Ok(VariableValue::SByte(i.abs())),
        VariableValue::SWord(i) => Ok(VariableValue::SWord(i.abs())),

        VariableValue::Real(r) => Ok(VariableValue::Real(r.abs())),

        VariableValue::String(_) => abs(convert_to(VariableType::Integer, &x)),

        VariableValue::Dim1(_, _) | VariableValue::Dim2(_, _) | VariableValue::Dim3(_, _) => {
            Err(Box::new(IcyError::NotSupported))
        }
    }
}

pub fn grafmode(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn psa(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn fileinf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn ppename(interpreter: &Interpreter) -> VariableValue {
    let p = interpreter.prg.file_name.with_extension("");
    let Some(dir) = p.file_name() else {
        return VariableValue::String(String::new());
    };
    let res = dir.to_string_lossy().to_string();
    VariableValue::String(res)
}

pub fn mkdate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn curcolor(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn kinkey(interpreter: &mut Interpreter) -> Res<VariableValue> {
    inkey(interpreter)
}
pub fn minkey(interpreter: &mut Interpreter) -> Res<VariableValue> {
    inkey(interpreter)
}
pub fn maxnode(interpreter: &mut Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.icy_board_data.nodes.len() as i32)
}
pub fn slpath(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn helppath(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn temppath(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn modem(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn loggedon(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn callnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mgetbyte(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tokcount(interpreter: &mut Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.cur_tokens.len() as i32)
}

pub fn u_recnum(interpreter: &mut Interpreter, user_name: VariableValue) -> VariableValue {
    let user_name = get_string(&user_name).to_uppercase();
    for (i, user) in interpreter.icy_board_data.users.iter().enumerate() {
        if user.name.to_uppercase() == user_name {
            return VariableValue::Integer(i as i32);
        }
    }
    VariableValue::Integer(-1)
}

pub fn u_inconf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn peekdw(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dbglevel(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn scrtext(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn showstat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pagestat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tobigstr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn toboolean(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tobyte(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn todate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn todreal(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn toedate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tointeger(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tomoney(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn toreal(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tosbyte(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tosword(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn totime(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tounsigned(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn toword(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mixed(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn alias(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confreg(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confexp(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confsel(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confsys(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confmw(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn lprinted(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn isnonstop(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn errcorrect(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confalias(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn useralias(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn curuser(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn chatstat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn defans(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn lastans(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn meganum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn evttimeadj(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn isbitset(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fmtreal(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn flagcnt(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn kbdbufsize(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pplbufsize(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn kbdfilused(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn lomsgnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn himsgnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn drivespace(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn outbytes() -> VariableValue {
    VariableValue::Integer(0)
}
pub fn hiconfnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn inbytes(interpreter: &mut Interpreter) -> VariableValue {
    VariableValue::Integer(interpreter.ctx.inbytes())
}

pub fn crc32(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pcbmac(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn actmsgnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn stackleft(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn stackerr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn dgetalias(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dbof(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dchanged(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ddecimals(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ddeleted(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn deof(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn derr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dfields(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dlength(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dname(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dreccount(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn drecno(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dtype(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fnext(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dnext(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn toddate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dcloseall(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dopen(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dclose(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dsetalias(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dpack(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dlockf(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dlock(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dlockr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dunlock(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dnopen(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dnclose(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dncloseall(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dnew(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dadd(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dappend(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dtop(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dgo(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dbottom(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dskip(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dblank(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ddelete(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn drecall(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dtag(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dseek(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dfblank(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dget(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dput(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dfcopy(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dselect(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn dchkstat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn pcbaccount(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pcbaccstat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn derrmsg(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn account(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn scanmsghdr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn checkrip(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ripver(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn qwklimits(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn findfirst(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn findnext(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn uselmrs(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn confinfo(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tinkey(interpreter: &mut Interpreter) -> Res<VariableValue> {
    inkey(interpreter)
}
pub fn cwd(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn instrr(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fdordaka(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fdordorg(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fdordarea(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn fdoqrd(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn getdrive(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn setdrive(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn bs2i(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn bd2i(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn i2bs(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn i2bd(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn ftell(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
