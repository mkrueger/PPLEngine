#![allow(clippy::needless_pass_by_value)]

use regex::Regex;
use substring::Substring;

use crate::{interpreter::{ VariableValue, Interpreter}};

use super::get_int;

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
pub fn mid(str: VariableValue, pos: VariableValue, chars: VariableValue) -> VariableValue {
    let mut pos = get_int(&pos) - 1; // 1 based
    let mut chars = get_int(&chars);
    if chars <= 0 {
        return VariableValue::String(String::new());
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
    VariableValue::String(res)
}

pub fn left(str: VariableValue, chars: VariableValue) -> VariableValue {
    let mut chars = get_int(&chars);
    if chars <= 0 {
        return VariableValue::String(String::new());
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
    VariableValue::String(res)
}

pub fn right(str: VariableValue, chars: VariableValue) -> VariableValue {
    let chars = get_int(&chars);
    if chars <= 0 {
        return VariableValue::String(String::new());
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
    VariableValue::String(res)
}

pub fn space(chars: VariableValue) -> VariableValue {
    let mut chars = get_int(&chars);
    if chars <= 0 {
        return VariableValue::String(String::new());
    }
    let mut res = String::new();
    while chars > 0 {
        res.push(' ');
        chars -= 1;
    }
    VariableValue::String(res)
}

pub fn ferr(interpreter: &mut Interpreter, channel: VariableValue) -> VariableValue {
    let channel = get_int(&channel);
    VariableValue::Boolean(interpreter.io.ferr(channel as usize))
}

pub fn chr(chr: VariableValue) -> VariableValue {
    let c = get_int(&chr);
    if c <= 0 {
        return VariableValue::String(String::new());
    }
    // undocumented: returns a space for c > 255
    if c > 255 {
        return VariableValue::String(" ".to_string());
    }
    let mut res = String::new();
    unsafe {
        res.push(char::from_u32_unchecked(c as u32));
    }
    VariableValue::String(res)
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
pub fn replace(_str: VariableValue, _old: VariableValue, _new: VariableValue) -> VariableValue {
    panic!("TODO")
}

/// Remove all occurences of a given character in a string
/// # Arguments
///  * `str` - A string value
///  * `ch` - A string with the character to remove
pub fn strip(_str: VariableValue, _ch: VariableValue) -> VariableValue {
    panic!("TODO")
}

/// Remove @X codes from a string
/// # Arguments
///  * `str` - A string value
/// # Returns
/// A string without any @X codes
pub fn strip_atx(_str: VariableValue) -> VariableValue {
    panic!("TODO")
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

pub fn random(_upper: VariableValue) -> VariableValue {
    panic!("TODO")
}

pub fn date() -> VariableValue {
    panic!("TODO")
}

pub fn time() -> VariableValue {
    panic!("TODO")
}

pub fn u_name(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_ldate(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
}
pub fn u_ltime(interpreter: &Interpreter) -> VariableValue {
    panic!("TODO") // TODO
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
pub fn inkey(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn tostring(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn ppepath(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn valdate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn valtime(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn pcbnode(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn readline(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn sysopsec(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn onlocal(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn un_stat(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn un_name(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn un_city(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn un_oper(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn cursec(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn gettoken(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn exist(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn i2s(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn s2i(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
    interpreter.ctx.print("\x1B[6n");
    let str = interpreter.ctx.read(); 
    let re = Regex::new(r"\x1B\[(\d+);(\d+)R").unwrap();
    let xy = re.captures(&str).unwrap();
    VariableValue::Integer(i32::from_str_radix(&xy[0], 10).unwrap())
}

pub fn gety(interpreter: &mut Interpreter) -> VariableValue {
    interpreter.ctx.print("\x1B[6n");
    let str = interpreter.ctx.read(); 
    let re = Regex::new(r"\x1B\[(\d+);(\d+)R").unwrap();
    let xy = re.captures(&str).unwrap();
    VariableValue::Integer(i32::from_str_radix(&xy[1], 10).unwrap())
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
pub fn abs(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn ppename(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn mkdate(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn curcolor(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn kinkey(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn minkey(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn maxnode(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn tokcount(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn u_recnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn outbytes(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn hiconfnum(_x: VariableValue) -> VariableValue {
    panic!("TODO")
}
pub fn inbytes(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
pub fn tinkey(_x: VariableValue) -> VariableValue {
    panic!("TODO")
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
