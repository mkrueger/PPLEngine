use std::fmt;

use crate::output_keyword;

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Money(f64),
    Integer(i32),
    Unsigned(u32),
    String(String),
    Real(f64),
    Boolean(bool),
    Builtin(&'static str),
}

impl Constant {
    pub const AUTO: Constant = Constant::Builtin("AUTO");
    pub const BELL: Constant = Constant::Builtin("BELL");
    pub const DEFS: Constant = Constant::Builtin("DEFS");
    pub const ECHODOTS: Constant = Constant::Builtin("ECHODOTS");
    pub const ERASELINE: Constant = Constant::Builtin("ERASELINE");
    pub const FCL: Constant = Constant::Builtin("FCL");
    pub const FIELDLEN: Constant = Constant::Builtin("FIELDLEN");
    pub const FNS: Constant = Constant::Builtin("FNS");
    pub const F_EXP: Constant = Constant::Builtin("F_EXP");
    pub const F_MW: Constant = Constant::Builtin("F_MW");
    pub const F_REG: Constant = Constant::Builtin("F_REG");
    pub const F_SEL: Constant = Constant::Builtin("F_SEL");
    pub const F_SYS: Constant = Constant::Builtin("F_SYS");
    pub const GRAPH: Constant = Constant::Builtin("GRAPH");
    pub const GUIDE: Constant = Constant::Builtin("GUIDE");
    pub const HIGHASCII: Constant = Constant::Builtin("HIGHASCII");
    pub const LANG: Constant = Constant::Builtin("LANG");
    pub const LFAFTER: Constant = Constant::Builtin("LFAFTER");
    pub const LFBEFORE: Constant = Constant::Builtin("LFBEFORE");
    pub const LOGIT: Constant = Constant::Builtin("LOGIT");
    pub const LOGITLEFT: Constant = Constant::Builtin("LOGITLEFT");
    pub const NC: Constant = Constant::Builtin("NC");
    pub const NEWLINE: Constant = Constant::Builtin("NEWLINE");
    pub const NOCLEAR: Constant = Constant::Builtin("NOCLEAR");
    pub const O_RD: Constant = Constant::Builtin("O_RD");
    pub const O_RW: Constant = Constant::Builtin("O_RW");
    pub const O_WR: Constant = Constant::Builtin("O_WR");
    pub const SEC: Constant = Constant::Builtin("SEC");
    pub const SEEK_CUR: Constant = Constant::Builtin("SEEK_CUR");
    pub const SEEK_END: Constant = Constant::Builtin("SEEK_END");
    pub const SEEK_SET: Constant = Constant::Builtin("SEEK_SET");
    pub const STACKED: Constant = Constant::Builtin("STACKED");
    pub const S_DB: Constant = Constant::Builtin("S_DB");
    pub const S_DN: Constant = Constant::Builtin("S_DN");
    pub const S_DR: Constant = Constant::Builtin("S_DR");
    pub const S_DW: Constant = Constant::Builtin("S_DW");
    pub const UPCASE: Constant = Constant::Builtin("UPCASE");
    pub const WORDWRAP: Constant = Constant::Builtin("WORDWRAP");
    pub const YESNO: Constant = Constant::Builtin("YESNO");

    // Debug
    pub const START_BAL: Constant = Constant::Builtin("START_BAL");
    pub const START_SESSION: Constant = Constant::Builtin("START_SESSION");
    pub const DEB_CALL: Constant = Constant::Builtin("DEB_CALL");
    pub const DEB_TIME: Constant = Constant::Builtin("DEB_TIME");
    pub const DEB_MSGREAD: Constant = Constant::Builtin("DEB_MSGREAD");
    pub const DEB_MSGCAP: Constant = Constant::Builtin("DEB_MSGCAP");
    pub const DEB_MSGWRITE: Constant = Constant::Builtin("DEB_MSGWRITE");
    pub const DEB_MSGECHOED: Constant = Constant::Builtin("DEB_MSGECHOED");
    pub const DEB_MSGPRIVATE: Constant = Constant::Builtin("DEB_MSGPRIVATE");
    pub const DEB_DOWNFILE: Constant = Constant::Builtin("DEB_DOWNFILE");
    pub const DEB_DOWNBYTES: Constant = Constant::Builtin("DEB_DOWNBYTES");
    pub const DEB_CHAT: Constant = Constant::Builtin("DEB_CHAT");
    pub const DEB_TPU: Constant = Constant::Builtin("DEB_TPU");
    pub const DEB_SPECIAL: Constant = Constant::Builtin("DEB_SPECIAL");
    pub const CRED_UPFILE: Constant = Constant::Builtin("CRED_UPFILE");
    pub const CRED_UPBYTES: Constant = Constant::Builtin("CRED_UPBYTES");
    pub const CRED_SPECIAL: Constant = Constant::Builtin("CRED_SPECIAL");
    pub const SEC_DROP: Constant = Constant::Builtin("SEC_DROP");
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Money(i) => write!(f, "${i}"),
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Unsigned(i) => write!(f, "{i}"),
            Constant::String(str) => write!(f, "\"{str}\""),
            Constant::Real(i) => write!(f, "{i}"),
            Constant::Boolean(b) => write!(
                f,
                "{}",
                if *b {
                    output_keyword("True")
                } else {
                    output_keyword("False")
                }
            ),
            Constant::Builtin(s) => write!(f, "{s}"),
        }
    }
}
