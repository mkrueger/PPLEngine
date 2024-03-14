use std::fmt;

use super::{VariableData, VariableType};

#[allow(non_camel_case_types)]
#[derive(Debug, PartialEq, Clone)]
pub enum Constant {
    Money(i32),
    Integer(i32),
    Unsigned(u32),
    String(String),
    Real(f64),
    Boolean(bool),
    Builtin(&'static BuiltinConst),
}

#[derive(Debug, PartialEq, Clone)]
pub struct BuiltinConst {
    pub name: &'static str,
    pub value: i32,
}

impl BuiltinConst {
    pub const ACC_CUR_BAL: BuiltinConst = BuiltinConst::new("ACC_CUR_BAL", 0x04);
    pub const ACC_MSGREAD: BuiltinConst = BuiltinConst::new("ACC_MSGREAD", 0x02);
    pub const ACC_MSGWRITE: BuiltinConst = BuiltinConst::new("ACC_MSGWRITE", 0x03);
    pub const ACC_STAT: BuiltinConst = BuiltinConst::new("ACC_STAT", 0x00);
    pub const ACC_TIME: BuiltinConst = BuiltinConst::new("ACC_TIME", 0x01);
    pub const ATTACH_LIM_P: BuiltinConst = BuiltinConst::new("ATTACH_LIM_P", 0x03);
    pub const ATTACH_LIM_U: BuiltinConst = BuiltinConst::new("ATTACH_LIM_U", 0x02);
    pub const AUTO: BuiltinConst = BuiltinConst::new("AUTO", 0x02000);
    pub const BELL: BuiltinConst = BuiltinConst::new("BELL", 0x00800);
    pub const CHRG_CALL: BuiltinConst = BuiltinConst::new("CHRG_CALL", 0x01);
    pub const CHRG_CHAT: BuiltinConst = BuiltinConst::new("CHRG_CHAT", 0x04);
    pub const CHRG_DOWNBYTES: BuiltinConst = BuiltinConst::new("CHRG_DOWNBYTES", 0x0B);
    pub const CHRG_DOWNFILE: BuiltinConst = BuiltinConst::new("CHRG_DOWNFILE", 0x0A);
    pub const CHRG_MSGCAP: BuiltinConst = BuiltinConst::new("CHRG_MSGCAP", 0x06);
    pub const CHRG_MSGECHOED: BuiltinConst = BuiltinConst::new("CHRG_MSGECHOED", 0x08);
    pub const CHRG_MSGPRIVATE: BuiltinConst = BuiltinConst::new("CHRG_MSGPRIVATE", 0x09);
    pub const CHRG_MSGREAD: BuiltinConst = BuiltinConst::new("CHRG_MSGREAD", 0x05);
    pub const CHRG_MSGWRITE: BuiltinConst = BuiltinConst::new("CHRG_MSGWRITE", 0x07);
    pub const CHRG_PEAKTIME: BuiltinConst = BuiltinConst::new("CHRG_PEAKTIME", 0x03);
    pub const CHRG_TIME: BuiltinConst = BuiltinConst::new("CHRG_TIME", 0x02);
    pub const CMAXMSGS: BuiltinConst = BuiltinConst::new("CMAXMSGS", 0x01);
    pub const CRC_FILE: BuiltinConst = BuiltinConst::new("CRC_FILE", 0x01);
    pub const CRC_STR: BuiltinConst = BuiltinConst::new("CRC_STR", 0x00);
    pub const CRED_SPECIAL: BuiltinConst = BuiltinConst::new("CRED_SPECIAL", 0x10);
    pub const CRED_UPBYTES: BuiltinConst = BuiltinConst::new("CRED_UPBYTES", 0x0F);
    pub const CRED_UPFILE: BuiltinConst = BuiltinConst::new("CRED_UPFILE", 0x0E);
    pub const CUR_USER: BuiltinConst = BuiltinConst::new("CUR_USER", 0);
    pub const DEB_CALL: BuiltinConst = BuiltinConst::new("DEB_CALL", 0x02);
    pub const DEB_CHAT: BuiltinConst = BuiltinConst::new("DEB_CHAT", 0x0B);
    pub const DEB_DOWNBYTES: BuiltinConst = BuiltinConst::new("DEB_DOWNBYTES", 0x0A);
    pub const DEB_DOWNFILE: BuiltinConst = BuiltinConst::new("DEB_DOWNFILE", 0x09);
    pub const DEB_MSGCAP: BuiltinConst = BuiltinConst::new("DEB_MSGCAP", 0x05);
    pub const DEB_MSGECHOED: BuiltinConst = BuiltinConst::new("DEB_MSGECHOED", 0x07);
    pub const DEB_MSGPRIVATE: BuiltinConst = BuiltinConst::new("DEB_MSGPRIVATE", 0x08);
    pub const DEB_MSGREAD: BuiltinConst = BuiltinConst::new("DEB_MSGREAD", 0x04);
    pub const DEB_MSGWRITE: BuiltinConst = BuiltinConst::new("DEB_MSGWRITE", 0x06);
    pub const DEB_SPECIAL: BuiltinConst = BuiltinConst::new("DEB_SPECIAL", 0x0D);
    pub const DEB_TIME: BuiltinConst = BuiltinConst::new("DEB_TIME", 0x03);
    pub const DEB_TPU: BuiltinConst = BuiltinConst::new("DEB_TPU", 0x0C);
    pub const DEFS: BuiltinConst = BuiltinConst::new("DEFS", 0x00);
    pub const ECHODOTS: BuiltinConst = BuiltinConst::new("ECHODOTS", 0x00001);
    pub const ERASELINE: BuiltinConst = BuiltinConst::new("ERASELINE", 0x00020);
    pub const FALSE: BuiltinConst = BuiltinConst::new("FALSE", 0x00);
    pub const FCL: BuiltinConst = BuiltinConst::new("FCL", 0x02);
    pub const FIELDLEN: BuiltinConst = BuiltinConst::new("FIELDLEN", 0x00002);
    pub const FNS: BuiltinConst = BuiltinConst::new("FNS", 0x01);
    pub const F_EXP: BuiltinConst = BuiltinConst::new("F_EXP", 0x02);
    pub const F_MW: BuiltinConst = BuiltinConst::new("F_MW", 0x10);
    pub const F_NET: BuiltinConst = BuiltinConst::new("F_NET", 0x20);
    pub const F_REG: BuiltinConst = BuiltinConst::new("F_REG", 0x01);
    pub const F_SEL: BuiltinConst = BuiltinConst::new("F_SEL", 0x04);
    pub const F_SYS: BuiltinConst = BuiltinConst::new("F_SYS", 0x08);
    pub const GRAPH: BuiltinConst = BuiltinConst::new("GRAPH", 0x01);
    pub const GUIDE: BuiltinConst = BuiltinConst::new("GUIDE", 0x00004);
    pub const HDR_ACTIVE: BuiltinConst = BuiltinConst::new("HDR_ACTIVE", 0x0E);
    pub const HDR_BLOCKS: BuiltinConst = BuiltinConst::new("HDR_BLOCKS", 0x04);
    pub const HDR_DATE: BuiltinConst = BuiltinConst::new("HDR_DATE", 0x05);
    pub const HDR_ECHO: BuiltinConst = BuiltinConst::new("HDR_ECHO", 0x0F);
    pub const HDR_FROM: BuiltinConst = BuiltinConst::new("HDR_FROM", 0x0B);
    pub const HDR_MSGNUM: BuiltinConst = BuiltinConst::new("HDR_MSGNUM", 0x02);
    pub const HDR_MSGREF: BuiltinConst = BuiltinConst::new("HDR_MSGREF", 0x03);
    pub const HDR_PWD: BuiltinConst = BuiltinConst::new("HDR_PWD", 0x0D);
    pub const HDR_REPLY: BuiltinConst = BuiltinConst::new("HDR_REPLY", 0x0A);
    pub const HDR_RPLYDATE: BuiltinConst = BuiltinConst::new("HDR_RPLYDATE", 0x08);
    pub const HDR_RPLYTIME: BuiltinConst = BuiltinConst::new("HDR_RPLYTIME", 0x09);
    pub const HDR_STATUS: BuiltinConst = BuiltinConst::new("HDR_STATUS", 0x01);
    pub const HDR_SUBJ: BuiltinConst = BuiltinConst::new("HDR_SUBJ", 0x0C);
    pub const HDR_TIME: BuiltinConst = BuiltinConst::new("HDR_TIME", 0x06);
    pub const HDR_TO: BuiltinConst = BuiltinConst::new("HDR_TO", 0x07);
    pub const HIGHASCII: BuiltinConst = BuiltinConst::new("HIGHASCII", 0x01000);
    pub const LANG: BuiltinConst = BuiltinConst::new("LANG", 0x04);
    pub const LFAFTER: BuiltinConst = BuiltinConst::new("LFAFTER", 0x00100);
    pub const LFBEFORE: BuiltinConst = BuiltinConst::new("LFBEFORE", 0x00080);
    pub const LOGIT: BuiltinConst = BuiltinConst::new("LOGIT", 0x08000);
    pub const LOGITLEFT: BuiltinConst = BuiltinConst::new("LOGITLEFT", 0x10000);
    pub const MAXMSGS: BuiltinConst = BuiltinConst::new("MAXMSGS", 0x00);
    pub const NC: BuiltinConst = BuiltinConst::new("NC", 0x00);
    pub const NEWBALANCE: BuiltinConst = BuiltinConst::new("NEWBALANCE", 0x00);
    pub const NEWLINE: BuiltinConst = BuiltinConst::new("NEWLINE", 0x00040);
    pub const NOCLEAR: BuiltinConst = BuiltinConst::new("NOCLEAR", 0x00400);
    pub const NO_USER: BuiltinConst = BuiltinConst::new("NO_USER", -1);
    pub const O_RD: BuiltinConst = BuiltinConst::new("O_RD", 0x00);
    pub const O_RW: BuiltinConst = BuiltinConst::new("O_RW", 0x02);
    pub const O_WR: BuiltinConst = BuiltinConst::new("O_WR", 0x01);
    pub const PAY_UPBYTES: BuiltinConst = BuiltinConst::new("PAY_UPBYTES", 0x0D);
    pub const PAY_UPFILE: BuiltinConst = BuiltinConst::new("PAY_UPFILE", 0x0C);
    pub const SEC: BuiltinConst = BuiltinConst::new("SEC", 0x02);
    pub const SEC_DROP: BuiltinConst = BuiltinConst::new("SEC_DROP", 0x11);
    pub const SEEK_CUR: BuiltinConst = BuiltinConst::new("SEEK_CUR", 0x01);
    pub const SEEK_END: BuiltinConst = BuiltinConst::new("SEEK_END", 0x02);
    pub const SEEK_SET: BuiltinConst = BuiltinConst::new("SEEK_SET", 0x00);
    pub const STACKED: BuiltinConst = BuiltinConst::new("STACKED", 0x00010);
    pub const START_BAL: BuiltinConst = BuiltinConst::new("START_BAL", 0x00);
    pub const START_SESSION: BuiltinConst = BuiltinConst::new("START_SESSION", 0x01);
    pub const STK_LIMIT: BuiltinConst = BuiltinConst::new("STK_LIMIT", 6022 + 1024);
    pub const S_DB: BuiltinConst = BuiltinConst::new("S_DB", 0x03);
    pub const S_DN: BuiltinConst = BuiltinConst::new("S_DN", 0x00);
    pub const S_DR: BuiltinConst = BuiltinConst::new("S_DR", 0x01);
    pub const S_DW: BuiltinConst = BuiltinConst::new("S_DW", 0x02);
    pub const TRUE: BuiltinConst = BuiltinConst::new("TRUE", 0x01);
    pub const UPCASE: BuiltinConst = BuiltinConst::new("UPCASE", 0x00008);
    pub const WARNLEVEL: BuiltinConst = BuiltinConst::new("WARNLEVEL", 0x0E);
    pub const WORDWRAP: BuiltinConst = BuiltinConst::new("WORDWRAP", 0x00200);
    pub const YESNO: BuiltinConst = BuiltinConst::new("YESNO", 0x04000);

    const fn new(name: &'static str, value: i32) -> Self {
        Self { name, value }
    }
}

impl Constant {
    pub fn get_var_type(&self) -> VariableType {
        match self {
            Constant::Money(_) => VariableType::Money,
            Constant::Unsigned(_) => VariableType::Unsigned,
            Constant::String(_) => VariableType::String,
            Constant::Real(_) => VariableType::Real,
            Constant::Boolean(_) => VariableType::Boolean,
            Constant::Integer(_) | Constant::Builtin(_) => VariableType::Integer,
        }
    }

    pub fn get_value(&self) -> super::Variable {
        match self {
            Constant::Money(i) => {
                super::Variable::new(VariableType::Money, VariableData { money_value: *i })
            }
            Constant::Integer(i) => {
                super::Variable::new(VariableType::Integer, VariableData { int_value: *i })
            }
            Constant::Unsigned(i) => {
                super::Variable::new(VariableType::Unsigned, VariableData { unsigned_value: *i })
            }
            Constant::String(s) => super::Variable::new_string(s.clone()),
            Constant::Real(i) => {
                super::Variable::new(VariableType::DoubleReal, VariableData { dreal_value: *i })
            }
            Constant::Boolean(b) => {
                super::Variable::new(VariableType::Boolean, VariableData { bool_value: *b })
            }
            Constant::Builtin(s) => {
                super::Variable::new(VariableType::Integer, VariableData { int_value: s.value })
            }
        }
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constant::Money(i) => write!(f, "${i}"),
            Constant::Integer(i) => write!(f, "{i}"),
            Constant::Unsigned(i) => write!(f, "{i}"),
            Constant::String(str) => write!(f, "\"{str}\""),
            Constant::Real(i) => write!(f, "{i}"),
            Constant::Boolean(b) => write!(f, "{}", if *b { "TRUE" } else { "FALSE" }),
            Constant::Builtin(s) => write!(f, "{}", s.name),
        }
    }
}
