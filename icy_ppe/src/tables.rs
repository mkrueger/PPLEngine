use crate::ast::{constant::BuiltinConst, BinOp, VariableType};

// Statement Variable-Types, 0 = none / aa = not defined
//				 1- f= number of exp
//				11-1f= number of exp, exp 1 is a var
//              21-2f= number of exp, exp 2 is a var
//              0xf6 = special case procedure
//              0xf7 = special case dlockg
//              0xf8 = special case dcreate
//              0xf9 = special case sort
//				0xfa = varies exp, exp 1 is a var (redim)
//              0xfc = varies var (pop)
//              0xfd = label (goto)
//			    0xfe = varies exp (println)
//			    0xff = special case if
pub const STATEMENT_SIGNATURE_TABLE: [i16; 227] = [
    0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01, 0xfd, //0
    0x12, 0xfe, 0xfe, 0xff, 0x02, 0x02, 0x02, 0x22, 0x04, 0x04, 0x04, 0x01, 0x22, 0xfe, 0xfe,
    0x00, //1
    0x01, 0x03, 0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x01, 0x02, 0x26, 0x23, 0x23, 0x23, 0x23,
    0x23, //2
    0x23, 0xfd, 0x00, 0x25, 0x00, 0x00, 0x00, 0x00, 0x01, 0x01, 0x11, 0x11, 0x00, 0x01, 0x01,
    0x11, //3
    0x24, 0x02, 0x00, 0x24, 0x00, 0xfe, 0xfc, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0x00,
    0x00, //4
    0x03, 0x23, 0x00, 0x00, 0x01, 0x01, 0x01, 0x06, 0x0a, 0xf2, 0xf2, 0x02, 0x02, 0xf2, 0x02,
    0x01, //5
    0x01, 0x00, 0x00, 0x00, 0x22, 0x22, 0x00, 0x09, 0x00, 0x00, 0x01, 0x00, 0xfe, 0xfe, 0xfe,
    0xfe, //6
    0x02, 0x01, 0x02, 0x01, 0x00, 0x00, 0x00, 0x00, 0x03, 0x01, 0x23, 0x03, 0x01, 0x01, 0x11,
    0xfe, //7
    0xfe, 0x02, 0x12, 0x02, 0x01, 0x01, 0x01, 0xfa, 0x02, 0x02, 0x00, 0x00, 0x00, 0x01, 0x01,
    0x01, //8
    0x01, 0x01, 0x01, 0x01, 0x01, 0x01, 0xf9, 0x0a, 0xf2, 0x02, 0x22, 0x00, 0xfe, 0xfe, 0x22,
    0x02, //9
    0x23, 0x03, 0x22, 0x02, 0x23, 0x03, 0x12, 0x12, 0x00, 0x00, 0x02, 0x01, 0x00, 0xaa, 0xaa,
    0xaa, //A
    0xf6, 0x00, 0xaa, 0x00, 0xaa, 0x01, 0xf8, 0x03, 0x01, 0x02, 0x01, 0x00, 0x01, 0x02, 0xf7,
    0x01, //B
    0x03, 0x02, 0x02, 0x01, 0x01, 0x01, 0x01, 0x01, 0x02, 0x01, 0x02, 0x01, 0x01, 0x01, 0x02,
    0x02, //C
    0x02, 0x33, 0x03, 0x04, 0x01, 0x02, 0x05, 0x03, 0x02, 0x02, 0x01, 0x03, 0x01, 0x01, 0x02,
    0x02, //D
    0x01, 0x01, 0x01, 0x02, 0x03, 0x02, 0x03, 0x04, 0x03, 0x01, 0x02, //E
];

pub const TYPE_NAMES: [VariableType; 18] = [
    VariableType::Boolean,
    VariableType::Unsigned,
    VariableType::Date,
    VariableType::EDate,
    VariableType::Integer,
    VariableType::Money,
    VariableType::Float,
    VariableType::String,
    VariableType::Time,
    VariableType::Byte,
    VariableType::Word,
    VariableType::SByte,
    VariableType::SWord,
    VariableType::BigStr,
    VariableType::Float,
    VariableType::Unknown,
    VariableType::Unknown,
    VariableType::DDate,
];

pub const CONSTANT_2_NAMES: [BuiltinConst; 18] = [
    BuiltinConst::START_BAL,
    BuiltinConst::START_SESSION,
    BuiltinConst::DEB_CALL,
    BuiltinConst::DEB_TIME,
    BuiltinConst::DEB_MSGREAD,
    BuiltinConst::DEB_MSGCAP,
    BuiltinConst::DEB_MSGWRITE,
    BuiltinConst::DEB_MSGECHOED,
    BuiltinConst::DEB_MSGPRIVATE,
    BuiltinConst::DEB_DOWNFILE,
    BuiltinConst::DEB_DOWNBYTES,
    BuiltinConst::DEB_CHAT,
    BuiltinConst::DEB_TPU,
    BuiltinConst::DEB_SPECIAL,
    BuiltinConst::CRED_UPFILE,
    BuiltinConst::CRED_UPBYTES,
    BuiltinConst::CRED_SPECIAL,
    BuiltinConst::SEC_DROP,
];

pub const CONSTANT_CONFERENCE_NAMES: [BuiltinConst; 5] = [
    BuiltinConst::F_EXP,
    BuiltinConst::F_MW,
    BuiltinConst::F_REG,
    BuiltinConst::F_SEL,
    BuiltinConst::F_SYS,
];

pub const CONSTANT_NAMES_DISPLAY: [BuiltinConst; 4] = [
    BuiltinConst::DEFS,
    BuiltinConst::GRAPH,
    BuiltinConst::LANG,
    BuiltinConst::SEC,
];

pub const CONSTANT_FACCESS_NAMES: [BuiltinConst; 3] =
    [BuiltinConst::O_RD, BuiltinConst::O_RW, BuiltinConst::O_WR];

pub const CONSTANT_SEEK_NAMES: [BuiltinConst; 3] = [
    BuiltinConst::SEEK_CUR,
    BuiltinConst::SEEK_END,
    BuiltinConst::SEEK_SET,
];

pub const CONSTANT_OPENFLAGS_NAMES: [BuiltinConst; 4] = [
    BuiltinConst::S_DB,
    BuiltinConst::S_DN,
    BuiltinConst::S_DR,
    BuiltinConst::S_DW,
];

pub const CONSTANT_1_NAMES: [BuiltinConst; 18] = [
    BuiltinConst::AUTO,
    BuiltinConst::BELL,
    BuiltinConst::DEFS,
    BuiltinConst::ECHODOTS,
    BuiltinConst::ERASELINE,
    BuiltinConst::FIELDLEN,
    BuiltinConst::GUIDE,
    BuiltinConst::HIGHASCII,
    BuiltinConst::LFAFTER,
    BuiltinConst::LFBEFORE,
    BuiltinConst::LOGIT,
    BuiltinConst::LOGITLEFT,
    BuiltinConst::NEWLINE,
    BuiltinConst::NOCLEAR,
    BuiltinConst::STACKED,
    BuiltinConst::UPCASE,
    BuiltinConst::WORDWRAP,
    BuiltinConst::YESNO,
];

pub const CONSTANT_LINECOUNT_NAMES: [BuiltinConst; 3] =
    [BuiltinConst::FCL, BuiltinConst::FNS, BuiltinConst::NC];

pub const BIN_EXPR: [BinOp; 19] = [
    BinOp::Add, // END
    BinOp::Add, // CPAR
    BinOp::Add, // UPLUS
    BinOp::Sub, // UMINUS
    BinOp::PoW,
    BinOp::Mul,
    BinOp::Div,
    BinOp::Mod,
    BinOp::Add,
    BinOp::Sub,
    BinOp::Eq,
    BinOp::NotEq,
    BinOp::Lower,
    BinOp::LowerEq,
    BinOp::Greater,
    BinOp::GreaterEq,
    BinOp::Add, // NOT
    BinOp::And,
    BinOp::Or,
];

#[repr(i16)]
#[allow(dead_code)]
#[allow(non_camel_case_types)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum FuncOpCode {
    END = 0,
    CPAR = -1,
    UPLUS = -2,
    UMINUS = -3,
    EXP = -4,
    TIMES = -5,
    DIVIDE = -6,
    MOD = -7,
    PLUS = -8,
    MINUS = -9,
    EQ = -10,
    NE = -11,
    LT = -12,
    LE = -13,
    GT = -14,
    GE = -15,
    NOT = -16,
    AND = -17,
    OR = -18,
    OPAR = -19,
    LEN = -20,
    LOWER = -21,
    UPPER = -22,
    MID = -23,
    LEFT = -24,
    RIGHT = -25,
    SPACE = -26,
    FERR = -27,
    CHR = -28,
    ASC = -29,
    INSTR = -30,
    ABORT = -31,
    LTRIM = -32,
    RTRIM = -33,
    TRIM = -34,
    RANDOM = -35,
    DATE = -36,
    TIME = -37,
    U_NAME = -38,
    U_LDATE = -39,
    U_LTIME = -40,
    U_LDIR = -41,
    U_LOGONS = -42,
    U_FUL = -43,
    U_FDL = -44,
    U_BDLDAY = -45,
    U_TIMEON = -46,
    U_BDL = -47,
    U_BUL = -48,
    YEAR = -49,
    MONTH = -50,
    DAY = -51,
    DOW = -52,
    HOUR = -53,
    MIN = -54,
    SEC = -55,
    TIMEAP = -56,
    VER = -57,
    NOCHAR = -58,
    YESCHAR = -59,
    STRIPATX = -60,
    REPLACE = -61,
    STRIP = -62,
    INKEY = -63,
    TOSTRING = -64,
    MASK_PWD = -65,
    MASK_ALPHA = -66,
    MASK_NUM = -67,
    MASK_ALNUM = -68,
    MASK_FILE = -69,
    MASK_PATH = -70,
    MASK_ASCII = -71,
    CURCONF = -72,
    PCBDAT = -73,
    PPEPATH = -74,
    VALDATE = -75,
    VALTIME = -76,
    U_MSGRD = -77,
    U_MSGWR = -78,
    PCBNODE = -79,
    READLINE = -80,
    SYSOPSEC = -81,
    ONLOCAL = -82,
    UN_STAT = -83,
    UN_NAME = -84,
    UN_CITY = -85,
    UN_OPER = -86,
    CURSEC = -87,
    GETTOKEN = -88,
    MINLEFT = -89,
    MINON = -90,
    GETENV = -91,
    CALLID = -92,
    REGAL = -93,
    REGAH = -94,
    REGBL = -95,
    REGBH = -96,
    REGCL = -97,
    REGCH = -98,
    REGDL = -99,
    REGDH = -100,
    REGAX = -101,
    REGBX = -102,
    REGCX = -103,
    REGDX = -104,
    REGSI = -105,
    REGDI = -106,
    REGF = -107,
    REGCF = -108,
    REGDS = -109,
    REGES = -110,
    B2W = -111,
    PEEKB = -112,
    PEEKW = -113,
    MKADDR = -114,
    EXIST = -115,
    I2S = -116,
    S2I = -117,
    CARRIER = -118,
    TOKENSTR = -119,
    CDON = -120,
    LANGEXT = -121,
    ANSION = -122,
    VALCC = -123,
    FMTCC = -124,
    CCTYPE = -125,
    GETX = -126,
    GETY = -127,
    BAND = -128,
    BOR = -129,
    BXOR = -130,
    BNOT = -131,
    U_PWDHIST = -132,
    U_PWDLC = -133,
    U_PWDTC = -134,
    U_STAT = -135,
    DEFCOLOR = -136,
    ABS = -137,
    GRAFMODE = -138,
    PSA = -139,
    FILEINF = -140,
    PPENAME = -141,
    MKDATE = -142,
    CURCOLOR = -143,
    KINKEY = -144,
    MINKEY = -145,
    MAXNODE = -146,
    SLPATH = -147,
    HELPPATH = -148,
    TEMPPATH = -149,
    MODEM = -150,
    LOGGEDON = -151,
    CALLNUM = -152,
    MGETBYTE = -153,
    TOKCOUNT = -154,
    U_RECNUM = -155,
    U_INCONF = -156,
    PEEKDW = -157,
    DBGLEVEL = -158,
    SCRTEXT = -159,
    SHOWSTAT = -160,
    PAGESTAT = -161,

    REPLACESTR = -162,
    STRIPSTR = -163,
    TOBIGSTR = -164,
    TOBOOLEAN = -165,
    TOBYTE = -166,
    TODATE = -167,
    TODREAL = -168,
    TOEDATE = -169,
    TOINTEGER = -170,
    TOMONEY = -171,
    TOREAL = -172,
    TOSBYTE = -173,
    TOSWORD = -174,
    TOTIME = -175,
    TOUNSIGNED = -176,
    TOWORD = -177,
    MIXED = -178,
    ALIAS = -179,
    CONFREG = -180,
    CONFEXP = -181,
    CONFSEL = -182,
    CONFSYS = -183,
    CONFMW = -184,
    LPRINTED = -185,
    ISNONSTOP = -186,
    ERRCORRECT = -187,
    CONFALIAS = -188,
    USERALIAS = -189,
    CURUSER = -190,
    U_LMR = -191,
    CHATSTAT = -192,
    DEFANS = -193,
    LASTANS = -194,
    MEGANUM = -195,
    EVTTIMEADJ = -196,
    ISBITSET = -197,
    FMTREAL = -198,
    FLAGCNT = -199,
    KBDBUFSIZE = -200,
    PPLBUFSIZE = -201,
    KBDFILUSED = -202,
    LOMSGNUM = -203,
    HIMSGNUM = -204,

    DRIVESPACE = -205,
    OUTBYTES = -206,
    HICONFNUM = -207,
    INBYTES = -208,
    CRC32 = -209,
    PCBMAC = -210,
    ACTMSGNUM = -211,
    STACKLEFT = -212,
    STACKERR = -213,

    DGETALIAS = -214,
    DBOF = -215,
    DCHANGED = -216,
    DDECIMALS = -217,
    DDELETED = -218,
    DEOF = -219,
    DERR = -220,
    DFIELDS = -221,
    DLENGTH = -222,
    DNAME = -223,
    DRECCOUNT = -224,
    DRECNO = -225,
    DTYPE = -226,
    FNEXT = -227,
    DNEXT = -228,
    TODDATE = -229,
    DCLOSEALL = -230,
    DOPEN = -231,
    DCLOSE = -232,
    DSETALIAS = -233,
    DPACK = -234,
    DLOCKF = -235,
    DLOCK = -236,
    DLOCKR = -237,
    DUNLOCK = -238,
    DNOPEN = -239,
    DNCLOSE = -240,
    DNCLOSEALL = -241,
    DNEW = -242,
    DADD = -243,
    DAPPEND = -244,
    DTOP = -245,
    DGO = -246,
    DBOTTOM = -247,
    DSKIP = -248,
    DBLANK = -249,
    DDELETE = -250,
    DRECALL = -251,
    DTAG = -252,
    DSEEK = -253,
    DFBLANK = -254,
    DGET = -255,
    DPUT = -256,
    DFCOPY = -257,
    DSELECT = -258,
    DCHKSTAT = -259,

    PCBACCOUNT = -260,
    PCBACCSTAT = -261,
    DERRMSG = -262,
    ACCOUNT = -263,
    SCANMSGHDR = -264,
    CHECKRIP = -265,
    RIPVER = -266,
    QWKLIMITS = -267,
    FINDFIRST = -268,
    FINDNEXT = -269,
    USELMRS = -270,
    CONFINFO = -271,
    TINKEY = -272,
    CWD = -273,
    INSTRR = -274,
    FDORDAKA = -275,
    FDORDORG = -276,
    FDORDAREA = -277,
    FDOQRD = -278,
    GETDRIVE = -279,
    SETDRIVE = -280,
    BS2I = -281,
    BD2I = -282,
    I2BS = -283,
    I2BD = -284,
    FTELL = -285,
    OS = -286,
    SHORT_DESC = -287,
    GetBankBal = -288,
    GetMsgHdr = -289,
    SetMsgHdr = -290,
}
pub const LAST_FUNC: i16 = -290;

#[derive(Debug, PartialEq)]
pub struct FunctionDefinition<'a> {
    pub name: &'a str,
    pub opcode: FuncOpCode,
    pub args: i8,
}

pub fn get_function_definition(str: &str) -> i32 {
    for (i, item) in FUNCTION_DEFINITIONS.iter().enumerate() {
        if item.name.eq_ignore_ascii_case(str) {
            return i as i32;
        }
    }
    -1
}

pub static FUNCTION_DEFINITIONS: [FunctionDefinition; 291] = [
    FunctionDefinition {
        name: "END",
        opcode: FuncOpCode::END,
        args: -1,
    },
    FunctionDefinition {
        name: "CPAR",
        opcode: FuncOpCode::CPAR,
        args: -1,
    },
    FunctionDefinition {
        name: "UPLUS",
        opcode: FuncOpCode::UPLUS,
        args: 0x10,
    },
    FunctionDefinition {
        name: "UMINUS",
        opcode: FuncOpCode::UMINUS,
        args: 0x10,
    },
    FunctionDefinition {
        name: "EXP",
        opcode: FuncOpCode::EXP,
        args: 0x11,
    },
    FunctionDefinition {
        name: "TIMES",
        opcode: FuncOpCode::TIMES,
        args: 0x11,
    },
    FunctionDefinition {
        name: "DIVIDE",
        opcode: FuncOpCode::DIVIDE,
        args: 0x11,
    },
    FunctionDefinition {
        name: "MOD",
        opcode: FuncOpCode::MOD,
        args: 0x11,
    },
    FunctionDefinition {
        name: "PLUS",
        opcode: FuncOpCode::PLUS,
        args: 0x11,
    },
    FunctionDefinition {
        name: "MINUS",
        opcode: FuncOpCode::MINUS,
        args: 0x11,
    },
    FunctionDefinition {
        name: "EQ",
        opcode: FuncOpCode::EQ,
        args: 0x11,
    },
    FunctionDefinition {
        name: "NE",
        opcode: FuncOpCode::NE,
        args: 0x11,
    },
    FunctionDefinition {
        name: "LT",
        opcode: FuncOpCode::LT,
        args: 0x11,
    },
    FunctionDefinition {
        name: "LE",
        opcode: FuncOpCode::LE,
        args: 0x11,
    },
    FunctionDefinition {
        name: "GT",
        opcode: FuncOpCode::GT,
        args: 0x11,
    },
    FunctionDefinition {
        name: "GE",
        opcode: FuncOpCode::GE,
        args: 0x11,
    },
    FunctionDefinition {
        name: "NOT",
        opcode: FuncOpCode::NOT,
        args: 0x10,
    },
    FunctionDefinition {
        name: "AND",
        opcode: FuncOpCode::AND,
        args: 0x11,
    },
    FunctionDefinition {
        name: "OR",
        opcode: FuncOpCode::OR,
        args: 0x11,
    },
    FunctionDefinition {
        name: "OPAR",
        opcode: FuncOpCode::OPAR,
        args: -1,
    },
    // Builtin functions begin here.
    FunctionDefinition {
        name: "Len",
        opcode: FuncOpCode::LEN,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Lower",
        opcode: FuncOpCode::LOWER,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Upper",
        opcode: FuncOpCode::UPPER,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Mid",
        opcode: FuncOpCode::MID,
        args: 0x03,
    },
    FunctionDefinition {
        name: "Left",
        opcode: FuncOpCode::LEFT,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Right",
        opcode: FuncOpCode::RIGHT,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Space",
        opcode: FuncOpCode::SPACE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FErr",
        opcode: FuncOpCode::FERR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Chr",
        opcode: FuncOpCode::CHR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Asc",
        opcode: FuncOpCode::ASC,
        args: 0x01,
    },
    FunctionDefinition {
        name: "InStr",
        opcode: FuncOpCode::INSTR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Abort",
        opcode: FuncOpCode::ABORT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "LTrim",
        opcode: FuncOpCode::LTRIM,
        args: 0x02,
    },
    FunctionDefinition {
        name: "RTrim",
        opcode: FuncOpCode::RTRIM,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Trim",
        opcode: FuncOpCode::TRIM,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Random",
        opcode: FuncOpCode::RANDOM,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Date",
        opcode: FuncOpCode::DATE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Time",
        opcode: FuncOpCode::TIME,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_Name",
        opcode: FuncOpCode::U_NAME,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_LDate",
        opcode: FuncOpCode::U_LDATE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_LTime",
        opcode: FuncOpCode::U_LTIME,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_LDir",
        opcode: FuncOpCode::U_LDIR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_Logons",
        opcode: FuncOpCode::U_LOGONS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_FUl",
        opcode: FuncOpCode::U_FUL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_FDl",
        opcode: FuncOpCode::U_FDL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_BDLDay",
        opcode: FuncOpCode::U_BDLDAY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_TimeOn",
        opcode: FuncOpCode::U_TIMEON,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_BDl",
        opcode: FuncOpCode::U_BDL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_BUl",
        opcode: FuncOpCode::U_BUL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Year",
        opcode: FuncOpCode::YEAR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Month",
        opcode: FuncOpCode::MONTH,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Day",
        opcode: FuncOpCode::DAY,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Dow",
        opcode: FuncOpCode::DOW,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Hour",
        opcode: FuncOpCode::HOUR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Min",
        opcode: FuncOpCode::MIN,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Sec",
        opcode: FuncOpCode::SEC,
        args: 0x01,
    },
    FunctionDefinition {
        name: "TimeAP",
        opcode: FuncOpCode::TIMEAP,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Ver",
        opcode: FuncOpCode::VER,
        args: 0x00,
    },
    FunctionDefinition {
        name: "NoChar",
        opcode: FuncOpCode::NOCHAR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "YesChar",
        opcode: FuncOpCode::YESCHAR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "StripATX",
        opcode: FuncOpCode::STRIPATX,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Replace",
        opcode: FuncOpCode::REPLACE,
        args: 0x03,
    },
    FunctionDefinition {
        name: "Strip",
        opcode: FuncOpCode::STRIP,
        args: 0x02,
    },
    FunctionDefinition {
        name: "InKey",
        opcode: FuncOpCode::INKEY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "String",
        opcode: FuncOpCode::TOSTRING,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Mask_Pwd",
        opcode: FuncOpCode::MASK_PWD,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_Alpha",
        opcode: FuncOpCode::MASK_ALPHA,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_Num",
        opcode: FuncOpCode::MASK_NUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_Alnum",
        opcode: FuncOpCode::MASK_ALNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_File",
        opcode: FuncOpCode::MASK_FILE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_Path",
        opcode: FuncOpCode::MASK_PATH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Mask_ASCII",
        opcode: FuncOpCode::MASK_ASCII,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CurConf",
        opcode: FuncOpCode::CURCONF,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PCBDat",
        opcode: FuncOpCode::PCBDAT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PPEPath",
        opcode: FuncOpCode::PPEPATH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ValDate",
        opcode: FuncOpCode::VALDATE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ValTime",
        opcode: FuncOpCode::VALTIME,
        args: 0x01,
    },
    FunctionDefinition {
        name: "U_MsgRd",
        opcode: FuncOpCode::U_MSGRD,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_MsgWr",
        opcode: FuncOpCode::U_MSGWR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PCBNode",
        opcode: FuncOpCode::PCBNODE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ReadLine",
        opcode: FuncOpCode::READLINE,
        args: 0x02,
    },
    FunctionDefinition {
        name: "SysopSec",
        opcode: FuncOpCode::SYSOPSEC,
        args: 0x00,
    },
    FunctionDefinition {
        name: "OnLocal",
        opcode: FuncOpCode::ONLOCAL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "UN_Stat",
        opcode: FuncOpCode::UN_STAT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "UN_Name",
        opcode: FuncOpCode::UN_NAME,
        args: 0x00,
    },
    FunctionDefinition {
        name: "UN_City",
        opcode: FuncOpCode::UN_CITY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "UN_Oper",
        opcode: FuncOpCode::UN_OPER,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CurSec",
        opcode: FuncOpCode::CURSEC,
        args: 0x00,
    },
    FunctionDefinition {
        name: "GetToken",
        opcode: FuncOpCode::GETTOKEN,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MinLeft",
        opcode: FuncOpCode::MINLEFT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MinOn",
        opcode: FuncOpCode::MINON,
        args: 0x00,
    },
    FunctionDefinition {
        name: "GetEnv",
        opcode: FuncOpCode::GETENV,
        args: 0x01,
    },
    FunctionDefinition {
        name: "CallId",
        opcode: FuncOpCode::CALLID,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegAL",
        opcode: FuncOpCode::REGAL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegAH",
        opcode: FuncOpCode::REGAH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegBL",
        opcode: FuncOpCode::REGBL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegBH",
        opcode: FuncOpCode::REGBH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegCL",
        opcode: FuncOpCode::REGCL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegCH",
        opcode: FuncOpCode::REGCH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegDL",
        opcode: FuncOpCode::REGDL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegDH",
        opcode: FuncOpCode::REGDH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegAX",
        opcode: FuncOpCode::REGAX,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegBX",
        opcode: FuncOpCode::REGBX,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegCX",
        opcode: FuncOpCode::REGCX,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegDX",
        opcode: FuncOpCode::REGDX,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegSI",
        opcode: FuncOpCode::REGSI,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegDI",
        opcode: FuncOpCode::REGDI,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegF",
        opcode: FuncOpCode::REGF,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegCF",
        opcode: FuncOpCode::REGCF,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegDS",
        opcode: FuncOpCode::REGDS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RegES",
        opcode: FuncOpCode::REGES,
        args: 0x00,
    },
    FunctionDefinition {
        name: "B2W",
        opcode: FuncOpCode::B2W,
        args: 0x02,
    },
    FunctionDefinition {
        name: "PeekB",
        opcode: FuncOpCode::PEEKB,
        args: 0x01,
    },
    FunctionDefinition {
        name: "PeekW",
        opcode: FuncOpCode::PEEKW,
        args: 0x01,
    },
    FunctionDefinition {
        name: "MkAddr",
        opcode: FuncOpCode::MKADDR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Exist",
        opcode: FuncOpCode::EXIST,
        args: 0x01,
    },
    FunctionDefinition {
        name: "I2S",
        opcode: FuncOpCode::I2S,
        args: 0x02,
    },
    FunctionDefinition {
        name: "S2I",
        opcode: FuncOpCode::S2I,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Carrier",
        opcode: FuncOpCode::CARRIER,
        args: 0x00,
    },
    FunctionDefinition {
        name: "TokenStr",
        opcode: FuncOpCode::TOKENSTR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CDOn",
        opcode: FuncOpCode::CDON,
        args: 0x00,
    },
    FunctionDefinition {
        name: "LangExt",
        opcode: FuncOpCode::LANGEXT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ANSIOn",
        opcode: FuncOpCode::ANSION,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ValCC",
        opcode: FuncOpCode::VALCC,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FmtCC",
        opcode: FuncOpCode::FMTCC,
        args: 0x01,
    },
    FunctionDefinition {
        name: "CCType",
        opcode: FuncOpCode::CCTYPE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "GetX",
        opcode: FuncOpCode::GETX,
        args: 0x00,
    },
    FunctionDefinition {
        name: "GetY",
        opcode: FuncOpCode::GETY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "And",
        opcode: FuncOpCode::BAND,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Or",
        opcode: FuncOpCode::BOR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Xor",
        opcode: FuncOpCode::BXOR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "Not",
        opcode: FuncOpCode::BNOT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "U_PwdHist",
        opcode: FuncOpCode::U_PWDHIST,
        args: 0x01,
    },
    FunctionDefinition {
        name: "U_PwdLc",
        opcode: FuncOpCode::U_PWDLC,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_PwdTc",
        opcode: FuncOpCode::U_PWDTC,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_Stat",
        opcode: FuncOpCode::U_STAT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DefColor",
        opcode: FuncOpCode::DEFCOLOR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Abs",
        opcode: FuncOpCode::ABS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "GrafMode",
        opcode: FuncOpCode::GRAFMODE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PSA",
        opcode: FuncOpCode::PSA,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FileInf",
        opcode: FuncOpCode::FILEINF,
        args: 0x02,
    },
    FunctionDefinition {
        name: "PPEName",
        opcode: FuncOpCode::PPENAME,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MkDate",
        opcode: FuncOpCode::MKDATE,
        args: 0x03,
    },
    FunctionDefinition {
        name: "CurColor",
        opcode: FuncOpCode::CURCOLOR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "KInkey",
        opcode: FuncOpCode::KINKEY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MInkey",
        opcode: FuncOpCode::MINKEY,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MaxNode",
        opcode: FuncOpCode::MAXNODE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "SLPath",
        opcode: FuncOpCode::SLPATH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "HelpPath",
        opcode: FuncOpCode::HELPPATH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "TempPath",
        opcode: FuncOpCode::TEMPPATH,
        args: 0x00,
    },
    FunctionDefinition {
        name: "Modem",
        opcode: FuncOpCode::MODEM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "LoggedOn",
        opcode: FuncOpCode::LOGGEDON,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CallNum",
        opcode: FuncOpCode::CALLNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MGetByte",
        opcode: FuncOpCode::MGETBYTE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "TokCount",
        opcode: FuncOpCode::TOKCOUNT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_RecNum",
        opcode: FuncOpCode::U_RECNUM,
        args: 0x01,
    },
    FunctionDefinition {
        name: "U_InConf",
        opcode: FuncOpCode::U_INCONF,
        args: 0x02,
    },
    FunctionDefinition {
        name: "PeekDW",
        opcode: FuncOpCode::PEEKDW,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DbgLevel",
        opcode: FuncOpCode::DBGLEVEL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ScrText",
        opcode: FuncOpCode::SCRTEXT,
        args: 0x04,
    },
    FunctionDefinition {
        name: "ShowStat",
        opcode: FuncOpCode::SHOWSTAT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PageStat",
        opcode: FuncOpCode::PAGESTAT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ReplaceStr",
        opcode: FuncOpCode::REPLACESTR,
        args: 0x03,
    },
    FunctionDefinition {
        name: "StripStr",
        opcode: FuncOpCode::STRIPSTR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "ToBigStr",
        opcode: FuncOpCode::TOBIGSTR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToBoolean",
        opcode: FuncOpCode::TOBOOLEAN,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToByte",
        opcode: FuncOpCode::TOBYTE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToDate",
        opcode: FuncOpCode::TODATE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToDReal",
        opcode: FuncOpCode::TODREAL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToEDate",
        opcode: FuncOpCode::TOEDATE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToInteger",
        opcode: FuncOpCode::TOINTEGER,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToMoney",
        opcode: FuncOpCode::TOMONEY,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToReal",
        opcode: FuncOpCode::TOREAL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToSByte",
        opcode: FuncOpCode::TOSBYTE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToSWord",
        opcode: FuncOpCode::TOSWORD,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToTime",
        opcode: FuncOpCode::TOTIME,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToUnsigned",
        opcode: FuncOpCode::TOUNSIGNED,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ToWord",
        opcode: FuncOpCode::TOWORD,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Mixed",
        opcode: FuncOpCode::MIXED,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Alias",
        opcode: FuncOpCode::ALIAS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ConfReg",
        opcode: FuncOpCode::CONFREG,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ConfExp",
        opcode: FuncOpCode::CONFEXP,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ConfSel",
        opcode: FuncOpCode::CONFSEL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ConfSys",
        opcode: FuncOpCode::CONFSYS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ConfMW",
        opcode: FuncOpCode::CONFMW,
        args: 0x01,
    },
    FunctionDefinition {
        name: "LPrinted",
        opcode: FuncOpCode::LPRINTED,
        args: 0x00,
    },
    FunctionDefinition {
        name: "IsNonStop",
        opcode: FuncOpCode::ISNONSTOP,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ErrCorrect",
        opcode: FuncOpCode::ERRCORRECT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ConfAlias",
        opcode: FuncOpCode::CONFALIAS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "UserAlias",
        opcode: FuncOpCode::USERALIAS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CurUser",
        opcode: FuncOpCode::CURUSER,
        args: 0x00,
    },
    FunctionDefinition {
        name: "U_LMR",
        opcode: FuncOpCode::U_LMR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ChatStat",
        opcode: FuncOpCode::CHATSTAT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "DefAns",
        opcode: FuncOpCode::DEFANS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "LastAns",
        opcode: FuncOpCode::LASTANS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "MegaNum",
        opcode: FuncOpCode::MEGANUM,
        args: 0x01,
    },
    FunctionDefinition {
        name: "EVTTimeAdj",
        opcode: FuncOpCode::EVTTIMEADJ,
        args: 0x00,
    },
    FunctionDefinition {
        name: "IsBitSet",
        opcode: FuncOpCode::ISBITSET,
        args: 0x02,
    },
    FunctionDefinition {
        name: "FmtReal",
        opcode: FuncOpCode::FMTREAL,
        args: 0x03,
    },
    FunctionDefinition {
        name: "FlagCNT",
        opcode: FuncOpCode::FLAGCNT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "KBDBufSize",
        opcode: FuncOpCode::KBDBUFSIZE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "PPLBufSize",
        opcode: FuncOpCode::PPLBUFSIZE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "KBDFilUsued",
        opcode: FuncOpCode::KBDFILUSED,
        args: 0x00,
    },
    FunctionDefinition {
        name: "LoMsgNum",
        opcode: FuncOpCode::LOMSGNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "HiMsgNum",
        opcode: FuncOpCode::HIMSGNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "DriveSpace",
        opcode: FuncOpCode::DRIVESPACE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "OutBytes",
        opcode: FuncOpCode::OUTBYTES,
        args: 0x00,
    },
    FunctionDefinition {
        name: "HiConfNum",
        opcode: FuncOpCode::HICONFNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "InBytes",
        opcode: FuncOpCode::INBYTES,
        args: 0x00,
    },
    FunctionDefinition {
        name: "CRC32",
        opcode: FuncOpCode::CRC32,
        args: 0x02,
    },
    FunctionDefinition {
        name: "PcbMac",
        opcode: FuncOpCode::PCBMAC,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ActMSGNum",
        opcode: FuncOpCode::ACTMSGNUM,
        args: 0x00,
    },
    FunctionDefinition {
        name: "StackLeft",
        opcode: FuncOpCode::STACKLEFT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "StackErr",
        opcode: FuncOpCode::STACKERR,
        args: 0x00,
    },
    FunctionDefinition {
        name: "DGetAlias",
        opcode: FuncOpCode::DGETALIAS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DBOF",
        opcode: FuncOpCode::DBOF,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DChanged",
        opcode: FuncOpCode::DCHANGED,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DDecimals",
        opcode: FuncOpCode::DDECIMALS,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DDeleted",
        opcode: FuncOpCode::DDELETED,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DEof",
        opcode: FuncOpCode::DEOF,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DErr",
        opcode: FuncOpCode::DERR,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DFields",
        opcode: FuncOpCode::DFIELDS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DLength",
        opcode: FuncOpCode::DLENGTH,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DName",
        opcode: FuncOpCode::DNAME,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DRecCount",
        opcode: FuncOpCode::DRECCOUNT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DRecNo",
        opcode: FuncOpCode::DRECNO,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DType",
        opcode: FuncOpCode::DTYPE,
        args: 0x02,
    },
    FunctionDefinition {
        name: "FNext",
        opcode: FuncOpCode::FNEXT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "DNext",
        opcode: FuncOpCode::DNEXT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ToDDate",
        opcode: FuncOpCode::TODDATE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DCloseAll",
        opcode: FuncOpCode::DCLOSEALL,
        args: 0x00,
    },
    FunctionDefinition {
        name: "DOpen",
        opcode: FuncOpCode::DOPEN,
        args: 0x03,
    },
    FunctionDefinition {
        name: "DClose",
        opcode: FuncOpCode::DCLOSE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DSetAlias",
        opcode: FuncOpCode::DSETALIAS,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DPack",
        opcode: FuncOpCode::DPACK,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DLockF",
        opcode: FuncOpCode::DLOCKF,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DLock",
        opcode: FuncOpCode::DLOCK,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DLockR",
        opcode: FuncOpCode::DLOCKR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DUnLock",
        opcode: FuncOpCode::DUNLOCK,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DNOpen",
        opcode: FuncOpCode::DNOPEN,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DNClose",
        opcode: FuncOpCode::DNCLOSE,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DNCloseAll",
        opcode: FuncOpCode::DNCLOSEALL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DNew",
        opcode: FuncOpCode::DNEW,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DAdd",
        opcode: FuncOpCode::DADD,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DAppend",
        opcode: FuncOpCode::DAPPEND,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DTop",
        opcode: FuncOpCode::DTOP,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DGo",
        opcode: FuncOpCode::DGO,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DBottom",
        opcode: FuncOpCode::DBOTTOM,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DSkip",
        opcode: FuncOpCode::DSKIP,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DBLank",
        opcode: FuncOpCode::DBLANK,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DDelete",
        opcode: FuncOpCode::DDELETE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DRecall",
        opcode: FuncOpCode::DRECALL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DTag",
        opcode: FuncOpCode::DTAG,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DSeek",
        opcode: FuncOpCode::DSEEK,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DFBlank",
        opcode: FuncOpCode::DFBLANK,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DGet",
        opcode: FuncOpCode::DGET,
        args: 0x02,
    },
    FunctionDefinition {
        name: "DPut",
        opcode: FuncOpCode::DPUT,
        args: 0x03,
    },
    FunctionDefinition {
        name: "DFCopy",
        opcode: FuncOpCode::DFCOPY,
        args: 0x04,
    },
    FunctionDefinition {
        name: "DSelect",
        opcode: FuncOpCode::DSELECT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DChkStat",
        opcode: FuncOpCode::DCHKSTAT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "PCBAccount",
        opcode: FuncOpCode::PCBACCOUNT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "PCBAccStat",
        opcode: FuncOpCode::PCBACCSTAT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "DErrMsg",
        opcode: FuncOpCode::DERRMSG,
        args: 0x01,
    },
    FunctionDefinition {
        name: "Account",
        opcode: FuncOpCode::ACCOUNT,
        args: 0x01,
    },
    FunctionDefinition {
        name: "ScanMsgHdr",
        opcode: FuncOpCode::SCANMSGHDR,
        args: 0x04,
    },
    FunctionDefinition {
        name: "CheckRIP",
        opcode: FuncOpCode::CHECKRIP,
        args: 0x00,
    },
    FunctionDefinition {
        name: "RIPVer",
        opcode: FuncOpCode::RIPVER,
        args: 0x00,
    },
    FunctionDefinition {
        name: "QWKLimits",
        opcode: FuncOpCode::QWKLIMITS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FindFirst",
        opcode: FuncOpCode::FINDFIRST,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FindNext",
        opcode: FuncOpCode::FINDNEXT,
        args: 0x00,
    },
    FunctionDefinition {
        name: "USelMrs",
        opcode: FuncOpCode::USELMRS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ConfInfo",
        opcode: FuncOpCode::CONFINFO,
        args: 0x02,
    },
    FunctionDefinition {
        name: "TInkey",
        opcode: FuncOpCode::TINKEY,
        args: 0x01,
    },
    FunctionDefinition {
        name: "CWD",
        opcode: FuncOpCode::CWD,
        args: 0x00,
    },
    FunctionDefinition {
        name: "InStrr",
        opcode: FuncOpCode::INSTRR,
        args: 0x02,
    },
    FunctionDefinition {
        name: "FDORDAKA",
        opcode: FuncOpCode::FDORDAKA,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FDORDOrg",
        opcode: FuncOpCode::FDORDORG,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FDORDArea",
        opcode: FuncOpCode::FDORDAREA,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FDOQRd",
        opcode: FuncOpCode::FDOQRD,
        args: 0x06,
    },
    FunctionDefinition {
        name: "GetDrive",
        opcode: FuncOpCode::GETDRIVE,
        args: 0x00,
    },
    FunctionDefinition {
        name: "SetDrive",
        opcode: FuncOpCode::SETDRIVE,
        args: 0x01,
    },
    FunctionDefinition {
        name: "BS2I",
        opcode: FuncOpCode::BS2I,
        args: 0x01,
    },
    FunctionDefinition {
        name: "BD2I",
        opcode: FuncOpCode::BD2I,
        args: 0x01,
    },
    FunctionDefinition {
        name: "I2BS",
        opcode: FuncOpCode::I2BS,
        args: 0x01,
    },
    FunctionDefinition {
        name: "I2BD",
        opcode: FuncOpCode::I2BD,
        args: 0x01,
    },
    FunctionDefinition {
        name: "FTell",
        opcode: FuncOpCode::FTELL,
        args: 0x01,
    },
    FunctionDefinition {
        name: "OS",
        opcode: FuncOpCode::OS,
        args: 0x00,
    },
    FunctionDefinition {
        name: "ShortDesc",
        opcode: FuncOpCode::SHORT_DESC,
        args: 0x00,
    },
    FunctionDefinition {
        name: "GetBankBal",
        opcode: FuncOpCode::GetBankBal,
        args: 0x02,
    },
    FunctionDefinition {
        name: "GetMsgHdr",
        opcode: FuncOpCode::GetMsgHdr,
        args: 0x03,
    },
    FunctionDefinition {
        name: "SetMsgHdr",
        opcode: FuncOpCode::SetMsgHdr,
        args: 0x04,
    },
];

lazy_static::lazy_static! {
    pub static ref UNICODE_TO_CP437: std::collections::HashMap<char, u8> = {
        let mut res = std::collections::HashMap::new();
        (0..256).for_each(|a| {
            res.insert(CP437_TO_UNICODE[a], a as u8);
        });
        res
    };
}

pub const CP437_TO_UNICODE: [char; 256] = [
    '\u{0000}', '\u{263a}', '\u{263b}', '\u{2665}', '\u{2666}', '\u{2663}', '\u{2660}', '\u{2022}',
    '\x08', '\x09', '\n', '\u{2642}', '\u{2640}', '\u{266a}', '\u{266b}', '\u{263c}', '\u{25ba}',
    '\u{25c4}', '\u{2195}', '\u{203c}', '\u{00b6}', '\u{00a7}', '\u{25ac}', '\u{21a8}', '\u{2191}',
    '\u{2193}', '\u{2192}', '\u{2190}', '\u{221f}', '\u{2194}', '\u{25b2}', '\u{25bc}', '\u{0020}',
    '\u{0021}', '\u{0022}', '\u{0023}', '\u{0024}', '\u{0025}', '\u{0026}', '\u{0027}', '\u{0028}',
    '\u{0029}', '\u{002a}', '\u{002b}', '\u{002c}', '\u{002d}', '\u{002e}', '\u{002f}', '\u{0030}',
    '\u{0031}', '\u{0032}', '\u{0033}', '\u{0034}', '\u{0035}', '\u{0036}', '\u{0037}', '\u{0038}',
    '\u{0039}', '\u{003a}', '\u{003b}', '\u{003c}', '\u{003d}', '\u{003e}', '\u{003f}', '\u{0040}',
    '\u{0041}', '\u{0042}', '\u{0043}', '\u{0044}', '\u{0045}', '\u{0046}', '\u{0047}', '\u{0048}',
    '\u{0049}', '\u{004a}', '\u{004b}', '\u{004c}', '\u{004d}', '\u{004e}', '\u{004f}', '\u{0050}',
    '\u{0051}', '\u{0052}', '\u{0053}', '\u{0054}', '\u{0055}', '\u{0056}', '\u{0057}', '\u{0058}',
    '\u{0059}', '\u{005a}', '\u{005b}', '\u{005c}', '\u{005d}', '\u{005e}', '\u{005f}', '\u{0060}',
    '\u{0061}', '\u{0062}', '\u{0063}', '\u{0064}', '\u{0065}', '\u{0066}', '\u{0067}', '\u{0068}',
    '\u{0069}', '\u{006a}', '\u{006b}', '\u{006c}', '\u{006d}', '\u{006e}', '\u{006f}', '\u{0070}',
    '\u{0071}', '\u{0072}', '\u{0073}', '\u{0074}', '\u{0075}', '\u{0076}', '\u{0077}', '\u{0078}',
    '\u{0079}', '\u{007a}', '\u{007b}', '\u{007c}', '\u{007d}', '\u{007e}', '\u{007f}', '\u{00c7}',
    '\u{00fc}', '\u{00e9}', '\u{00e2}', '\u{00e4}', '\u{00e0}', '\u{00e5}', '\u{00e7}', '\u{00ea}',
    '\u{00eb}', '\u{00e8}', '\u{00ef}', '\u{00ee}', '\u{00ec}', '\u{00c4}', '\u{00c5}', '\u{00c9}',
    '\u{00e6}', '\u{00c6}', '\u{00f4}', '\u{00f6}', '\u{00f2}', '\u{00fb}', '\u{00f9}', '\u{00ff}',
    '\u{00d6}', '\u{00dc}', '\u{00a2}', '\u{00a3}', '\u{00a5}', '\u{20a7}', '\u{0192}', '\u{00e1}',
    '\u{00ed}', '\u{00f3}', '\u{00fa}', '\u{00f1}', '\u{00d1}', '\u{00aa}', '\u{00ba}', '\u{00bf}',
    '\u{2310}', '\u{00ac}', '\u{00bd}', '\u{00bc}', '\u{00a1}', '\u{00ab}', '\u{00bb}', '\u{2591}',
    '\u{2592}', '\u{2593}', '\u{2502}', '\u{2524}', '\u{2561}', '\u{2562}', '\u{2556}', '\u{2555}',
    '\u{2563}', '\u{2551}', '\u{2557}', '\u{255d}', '\u{255c}', '\u{255b}', '\u{2510}', '\u{2514}',
    '\u{2534}', '\u{252c}', '\u{251c}', '\u{2500}', '\u{253c}', '\u{255e}', '\u{255f}', '\u{255a}',
    '\u{2554}', '\u{2569}', '\u{2566}', '\u{2560}', '\u{2550}', '\u{256c}', '\u{2567}', '\u{2568}',
    '\u{2564}', '\u{2565}', '\u{2559}', '\u{2558}', '\u{2552}', '\u{2553}', '\u{256b}', '\u{256a}',
    '\u{2518}', '\u{250c}', '\u{2588}', '\u{2584}', '\u{258c}', '\u{2590}', '\u{2580}', '\u{03b1}',
    '\u{00df}', '\u{0393}', '\u{03c0}', '\u{03a3}', '\u{03c3}', '\u{00b5}', '\u{03c4}', '\u{03a6}',
    '\u{0398}', '\u{03a9}', '\u{03b4}', '\u{221e}', '\u{03c6}', '\u{03b5}', '\u{2229}', '\u{2261}',
    '\u{00b1}', '\u{2265}', '\u{2264}', '\u{2320}', '\u{2321}', '\u{00f7}', '\u{2248}', '\u{00b0}',
    '\u{2219}', '\u{00b7}', '\u{221a}', '\u{207f}', '\u{00b2}', '\u{25a0}', '\u{00a0}',
];
