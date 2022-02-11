use crate::executable::VariableType;
use crate::parser::{BinOp, Constant, Expression};

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
pub const STATEMENT_SIGNATURE_TABLE : [i32; 227]= [
    0xff,0x00,0x00,0x00,0x00,0x00,0x01,0xfd, //0
    0x12,0xfe,0xfe,0xff,0x02,0x02,0x02,0x22,
    0x04,0x04,0x04,0x01,0x22,0xfe,0xfe,0x00, //1
    0x01,0x03,0x00,0x00,0x00,0x00,0x01,0x00,
    0x01,0x02,0x26,0x23,0x23,0x23,0x23,0x23, //2
    0x23,0xfd,0x00,0x25,0x00,0x00,0x00,0x00,
    0x01,0x01,0x11,0x11,0x00,0x01,0x01,0x11, //3
    0x24,0x02,0x00,0x24,0x00,0xfe,0xfc,0x01,
    0x01,0x01,0x01,0x01,0x01,0x01,0x00,0x00, //4
    0x03,0x23,0x00,0x00,0x01,0x01,0x01,0x06,
    0x0a,0xf2,0xf2,0x02,0x02,0xf2,0x02,0x01, //5
    0x01,0x00,0x00,0x00,0x22,0x22,0x00,0x09,
    0x00,0x00,0x01,0x00,0xfe,0xfe,0xfe,0xfe, //6
    0x02,0x01,0x02,0x01,0x00,0x00,0x00,0x00,
    0x03,0x01,0x23,0x03,0x01,0x01,0x11,0xfe, //7
    0xfe,0x02,0x12,0x02,0x01,0x01,0x01,0xfa,
    0x02,0x02,0x00,0x00,0x00,0x01,0x01,0x01, //8
    0x01,0x01,0x01,0x01,0x01,0x01,0xf9,0x0a,
    0xf2,0x02,0x22,0x00,0xfe,0xfe,0x22,0x02, //9
    0x23,0x03,0x22,0x02,0x23,0x03,0x12,0x12,
    0x00,0x00,0x02,0x01,0x00,0xaa,0xaa,0xaa, //A
    0xf6,0x00,0xaa,0x00,0xaa,0x01,0xf8,0x03,
    0x01,0x02,0x01,0x00,0x01,0x02,0xf7,0x01, //B
    0x03,0x02,0x02,0x01,0x01,0x01,0x01,0x01,
    0x02,0x01,0x02,0x01,0x01,0x01,0x02,0x02, //C
    0x02,0x33,0x03,0x04,0x01,0x02,0x05,0x03,
    0x02,0x02,0x01,0x03,0x01,0x01,0x02,0x02, //D
    0x01,0x01,0x01,0x02,0x03,0x02,0x03,0x04,
    0x03,0x01,0x02                           //E
];

// Funktions Variable-Types, 0 = no exp / aa = not defined
//			     1- f= numbers of exp using ()
//              0x10 = only rvalue (!)
//			    0x11 = lvalue rvalue (+,-,...)
pub const FUNCTION_SIGNATURE_TABLE : [i32; 286]= [
    0xaa,0x10,0x10,0x11,0x11,0x11,0x11,0x11,
    0x11,0x11,0x11,0x11,0x11,0x11,0x11,0x10, //F
    0x11,0x11,0xaa,0x01,0x01,0x01,0x03,0x02,
    0x02,0x01,0x01,0x01,0x01,0x02,0x00,0x02, //E
    0x02,0x02,0x01,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, //D
    0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01,
    0x00,0x00,0x00,0x01,0x03,0x02,0x00,0x01, //C
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x01,0x01,0x00,0x00,0x00,0x02, //B
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x01,0x00,0x00,0x00,0x00,0x00, //A
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x00,0x00,0x00,0x00,0x02,0x01, //9
    0x01,0x02,0x01,0x02,0x02,0x00,0x00,0x00,
    0x00,0x00,0x01,0x01,0x01,0x00,0x00,0x02, //8
    0x02,0x02,0x01,0x01,0x00,0x00,0x01,0x00,
    0x01,0x00,0x01,0x02,0x00,0x03,0x00,0x00, //7
    0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
    0x00,0x00,0x01,0x02,0x01,0x00,0x04,0x00, //6
    0x00,0x03,0x02,0x01,0x01,0x01,0x01,0x01,
    0x01,0x01,0x01,0x01,0x01,0x01,0x01,0x01, //5
    0x01,0x01,0x00,0x01,0x01,0x01,0x01,0x01,
    0x00,0x00,0x00,0x00,0x00,0x00,0x01,0x00, //4
    0x00,0x00,0x01,0x00,0x02,0x03,0x00,0x00,
    0x00,0x00,0x00,0x00,0x01,0x00,0x00,0x00, //3
    0x02,0x01,0x00,0x00,0x00,0x01,0x01,0x01,
    0x02,0x01,0x01,0x01,0x01,0x02,0x02,0x01, //2
    0x01,0x02,0x00,0x00,0x01,0x00,0x03,0x01,
    0x02,0x01,0x01,0x01,0x02,0x01,0x02,0x02, //1
    0x01,0x01,0x01,0x01,0x01,0x02,0x01,0x02,
    0x01,0x01,0x01,0x02,0x02,0x02,0x02,0x03, //0
    0x04,0x01,0x01,0x01,0x01,0x01,0x01,0x04,
    0x00,0x00,0x01,0x01,0x00,0x00,0x02,0x01, //F
    0x00,0x02,0x01,0x01,0x01,0x06,0x00,0x01,
    0x01,0x01,0x01,0x01,0x01,0x00            //E
];

pub const TYPE_NAMES : [VariableType; 18] = [
    VariableType::Boolean,
    VariableType::Unsigned,
    VariableType::Date,
    VariableType::EDate,
    VariableType::Integer,
    VariableType::Money,
    VariableType::Real,
    VariableType::String,
    VariableType::Time,
    VariableType::Byte,
    VariableType::Word,
    VariableType::SByte,
    VariableType::SWord,
    VariableType::BigStr,
    VariableType::Real,
    VariableType::Unknown,
    VariableType::Unknown,
    VariableType::DDate
];


pub const CONSTANT_2_OFFSETS : [i32;18]  =  [
    0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
    0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,
    0x10,0x11
];
pub const CONSTANT_2_NAMES : [Constant; 18] = [
    Constant::START_BAL,
    Constant::START_SESSION,
    Constant::DEB_CALL,
    Constant::DEB_TIME,
    Constant::DEB_MSGREAD,
    Constant::DEB_MSGCAP,
    Constant::DEB_MSGWRITE,
    Constant::DEB_MSGECHOED,
    Constant::DEB_MSGPRIVATE,
    Constant::DEB_DOWNFILE,
    Constant::DEB_DOWNBYTES,
    Constant::DEB_CHAT,
    Constant::DEB_TPU,
    Constant::DEB_SPECIAL,
    Constant::CRED_UPFILE,
    Constant::CRED_UPBYTES,
    Constant::CRED_SPECIAL,
    Constant::SEC_DROP
];

pub const CONSTANT_CONFERENCE_OFFSETS : [i32; 6] = [ 0x02, 0x10, 0x01, 0x04, 0x08, -1 ];
pub const CONSTANT_CONFERENCE_NAMES : [Constant; 5] = [ Constant::F_EXP, Constant::F_MW, Constant::F_REG, Constant::F_SEL, Constant::F_SYS ];

pub const CONSTANT_NAMES_OFFSETS : [i32; 5] = [ 0, 1, 4, 2, -1 ];
pub const CONSTANT_NAMES_DISPLAY : [Constant; 4] = [ Constant::DEFS, Constant::GRAPH, Constant::LANG, Constant::SEC ];

pub const CONSTANT_FACCESS_OFFSETS : [i32; 4] = [ 0, 2, 1, -1 ];
pub const CONSTANT_FACCESS_NAMES : [Constant; 3] = [ Constant::O_RD, Constant::O_RW, Constant::O_WR ];

pub const CONSTANT_SEEK_OFFSETS : [i32; 4] = [ 1, 2, 0, -1 ];
pub const CONSTANT_SEEK_NAMES : [Constant; 3] = [ Constant::SEEK_CUR, Constant::SEEK_END, Constant::SEEK_SET ];

pub const CONSTANT_OPENFLAGS_OFFSETS : [i32; 5] = [ 3, 0, 1, 2, -1 ];
pub const CONSTANT_OPENFLAGS_NAMES : [Constant; 4] = [ Constant::S_DB, Constant::S_DN, Constant::S_DR, Constant::S_DW ];

pub const CONSTANT_1_OFFSETS : [i32; 19]= [
    0x2000,
    0x0800,
    0x0000,
    0x0001,
    0x0020,
    0x0002,
    0x0004,
    0x1000,
    0x0100,
    0x0080,
    0x8000,
    0x10000,
    0x0040,
    0x0400,
    0x0010,
    0x0008,
    0x0200,
    0x4000,
    -1
];
pub const CONSTANT_1_NAMES : [Constant; 18] = [
    Constant::AUTO,
    Constant::BELL,
    Constant::DEFS,
    Constant::ECHODOTS,
    Constant::ERASELINE,
    Constant::FIELDLEN,
    Constant::GUIDE,
    Constant::HIGHASCII,
    Constant::LFAFTER,
    Constant::LFBEFORE,
    Constant::LOGIT,
    Constant::LOGITLEFT,
    Constant::NEWLINE,
    Constant::NOCLEAR,
    Constant::STACKED,
    Constant::UPCASE,
    Constant::WORDWRAP,
    Constant::YESNO
];

pub const CONSTANT_LINECOUNT_OFFSETS : [i32; 4] = [ 2, 1, 0, -1 ];
pub const CONSTANT_LINECOUNT_NAMES : [Constant; 3] = [ Constant::FCL, Constant::FNS,  Constant::NC ];

pub const BIN_EXPR : [BinOp; 19] = [
    BinOp::Add,
    BinOp::Add,
    BinOp::Sub,
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
    BinOp::NotEq,
    BinOp::Greater,
    BinOp::GreaterEq,
    BinOp::Add, // ???
    BinOp::And,
    BinOp::Or,
    ];

pub const EXPR_NAMES : [&str; 286] = [
    "???","+","-","^","*","/","%","+","-","=","<>","<","<=",
    ">",">=","!","&","|","???","LEN","LOWER","UPPER","MID",
    "LEFT","RIGHT","SPACE","FERR","CHR","ASC","INSTR",
    "ABORT","LTRIM","RTRIM","TRIM","RANDOM","DATE","TIME",
    "U_NAME","U_LDATE","U_LTIME","U_LDIR","U_LOGONS",
    "U_FUL","U_FDL","U_BDLDAY","U_TIMEON","U_BDL","U_BUL",
    "YEAR","MONTH","DAY","DOW","HOUR","MIN","SEC",
    "TIMEAP","VER","NOCHAR","YESCHAR","STRIPATX",
    "REPLACE","STRIP","INKEY","STRING","MASK_PWD",
    "MASK_ALPHA","MASK_NUM","MASK_ALNUM","MASK_FILE",
    "MASK_PATH","MASK_ASCII","CURCONF","PCBDAT","PPEPATH",
    "VALDATE","VALTIME","U_MSGRD","U_MSGWR","PCBNODE",
    "READLINE","SYSOPSEC","ONLOCAL","UN_STAT","UN_NAME",
    "UN_CITY","UN_OPER","CURSEC","GETTOKEN","MINLEFT",
    "MINON","GETENV","CALLID","REGAL","REGAH","REGBL",
    "REGBH","REGCL","REGCH","REGDL","REGDH","REGAX",
    "REGBX","REGCX","REGDX","REGSI","REGDI","REGF",
    "REGCF","REGDS","REGES","B2W","PEEKB","PEEKW",
    "MKADDR","EXIST","I2S","S2I","CARRIER","TOKENSTR",
    "CDON","LANGEXT","ANSION","VALCC","FMTCC","CCTYPE",
    "GETX","GETY","AND","OR","XOR","NOT","U_PWDHIST",
    "U_PWDLC","U_PWDTC","U_STAT","DEFCOLOR","ABS",
    "GRAFMODE","PSA","FILEINF","PPENAME","MKDATE",
    "CURCOLOR","KINKEY","MINKEY","MAXNODE","SLPATH",
    "HELPPATH","TEMPPATH","MODEM","LOGGEDON","CALLNUM",
    "MGETBYTE","TOKCOUNT","U_RECNUM","U_INCONF","PEEKDW",
    "DBGLEVEL","SCRTEXT","SHOWSTAT","PAGESTAT",
    "REPLACESTR","STRIPSTR","TOBIGSTR","TOBOOLEAN",
    "TOBYTE","TODATE","TODREAL","TOEDATE","TOINTEGER",
    "TOMONEY","TOREAL","TOSBYTE","TOSWORD","TOTIME",
    "TOUNSIGNED","TOWORD","MIXED","ALIAS","CONFREG",
    "CONFEXP","CONFSEL","CONFSYS","CONFMW","LPRINTED",
    "ISNONSTOP","ERRCORRECT","CONFALIAS","USERALIAS",
    "CURUSER","U_LMR","CHATSTAT","DEFANS","LASTANS",
    "MEGANUM","EVTTIMEADJ","ISBITSET","FMTREAL","FLAGCNT",
    "KBDBUFSIZE","PPLBUFSIZE","KBDFILUSED","LOMSGNUM",
    "HIMSGNUM","DRIVESPACE","OUTBYTES","HICONFNUM",
    "INBYTES","CRC32","PCBMAC","ACTMSGNUM","STACKLEFT",
    "STACKERR","DGETALIAS","DBOF","DCHANGED","DDECIMALS",
    "DDELETED","DEOF","DERR","DFIELDS","DLENGTH","DNAME",
    "DRECCOUNT","DRECNO","DTYPE","FNEXT","DNEXT","TODDATE",
    "DCLOSEALL","DOPEN","DCLOSE","DSETALIAS","DPACK",
    "DLOCKF","DLOCK","DLOCKR","DUNLOCK","DNOPEN","DNCLOSE",
    "DNCLOSEALL","DNEW","DADD","DAPPEND","DTOP","DGO",
    "DBOTTOM","DSKIP","DBLANK","DDELETE","DRECALL","DTAG",
    "DSEEK","DFBLANK","DGET","DPUT","DFCOPY","DSELECT",
    "DCHKSTAT","PCBACCOUNT","PCBACCSTAT","DERRMSG",
    "ACCOUNT","SCANMSGHDR","CHECKRIP","RIPVER","QWKLIMITS",
    "FINDFIRST","FINDNEXT","USELMRS","CONFINFO","TINKEY",
    "CWD","INSTRR","FDORDAKA","FDORDORG","FDORDAREA",
    "FDOQRD","GETDRIVE","SETDRIVE","BS2I","BD2I","I2BS",
    "I2BD","FTELL","OS"
];

#[derive(Debug)]
pub struct StatementDefinition<'a>
{
    pub name   : &'a str,
    pub opcode : u8,
    pub min_args : i8,
    pub max_args : i8,
}
#[repr(u8)]
#[derive(Copy, Clone)]
pub enum OpCode {
    END = 1,
    CLS = 2,
    CLREOL = 3,
    MORE = 4,
    WAIT = 5,
    COLOR = 6,
    GOTO = 7,
    LET = 8,
    PRINT = 9,
    PRINTLN = 10,
    IF = 11,
    CONFFLAG = 12,
    CONFUNFLAG = 13,
    DISPFILE = 14,
    INPUT = 15,
    FCREATE = 16,
    FOPEN = 17,
    FAPPEND = 18,
    FCLOSE = 19,
    FGET = 20,
    FPUT = 21,
    FPUTLN = 22,
    RESETDISP = 23,
    STARTDISP = 24,
    FPUTPAD = 25,
    HANGUP = 26,
    GETUSER = 27,
    PUTUSER = 28,
    DEFCOLOR = 29,
    DELETE = 30,
    DELUSER = 31,
    ADJTIME = 32,
    LOG = 33,
    INPUTSTR = 34,
    INPUTYN = 35,
    INPUTMONEY = 36,
    INPUTINT = 37,
    INPUTCC = 38,
    INPUTDATE = 39,
    INPUTTIME = 40,
    GOSUB = 41,
    RETURN = 42,
    PROMPTSTR = 43,
    DTRON = 44,
    DTROFF = 45,
    CDCHKON = 46,
    CDCHKOFF = 47,
    DELAY = 48,
    SENDMODEM = 49,
    INC = 50,
    DEC = 51,
    NEWLINE = 52,
    NEWLINES = 53,
    TOKENIZE = 54,
    GETTOKEN = 55,
    SHELL = 56,
    DISPTEXT = 57,
    STOP = 58,
    INPUTTEXT = 59,
    BEEP = 60,
    PUSH = 61,
    POP = 62,
    KBDSTUFF = 63,
    CALL = 64,
    JOIN = 65,
    QUEST = 66,
    BLT = 67,
    DIR = 68,
    KBDFILE = 69,
    BYE = 70,
    GOODBYE = 71,
    BROADCAST = 72,
    WAITFOR = 73,
    KBDCHKON = 74,
    KBDCHKOFF = 75,
    OPTEXT = 76,
    DISPSTR = 77,
    RDUNET = 78,
    WRUNET = 79,
    DOINTR = 80,
    VARSEG = 81,
    VAROFF = 82,
    POKEB = 83,
    POKEW = 84,
    VARADDR = 85,
    ANSIPOS = 86,
    BACKUP = 87,
    FORWARD = 88,
    FRESHLINE = 89,
    WRUSYS = 90,
    RDUSYS = 91,
    NEWPWD = 92,
    OPENCAP = 93,
    CLOSECAP = 94,
    MESSAGE = 95,
    SAVESCRN = 96,
    RESTSCRN = 97,
    SOUND = 98,
    CHAT = 99,
    SPRINT = 100,
    SPRINTLN = 101,
    MPRINT = 102,
    MPRINTLN = 103,
    RENAME = 104,
    FREWIND = 105,
    POKEDW = 106,
    DBGLEVEL = 107,
    SHOWON = 108,
    SHOWOFF = 109,
    PAGEON = 110,
    PAGEOFF = 111,
    FSEEK = 112,
    FFLUSH = 113,
    FREAD = 114,
    FWRITE = 115,
    FDEFIN = 116,
    FDEFOUT = 117,
    FDGET = 118,
    FDPUT = 119,
    FDPUTLN = 120,
    FDPUTPAD = 121,
    FDREAD = 122,
    FDWRITE = 123,
    ADJBYTES = 124,
    KBDSTRING = 125,
    ALIAS = 126,
    REDIM = 127,
    APPEND = 128,
    COPY = 129,
    KBDFLUSH = 130,
    MDMFLUSH = 131,
    KEYFLUSH = 132,
    LASTIN = 133,
    FLAG = 134,
    DOWNLOAD = 135,
    WRUSYSDOOR = 136,
    GETALTUSER = 137,
    ADJDBYTES = 138,
    ADJTBYTES = 139,
    ADJTFILES = 140,
    LANG = 141,
    SORT = 142,
    MOUSEREG = 143,
    SCRFILE = 144,
    SEARCHINIT = 145,
    SEARCHFIND = 146,
    SEARCHSTOP = 147,
    PRFOUND = 148,
    PRFOUNDLN = 149,
    TPAGET = 150,
    TPAPUT = 151,
    TPACGET = 152,
    TPACPUT = 153,
    TPAREAD = 154,
    TPAWRITE = 155,
    TPACREAD = 156,
    TPACWRITE = 157,
    BITSET = 158,
    BITCLEAR = 159,
    BRAG = 160,
    FREALTUSER = 161,
    SETLMR = 162,
    SETENV = 163,
    FCLOSEALL = 164,
    DECLARE = 165,
    FUNCTION = 166,
    PROCEDURE = 167,
    PCALL = 168,
    FPCLR = 169,
    BEGIN = 170,
    FEND = 171,
    STATIC = 172,
    STACKABORT = 173,
    DCREATE = 174,    // dbfchan,name,exclusive, fieldinfo, taginfo
    DOPEN = 175,      // dbfchan,name, exclusive
    DCLOSE = 176,     // dbfchan
    DSETALIAS = 177,  // dbfchan,name
    DPACK = 178,      // dbfchannel
    DCLOSEALL = 179,  // -
    DLOCK = 180,      // dbfchannel dlock & dlockf do the same thing
    DLOCKR = 181,     // dbfchannel, recno
    DLOCKG = 182,     // dbfchannel, recnos,count
    DUNLOCK = 183,    // dbfchannel
    DNCREATE = 184,   // dbfchannel,name,taginfo
    DNOPEN = 185,     // dbfchannle, name
    DNCLOSE = 186,    // dbfchannel, name
    DNCLOSEALL = 187, // channel
    DNEW = 188,       // channel
    DADD = 189,       // channel
    DAPPEND = 190,    // channel
    DTOP = 191,       // channel
    DGO = 192,        // channel, recno
    DBOTTOM = 193,    // channel
    DSKIP = 194,      // channel, number
    DBLANK = 195,     // channel
    DDELETE = 196,    // channel
    DRECALL = 197,    // channel
    DTAG = 198,       // channel, name
    DSEEK = 199,      // channel, expression
    DFBLANK = 200,    // channel, name
    DGET = 201,       // channel, name , var
    DPUT = 202,       // channel, name, expression
    DFCOPY = 203,     // channel, name, channel, name

    EVAL = 204,
    ACCOUNT = 205,
    RECORDUSAGE = 206,
    MSGTOFILE = 207,
    QWKLIMITS = 208,
    COMMAND = 209,
    USELMRS = 210,
    CONFINFO = 211,
    ADJTUBYTES = 212,
    GRAFMODE = 213,
    ADDUSER = 214,
    KILLMSG = 215,
    CHDIR = 216,
    MKDIR = 217,
    REDIR = 218,
    FDOWRAKA = 219,
    FDOADDAKA = 220,
    FDOWRORG = 221,
    FDOADDORG = 222,
    FDOQMOD = 223,
    FDOQADD = 224,
    FDOQDEL = 225,
    SOUNDDELAY = 226
}

pub const LAST_STMT : i32 = 226;

pub static StatementDefinitions : [StatementDefinition;227] = [
    StatementDefinition { name: "END", opcode: OpCode::END as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "CLS", opcode: OpCode::CLS as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "CLREOL", opcode: OpCode::CLREOL as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "MORE", opcode: OpCode::MORE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "WAIT", opcode: OpCode::WAIT as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "COLOR", opcode: OpCode::COLOR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "GOTO", opcode: OpCode::GOTO as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "LET", opcode: OpCode::LET as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "PRINT", opcode: OpCode::PRINT as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "PRINTLN", opcode: OpCode::PRINTLN as u8, min_args:0, max_args:99 },
    StatementDefinition { name: "IF", opcode: OpCode::IF as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "CONFFLAG", opcode: OpCode::CONFFLAG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "CONFUNFLAG", opcode: OpCode::CONFUNFLAG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DISPFILE", opcode: OpCode::DISPFILE as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "INPUT", opcode: OpCode::INPUT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FCREATE", opcode: OpCode::FCREATE as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "FOPEN", opcode: OpCode::FOPEN as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "FAPPEND", opcode: OpCode::FAPPEND as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "FCLOSE", opcode: OpCode::FCLOSE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FGET", opcode: OpCode::FGET as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FPUT", opcode: OpCode::FPUT as u8, min_args:2, max_args:99 },
    StatementDefinition { name: "FPUTLN", opcode: OpCode::FPUTLN as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "RESETDISP", opcode: OpCode::RESETDISP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "STARTDISP", opcode: OpCode::STARTDISP as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FPUTPAD", opcode: OpCode::FPUTPAD as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "HANGUP", opcode: OpCode::HANGUP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "GETUSER", opcode: OpCode::GETUSER as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PUTUSER", opcode: OpCode::PUTUSER as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DEFCOLOR", opcode: OpCode::DEFCOLOR as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DELETE", opcode: OpCode::DELETE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DELUSER", opcode: OpCode::DELUSER as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "ADJTIME", opcode: OpCode::ADJTIME as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "LOG", opcode: OpCode::LOG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "INPUTSTR", opcode: OpCode::INPUTSTR as u8, min_args:6, max_args:6 },
    StatementDefinition { name: "INPUTYN", opcode: OpCode::INPUTYN as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "INPUTMONEY", opcode: OpCode::INPUTMONEY as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "INPUTINT", opcode: OpCode::INPUTINT as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "INPUTCC", opcode: OpCode::INPUTCC as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "INPUTDATE", opcode: OpCode::INPUTDATE as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "INPUTTIME", opcode: OpCode::INPUTTIME as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "GOSUB", opcode: OpCode::GOSUB as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "RETURN", opcode: OpCode::RETURN as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PROMPTSTR", opcode: OpCode::PROMPTSTR as u8, min_args:5, max_args:5 },
    StatementDefinition { name: "DTRON", opcode: OpCode::DTRON as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DTROFF", opcode: OpCode::DTROFF as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "CDCHKON", opcode: OpCode::CDCHKON as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "CDCHKOFF", opcode: OpCode::CDCHKOFF as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DELAY", opcode: OpCode::DELAY as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "SENDMODEM", opcode: OpCode::SENDMODEM as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "INC", opcode: OpCode::INC as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DEC", opcode: OpCode::DEC as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "NEWLINE", opcode: OpCode::NEWLINE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "NEWLINES", opcode: OpCode::NEWLINES as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "TOKENIZE", opcode: OpCode::TOKENIZE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "GETTOKEN", opcode: OpCode::GETTOKEN as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "SHELL", opcode: OpCode::SHELL as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "DISPTEXT", opcode: OpCode::DISPTEXT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "STOP", opcode: OpCode::STOP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "INPUTTEXT", opcode: OpCode::INPUTTEXT as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "BEEP", opcode: OpCode::BEEP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PUSH", opcode: OpCode::PUSH as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "POP", opcode: OpCode::POP as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "KBDSTUFF", opcode: OpCode::KBDSTUFF as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "CALL", opcode: OpCode::CALL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "JOIN", opcode: OpCode::JOIN as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "QUEST", opcode: OpCode::QUEST as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "BLT", opcode: OpCode::BLT as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DIR", opcode: OpCode::DIR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "KBDFILE", opcode: OpCode::KBDFILE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "BYE", opcode: OpCode::BYE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "GOODBYE", opcode: OpCode::GOODBYE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "BROADCAST", opcode: OpCode::BROADCAST as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "WAITFOR", opcode: OpCode::WAITFOR as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "KBDCHKON", opcode: OpCode::KBDCHKON as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "KBDCHKOFF", opcode: OpCode::KBDCHKOFF as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "OPTEXT", opcode: OpCode::OPTEXT as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DISPSTR", opcode: OpCode::DISPSTR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "RDUNET", opcode: OpCode::RDUNET as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "WRUNET", opcode: OpCode::WRUNET as u8, min_args:6, max_args:6 },
    StatementDefinition { name: "DOINTR", opcode: OpCode::DOINTR as u8, min_args:10, max_args:10 },
    StatementDefinition { name: "VARSEG", opcode: OpCode::VARSEG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "VAROFF", opcode: OpCode::VAROFF as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "POKEB", opcode: OpCode::POKEB as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "POKEW", opcode: OpCode::POKEW as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "VARADDR", opcode: OpCode::VARADDR as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "ANSIPOS", opcode: OpCode::ANSIPOS as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "BACKUP", opcode: OpCode::BACKUP as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FORWARD", opcode: OpCode::FORWARD as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FRESHLINE", opcode: OpCode::FRESHLINE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "WRUSYS", opcode: OpCode::WRUSYS as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "RDUSYS", opcode: OpCode::RDUSYS as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "NEWPWD", opcode: OpCode::NEWPWD as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "OPENCAP", opcode: OpCode::OPENCAP as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "CLOSECAP", opcode: OpCode::CLOSECAP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "MESSAGE", opcode: OpCode::MESSAGE as u8, min_args:9, max_args:9 },
    StatementDefinition { name: "SAVESCRN", opcode: OpCode::SAVESCRN as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "RESTSCRN", opcode: OpCode::RESTSCRN as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "SOUND", opcode: OpCode::SOUND as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "CHAT", opcode: OpCode::CHAT as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "SPRINT", opcode: OpCode::SPRINT as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "SPRINTLN", opcode: OpCode::SPRINTLN as u8, min_args:0, max_args:99 },
    StatementDefinition { name: "MPRINT", opcode: OpCode::MPRINT as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "MPRINTLN", opcode: OpCode::MPRINTLN as u8, min_args:0, max_args:99 },
    StatementDefinition { name: "RENAME", opcode: OpCode::RENAME as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FREWIND", opcode: OpCode::FREWIND as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "POKEDW", opcode: OpCode::POKEDW as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DBGLEVEL", opcode: OpCode::DBGLEVEL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "SHOWON", opcode: OpCode::SHOWON as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "SHOWOFF", opcode: OpCode::SHOWOFF as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PAGEON", opcode: OpCode::PAGEON as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PAGEOFF", opcode: OpCode::PAGEOFF as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "FSEEK", opcode: OpCode::FSEEK as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FFLUSH", opcode: OpCode::FFLUSH as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FREAD", opcode: OpCode::FREAD as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FWRITE", opcode: OpCode::FWRITE as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FDEFIN", opcode: OpCode::FDEFIN as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FDEFOUT", opcode: OpCode::FDEFOUT as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FDGET", opcode: OpCode::FDGET as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FDPUT", opcode: OpCode::FDPUT as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "FDPUTLN", opcode: OpCode::FDPUTLN as u8, min_args:0, max_args:99 },
    StatementDefinition { name: "FDPUTPAD", opcode: OpCode::FDPUTPAD as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FDREAD", opcode: OpCode::FDREAD as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FDWRITE", opcode: OpCode::FDWRITE as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "ADJBYTES", opcode: OpCode::ADJBYTES as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "KBDSTRING", opcode: OpCode::KBDSTRING as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ALIAS", opcode: OpCode::ALIAS as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "REDIM", opcode: OpCode::REDIM as u8, min_args:1, max_args:4 },
    StatementDefinition { name: "APPEND", opcode: OpCode::APPEND as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "COPY", opcode: OpCode::COPY as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "KBDFLUSH", opcode: OpCode::KBDFLUSH as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "MDMFLUSH", opcode: OpCode::MDMFLUSH as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "KEYFLUSH", opcode: OpCode::KEYFLUSH as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "LASTIN", opcode: OpCode::LASTIN as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FLAG", opcode: OpCode::FLAG as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DOWNLOAD", opcode: OpCode::DOWNLOAD as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "WRUSYSDOOR", opcode: OpCode::WRUSYSDOOR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "GETALTUSER", opcode: OpCode::GETALTUSER as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ADJDBYTES", opcode: OpCode::ADJDBYTES as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ADJTBYTES", opcode: OpCode::ADJTBYTES as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ADJTFILES", opcode: OpCode::ADJTFILES as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "LANG", opcode: OpCode::LANG as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "SORT", opcode: OpCode::SORT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "MOUSEREG", opcode: OpCode::MOUSEREG as u8, min_args:10, max_args:10 },
    StatementDefinition { name: "SCRFILE", opcode: OpCode::SCRFILE as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "SEARCHINIT", opcode: OpCode::SEARCHINIT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "SEARCHFIND", opcode: OpCode::SEARCHFIND as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "SEARCHSTOP", opcode: OpCode::SEARCHSTOP as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "PRFOUND", opcode: OpCode::PRFOUND as u8, min_args:1, max_args:99 },
    StatementDefinition { name: "PRFOUNDLN", opcode: OpCode::PRFOUNDLN as u8, min_args:0, max_args:99 },
    StatementDefinition { name: "TPAGET", opcode: OpCode::TPAGET as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "TPAPUT", opcode: OpCode::TPAPUT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "TPACGET", opcode: OpCode::TPACGET as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "TPACPUT", opcode: OpCode::TPACPUT as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "TPAREAD", opcode: OpCode::TPAREAD as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "TPAWRITE", opcode: OpCode::TPAWRITE as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "TPACREAD", opcode: OpCode::TPACREAD as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "TPACWRITE", opcode: OpCode::TPACWRITE as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "BITSET", opcode: OpCode::BITSET as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "BITCLEAR", opcode: OpCode::BITCLEAR as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "BRAG", opcode: OpCode::BRAG as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "FREALTUSER", opcode: OpCode::FREALTUSER as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "SETLMR", opcode: OpCode::SETLMR as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "SETENV", opcode: OpCode::SETENV as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FCLOSEALL", opcode: OpCode::FCLOSEALL as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DECLARE", opcode: OpCode::DECLARE as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FUNCTION", opcode: OpCode::FUNCTION as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "PROCEDURE", opcode: OpCode::PROCEDURE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "PCALL", opcode: OpCode::PCALL as u8, min_args:-1, max_args:-1 }, // ????
    StatementDefinition { name: "FPCLR", opcode: OpCode::FPCLR as u8, min_args:0, max_args:0 }, // ????
    StatementDefinition { name: "BEGIN", opcode: OpCode::BEGIN as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "FEND", opcode: OpCode::FEND as u8, min_args:0, max_args:0 }, // Both functions and procedures
    StatementDefinition { name: "STATIC", opcode: OpCode::STATIC as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "STACKABORT", opcode: OpCode::STACKABORT as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DCREATE", opcode: OpCode::DCREATE as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "DOPEN", opcode: OpCode::DOPEN as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "DCLOSE", opcode: OpCode::DCLOSE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DSETALIAS", opcode: OpCode::DSETALIAS as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DPACK", opcode: OpCode::DPACK as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DCLOSEALL", opcode: OpCode::DCLOSEALL as u8, min_args:0, max_args:0 },
    StatementDefinition { name: "DLOCK", opcode: OpCode::DLOCK as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DLOCKF", opcode: OpCode::DLOCK as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DLOCKR", opcode: OpCode::DLOCKR as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DLOCKG", opcode: OpCode::DLOCKG as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "DUNLOCK", opcode: OpCode::DUNLOCK as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DNCREATE", opcode: OpCode::DNCREATE as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "DNOPEN", opcode: OpCode::DNOPEN as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DNCLOSE", opcode: OpCode::DNCLOSE as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DNCLOSEALL", opcode: OpCode::DNCLOSEALL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DNEW", opcode: OpCode::DNEW as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DADD", opcode: OpCode::DADD as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DAPPEND", opcode: OpCode::DAPPEND as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DTOP", opcode: OpCode::DTOP as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DGO", opcode: OpCode::DGO as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DBOTTOM", opcode: OpCode::DBOTTOM as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DSKIP", opcode: OpCode::DSKIP as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DBLANK", opcode: OpCode::DBLANK as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DDELETE", opcode: OpCode::DDELETE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DRECALL", opcode: OpCode::DRECALL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "DTAG", opcode: OpCode::DTAG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DSEEK", opcode: OpCode::DSEEK as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DFBLANK", opcode: OpCode::DFBLANK as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "DGET", opcode: OpCode::DGET as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "DPUT", opcode: OpCode::DPUT as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "DFCOPY", opcode: OpCode::DFCOPY as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "EVAL", opcode: OpCode::EVAL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ACCOUNT", opcode: OpCode::ACCOUNT as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "RECORDUSAGE", opcode: OpCode::RECORDUSAGE as u8, min_args:5, max_args:5 },
    StatementDefinition { name: "MSGTOFILE", opcode: OpCode::MSGTOFILE as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "QWKLIMITS", opcode: OpCode::QWKLIMITS as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "COMMAND", opcode: OpCode::COMMAND as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "USELMRS", opcode: OpCode::USELMRS as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "CONFINFO", opcode: OpCode::CONFINFO as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "ADJTUBYTES", opcode: OpCode::ADJTUBYTES as u8, min_args:-1, max_args:-1 },
    StatementDefinition { name: "GRAFMODE", opcode: OpCode::GRAFMODE as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "ADDUSER", opcode: OpCode::ADDUSER as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "KILLMSG", opcode: OpCode::KILLMSG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "CHDIR", opcode: OpCode::CHDIR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "MKDIR", opcode: OpCode::MKDIR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "REDIR", opcode: OpCode::REDIR as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "FDOWRAKA", opcode: OpCode::FDOWRAKA as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FDOADDAKA", opcode: OpCode::FDOADDAKA as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FDOWRORG", opcode: OpCode::FDOWRORG as u8, min_args:2, max_args:2 },
    StatementDefinition { name: "FDOADDORG", opcode: OpCode::FDOADDORG as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FDOQMOD", opcode: OpCode::FDOQMOD as u8, min_args:4, max_args:4 },
    StatementDefinition { name: "FDOQADD", opcode: OpCode::FDOQADD as u8, min_args:3, max_args:3 },
    StatementDefinition { name: "FDOQDEL", opcode: OpCode::FDOQDEL as u8, min_args:1, max_args:1 },
    StatementDefinition { name: "SOUNDDELAY", opcode: OpCode::SOUNDDELAY as u8, min_args:2, max_args:2 }
];


