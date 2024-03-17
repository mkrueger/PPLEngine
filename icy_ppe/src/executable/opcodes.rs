use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum StatementSignature {
    /// The first usize is the variable - 0 for none, second is the number of arguments
    ArgumentsWithVariable(usize, usize),

    SpecialCaseProcedure,
    SpecialCaseDlockg,
    SpecialCaseDcreate,
    SpecialCaseSort,

    /// The first i32 is a variable, 0 for none
    VariableArguments(usize),

    SpecialCasePop,
    Label,
    SpecialIfWhen,
}

#[repr(i16)]
#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum OpCode {
    WHILE = 0,
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
    DCREATE = 174,
    DOPEN = 175,
    DCLOSE = 176,
    DSETALIAS = 177,
    DPACK = 178,
    DCLOSEALL = 179,
    DLOCK = 180,
    DLOCKR = 181,
    DLOCKG = 182,
    DUNLOCK = 183,
    DNCREATE = 184,
    DNOPEN = 185,
    DNCLOSE = 186,
    DNCLOSEALL = 187,
    DNEW = 188,
    DADD = 189,
    DAPPEND = 190,
    DTOP = 191,
    DGO = 192,
    DBOTTOM = 193,
    DSKIP = 194,
    DBLANK = 195,
    DDELETE = 196,
    DRECALL = 197,
    DTAG = 198,
    DSEEK = 199,
    DFBLANK = 200,
    DGET = 201,
    DPUT = 202,
    DFCOPY = 203,

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
    SOUNDDELAY = 226,
    ShortDesc = 227,
    MoveMsg = 228,
    SetBankBal = 229,
}

impl OpCode {
    pub fn get_definition(self) -> &'static StatementDefinition<'static> {
        &STATEMENT_DEFINITIONS[self as usize]
    }
}

impl Display for OpCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

#[derive(Debug, PartialEq)]
pub struct StatementDefinition<'a> {
    pub name: &'a str,
    pub opcode: OpCode,
    pub sig: StatementSignature,
}

pub static STATEMENT_DEFINITIONS: [StatementDefinition; 231] = [
    StatementDefinition {
        name: "WHILE",
        opcode: OpCode::WHILE,
        sig: StatementSignature::SpecialIfWhen,
    },
    StatementDefinition {
        name: "END",
        opcode: OpCode::END,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Cls",
        opcode: OpCode::CLS,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "ClrEOL",
        opcode: OpCode::CLREOL,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "More",
        opcode: OpCode::MORE,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Wait",
        opcode: OpCode::WAIT,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Color",
        opcode: OpCode::COLOR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "GOTO",
        opcode: OpCode::GOTO,
        sig: StatementSignature::Label,
    },
    StatementDefinition {
        name: "LET",
        opcode: OpCode::LET,
        sig: StatementSignature::ArgumentsWithVariable(1, 2),
    },
    StatementDefinition {
        name: "Print",
        opcode: OpCode::PRINT,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "PrintLn",
        opcode: OpCode::PRINTLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "IF",
        opcode: OpCode::IF,
        sig: StatementSignature::SpecialIfWhen,
    },
    StatementDefinition {
        name: "ConfFlag",
        opcode: OpCode::CONFFLAG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "ConfUnflag",
        opcode: OpCode::CONFUNFLAG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DispFile",
        opcode: OpCode::DISPFILE,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "Input",
        opcode: OpCode::INPUT,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "FCreate",
        opcode: OpCode::FCREATE,
        sig: StatementSignature::ArgumentsWithVariable(0, 4),
    },
    StatementDefinition {
        name: "FOpen",
        opcode: OpCode::FOPEN,
        sig: StatementSignature::ArgumentsWithVariable(0, 4),
    },
    StatementDefinition {
        name: "FAppend",
        opcode: OpCode::FAPPEND,
        sig: StatementSignature::ArgumentsWithVariable(0, 4),
    },
    StatementDefinition {
        name: "FClose",
        opcode: OpCode::FCLOSE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FGet",
        opcode: OpCode::FGET,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "FPut",
        opcode: OpCode::FPUT,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "FPutLn",
        opcode: OpCode::FPUTLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "ResetDisp",
        opcode: OpCode::RESETDISP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "StartDisp",
        opcode: OpCode::STARTDISP,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FPutPad",
        opcode: OpCode::FPUTPAD,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "Hangup",
        opcode: OpCode::HANGUP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "GetUser",
        opcode: OpCode::GETUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "PutUser",
        opcode: OpCode::PUTUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "DefColor",
        opcode: OpCode::DEFCOLOR,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Delete",
        opcode: OpCode::DELETE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DelUser",
        opcode: OpCode::DELUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "AdjTime",
        opcode: OpCode::ADJTIME,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Log",
        opcode: OpCode::LOG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "InputStr",
        opcode: OpCode::INPUTSTR,
        sig: StatementSignature::ArgumentsWithVariable(2, 6),
    },
    StatementDefinition {
        name: "InputYN",
        opcode: OpCode::INPUTYN,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "InputMoney",
        opcode: OpCode::INPUTMONEY,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "InputInt",
        opcode: OpCode::INPUTINT,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "InputCC",
        opcode: OpCode::INPUTCC,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "InputDate",
        opcode: OpCode::INPUTDATE,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "InputTime",
        opcode: OpCode::INPUTTIME,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "GOSUB",
        opcode: OpCode::GOSUB,
        sig: StatementSignature::Label,
    },
    StatementDefinition {
        name: "RETURN",
        opcode: OpCode::RETURN,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "PromptStr",
        opcode: OpCode::PROMPTSTR,
        sig: StatementSignature::ArgumentsWithVariable(2, 5),
    },
    StatementDefinition {
        name: "DtrOn",
        opcode: OpCode::DTRON,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "DtrOff",
        opcode: OpCode::DTROFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "CdchkOn",
        opcode: OpCode::CDCHKON,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "CdchkOff",
        opcode: OpCode::CDCHKOFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Delay",
        opcode: OpCode::DELAY,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "SendModem",
        opcode: OpCode::SENDMODEM,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Inc",
        opcode: OpCode::INC,
        sig: StatementSignature::ArgumentsWithVariable(1, 1),
    },
    StatementDefinition {
        name: "Dec",
        opcode: OpCode::DEC,
        sig: StatementSignature::ArgumentsWithVariable(1, 1),
    },
    StatementDefinition {
        name: "NewLine",
        opcode: OpCode::NEWLINE,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "NewLines",
        opcode: OpCode::NEWLINES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Tokenize",
        opcode: OpCode::TOKENIZE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "GetToken",
        opcode: OpCode::GETTOKEN,
        sig: StatementSignature::ArgumentsWithVariable(1, 1),
    },
    StatementDefinition {
        name: "Shell",
        opcode: OpCode::SHELL,
        sig: StatementSignature::ArgumentsWithVariable(2, 4),
    },
    StatementDefinition {
        name: "DispText",
        opcode: OpCode::DISPTEXT,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "STOP",
        opcode: OpCode::STOP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "InputText",
        opcode: OpCode::INPUTTEXT,
        sig: StatementSignature::ArgumentsWithVariable(2, 4),
    },
    StatementDefinition {
        name: "Beep",
        opcode: OpCode::BEEP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Push",
        opcode: OpCode::PUSH,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "Pop",
        opcode: OpCode::POP,
        sig: StatementSignature::ArgumentsWithVariable(12, 15),
    },
    StatementDefinition {
        name: "KbdStuff",
        opcode: OpCode::KBDSTUFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Call",
        opcode: OpCode::CALL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Join",
        opcode: OpCode::JOIN,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Quest",
        opcode: OpCode::QUEST,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Blt",
        opcode: OpCode::BLT,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Dir",
        opcode: OpCode::DIR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "KbdFile",
        opcode: OpCode::KBDFILE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Bye",
        opcode: OpCode::BYE,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Goodbye",
        opcode: OpCode::GOODBYE,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Broadcast",
        opcode: OpCode::BROADCAST,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "WaitFor",
        opcode: OpCode::WAITFOR,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "KbdchkOn",
        opcode: OpCode::KBDCHKON,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "KbdchkOff",
        opcode: OpCode::KBDCHKOFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "OpText",
        opcode: OpCode::OPTEXT,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DispStr",
        opcode: OpCode::DISPSTR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "RDUnet",
        opcode: OpCode::RDUNET,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "WRUnet",
        opcode: OpCode::WRUNET,
        sig: StatementSignature::ArgumentsWithVariable(0, 6),
    },
    StatementDefinition {
        name: "DoIntr",
        opcode: OpCode::DOINTR,
        sig: StatementSignature::ArgumentsWithVariable(0, 10),
    },
    StatementDefinition {
        name: "VarSeg",
        opcode: OpCode::VARSEG,
        sig: StatementSignature::ArgumentsWithVariable(2, 15),
    },
    StatementDefinition {
        name: "VarOff",
        opcode: OpCode::VAROFF,
        sig: StatementSignature::ArgumentsWithVariable(2, 15),
    },
    StatementDefinition {
        name: "PokeB",
        opcode: OpCode::POKEB,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "PokeW",
        opcode: OpCode::POKEW,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "VarAddr",
        opcode: OpCode::VARADDR,
        sig: StatementSignature::ArgumentsWithVariable(2, 15),
    },
    StatementDefinition {
        name: "AnsiPos",
        opcode: OpCode::ANSIPOS,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "Backup",
        opcode: OpCode::BACKUP,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Forward",
        opcode: OpCode::FORWARD,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Freshline",
        opcode: OpCode::FRESHLINE,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "WRUSys",
        opcode: OpCode::WRUSYS,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "RDUSys",
        opcode: OpCode::RDUSYS,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "NewPwd",
        opcode: OpCode::NEWPWD,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "OpenCap",
        opcode: OpCode::OPENCAP,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "CloseCap",
        opcode: OpCode::CLOSECAP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Message",
        opcode: OpCode::MESSAGE,
        sig: StatementSignature::ArgumentsWithVariable(0, 9),
    },
    StatementDefinition {
        name: "SaveScrn",
        opcode: OpCode::SAVESCRN,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "RestScrn",
        opcode: OpCode::RESTSCRN,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Sound",
        opcode: OpCode::SOUND,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Chat",
        opcode: OpCode::CHAT,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "SPrint",
        opcode: OpCode::SPRINT,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "SPrintLN",
        opcode: OpCode::SPRINTLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "MPrint",
        opcode: OpCode::MPRINT,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "MPrintLn",
        opcode: OpCode::MPRINTLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "Rename",
        opcode: OpCode::RENAME,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "FRewind",
        opcode: OpCode::FREWIND,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "PokeDW",
        opcode: OpCode::POKEDW,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DbgLevel",
        opcode: OpCode::DBGLEVEL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "ShowOn",
        opcode: OpCode::SHOWON,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "ShowOff",
        opcode: OpCode::SHOWOFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "PageOn",
        opcode: OpCode::PAGEON,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "PageOFf",
        opcode: OpCode::PAGEOFF,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "FSeek",
        opcode: OpCode::FSEEK,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "FFlush",
        opcode: OpCode::FFLUSH,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FRead",
        opcode: OpCode::FREAD,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "FWrite",
        opcode: OpCode::FWRITE,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "FDefIn",
        opcode: OpCode::FDEFIN,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FDefOut",
        opcode: OpCode::FDEFOUT,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FDGet",
        opcode: OpCode::FDGET,
        sig: StatementSignature::ArgumentsWithVariable(1, 1),
    },
    StatementDefinition {
        name: "FDPut",
        opcode: OpCode::FDPUT,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "FDPutLn",
        opcode: OpCode::FDPUTLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "FDPutPad",
        opcode: OpCode::FDPUTPAD,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "FDRead",
        opcode: OpCode::FDREAD,
        sig: StatementSignature::ArgumentsWithVariable(1, 2),
    },
    StatementDefinition {
        name: "FDWrite",
        opcode: OpCode::FDWRITE,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "AdjBytes",
        opcode: OpCode::ADJBYTES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "KbdString",
        opcode: OpCode::KBDSTRING,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Alias",
        opcode: OpCode::ALIAS,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "ReDim",
        opcode: OpCode::REDIM,
        sig: StatementSignature::VariableArguments(1),
    },
    StatementDefinition {
        name: "Append",
        opcode: OpCode::APPEND,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "Copy",
        opcode: OpCode::COPY,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "KbdFlush",
        opcode: OpCode::KBDFLUSH,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "MdmFlush",
        opcode: OpCode::MDMFLUSH,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "KeyFlush",
        opcode: OpCode::KEYFLUSH,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "LastIn",
        opcode: OpCode::LASTIN,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Flag",
        opcode: OpCode::FLAG,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Download",
        opcode: OpCode::DOWNLOAD,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "WRUsysDoor",
        opcode: OpCode::WRUSYSDOOR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "GetAltUser",
        opcode: OpCode::GETALTUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "AdjDBytes",
        opcode: OpCode::ADJDBYTES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "AdjTBytes",
        opcode: OpCode::ADJTBYTES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "AyjTFiles",
        opcode: OpCode::ADJTFILES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Lang",
        opcode: OpCode::LANG,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Sort",
        opcode: OpCode::SORT,
        sig: StatementSignature::SpecialCaseSort,
    },
    StatementDefinition {
        name: "MouseReg",
        opcode: OpCode::MOUSEREG,
        sig: StatementSignature::ArgumentsWithVariable(0, 10),
    },
    StatementDefinition {
        name: "ScrFile",
        opcode: OpCode::SCRFILE,
        sig: StatementSignature::ArgumentsWithVariable(2, 15),
    },
    StatementDefinition {
        name: "SearchInit",
        opcode: OpCode::SEARCHINIT,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "SearchFind",
        opcode: OpCode::SEARCHFIND,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "SearchStop",
        opcode: OpCode::SEARCHSTOP,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "PrFound",
        opcode: OpCode::PRFOUND,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "PrFoundLn",
        opcode: OpCode::PRFOUNDLN,
        sig: StatementSignature::VariableArguments(0),
    },
    StatementDefinition {
        name: "TPAGet",
        opcode: OpCode::TPAGET,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "TPAPut",
        opcode: OpCode::TPAPUT,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "TPACGea",
        opcode: OpCode::TPACGET,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "TPACPut",
        opcode: OpCode::TPACPUT,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "TPARead",
        opcode: OpCode::TPAREAD,
        sig: StatementSignature::ArgumentsWithVariable(2, 2),
    },
    StatementDefinition {
        name: "TPAWrite",
        opcode: OpCode::TPAWRITE,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "TPACRead",
        opcode: OpCode::TPACREAD,
        sig: StatementSignature::ArgumentsWithVariable(2, 3),
    },
    StatementDefinition {
        name: "TPACWrite",
        opcode: OpCode::TPACWRITE,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "BitSet",
        opcode: OpCode::BITSET,
        sig: StatementSignature::ArgumentsWithVariable(1, 2),
    },
    StatementDefinition {
        name: "BitClear",
        opcode: OpCode::BITCLEAR,
        sig: StatementSignature::ArgumentsWithVariable(1, 2),
    },
    StatementDefinition {
        name: "Brag",
        opcode: OpCode::BRAG,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "FRealTUser",
        opcode: OpCode::FREALTUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "SetLMR",
        opcode: OpCode::SETLMR,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "SetEnv",
        opcode: OpCode::SETENV,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FCloseAll",
        opcode: OpCode::FCLOSEALL,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Declare",
        opcode: OpCode::DECLARE,
        sig: StatementSignature::ArgumentsWithVariable(10, 10),
    },
    StatementDefinition {
        name: "Function",
        opcode: OpCode::FUNCTION,
        sig: StatementSignature::ArgumentsWithVariable(10, 10),
    },
    StatementDefinition {
        name: "Procedure",
        opcode: OpCode::PROCEDURE,
        sig: StatementSignature::ArgumentsWithVariable(10, 10),
    },
    StatementDefinition {
        name: "PCALL",
        opcode: OpCode::PCALL,
        sig: StatementSignature::SpecialCaseProcedure,
    },
    StatementDefinition {
        name: "FPCLR",
        opcode: OpCode::FPCLR,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Begin",
        opcode: OpCode::BEGIN,
        sig: StatementSignature::ArgumentsWithVariable(10, 10),
    },
    StatementDefinition {
        name: "FEND",
        opcode: OpCode::FEND,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "Static",
        opcode: OpCode::STATIC,
        sig: StatementSignature::ArgumentsWithVariable(10, 10),
    },
    StatementDefinition {
        name: "StackAbort",
        opcode: OpCode::STACKABORT,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DCreate",
        opcode: OpCode::DCREATE,
        sig: StatementSignature::SpecialCaseDcreate,
    },
    StatementDefinition {
        name: "DOpen",
        opcode: OpCode::DOPEN,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "DClose",
        opcode: OpCode::DCLOSE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DSetAlias",
        opcode: OpCode::DSETALIAS,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DPack",
        opcode: OpCode::DPACK,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DCloseAll",
        opcode: OpCode::DCLOSEALL,
        sig: StatementSignature::ArgumentsWithVariable(0, 0),
    },
    StatementDefinition {
        name: "DLock",
        opcode: OpCode::DLOCK,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DLockR",
        opcode: OpCode::DLOCKR,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DLockG",
        opcode: OpCode::DLOCKG,
        sig: StatementSignature::SpecialCaseDlockg,
    },
    StatementDefinition {
        name: "DUnlock",
        opcode: OpCode::DUNLOCK,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DNCreate",
        opcode: OpCode::DNCREATE,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "DNOpen",
        opcode: OpCode::DNOPEN,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DNClose",
        opcode: OpCode::DNCLOSE,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DNCloseAll",
        opcode: OpCode::DNCLOSEALL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DNew",
        opcode: OpCode::DNEW,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DAdd",
        opcode: OpCode::DADD,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DAppend",
        opcode: OpCode::DAPPEND,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DTop",
        opcode: OpCode::DTOP,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DGo",
        opcode: OpCode::DGO,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DBottom",
        opcode: OpCode::DBOTTOM,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DSkip",
        opcode: OpCode::DSKIP,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DBlank",
        opcode: OpCode::DBLANK,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DDelete",
        opcode: OpCode::DDELETE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DRecall",
        opcode: OpCode::DRECALL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "DTag",
        opcode: OpCode::DTAG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DSeek",
        opcode: OpCode::DSEEK,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DFBlank",
        opcode: OpCode::DFBLANK,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "DGet",
        opcode: OpCode::DGET,
        sig: StatementSignature::ArgumentsWithVariable(3, 3),
    },
    StatementDefinition {
        name: "DPut",
        opcode: OpCode::DPUT,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "DFCopy",
        opcode: OpCode::DFCOPY,
        sig: StatementSignature::ArgumentsWithVariable(0, 4),
    },
    StatementDefinition {
        name: "Eval",
        opcode: OpCode::EVAL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "Account",
        opcode: OpCode::ACCOUNT,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "RecordUsage",
        opcode: OpCode::RECORDUSAGE,
        sig: StatementSignature::ArgumentsWithVariable(0, 5),
    },
    StatementDefinition {
        name: "MsgToFile",
        opcode: OpCode::MSGTOFILE,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "QwkLimits",
        opcode: OpCode::QWKLIMITS,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "Command",
        opcode: OpCode::COMMAND,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "USelMrs",
        opcode: OpCode::USELMRS,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "ConfInfo",
        opcode: OpCode::CONFINFO,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "AdjTUBytes",
        opcode: OpCode::ADJTUBYTES,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "GrafMode",
        opcode: OpCode::GRAFMODE,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "AddUser",
        opcode: OpCode::ADDUSER,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "KillMsg",
        opcode: OpCode::KILLMSG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "ChDir",
        opcode: OpCode::CHDIR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "MkDir",
        opcode: OpCode::MKDIR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "ReDir",
        opcode: OpCode::REDIR,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "FDOWRAka",
        opcode: OpCode::FDOWRAKA,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "FDOADDAka",
        opcode: OpCode::FDOADDAKA,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "FDOWROrg",
        opcode: OpCode::FDOWRORG,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "FDOADDOrg",
        opcode: OpCode::FDOADDORG,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "FDOQMod",
        opcode: OpCode::FDOQMOD,
        sig: StatementSignature::ArgumentsWithVariable(0, 4),
    },
    StatementDefinition {
        name: "FDOQAdd",
        opcode: OpCode::FDOQADD,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "FDOQDel",
        opcode: OpCode::FDOQDEL,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "SoundDelay",
        opcode: OpCode::SOUNDDELAY,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    StatementDefinition {
        name: "ShortDesc",
        opcode: OpCode::ShortDesc,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
    StatementDefinition {
        name: "MoveMsg",
        opcode: OpCode::MoveMsg,
        sig: StatementSignature::ArgumentsWithVariable(0, 3),
    },
    StatementDefinition {
        name: "SetBankBal",
        opcode: OpCode::SetBankBal,
        sig: StatementSignature::ArgumentsWithVariable(0, 2),
    },
    // Special case 'DLOCKF' is a synonym  to DLOCK
    // Moving to the end, so that the opcode <--> index mapping is not broken
    StatementDefinition {
        name: "DLockF",
        opcode: OpCode::DLOCK,
        sig: StatementSignature::ArgumentsWithVariable(0, 1),
    },
];

#[test]
fn check_table_consistency() {
    for def in &STATEMENT_DEFINITIONS {
        assert!(
            def.opcode == STATEMENT_DEFINITIONS[def.opcode as usize].opcode,
            "Opcode table mismatch: {:?}/{} was '{}':{:?}",
            def.opcode,
            def.opcode as usize,
            STATEMENT_DEFINITIONS[def.opcode as usize].name,
            STATEMENT_DEFINITIONS[def.opcode as usize].opcode
        );
    }
}

/*
#[test]
fn convert_definitions() {
    for def in &STATEMENT_DEFINITIONS {
        if def.opcode as usize >= STATEMENT_SIGNATURE_TABLE.len() {
            break;
        }
        print!("StatementDefinition {{ name: \"{}\", opcode: OpCode::{}, sig:", def.name, def.opcode);
        let sig = STATEMENT_SIGNATURE_TABLE[def.opcode as usize];
        match sig {
            0 => print!("StatementSignature::ArgumentsWithVariable(0, 0)"),
            1..=0xF => print!("StatementSignature::ArgumentsWithVariable(0, {})", sig),
            0x11..=0x1F => print!("StatementSignature::ArgumentsWithVariable(1, {})", sig - 0x10),
            0x21..=0x2F => print!("StatementSignature::ArgumentsWithVariable(2, {})", sig - 0x20),
            0xF6 => print!("StatementSignature::SpecialProcedure"),
            0xF7 => print!("StatementSignature::SpecialDLockG"),
            0xF8 => print!("StatementSignature::SpecialDCreate"),
            0xF9 => print!("StatementSignature::SpecialSort"),
            0xFA => print!("StatementSignature::VariableArguments(1)"),
            0xFD => print!("StatementSignature::Label"),
            0xFE => print!("StatementSignature::VariableArguments(0)"),
            0xFF => print!("StatementSignature::SpecialIfWhen"),
            _ => {
                let max = (STATEMENT_SIGNATURE_TABLE[def.opcode as usize] & 0xf0) * 16 + (STATEMENT_SIGNATURE_TABLE[def.opcode as usize] & 0x0f);
                let argument = max & 0xFF;
                let variable = max / 256;
                print!("StatementSignature::ArgumentsWithVariable({}, {})", argument, variable);
            }

        }
        print!("}},\n");
    }
}
*/
