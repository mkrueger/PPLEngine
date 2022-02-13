using System;
namespace PPLD.Decompiler
{
    partial class Decompiler
    {
        /*
		long int PrASVars[]={0,1,2,3};
		*/

        /*
		long int PrPAVars[]={0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
							 0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e
							};
		*/

        /*
		long int PrSMVars[]={0x01,0x02,0x03,0x04,0x05,0x06,0x07,
							 0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f
							};
		*/

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
        static readonly short[] STATEMENT_SIGNATURE_TABLE =
        {
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
		};

        // Funktions Variable-Types, 0 = no exp / aa = not defined
        //			     1- f= numbers of exp using ()
        //              0x10 = only rvalue (!)
        //			    0x11 = lvalue rvalue (+,-,...)
        static readonly int[] FUNCTION_SIGNATURE_TABLE =
        {
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
		};

        static readonly string[] TYPE_NAMES =
        {
            "BOOLEAN","UNSIGNED","DATE","EDATE","INTEGER",
            "MONEY","REAL","STRING","TIME","BYTE","WORD",
            "SBYTE","SWORD","BIGSTR","DREAL","---","---","DDATE"
        };

        static readonly string[] USER_VARIABLE_NAMES =
        {
            "","U_EXPERT","U_FSE","U_FSEP","U_CLS","U_EXPDATE",
            "U_SEC","U_PAGELEN","U_EXPSEC","U_CITY","U_BDPHONE",
            "U_HVPHONE","U_TRANS","U_CMNT1","U_CMNT2","U_PWD",
            "U_SCROLL","U_LONGHDR","U_DEF79","U_ALIAS","U_VER",
            "U_ADDR","U_NOTES","U_PWDEXP","U_ACCOUNT"
        };

        static readonly int[] CONSTANT_2_OFFSETS =
        {
            0x00,0x01,0x02,0x03,0x04,0x05,0x06,0x07,
            0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f,
            0x10,0x11
        };
        static readonly string[] CONSTANT_2_NAMES =
        {
            "START_BAL","START_SESSION","DEB_CALL","DEB_TIME",
            "DEB_MSGREAD","DEB_MSGCAP","DEB_MSGWRITE",
            "DEB_MSGECHOED","DEB_MSGPRIVATE","DEB_DOWNFILE",
            "DEB_DOWNBYTES","DEB_CHAT","DEB_TPU","DEB_SPECIAL",
            "CRED_UPFILE","CRED_UPBYTES","CRED_SPECIAL","SEC_DROP",
        };

        /*
		char    *PrASNames[]={"ACC_STAT","ACC_TIME","ACC_MSGR","ACC_MSGW"};
		*/

        static readonly int[] CONSTANT_CONFERENCE_OFFSETS = { 0x02, 0x10, 0x01, 0x04, 0x08, -1 };
        static readonly string[] CONSTANT_CONFERENCE_NAMES = { "F_EXP", "F_MW", "F_REG", "F_SEL", "F_SYS" };

        static readonly int[] CONSTANT_NAMES_OFFSETS = { 0, 1, 4, 2, -1 };
        static readonly string[] CONSTANT_NAMES_DISPLAY = { "DEFS", "GRAPH", "LANG", "SEC" };

        static readonly int[] CONSTANT_FACCESS_OFFSETS = { 0, 2, 1, -1 };
        static readonly string[] CONSTANT_FACCESS_NAMES = { "O_RD", "O_RW", "O_WR" };

        static readonly int[] CONSTANT_SEEK_OFFSETS = { 1, 2, 0, -1 };
        static readonly string[] CONSTANT_SEEK_NAMES = { "SEEK_CUR", "SEEK_END", "SEEK_SET" };

        static readonly int[] CONSTANT_OPENFLAGS_OFFSETS = { 3, 0, 1, 2, -1 };
        static readonly string[] CONSTANT_OPENFLAGS_NAMES = { "S_DB", "S_DN", "S_DR", "S_DW" };

        static readonly int[] CONSTANT_1_OFFSETS = {
            0x2000,0x0800,0x0000,0x0001,0x0020,0x0002,0x0004,
            0x1000,0x0100,0x0080,0x8000,0x10000,0x0040,0x0400,
            0x0010,0x0008,0x0200,0x4000,-1
        };
        static readonly string[] CONSTANT_1_NAMES =
            {
            "AUTO","BELL","DEFS","ECHODOTS","ERASELINE","FIELDLEN",
            "GUIDE","HIGHASCII","LFAFTER","LFBEFORE","LOGIT",
            "LOGITLEFT","NEWLINE","NOCLEAR","STACKED","UPCASE",
            "WORDWRAP","YESNO"
        };

        /*
		char    *PrPANames[]={"NEWBALANCE","CHRG_CALL","CHRG_TIME","CHRG_PEAKTIME",
							  "CHRG_CHAT","CHRG_MSGREAD","CHRG_MSGCAP",
							  "CHRG_MSGWRITE","CHRG_MSGECHOED","CHRG_MSGPRIVATE",
							  "CHRG_DOWNFILE","CHRG_DOWNBYTES","PAY_UPFILE",
							  "PAY_UPBYTES","WARN_LEVEL"
							 };
		*/

        static readonly int[] CONSTANT_LINECOUNT_OFFSETS = {
            2, 1, 0, -1
        };
        static readonly string[] CONSTANT_LINECOUNT_NAMES = { "FCL", "FNS", "NC" };

        /*
		char    *PrSMNames[]={"HDR_STATUS","HDR_MSGNUM","HDR_MSGREF","HDR_BLOCKS",
							  "HDR_DATE","HDR_TIME","HDR_TO","HDR_RPLYDATE",
							  "HDR_RPLYTIME","HDR_REPLY","HDR_FROM","HDR_SUBJ",
							  "HDR_PWD","HDR_ACTIVE","HDR_ECHO"
							 };
		*/

        static readonly string[] STATEMENT_NAMES =
        {
            "WHILE","END","CLS","CLREOL","MORE","WAIT","COLOR","GOTO",
            "LET","PRINT","PRINTLN","IF","CONFFLAG","CONFUNFLAG",
            "DISPFILE","INPUT","FCREATE","FOPEN","FAPPEND",
            "FCLOSE","FGET","FPUT","FPUTLN","RESETDISP",
            "STARTDISP","FPUTPAD","HANGUP","GETUSER","PUTUSER",
            "DEFCOLOR","DELETE","DELUSER","ADJTIME","LOG",
            "INPUTSTR","INPUTYN","INPUTMONEY","INPUTINT","INPUTCC",
            "INPUTDATE","INPUTTIME","GOSUB","RETURN","PROMPTSTR",
            "DTRON","DTROFF","CDCHKON","CDCHKOFF","DELAY",
            "SENDMODEM","INC","DEC","NEWLINE","NEWLINES",
            "TOKENIZE","GETTOKEN","SHELL","DISPTEXT","STOP",
            "INPUTTEXT","BEEP","PUSH","POP","KBDSTUFF","CALL",
            "JOIN","QUEST","BLT","DIR","KBDFILE","BYE","GOODBYE",
            "BROADCAST","WAITFOR","KBDCHKON","KBDCHKOFF","OPTEXT",
            "DISPSTR","RDUNET","WRUNET","DOINTR","VARSEG","VAROFF",
            "POKEB","POKEW","VARADDR","ANSIPOS","BACKUP","FORWARD",
            "FRESHLINE","WRUSYS","RDUSYS","NEWPWD","OPENCAP",
            "CLOSECAP","MESSAGE","SAVESCRN","RESTSCRN","SOUND",
            "CHAT","SPRINT","SPRINTLN","MPRINT","MPRINTLN",
            "RENAME","FREWIND","POKEDW","DBGLEVEL","SHOWON",
            "SHOWOFF","PAGEON","PAGEOFF","FSEEK","FFLUSH","FREAD",
            "FWRITE","FDEFIN","FDEFOUT","FDGET","FDPUT","FDPUTLN",
            "FDPUTPAD","FDREAD","FDWRITE","ADJBYTES","KBDSTRING",
            "ALIAS","REDIM","APPEND","COPY","KBDFLUSH","MDMFLUSH",
            "KEYFLUSH","LASTIN","FLAG","DOWNLOAD","WRUSYSDOOR",
            "GETALTUSER","ADJDBYTES","ADJTBYTES","ADJTFILES",
            "LANG","SORT","MOUSEREG","SCRFILE","SEARCHINIT",
            "SEARCHFIND","SEARCHSTOP","PRFOUND","PRFOUNDLN",
            "TPAGET","TPAPUT","TPACGET","TPACPUT","TPAREAD",
            "TPAWRITE","TPACREAD","TPACWRITE","BITSET","BITCLEAR",
            "BRAG","FREALTUSER","SETLMR","SETENV","FCLOSEALL",
            "???","???","???","PROC","ENDPROC","???",
            "ENDFUNC","???","STACKABORT","DCREATE","DOPEN",
            "DCLOSE","DSETALIAS","DPACK","DCLOSEALL","DLOCK",
            "DLOCKR","DLOCKG","DUNLOCK","DNCREATE","DNOPEN",
            "DNCLOSE","DNCLOSEALL","DNEW","DADD","DAPPEND","DTOP",
            "DGO","DBOTTOM","DSKIP","DBLANK","DDELETE","DRECALL",
            "DTAG","DSEEK","DFBLANK","DGET","DPUT","DFCOPY","EVAL",
            "ACCOUNT","RECORDUSAGE","MSGTOFILE","QWKLIMITS",
            "COMMAND","USELMRS","CONFINFO","ADJTUBYTES","GRAFMODE",
            "ADDUSER","KILLMSG","CHDIR","MKDIR","REDIR","FDOWRAKA",
            "FDOADDAKA","FDOWRORG","FDOADDORG","FDOQMOD","FDOQADD",
            "FDOQDEL","SOUNDDELAY"
        };

        static readonly string[] EXPR_NAMES =
        {
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
        };
    }
}