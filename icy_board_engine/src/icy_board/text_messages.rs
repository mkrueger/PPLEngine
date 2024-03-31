#![allow(dead_code)]
use std::{fs, path::Path};

use icy_ppe::Res;
use thiserror::Error;

pub const LEAVECOMMENT: usize = 1;
pub const COMMENTFIELDPROMPT: usize = 2;
pub const ACCESSDENIEDFOREVENT: usize = 3;
pub const TIMELIMITEXCEEDED: usize = 4;
pub const UNAUTHORIZEDNAME: usize = 5;
pub const LOCKEDOUT: usize = 6;
pub const DENIEDWRONGPWRD: usize = 7;
pub const DENIEDREFUSETOREG: usize = 8;
pub const DENIEDPWRDFAILED: usize = 9;
pub const NAMEALREADYINUSE: usize = 10;
pub const CLOSEDBOARD: usize = 11;
pub const SECURITYVIOL: usize = 12;
pub const NOROOMFORTEXT: usize = 13;
pub const CANTPROTECTMSGTOALL: usize = 14;
pub const CALLERMUSTKNOWPWRD: usize = 15;
pub const WRONGMODEMTYPE: usize = 16;
pub const LOCALDNLDPATH: usize = 17;
pub const PASSWORDTOREADMSG: usize = 18;
pub const LOCALUPLDPATH: usize = 19;
pub const SECURITYPASSWORD: usize = 21;
pub const REPLYTOMSGS: usize = 22;
pub const NOTAVAILABLELOCALLY: usize = 24;
pub const SYSOPLEVELREMOVED: usize = 25;
pub const SYSOPLEVELGRANTED: usize = 26;
pub const GRAPHICSUNAVAIL: usize = 27;
pub const AUTOMATICLOCKOUT: usize = 28;
pub const RIPMODEON: usize = 29;
pub const TEXTENTRYFULL: usize = 30;
pub const ASCIINOTAVAILONBIN: usize = 31;
pub const MUSTENDWITHCTRLZ: usize = 32;
pub const NONE: usize = 33;
pub const NOCONNECT: usize = 34;
pub const FULLCOUNTDOWN: usize = 35;
pub const NOCARRIER: usize = 36;
pub const DISKFULL: usize = 37;
pub const NODESBUSY: usize = 38;
pub const ALLNAME: usize = 39;
pub const UPLOADMODE: usize = 40;
pub const ECHO: usize = 41;
pub const ALLCONFIND: usize = 42;
pub const READONLYIND: usize = 43;
pub const MSGBASEINUSE: usize = 45;
pub const NOREMOTESYSFILE: usize = 46;
pub const HANDLEFORCHAT: usize = 49;
pub const ATTACHMENT: usize = 50;
pub const LONGERDESCRIPTION: usize = 51;
pub const NOQUESTIONNAIRES: usize = 52;
pub const REENTERNAME: usize = 53;
pub const REGISTER: usize = 54;
pub const KBDTIMEEXPIRED: usize = 55;
pub const INVALIDENTRY: usize = 56;
pub const UPLOADSAREPRIVATE: usize = 57;
pub const ONLINEUPGRADE: usize = 58;
pub const AUTODISCONNECTNOW: usize = 59;
pub const FULLUSERNAMETOFIND: usize = 60;
pub const FILENAMETODOWNLOAD: usize = 61;
pub const TEXTVIEWFILENAME: usize = 62;
pub const HELPPROMPT: usize = 63;
pub const JOINCONFNUM: usize = 64;
pub const NODENUMTOLOGOFF: usize = 65;
pub const NODETOVIEW: usize = 66;
pub const QNUMTOANSWER: usize = 67;
pub const FILENAMETOUPLOAD: usize = 68;
pub const DOORNUMBER: usize = 69;
pub const TEXTTOSCANFOR: usize = 70;
pub const SEARCHFILENAME: usize = 71;
pub const DATETOSEARCH: usize = 72;
pub const PRIVATETOPIC: usize = 73;
pub const NOTREGINCONF: usize = 74;
pub const OPENTOPIC: usize = 75;
pub const AUTODISCONNECT: usize = 76;
pub const MSGNUMTOACTIVATE: usize = 77;
pub const NOMAILFOUND: usize = 78;
pub const PACKTHEMSGBASE: usize = 79;
pub const DELETECALLERSLOG: usize = 80;
pub const RENUMBERDURINGPACK: usize = 82;
pub const NEWLOWMSGNUM: usize = 83;
pub const COMPLETEQUESTION: usize = 84;
pub const WANTTODELETELINE: usize = 85;
pub const PACKTHEUSERSFILE: usize = 86;
pub const OVERWRITE: usize = 87;
pub const VIEWCONFMEMBERS: usize = 88;
pub const PURGEPRIVRECEIVED: usize = 89;
pub const EXITTODOS: usize = 90;
pub const WRONGPWRDENTERED: usize = 92;
pub const NOSUCHLINENUMBER: usize = 93;
pub const NOSUCHMSGNUMBER: usize = 94;
pub const YOUCANNOTKILLMSG: usize = 95;
pub const INVALIDSELECTION: usize = 96;
pub const PAGING: usize = 97;
pub const CHANNELTEXT: usize = 98;
pub const ENTERNOCHANGE: usize = 99;
pub const SHELLCOMPLETED: usize = 100;
pub const PAGELENGTHSETTO: usize = 101;
pub const USERNOTREGINCONF: usize = 102;
pub const INFOSAVED: usize = 103;
pub const AUTOLOGOFF: usize = 104;
pub const KEEPLOCKEDOUT: usize = 105;
pub const PURGEOLDERTHAN: usize = 106;
pub const KEEPSECURITY: usize = 107;
pub const USERSFILEPACKED: usize = 108;
pub const JOININGCHANNEL: usize = 109;
pub const PWRDSDONTMATCH: usize = 110;
pub const REENTERPASSWORD: usize = 111;
pub const ENTERUSERSECLEVEL: usize = 112;
pub const BUSDATAPHONE: usize = 113;
pub const HOMEVOICEPHONE: usize = 114;
pub const TIMEADJUSTED: usize = 116;
pub const RESPONSEREQUIRED: usize = 117;
pub const REALNAMESONLY: usize = 118;
pub const INSUFSECTOVIEW: usize = 119;
pub const DOORNOTAVAILABLE: usize = 120;
pub const NONETWORKACTIVE: usize = 121;
pub const REGINFOSAVED: usize = 123;
pub const ERRORSAVINGMESSAGE: usize = 125;
pub const ERRORINUSERSFILE: usize = 126;
pub const CHANNELBUSY: usize = 127;
pub const SYSOPUNAVAILABLE: usize = 128;
pub const SYSOPBUSY: usize = 129;
pub const USERBUSY: usize = 130;
pub const DOSBUSY: usize = 131;
pub const DOSNOTBUSY: usize = 132;
pub const TURNPRINTERON: usize = 133;
pub const TURNALARMON: usize = 134;
pub const TURNPAGEON: usize = 135;
pub const NEWHANDLE: usize = 136;
pub const BULLETINSUPDATED: usize = 137;
pub const NOTIMEFORXFER: usize = 138;
pub const INSUFUPLOADSPACE: usize = 139;
pub const UPLOADSDISABLED: usize = 140;
pub const INEFFECT_71: usize = 141;
pub const DOSFUNCTION: usize = 142;
pub const ENTERLOGONPWRD: usize = 143;
pub const EXCESSIVEERRORS: usize = 144;
pub const ABORTKEYS: usize = 145;
pub const ENTERPAGELENGTH: usize = 146;
pub const YOURPASSWORD: usize = 148;
pub const WANTGRAPHICS: usize = 149;
pub const REENTERUSERSNAME: usize = 150;
pub const NEWPASSWORD: usize = 152;
pub const CHECKINGFILEXFER: usize = 153;
pub const DUPLICATEFILE: usize = 154;
pub const MSGSCANPROMPT: usize = 155;
pub const JOINGROUPCHAT: usize = 156;
pub const WAITINGFOREVENT: usize = 157;
pub const FIVESCANHEADER: usize = 158;
pub const BYTESLEFTARE: usize = 159;
pub const ENTERDESCRIPTION: usize = 160;
pub const SLASHFORPRIVATE: usize = 161;
pub const THANKSFORTHEFILES: usize = 162;
pub const MSGCOMMANDEXPERT: usize = 163;
pub const MSGCOMMANDNOVICE1: usize = 164;
pub const MODEMERROR: usize = 165;
pub const THANKSFORCALLING: usize = 166;
pub const USERMODEXPERT: usize = 167;
pub const USERMODNONEXPERT: usize = 168;
pub const TO: usize = 169;
pub const FROM: usize = 170;
pub const SUBJ: usize = 171;
pub const NEWTOPIC: usize = 173;
pub const SYSOPCHATACTIVE: usize = 175;
pub const TRANSFERSUCCESSFUL: usize = 176;
pub const INVALIDFILENAME: usize = 177;
pub const CHATPROMPTEXPERT: usize = 178;
pub const CHATPROMPTNOVICE: usize = 179;
pub const TOPICNOWPRIVATE: usize = 180;
pub const CHANNELISPRIVATE: usize = 181;
pub const NOTAVAILABLE_7E1: usize = 182;
pub const RELOADINGPCBOARD: usize = 184;
pub const USERRECNUMISBAD: usize = 185;
pub const PRESSESCFORCOMMAND: usize = 186;
pub const ABORTEDUSING: usize = 187;
pub const COMPLETEDUSING: usize = 188;
pub const MADETOPICPRIVATE: usize = 189;
pub const RINGDETECTED: usize = 190;
pub const SPACETOACKNOWLEDGE: usize = 191;
pub const MINUTESUSED: usize = 192;
pub const NEWSUBJECT: usize = 193;
pub const MSGSECURITY: usize = 194;
pub const MSGSEARCHFROM: usize = 195;
pub const MOREPROMPT: usize = 196;
pub const ENDOFMESSAGE: usize = 197;
pub const DESIREDPROTOCOL: usize = 198;
pub const MSGTO: usize = 199;
pub const MSGSUBJ: usize = 200;
pub const CREATINGNEWMSGBASE: usize = 203;
pub const ONLINEUPGRADEDONE: usize = 204;
pub const NOTINUSERSFILE: usize = 208;
pub const TWOLINESLEFT: usize = 209;
pub const ENTEREDCHANNEL: usize = 210;
pub const LEFTCHANNEL: usize = 211;
pub const VIEWCALLERS: usize = 212;
pub const VIEWPRINTUSERS: usize = 213;
pub const WASNOTFOUNDINLINE: usize = 214;
pub const CARRIERLOST: usize = 215;
pub const ENTERSTARTS: usize = 216;
pub const ABORTSTRANSFER: usize = 217;
pub const FILESIZEISZERO: usize = 218;
pub const SYSOPCHATENDED: usize = 219;
pub const EDITHEADER: usize = 220;
pub const ECHOMESSAGE: usize = 221;
pub const TEXTENTRYCOMMAND: usize = 222;
pub const FILELISTCOMMAND: usize = 223;
pub const BLTLISTCOMMAND: usize = 224;
pub const WANTSTOCHAT: usize = 225;
pub const TORESPONDTOCHAT: usize = 226;
pub const NODECHATUPROMPT: usize = 227;
pub const NODECHATAPROMPT: usize = 228;
pub const NODENOTAVAILABLE: usize = 229;
pub const NODECHATENDED: usize = 230;
pub const NODECHATENTERED: usize = 231;
pub const TOPICFORCHAT: usize = 232;
pub const REFUSEDTOREGISTER: usize = 233;
pub const NEWCHANNEL: usize = 234;
pub const MONITOROFF: usize = 235;
pub const MONITORON: usize = 236;
pub const NOCHANNELSINUSE: usize = 237;
pub const MSGENTERTEXT: usize = 238;
pub const OUTTODOS: usize = 239;
pub const ARCVIEWFILENAME: usize = 240;
pub const ERRORVIEWINGFILE: usize = 241;
pub const VIEWEXECUTEDONFILE: usize = 242;
pub const CHECKINGARCVIEW: usize = 243;
pub const CHATWITHSYSOP: usize = 244;
pub const DEFAULTPROTOCOL: usize = 245;
pub const ERRORINPROTFILE: usize = 246;
pub const ANSWERSCRIPT: usize = 247;
pub const NODETOCALL: usize = 248;
pub const GETIGNORELIST: usize = 249;
pub const IGNORECANCELLED: usize = 250;
pub const TOPICNOWPUBLIC: usize = 251;
pub const RCVDMESSAGE: usize = 252;
pub const LOADINGPACKMODULE: usize = 253;
pub const PACKMODULEMISSING: usize = 254;
pub const IGNORINGNODES: usize = 255;
pub const USEREXISTS: usize = 256;
pub const USERSFIRSTNAME: usize = 257;
pub const USERSLASTNAME: usize = 258;
pub const YOURFIRSTNAME: usize = 259;
pub const YOURLASTNAME: usize = 260;
pub const SILENTOFF: usize = 261;
pub const SILENTON: usize = 262;
pub const SENDTONODE: usize = 263;
pub const ADDTOINDEX: usize = 264;
pub const CITYSTATE: usize = 265;
pub const EXPDATE: usize = 266;
pub const EXPSECLEVEL: usize = 267;
pub const EXPREGCONF: usize = 268;
pub const SPECIALCOMMENT: usize = 269;
pub const BAUDNOTSUPPORTED: usize = 270;
pub const BAUDSUPPORTEDFROM: usize = 271;
pub const SYSTEMUNAVAILABLE: usize = 272;
pub const USERRECORDUPDATED: usize = 273;
pub const NODENUMTODROP: usize = 274;
pub const NOBLTSAVAILABLE: usize = 275;
pub const NODOORSAVAILABLE: usize = 276;
pub const NODIRSAVAILABLE: usize = 277;
pub const NOCONFAVAILABLE: usize = 278;
pub const USERNETUNDERLINE: usize = 279;
pub const PROTOCOLFORXFER: usize = 280;
pub const INVALIDCONFNUM: usize = 281;
pub const INVALIDDOOR: usize = 282;
pub const SCANNINGDIRECTORY: usize = 283;
pub const CURPAGELEN: usize = 284;
pub const BADUPLOADFORMAT: usize = 285;
pub const USERNETHEADER: usize = 286;
pub const GRAPHICSON: usize = 287;
pub const GRAPHICSOFF: usize = 288;
pub const LOGOFFINFOSAVED: usize = 289;
pub const TRANSFERABORTED: usize = 290;
pub const INVALIDBLTNUM: usize = 291;
pub const INVALIDFILENUM: usize = 292;
pub const EVENTRAN: usize = 293;
pub const EXITEDTODOSAT: usize = 294;
pub const TEXTFILEVIEWED: usize = 295;
pub const SCANMSGBASE: usize = 296;
pub const NEWINFO: usize = 297;
pub const TEXTTOSEND: usize = 298;
pub const RCVDPRIVATEMESSAGE: usize = 299;
pub const DFLTFILENAMETODNLD: usize = 300;
pub const CARBONCOPYTO: usize = 301;
pub const OPENEDDOOR: usize = 302;
pub const NOCALLER: usize = 303;
pub const AVAILABLE: usize = 304;
pub const INADOOR: usize = 305;
pub const LOGOFFPENDING: usize = 306;
pub const RECYCLEBBS: usize = 307;
pub const UNAVAILABLE: usize = 308;
pub const TRANSFER: usize = 309;
pub const ENTERMESSAGE: usize = 310;
pub const GROUPCHAT: usize = 311;
pub const DROPDOSDELAYED: usize = 312;
pub const MADETOPICPUBLIC: usize = 313;
pub const CANTBEPRIVATE: usize = 314;
pub const BADDOWNLOADPWRD: usize = 315;
pub const BADUPLOADPWRD: usize = 316;
pub const BADVIEWPWRD: usize = 317;
pub const BADPWRDFORDOOR: usize = 318;
pub const NODENOTINCHAT: usize = 319;
pub const ANNOUNCECOPY: usize = 320;
pub const BULLETINREAD: usize = 321;
pub const INPUTFILENAME: usize = 322;
pub const FILESELECTED: usize = 323;
pub const PROTOCOLTYPE: usize = 324;
pub const SELECTCONFS: usize = 325;
pub const NOTFOUNDONDISK: usize = 326;
pub const THANKSFORWAITING: usize = 327;
pub const CONFJOINED: usize = 328;
pub const LOCALPASSWORD: usize = 329;
pub const MSGNUMBERTOKILL: usize = 330;
pub const PUNCTUATIONERROR: usize = 331;
pub const BACKFROMDOS: usize = 332;
pub const HELLOTHISIS: usize = 333;
pub const MSGRESTORED: usize = 334;
pub const MSGKILLED: usize = 335;
pub const COMMENTLEFT: usize = 336;
pub const MESSAGELEFT: usize = 337;
pub const SAVINGCOMMENT: usize = 338;
pub const SAVINGMESSAGE: usize = 339;
pub const MOREHELP_ENTER: usize = 340;
pub const MOREHELP_YES: usize = 341;
pub const MOREHELP_NO: usize = 342;
pub const MOREHELP_NONSTOP: usize = 343;
pub const DROPDOSNOW: usize = 344;
pub const DROPNOW: usize = 345;
pub const MOREHELP_VIEW: usize = 346;
pub const TOTALMSGSFOUND: usize = 347;
pub const RECYCLETHRUDOS: usize = 348;
pub const PASSWORDFAILURE: usize = 349;
pub const EXPERTON: usize = 350;
pub const EXPERTOFF: usize = 351;
pub const FILENUMEXPERT: usize = 352;
pub const FILENUMNOVICE: usize = 353;
pub const HELPLINE1: usize = 354;
pub const HELPLINE2: usize = 355;
pub const NETWORKDELAY: usize = 356;
pub const DOWNLOADTIME: usize = 357;
pub const DOWNLOADSIZE: usize = 358;
pub const TOTALWILLBE: usize = 359;
pub const UPLOADDRIVE: usize = 360;
pub const FREEDISKSPACE: usize = 361;
pub const MSGNUMBERMEMORIZED: usize = 363;
pub const INSUFMEMFORCHAT: usize = 365;
pub const UPLOADSTATUS: usize = 366;
pub const SCREENED: usize = 367;
pub const POSTEDIMMEDIATELY: usize = 368;
pub const CALLERNUMBER: usize = 369;
pub const CANTOPENCHATFILE: usize = 370;
pub const LASTDATEONE: usize = 371;
pub const EXPIREDATE: usize = 372;
pub const NUMTIMESON: usize = 373;
pub const PAGELENGTH: usize = 374;
pub const EXPERTMODEON: usize = 375;
pub const EXPERTMODEOFF: usize = 376;
pub const SECURITYLEVEL: usize = 377;
pub const NUMDOWNLOADS: usize = 378;
pub const NUMUPLOADS: usize = 379;
pub const BYTESAVAILABLE: usize = 380;
pub const LASTMSGREAD: usize = 381;
pub const HIGHMSGNUMBER: usize = 382;
pub const NUMACTIVEMSGS: usize = 383;
pub const TRANSFERPROTOCOL: usize = 384;
pub const LANGAVAIL: usize = 385;
pub const LANGALTNOTAVAIL: usize = 386;
pub const LANGENTERNUMBER: usize = 387;
pub const LANGACTIVE: usize = 388;
pub const LANGNOTAVAIL: usize = 389;
pub const MINUTES: usize = 390;
pub const RESETMODEM: usize = 393;
pub const SYSTEMAVAIL: usize = 394;
pub const MINUTESLEFT: usize = 395;
pub const COMMANDPROMPT: usize = 396;
pub const SCANNING: usize = 397;
pub const SCANNINGMAIN: usize = 398;
pub const CONFERENCE: usize = 399;
pub const MAINBOARD: usize = 400;
pub const CHATFORMATERROR: usize = 401;
pub const FOUNDNAME: usize = 402;
pub const MSGABORT: usize = 403;
pub const MSGABORTED: usize = 404;
pub const DELETELINENUM: usize = 405;
pub const CANNOTINSERT: usize = 406;
pub const INSERTBEFORENUM: usize = 407;
pub const EDITLINENUM: usize = 408;
pub const OPERATORPAGED: usize = 409;
pub const GOINGOFFHOOK: usize = 411;
pub const CONFABANDONED: usize = 412;
pub const MENUSELUNAVAIL: usize = 413;
pub const PWRDFORUPLOAD: usize = 414;
pub const PWRDFORDOOR: usize = 415;
pub const PWRDFORVIEW: usize = 416;
pub const PWRDFORDOWNLOAD: usize = 417;
pub const PRESSENTER: usize = 418;
pub const INSUFSECFORDOOR: usize = 419;
pub const EDITHEADERECHO: usize = 420;
pub const NONEWS: usize = 421;
pub const MSGSFORYOU: usize = 422;
pub const MSGSFROMYOU: usize = 423;
pub const MSGSCANCOMMAND: usize = 424;
pub const MSGREADCOMMAND: usize = 425;
pub const NOTMEMORIZED: usize = 426;
pub const CALLANYWAY: usize = 427;
pub const INSUFSECTODLFILE: usize = 428;
pub const CALLERSLOGVIEWED: usize = 429;
pub const DIRECTORYSCAN: usize = 430;
pub const USERSFILEVIEWED: usize = 431;
pub const BEGINUPTEST: usize = 433;
pub const HANGUPNOWORWAIT: usize = 434;
pub const HANGINGUPNOW: usize = 435;
pub const SYSOPEXITEDTODOS: usize = 436;
pub const UPLOADABORTED: usize = 437;
pub const OLDTEXTNEWTEXT: usize = 438;
pub const MSGDATENUM: usize = 439;
pub const MSGTOLINE: usize = 440;
pub const MSGREPLIES: usize = 441;
pub const MSGFROM: usize = 442;
pub const MSGNA: usize = 443;
pub const MSGNOTREAD: usize = 444;
pub const MSGRCVRONLY: usize = 445;
pub const MSGGROUPPWRD: usize = 446;
pub const MSGSENDERPWRD: usize = 447;
pub const MSGPUBLIC: usize = 448;
pub const CONTINUEUPLOAD: usize = 449;
pub const ERRORCORRECTING: usize = 450;
pub const MSGREFERNUM: usize = 451;
pub const MSGREAD: usize = 452;
pub const MSGSUBJLINE: usize = 453;
pub const NOTRECNUMONE: usize = 454;
pub const MSGSTATUS: usize = 455;
pub const NOMATCHINPWRD: usize = 457;
pub const NUMFREEMSGS: usize = 458;
pub const LOWMSGNUM: usize = 459;
pub const NUMDEFINEDMSGS: usize = 460;
pub const GENERATENEWINDEX: usize = 461;
pub const ECHODISABLED: usize = 463;
pub const ECHOENABLED: usize = 464;
pub const MOVEMESSAGETOCONF: usize = 465;
pub const MSGFILE: usize = 466;
pub const MESSAGEMOVED: usize = 467;
pub const MSGLIST: usize = 468;
pub const RECENTUPLOADS: usize = 469;
pub const SCROLLKEYS: usize = 470;
pub const SCROLLABOVE: usize = 471;
pub const SCROLLBELOW: usize = 472;
pub const GENERICMESSAGE: usize = 473;
pub const GOODBYEAFTERUP: usize = 474;
pub const READYTOSENDBATCH: usize = 475;
pub const MSGTOOWIDE: usize = 476;
pub const MSGTOOWIDESAVE: usize = 477;
pub const BATCHXFERENDED: usize = 478;
pub const OUTPUTFILENAME: usize = 479;
pub const BATCHDLTIME: usize = 480;
pub const BATCHDLSIZE: usize = 481;
pub const BATCHPROTOCOL: usize = 482;
pub const RESUMEALL: usize = 483;
pub const MSGSGENERAL: usize = 484;
pub const MSGSTHREAD: usize = 485;
pub const MSGSTEXTSCAN: usize = 486;
pub const MSGSREADFORYOU: usize = 487;
pub const MSGSREADFROMYOU: usize = 488;
pub const MSGSREADTOORFROM: usize = 489;
pub const BYEAFTERDOWNLOAD: usize = 490;
pub const ATTACHEDFILE: usize = 491;
pub const POINTERSRESTORED: usize = 492;
pub const BYEINTENSECONDS: usize = 493;
pub const CAPISEMPTY: usize = 494;
pub const TOTALMSGSINCAPTURE: usize = 495;
pub const SENDINGFILES: usize = 496;
pub const LOGINTOSYSTEM: usize = 497;
pub const USEFULLSCREEN: usize = 498;
pub const REQUIRESANSI: usize = 499;
pub const DOWNLOADTAGGED: usize = 500;
pub const NEWBULLETINS: usize = 501;
pub const NAMESTRIED: usize = 502;
pub const DOSOUNDEXSEARCH: usize = 503;
pub const USEFOUNDNAME: usize = 504;
pub const QUOTESTART: usize = 505;
pub const QUOTEEND: usize = 506;
pub const COMPRESSING: usize = 507;
pub const INSUFFICIENTMEMORY: usize = 508;
pub const SYSOPNOTBUSY: usize = 509;
pub const USERNOTBUSY: usize = 510;
pub const PCBSYSMGR: usize = 511;
pub const PCBFILER: usize = 512;
pub const PCBSETUP: usize = 513;
pub const SETSTATSLOCAL: usize = 514;
pub const PCBMONI: usize = 515;
pub const RESETSTATS: usize = 516;
pub const NOPRINTER: usize = 517;
pub const TURNPRINTEROFF: usize = 518;
pub const TURNALARMOFF: usize = 519;
pub const TURNPAGEOFF: usize = 520;
pub const USERBUSYDESC: usize = 521;
pub const USERNOTBUSYDESC: usize = 522;
pub const TOGGLEPRINTERDESC: usize = 523;
pub const PCBSYSMGRDESC: usize = 524;
pub const SETSTATSDESC: usize = 525;
pub const SYSOPBUSYDESC: usize = 526;
pub const SYSOPNOTBUSYDESC: usize = 527;
pub const TOGGLEPAGEDESC: usize = 528;
pub const PCBFILERDESC: usize = 529;
pub const PCBMONIDESC: usize = 530;
pub const DOSBUSYDESC: usize = 531;
pub const DOSNOTBUSYDESC: usize = 532;
pub const TOGGLEALARMDESC: usize = 533;
pub const PCBSETUPDESC: usize = 534;
pub const RESETSTATSDESC: usize = 535;
pub const NOMODEMSELECTED: usize = 536;
pub const LASTCALLER: usize = 537;
pub const NUMCALLS: usize = 538;
pub const NUMMESSAGES: usize = 539;
pub const NUMDOWN: usize = 540;
pub const NUMUP: usize = 541;
pub const ESCTOEXIT: usize = 542;
pub const INSFORINSERT: usize = 543;
pub const INSFOROVERWRITE: usize = 544;
pub const SETLASTMSGREADPTR: usize = 545;
pub const LASTMSGREADSETTO: usize = 546;
pub const UNLIMITED: usize = 547;
pub const MOREHELP_FLAG: usize = 548;
pub const FILESMOREPROMPT: usize = 549;
pub const GOODBYEAFTERDOWN: usize = 550;
pub const EDITBATCH: usize = 551;
pub const REMOVEFILENUMBER: usize = 552;
pub const REMOVEDFILE: usize = 553;
pub const DUPLICATEBATCHFILE: usize = 554;
pub const BATCHLIMITREACHED: usize = 555;
pub const CLSBETWEENMSGS: usize = 556;
pub const FLAGFORDOWNLOAD: usize = 557;
pub const USERSHEADER: usize = 558;
pub const USERSCAN: usize = 559;
pub const SETSTATSSYSTEM: usize = 560;
pub const DELETERECORD: usize = 561;
pub const CONFNUMBERS: usize = 562;
pub const CONFNUMBERS2: usize = 563;
pub const SELECTCONFFLAGS: usize = 564;
pub const SELECTINGALL: usize = 565;
pub const MAILWAITINGIN: usize = 566;
pub const USERSEARCHNAME: usize = 567;
pub const REQUIRETWONAMES: usize = 568;
pub const COPYMESSAGETOCONF: usize = 569;
pub const MESSAGECOPIED: usize = 570;
pub const COMMENTINSTEAD: usize = 571;
pub const FREEDOWNLOAD: usize = 572;
pub const NOTIMECHARGE: usize = 573;
pub const TESTFILENAME: usize = 574;
pub const VERIFYINGFILE: usize = 575;
pub const PASSED: usize = 576;
pub const FAILED: usize = 577;
pub const FILEVERIFYFAILED: usize = 578;
pub const PAGINGSYSOP: usize = 579;
pub const SCANHEADER1: usize = 580;
pub const SCANHEADER2: usize = 581;
pub const SCANHEADER3: usize = 582;
pub const SETFSEDEFAULT: usize = 583;
pub const MSGREADCMDEXPRT: usize = 584;
pub const FILELISTCMDEXPRT: usize = 585;
pub const CONFHEADER1: usize = 586;
pub const CONFHEADER2: usize = 587;
pub const CTTYON: usize = 588;
pub const ANSION: usize = 589;
pub const MODEM: usize = 590;
pub const CALLERNUM: usize = 591;
pub const CALLERSEC: usize = 592;
pub const SEPARATOR: usize = 593;
pub const OPENINGDOOR: usize = 594;
pub const WRONGPWRD: usize = 595;
pub const WRONGPWRDCMNT: usize = 596;
pub const WRONGPWRDSUBJECT: usize = 597;
pub const ISTHISCORRECT: usize = 598;
pub const CHANGENAMES: usize = 599;
pub const LOCKCALLEROUT: usize = 600;
pub const DISCONNECTNOW: usize = 601;
pub const GIVESYSOPPRIV: usize = 602;
pub const FILESAREFLAGGED: usize = 603;
pub const CONTINUEDOOR: usize = 604;
pub const CONTINUELOGOFF: usize = 605;
pub const SELECTED: usize = 606;
pub const DESELECTED: usize = 607;
pub const SEARCHINGFOR: usize = 608;
pub const UPLOADCREDITS: usize = 609;
pub const COULDNTFINDINUSERS: usize = 610;
pub const BLTLISTCMDEXPERT: usize = 611;
pub const ENDOFMSGEXPERT: usize = 612;
pub const MSGSCANCMDEXPERT: usize = 613;
pub const MAXMSGSPERCONF: usize = 614;
pub const MAXMSGS: usize = 615;
pub const BIRECEIVE: usize = 616;
pub const BIRECEIVELOG: usize = 617;
pub const RECEIVELOG: usize = 618;
pub const SENDLOG: usize = 619;
pub const BATCHSEND: usize = 620;
pub const BATCHRECEIVE: usize = 621;
pub const SENDAVECPS: usize = 622;
pub const RECEIVEAVECPS: usize = 623;
pub const SHOWATTACHCMDS: usize = 624;
pub const FILESIZE: usize = 625;
pub const PICKANOTHERHANDLE: usize = 626;
pub const SCROLLMSGBODY: usize = 627;
pub const USEBIGHEADERS: usize = 628;
pub const MSGCOMMANDNOVICE2: usize = 629;
pub const REQRETRECEIPT: usize = 630;
pub const RETRECEIPTREQ: usize = 631;
pub const GENERATERECEIPT: usize = 632;
pub const YOURMSGNUM: usize = 633;
pub const ADDRESSEDTO: usize = 634;
pub const RECEIVEDON: usize = 635;
pub const ROUTETO: usize = 636;
pub const DEFAULTWIDEMSGS: usize = 637;
pub const CONFISREADONLY: usize = 638;
pub const NOPRIVMSGS: usize = 639;
pub const PWRDTOJOIN: usize = 640;
pub const ALREADYATTACHED: usize = 641;
pub const CARBONLIST: usize = 642;
pub const CARBONNAME: usize = 643;
pub const USERSEARCHTONAME: usize = 644;
pub const USERSEARCHFROMNAME: usize = 645;
pub const ATTACHMENTMISSING: usize = 646;
pub const ATTACHNOTALLOWED: usize = 647;
pub const CARBONLIMITREACHED: usize = 648;
pub const ERRORSINPPE: usize = 649;
pub const NOMEMORYFORPPE: usize = 650;
pub const INVALIDTOKENINPPE: usize = 651;
pub const ERROREXECPPE: usize = 652;
pub const EVALERRORINPPE: usize = 653;
pub const ERRORLOADINGPPE: usize = 654;
pub const APPENDERRORINPPE: usize = 655;
pub const DELETEERRORINPPE: usize = 656;
pub const UPDATINGINDEX: usize = 657;
pub const COLUMNS_72: usize = 658;
pub const COLUMNS_79: usize = 659;
pub const COLUMNS_45: usize = 660;
pub const CARBONLISTMSG: usize = 661;
pub const READMAILNOW: usize = 662;
pub const SCANNINGFORMAIL: usize = 663;
pub const FORCEREADMAIL: usize = 664;
pub const SCANNINGBLTS: usize = 665;
pub const FILERATIO: usize = 666;
pub const BYTERATIO: usize = 667;
pub const RATIOLIMIT: usize = 668;
pub const FILERATIOEXCEEDED: usize = 669;
pub const BYTERATIOEXCEEDED: usize = 670;
pub const FILESDOWNLOADED: usize = 671;
pub const BYTESDOWNLOADED: usize = 672;
pub const DOWNLOADLIMIT: usize = 673;
pub const FILELIMITEXCEEDED: usize = 674;
pub const BYTELIMITEXCEEDED: usize = 675;
pub const CREATINGQWK: usize = 676;
pub const ERRORCOMPRESSING: usize = 677;
pub const QWKCOMMANDS: usize = 678;
pub const EXTRACTINGMSGS: usize = 679;
pub const ERROREXTRACTING: usize = 680;
pub const INVALIDMENUOPTION: usize = 681;
pub const MENUCOMMAND: usize = 682;
pub const RUNNINGEVENT: usize = 683;
pub const EVENTFINISHED: usize = 684;
pub const REPSUCCESSFUL: usize = 685;
pub const REPFAILED: usize = 686;
pub const FILENOTALLOWED: usize = 687;
pub const ENTERPACKDATE: usize = 688;
pub const DATETOPACKOUT: usize = 689;
pub const GETALIASNAME: usize = 690;
pub const HIDINGIDENTITY: usize = 691;
pub const CHANGEDNAMETO: usize = 692;
pub const IDENTITYPROTECTED: usize = 693;
pub const CHANGEDALIASTO: usize = 694;
pub const ATTEMPTEDALIAS: usize = 695;
pub const SHOWBYTERATIO: usize = 696;
pub const SHOWFILERATIO: usize = 697;
pub const ENTERADDRESS: usize = 698;
pub const STREET1: usize = 699;
pub const STREET2: usize = 700;
pub const CITY: usize = 701;
pub const STATE: usize = 702;
pub const ZIP: usize = 703;
pub const COUNTRY: usize = 704;
pub const PREVIOUSLYUSEDPWRD: usize = 705;
pub const ENTERVERIFYTEXT: usize = 706;
pub const ENTERCOMPARETEXT: usize = 707;
pub const PASSWORDTOOSHORT: usize = 708;
pub const NEEDUNIQUEPASSWORD: usize = 709;
pub const PASSWORDEXPIRED: usize = 710;
pub const PASSWORDWILLEXP: usize = 711;
pub const MSGSDATETOSEARCH: usize = 712;
pub const RECEIPTLEFT: usize = 713;
pub const FORWARDFROM: usize = 714;
pub const FORWARDBY: usize = 715;
pub const INSUFCREDITS: usize = 716;
pub const CREDITSUSED: usize = 717;
pub const CREDITSLEFT: usize = 718;
pub const CREDITEXCEEDED: usize = 719;
pub const SECURITYCHANGED: usize = 720;
pub const NOCOST: usize = 721;
pub const FROMRETRECEIPT: usize = 722;
pub const COMMENT: usize = 723;
pub const ERRORINFROMNAME: usize = 724;
pub const QUICKSCANHEADER: usize = 725;
pub const FILEFAILED: usize = 726;
pub const UPLOADEDBY: usize = 727;
pub const FILENAMETODNLDBTCH: usize = 728;
pub const FILENAMETOUPLDBTCH: usize = 729;
pub const USERSCANLINE: usize = 730;
pub const NETSTATUS: usize = 731;
pub const PERSONALMSGLIMIT: usize = 732;
pub const PERSONALCONFLIMIT: usize = 733;
pub const PERSONALQWKLIMIT: usize = 734;
pub const PUBLICQWKLIMIT: usize = 735;
pub const DESTNEWSGROUP: usize = 736;
pub const FOLLOWUPNEWSGROUP: usize = 737;
pub const HANDLINGMAIL: usize = 738;
pub const FILESSHOWPROMPT: usize = 739;
pub const ENTERDIRCMD: usize = 740;
pub const NOFILESFOUND: usize = 741;
pub const DIRECTORYOF: usize = 742;
pub const SHORTINEFFECT: usize = 743;
pub const LONGINEFFECT: usize = 744;
pub const SHOWLONGDESC: usize = 745;
pub const USESHORTDESC: usize = 746;
pub const ENTERGENDER: usize = 747;
pub const ENTERBIRTHDATE: usize = 748;
pub const ENTEREMAIL: usize = 749;
pub const ENTERWEBADDR: usize = 750;
pub const ENTERCOLOR: usize = 751;
pub const ENTERNODEORHANDLE: usize = 752;

#[derive(Error, Debug)]
pub enum TextError {
    #[error("invalid file size")]
    InvalidFileSize,

    #[error("invalid message number: {0}")]
    InvalidMessageNumber(usize),
}

#[derive(Debug, Clone)]
pub struct TextEntry {
    pub color: u8,
    pub text: String,
}

const HEADER: &str = "1IcyBoard text file v1.0\n";

lazy_static::lazy_static! {
    pub static ref DEFAULT_DISPLAY_TEXT: DisplayText = {
        let data = include_bytes!("ICETEXT");
        DisplayText::parse_file(data).unwrap()
    };
}

#[derive(Debug, Clone, Default)]
pub struct DisplayText {
    entries: Vec<TextEntry>,
}

impl DisplayText {
    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn get_display_text(&self, message_number: usize) -> Result<TextEntry, TextError> {
        if message_number > DEFAULT_DISPLAY_TEXT.entries.len() {
            return Err(TextError::InvalidMessageNumber(message_number));
        }

        if let Some(entry) = self.entries.get(message_number) {
            let color = match entry.color {
                0 => 0,
                1 => 12,
                2 => 10,
                3 => 14,
                4 => 9,
                5 => 13,
                6 => 11,
                7 => 15,
                _ => 7,
            };

            Ok(TextEntry {
                color,
                text: entry.text.replace('~', " "),
            })
        } else {
            DEFAULT_DISPLAY_TEXT.get_display_text(message_number)
        }
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn parse_file(data: &[u8]) -> Res<Self> {
        let entries = if data.starts_with(&HEADER.as_bytes()[..9]) {
            load_text_format(data)?
        } else {
            load_bin_format(data)?
        };
        Ok(Self { entries })
    }

    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn write_file(&self, path: &Path) -> Res<()> {
        let mut file = HEADER.to_string();
        for entry in self.entries.iter().skip(1) {
            let line = format!("{},{}\n", entry.color, entry.text);
            file.push_str(&line);
        }
        fs::write(path, &file)?;
        Ok(())
    }
}

impl TextEntry {
    fn new(color: u8, text: &str) -> Self {
        Self {
            color,
            text: text.to_string(),
        }
    }
}

fn load_text_format(data: &[u8]) -> Res<Vec<TextEntry>> {
    let mut res = Vec::new();
    let text = std::str::from_utf8(data)?;

    for line in text.lines() {
        let Some(comma) = line.find(',') else {
            res.push(TextEntry::new(7, "missing"));
            continue;
        };
        let color = line[..comma].parse::<u8>()?;
        let text = line[comma + 1..].trim_end();
        res.push(TextEntry::new(color, text));
    }

    Ok(res)
}

const BIN_ENTRY_SIZE: usize = 0x50;

fn load_bin_format(data: &[u8]) -> Result<Vec<TextEntry>, TextError> {
    let mut res = Vec::new();
    if data.len() % BIN_ENTRY_SIZE != 0 {
        return Err(TextError::InvalidFileSize);
    }
    for chunk in data.chunks(BIN_ENTRY_SIZE) {
        let mut str = String::new();
        for b in &chunk[1..] {
            str.push(*b as char);
        }
        let color = chunk[0] - b'0';
        let text = str.trim_end();
        res.push(TextEntry::new(color, text));
    }
    Ok(res)
}
