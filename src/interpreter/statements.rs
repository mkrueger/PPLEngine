use super::*;
use crate::{ tables::StatementDefinition};

pub fn call_predefined_procedure(
    interpreter: &mut Interpreter,
    cur_frame: &StackFrame,
    def: &'static StatementDefinition,
    params: &Vec<Expression>,
) {
    match def.opcode {
        OpCode::CLS => predefined_procedures::cls(interpreter, cur_frame, params),
        OpCode::CLREOL => predefined_procedures::clreol(interpreter, cur_frame, params),
        OpCode::MORE => predefined_procedures::more(interpreter, cur_frame, params),
        OpCode::WAIT => predefined_procedures::wait(interpreter, cur_frame, params),
        OpCode::COLOR => predefined_procedures::color(interpreter, cur_frame, params),
        OpCode::GOTO => predefined_procedures::goto(interpreter, cur_frame, params),
        OpCode::PRINT => {
            for expr in params {
                let value = evaluate_exp(interpreter, cur_frame, expr);
                interpreter.ctx.print(value.to_string());
            }
        }
        OpCode::PRINTLN => {
            for expr in params {
                let value = evaluate_exp(interpreter, cur_frame, expr);
                interpreter.ctx.print(value.to_string());
            }
            interpreter.ctx.print("\n".to_string());
        }
        OpCode::CONFFLAG => predefined_procedures::confflag(interpreter, cur_frame, params),
        OpCode::CONFUNFLAG => predefined_procedures::confunflag(interpreter, cur_frame, params),
        OpCode::DISPFILE => predefined_procedures::dispfile(interpreter, cur_frame, params),
        OpCode::INPUT => predefined_procedures::input(interpreter, cur_frame, params),
        OpCode::FCREATE => predefined_procedures::fcreate(interpreter, cur_frame, params),
        OpCode::FOPEN => predefined_procedures::fopen(interpreter, cur_frame, params),
        OpCode::FAPPEND => predefined_procedures::fappend(interpreter, cur_frame, params),
        OpCode::FCLOSE => predefined_procedures::fclose(interpreter, cur_frame, params),
        OpCode::FGET => predefined_procedures::fget(interpreter, cur_frame, params),
        OpCode::FPUT => predefined_procedures::fput(interpreter, cur_frame, params),
        OpCode::FPUTLN => predefined_procedures::fputln(interpreter, cur_frame, params),
        OpCode::RESETDISP => predefined_procedures::resetdisp(interpreter, cur_frame, params),
        OpCode::STARTDISP => predefined_procedures::startdisp(interpreter, cur_frame, params),
        OpCode::FPUTPAD => predefined_procedures::fputpad(interpreter, cur_frame, params),
        OpCode::HANGUP => predefined_procedures::hangup(interpreter, cur_frame, params),
        OpCode::GETUSER => predefined_procedures::getuser(interpreter, cur_frame, params),
        OpCode::PUTUSER => predefined_procedures::putuser(interpreter, cur_frame, params),
        OpCode::DEFCOLOR => predefined_procedures::defcolor(interpreter, cur_frame, params),
        OpCode::DELETE => predefined_procedures::delete(interpreter, cur_frame, params),
        OpCode::DELUSER => predefined_procedures::deluser(interpreter, cur_frame, params),
        OpCode::ADJTIME => predefined_procedures::adjtime(interpreter, cur_frame, params),
        OpCode::LOG => predefined_procedures::log(interpreter, cur_frame, params),
        OpCode::INPUTSTR => predefined_procedures::inputstr(interpreter, cur_frame, params),
        OpCode::INPUTYN => predefined_procedures::inputyn(interpreter, cur_frame, params),
        OpCode::INPUTMONEY => predefined_procedures::inputmoney(interpreter, cur_frame, params),
        OpCode::INPUTINT => predefined_procedures::inputint(interpreter, cur_frame, params),
        OpCode::INPUTCC => predefined_procedures::inputcc(interpreter, cur_frame, params),
        OpCode::INPUTDATE => predefined_procedures::inputdate(interpreter, cur_frame, params),
        OpCode::INPUTTIME => predefined_procedures::inputtime(interpreter, cur_frame, params),
        OpCode::PROMPTSTR => predefined_procedures::promptstr(interpreter, cur_frame, params),
        OpCode::DTRON => predefined_procedures::dtron(interpreter, cur_frame, params),
        OpCode::DTROFF => predefined_procedures::dtroff(interpreter, cur_frame, params),
        OpCode::CDCHKON => predefined_procedures::cdchkon(interpreter, cur_frame, params),
        OpCode::CDCHKOFF => predefined_procedures::cdchkoff(interpreter, cur_frame, params),
        OpCode::DELAY => predefined_procedures::delay(interpreter, cur_frame, params),
        OpCode::SENDMODEM => predefined_procedures::sendmodem(interpreter, cur_frame, params),
        OpCode::INC => predefined_procedures::inc(interpreter, cur_frame, params),
        OpCode::DEC => predefined_procedures::dec(interpreter, cur_frame, params),
        OpCode::NEWLINE => predefined_procedures::newline(interpreter, cur_frame, params),
        OpCode::NEWLINES => predefined_procedures::newlines(interpreter, cur_frame, params),
        OpCode::TOKENIZE => predefined_procedures::tokenize(interpreter, cur_frame, params),
        OpCode::GETTOKEN => predefined_procedures::gettoken(interpreter, cur_frame, params),
        OpCode::SHELL => predefined_procedures::shell(interpreter, cur_frame, params),
        OpCode::DISPTEXT => predefined_procedures::disptext(interpreter, cur_frame, params),
        OpCode::STOP => predefined_procedures::stop(interpreter, cur_frame, params),
        OpCode::INPUTTEXT => predefined_procedures::inputtext(interpreter, cur_frame, params),
        OpCode::BEEP => predefined_procedures::beep(interpreter, cur_frame, params),
        OpCode::PUSH => predefined_procedures::push(interpreter, cur_frame, params),
        OpCode::POP => predefined_procedures::pop(interpreter, cur_frame, params),
        OpCode::KBDSTUFF => predefined_procedures::kbdstuff(interpreter, cur_frame, params),
        OpCode::CALL => predefined_procedures::call(interpreter, cur_frame, params),
        OpCode::JOIN => predefined_procedures::join(interpreter, cur_frame, params),
        OpCode::QUEST => predefined_procedures::quest(interpreter, cur_frame, params),
        OpCode::BLT => predefined_procedures::blt(interpreter, cur_frame, params),
        OpCode::DIR => predefined_procedures::dir(interpreter, cur_frame, params),
        OpCode::KBDFILE => predefined_procedures::kbdfile(interpreter, cur_frame, params),
        OpCode::BYE => predefined_procedures::bye(interpreter, cur_frame, params),
        OpCode::GOODBYE => predefined_procedures::goodbye(interpreter, cur_frame, params),
        OpCode::BROADCAST => predefined_procedures::broadcast(interpreter, cur_frame, params),
        OpCode::WAITFOR => predefined_procedures::waitfor(interpreter, cur_frame, params),
        OpCode::KBDCHKON => predefined_procedures::kbdchkon(interpreter, cur_frame, params),
        OpCode::KBDCHKOFF => predefined_procedures::kbdchkoff(interpreter, cur_frame, params),
        OpCode::OPTEXT => predefined_procedures::optext(interpreter, cur_frame, params),
        OpCode::DISPSTR => predefined_procedures::dispstr(interpreter, cur_frame, params),
        OpCode::RDUNET => predefined_procedures::rdunet(interpreter, cur_frame, params),
        OpCode::WRUNET => predefined_procedures::wrunet(interpreter, cur_frame, params),
        OpCode::DOINTR => predefined_procedures::dointr(interpreter, cur_frame, params),
        OpCode::VARSEG => predefined_procedures::varseg(interpreter, cur_frame, params),
        OpCode::VAROFF => predefined_procedures::varoff(interpreter, cur_frame, params),
        OpCode::POKEB => predefined_procedures::pokeb(interpreter, cur_frame, params),
        OpCode::POKEW => predefined_procedures::pokew(interpreter, cur_frame, params),
        OpCode::VARADDR => predefined_procedures::varaddr(interpreter, cur_frame, params),
        OpCode::ANSIPOS => predefined_procedures::ansipos(interpreter, cur_frame, params),
        OpCode::BACKUP => predefined_procedures::backup(interpreter, cur_frame, params),
        OpCode::FORWARD => predefined_procedures::forward(interpreter, cur_frame, params),
        OpCode::FRESHLINE => predefined_procedures::freshline(interpreter, cur_frame, params),
        OpCode::WRUSYS => predefined_procedures::wrusys(interpreter, cur_frame, params),
        OpCode::RDUSYS => predefined_procedures::rdusys(interpreter, cur_frame, params),
        OpCode::NEWPWD => predefined_procedures::newpwd(interpreter, cur_frame, params),
        OpCode::OPENCAP => predefined_procedures::opencap(interpreter, cur_frame, params),
        OpCode::CLOSECAP => predefined_procedures::closecap(interpreter, cur_frame, params),
        OpCode::MESSAGE => predefined_procedures::message(interpreter, cur_frame, params),
        OpCode::SAVESCRN => predefined_procedures::savescrn(interpreter, cur_frame, params),
        OpCode::RESTSCRN => predefined_procedures::restscrn(interpreter, cur_frame, params),
        OpCode::SOUND => predefined_procedures::sound(interpreter, cur_frame, params),
        OpCode::CHAT => predefined_procedures::chat(interpreter, cur_frame, params),
        OpCode::SPRINT => predefined_procedures::sprint(interpreter, cur_frame, params),
        OpCode::SPRINTLN => predefined_procedures::sprintln(interpreter, cur_frame, params),
        OpCode::MPRINT => predefined_procedures::mprint(interpreter, cur_frame, params),
        OpCode::MPRINTLN => predefined_procedures::mprintln(interpreter, cur_frame, params),
        OpCode::RENAME => predefined_procedures::rename(interpreter, cur_frame, params),
        OpCode::FREWIND => predefined_procedures::frewind(interpreter, cur_frame, params),
        OpCode::POKEDW => predefined_procedures::pokedw(interpreter, cur_frame, params),
        OpCode::DBGLEVEL => predefined_procedures::dbglevel(interpreter, cur_frame, params),
        OpCode::SHOWON => predefined_procedures::showon(interpreter, cur_frame, params),
        OpCode::SHOWOFF => predefined_procedures::showoff(interpreter, cur_frame, params),
        OpCode::PAGEON => predefined_procedures::pageon(interpreter, cur_frame, params),
        OpCode::PAGEOFF => predefined_procedures::pageoff(interpreter, cur_frame, params),
        OpCode::FSEEK => predefined_procedures::fseek(interpreter, cur_frame, params),
        OpCode::FFLUSH => predefined_procedures::fflush(interpreter, cur_frame, params),
        OpCode::FREAD => predefined_procedures::fread(interpreter, cur_frame, params),
        OpCode::FWRITE => predefined_procedures::fwrite(interpreter, cur_frame, params),
        OpCode::FDEFIN => predefined_procedures::fdefin(interpreter, cur_frame, params),
        OpCode::FDEFOUT => predefined_procedures::fdefout(interpreter, cur_frame, params),
        OpCode::FDGET => predefined_procedures::fdget(interpreter, cur_frame, params),
        OpCode::FDPUT => predefined_procedures::fdput(interpreter, cur_frame, params),
        OpCode::FDPUTLN => predefined_procedures::fdputln(interpreter, cur_frame, params),
        OpCode::FDPUTPAD => predefined_procedures::fdputpad(interpreter, cur_frame, params),
        OpCode::FDREAD => predefined_procedures::fdread(interpreter, cur_frame, params),
        OpCode::FDWRITE => predefined_procedures::fdwrite(interpreter, cur_frame, params),
        OpCode::ADJBYTES => predefined_procedures::adjbytes(interpreter, cur_frame, params),
        OpCode::KBDSTRING => predefined_procedures::kbdstring(interpreter, cur_frame, params),
        OpCode::ALIAS => predefined_procedures::alias(interpreter, cur_frame, params),
        OpCode::REDIM => predefined_procedures::redim(interpreter, cur_frame, params),
        OpCode::APPEND => predefined_procedures::append(interpreter, cur_frame, params),
        OpCode::COPY => predefined_procedures::copy(interpreter, cur_frame, params),
        OpCode::KBDFLUSH => predefined_procedures::kbdflush(interpreter, cur_frame, params),
        OpCode::MDMFLUSH => predefined_procedures::mdmflush(interpreter, cur_frame, params),
        OpCode::KEYFLUSH => predefined_procedures::keyflush(interpreter, cur_frame, params),
        OpCode::LASTIN => predefined_procedures::lastin(interpreter, cur_frame, params),
        OpCode::FLAG => predefined_procedures::flag(interpreter, cur_frame, params),
        OpCode::DOWNLOAD => predefined_procedures::download(interpreter, cur_frame, params),
        OpCode::WRUSYSDOOR => predefined_procedures::wrusysdoor(interpreter, cur_frame, params),
        OpCode::GETALTUSER => predefined_procedures::getaltuser(interpreter, cur_frame, params),
        OpCode::ADJDBYTES => predefined_procedures::adjdbytes(interpreter, cur_frame, params),
        OpCode::ADJTBYTES => predefined_procedures::adjtbytes(interpreter, cur_frame, params),
        OpCode::ADJTFILES => predefined_procedures::adjtfiles(interpreter, cur_frame, params),
        OpCode::LANG => predefined_procedures::lang(interpreter, cur_frame, params),
        OpCode::SORT => predefined_procedures::sort(interpreter, cur_frame, params),
        OpCode::MOUSEREG => predefined_procedures::mousereg(interpreter, cur_frame, params),
        OpCode::SCRFILE => predefined_procedures::scrfile(interpreter, cur_frame, params),
        OpCode::SEARCHINIT => predefined_procedures::searchinit(interpreter, cur_frame, params),
        OpCode::SEARCHFIND => predefined_procedures::searchfind(interpreter, cur_frame, params),
        OpCode::SEARCHSTOP => predefined_procedures::searchstop(interpreter, cur_frame, params),
        OpCode::PRFOUND => predefined_procedures::prfound(interpreter, cur_frame, params),
        OpCode::PRFOUNDLN => predefined_procedures::prfoundln(interpreter, cur_frame, params),
        OpCode::TPAGET => predefined_procedures::tpaget(interpreter, cur_frame, params),
        OpCode::TPAPUT => predefined_procedures::tpaput(interpreter, cur_frame, params),
        OpCode::TPACGET => predefined_procedures::tpacget(interpreter, cur_frame, params),
        OpCode::TPACPUT => predefined_procedures::tpacput(interpreter, cur_frame, params),
        OpCode::TPAREAD => predefined_procedures::tparead(interpreter, cur_frame, params),
        OpCode::TPAWRITE => predefined_procedures::tpawrite(interpreter, cur_frame, params),
        OpCode::TPACREAD => predefined_procedures::tpacread(interpreter, cur_frame, params),
        OpCode::TPACWRITE => predefined_procedures::tpacwrite(interpreter, cur_frame, params),
        OpCode::BITSET => predefined_procedures::bitset(interpreter, cur_frame, params),
        OpCode::BITCLEAR => predefined_procedures::bitclear(interpreter, cur_frame, params),
        OpCode::BRAG => predefined_procedures::brag(interpreter, cur_frame, params),
        OpCode::FREALTUSER => predefined_procedures::frealtuser(interpreter, cur_frame, params),
        OpCode::SETLMR => predefined_procedures::setlmr(interpreter, cur_frame, params),
        OpCode::SETENV => predefined_procedures::setenv(interpreter, cur_frame, params),
        OpCode::FCLOSEALL => predefined_procedures::fcloseall(interpreter, cur_frame, params),
        OpCode::DECLARE => predefined_procedures::declare(interpreter, cur_frame, params),
        OpCode::FUNCTION => predefined_procedures::function(interpreter, cur_frame, params),
        OpCode::PROCEDURE => predefined_procedures::procedure(interpreter, cur_frame, params),
        OpCode::PCALL => predefined_procedures::pcall(interpreter, cur_frame, params),
        OpCode::FPCLR => predefined_procedures::fpclr(interpreter, cur_frame, params),
        OpCode::BEGIN => predefined_procedures::begin(interpreter, cur_frame, params),
        OpCode::FEND => predefined_procedures::fend(interpreter, cur_frame, params),
        // OpCode::STATIC => predefined_procedures::static(interpreter, cur_frame, params),
        OpCode::STACKABORT => predefined_procedures::stackabort(interpreter, cur_frame, params),
        OpCode::DCREATE => predefined_procedures::dcreate(interpreter, cur_frame, params),
        OpCode::DOPEN => predefined_procedures::dopen(interpreter, cur_frame, params),
        OpCode::DCLOSE => predefined_procedures::dclose(interpreter, cur_frame, params),
        OpCode::DSETALIAS => predefined_procedures::dsetalias(interpreter, cur_frame, params),
        OpCode::DPACK => predefined_procedures::dpack(interpreter, cur_frame, params),
        OpCode::DCLOSEALL => predefined_procedures::dcloseall(interpreter, cur_frame, params),
        OpCode::DLOCK => predefined_procedures::dlock(interpreter, cur_frame, params),
        OpCode::DLOCKR => predefined_procedures::dlockr(interpreter, cur_frame, params),
        OpCode::DLOCKG => predefined_procedures::dlockg(interpreter, cur_frame, params),
        OpCode::DUNLOCK => predefined_procedures::dunlock(interpreter, cur_frame, params),
        OpCode::DNCREATE => predefined_procedures::dncreate(interpreter, cur_frame, params),
        OpCode::DNOPEN => predefined_procedures::dnopen(interpreter, cur_frame, params),
        OpCode::DNCLOSE => predefined_procedures::dnclose(interpreter, cur_frame, params),
        OpCode::DNCLOSEALL => predefined_procedures::dncloseall(interpreter, cur_frame, params),
        OpCode::DNEW => predefined_procedures::dnew(interpreter, cur_frame, params),
        OpCode::DADD => predefined_procedures::dadd(interpreter, cur_frame, params),
        OpCode::DAPPEND => predefined_procedures::dappend(interpreter, cur_frame, params),
        OpCode::DTOP => predefined_procedures::dtop(interpreter, cur_frame, params),
        OpCode::DGO => predefined_procedures::dgo(interpreter, cur_frame, params),
        OpCode::DBOTTOM => predefined_procedures::dbottom(interpreter, cur_frame, params),
        OpCode::DSKIP => predefined_procedures::dskip(interpreter, cur_frame, params),
        OpCode::DBLANK => predefined_procedures::dblank(interpreter, cur_frame, params),
        OpCode::DDELETE => predefined_procedures::ddelete(interpreter, cur_frame, params),
        OpCode::DRECALL => predefined_procedures::drecall(interpreter, cur_frame, params),
        OpCode::DTAG => predefined_procedures::dtag(interpreter, cur_frame, params),
        OpCode::DSEEK => predefined_procedures::dseek(interpreter, cur_frame, params),
        OpCode::DFBLANK => predefined_procedures::dfblank(interpreter, cur_frame, params),
        OpCode::DGET => predefined_procedures::dget(interpreter, cur_frame, params),
        OpCode::DPUT => predefined_procedures::dput(interpreter, cur_frame, params),
        OpCode::DFCOPY => predefined_procedures::dfcopy(interpreter, cur_frame, params),

        OpCode::EVAL => predefined_procedures::eval(interpreter, cur_frame, params),
        OpCode::ACCOUNT => predefined_procedures::account(interpreter, cur_frame, params),
        OpCode::RECORDUSAGE => predefined_procedures::recordusage(interpreter, cur_frame, params),
        OpCode::MSGTOFILE => predefined_procedures::msgtofile(interpreter, cur_frame, params),
        OpCode::QWKLIMITS => predefined_procedures::qwklimits(interpreter, cur_frame, params),
        OpCode::COMMAND => predefined_procedures::command(interpreter, cur_frame, params),
        OpCode::USELMRS => predefined_procedures::uselmrs(interpreter, cur_frame, params),
        OpCode::CONFINFO => predefined_procedures::confinfo(interpreter, cur_frame, params),
        OpCode::ADJTUBYTES => predefined_procedures::adjtubytes(interpreter, cur_frame, params),
        OpCode::GRAFMODE => predefined_procedures::grafmode(interpreter, cur_frame, params),
        OpCode::ADDUSER => predefined_procedures::adduser(interpreter, cur_frame, params),
        OpCode::KILLMSG => predefined_procedures::killmsg(interpreter, cur_frame, params),
        OpCode::CHDIR => predefined_procedures::chdir(interpreter, cur_frame, params),
        OpCode::MKDIR => predefined_procedures::mkdir(interpreter, cur_frame, params),
        OpCode::REDIR => predefined_procedures::redir(interpreter, cur_frame, params),
        OpCode::FDOWRAKA => predefined_procedures::fdowraka(interpreter, cur_frame, params),
        OpCode::FDOADDAKA => predefined_procedures::fdoaddaka(interpreter, cur_frame, params),
        OpCode::FDOWRORG => predefined_procedures::fdowrorg(interpreter, cur_frame, params),
        OpCode::FDOADDORG => predefined_procedures::fdoaddorg(interpreter, cur_frame, params),
        OpCode::FDOQMOD => predefined_procedures::fdoqmod(interpreter, cur_frame, params),
        OpCode::FDOQADD => predefined_procedures::fdoqadd(interpreter, cur_frame, params),
        OpCode::FDOQDEL => predefined_procedures::fdoqdel(interpreter, cur_frame, params),
        OpCode::SOUNDDELAY => predefined_procedures::sounddelay(interpreter, cur_frame, params),
        _ => {
            panic!("unsupported op code {:?}", def.opcode);
        }
    }
}

#[allow(unused_variables)]
mod predefined_procedures {
    use crate::{ interpreter::*};

    pub fn cls(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn clreol(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn more(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn wait(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn color(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn goto(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn confflag(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn confunflag(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dispfile(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn input(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fcreate(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fopen(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fappend(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fclose(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fget(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fput(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fputln(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn resetdisp(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn startdisp(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fputpad(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn hangup(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn getuser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn putuser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn defcolor(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn delete(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn deluser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjtime(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn log(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputstr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputyn(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputmoney(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputint(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputcc(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputdate(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputtime(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn promptstr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dtron(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dtroff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn cdchkon(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn cdchkoff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn delay(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sendmodem(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inc(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dec(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn newline(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn newlines(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tokenize(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn gettoken(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn shell(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn disptext(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn stop(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn inputtext(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn beep(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn push(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pop(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdstuff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn call(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn join(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn quest(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn blt(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dir(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdfile(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn bye(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn goodbye(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn broadcast(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn waitfor(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdchkon(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdchkoff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn optext(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dispstr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn rdunet(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn wrunet(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dointr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn varseg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn varoff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pokeb(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pokew(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn varaddr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn ansipos(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn backup(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn forward(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn freshline(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn wrusys(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn rdusys(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn newpwd(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn opencap(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn closecap(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn message(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn savescrn(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn restscrn(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sound(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn chat(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sprint(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sprintln(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn mprint(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn mprintln(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn rename(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn frewind(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pokedw(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dbglevel(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn showon(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn showoff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pageon(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pageoff(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fseek(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fflush(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fread(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fwrite(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdefin(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdefout(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdget(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdput(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdputln(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdputpad(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdread(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdwrite(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjbytes(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdstring(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn alias(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn redim(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn append(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn copy(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn kbdflush(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn mdmflush(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn keyflush(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn lastin(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn flag(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn download(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn wrusysdoor(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn getaltuser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjdbytes(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjtbytes(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjtfiles(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn lang(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sort(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn mousereg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn scrfile(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn searchinit(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn searchfind(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn searchstop(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn prfound(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn prfoundln(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpaget(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpaput(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpacget(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpacput(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tparead(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpawrite(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpacread(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn tpacwrite(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn bitset(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn bitclear(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn brag(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn frealtuser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn setlmr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn setenv(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fcloseall(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn declare(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn function(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn procedure(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn pcall(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fpclr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn begin(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fend(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn stackabort(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dcreate(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dopen(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dclose(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dsetalias(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dpack(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dcloseall(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dlock(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dlockr(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dlockg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dunlock(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dncreate(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dnopen(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dnclose(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dncloseall(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dnew(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dadd(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dappend(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dtop(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dgo(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dbottom(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dskip(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dblank(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn ddelete(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn drecall(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dtag(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dseek(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dfblank(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dget(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dput(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn dfcopy(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }

    pub fn eval(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn account(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn recordusage(
        interpreter: &Interpreter,
        cur_frame: &StackFrame,

        params: &Vec<Expression>,
    ) {
        panic!("TODO")
    }
    pub fn msgtofile(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn qwklimits(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn command(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn uselmrs(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn confinfo(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adjtubytes(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn grafmode(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn adduser(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn killmsg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn chdir(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn mkdir(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn redir(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdowraka(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdoaddaka(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdowrorg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdoaddorg(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdoqmod(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdoqadd(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn fdoqdel(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
    pub fn sounddelay(interpreter: &Interpreter, cur_frame: &StackFrame, params: &Vec<Expression>) {
        panic!("TODO")
    }
}
