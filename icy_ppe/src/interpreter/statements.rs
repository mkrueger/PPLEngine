use crate::{
    ast::Expression,
    tables::{OpCode, StatementDefinition},
    Res,
};

use super::{evaluate_exp, get_int, get_string, Interpreter, InterpreterError};
/// .
///
/// # Examples
///
/// ```
/// use icy_ppe::interpreter::statements::call_predefined_procedure;
///
/// ```
///
/// # Panics
///
/// Panics if .
/// # Errors
///
/// Errors if .
pub fn call_predefined_procedure(
    interpreter: &mut Interpreter,
    def: &'static StatementDefinition,
    params: &Vec<Expression>,
) -> Res<()> {
    match def.opcode {
        OpCode::CLS => predefined_procedures::cls(interpreter, params),
        OpCode::CLREOL => predefined_procedures::clreol(interpreter, params),
        OpCode::MORE => predefined_procedures::more(interpreter),
        OpCode::WAIT => predefined_procedures::wait(interpreter),
        OpCode::COLOR => predefined_procedures::color(interpreter, params),
        OpCode::PRINT => {
            for expr in params {
                let value = evaluate_exp(interpreter, expr)?;
                interpreter
                    .ctx
                    .print(super::TerminalTarget::Both, &value.to_string())?;
            }
            Ok(())
        }
        OpCode::PRINTLN => {
            for expr in params {
                let value = evaluate_exp(interpreter, expr)?;
                interpreter
                    .ctx
                    .print(super::TerminalTarget::Both, &value.to_string())?;
            }
            interpreter.ctx.print(super::TerminalTarget::Both, "\n")?;
            Ok(())
        }
        OpCode::CONFFLAG => predefined_procedures::confflag(interpreter, params),
        OpCode::CONFUNFLAG => predefined_procedures::confunflag(interpreter, params),
        OpCode::DISPFILE => {
            let file = get_string(&evaluate_exp(interpreter, &params[0])?);
            let flags = get_int(&evaluate_exp(interpreter, &params[1])?)?;
            predefined_procedures::dispfile(interpreter, &file, flags)
        }

        OpCode::INPUT => predefined_procedures::input(interpreter, params),
        OpCode::FCREATE => predefined_procedures::fcreate(interpreter, params),
        OpCode::FOPEN => predefined_procedures::fopen(interpreter, params),
        OpCode::FAPPEND => predefined_procedures::fappend(interpreter, params),
        OpCode::FCLOSE => predefined_procedures::fclose(interpreter, params),
        OpCode::FGET => predefined_procedures::fget(interpreter, params),
        OpCode::FPUT => predefined_procedures::fput(interpreter, params),
        OpCode::FPUTLN => predefined_procedures::fputln(interpreter, params),
        OpCode::RESETDISP => {
            predefined_procedures::resetdisp(interpreter, params);
            Ok(())
        }
        OpCode::STARTDISP => {
            predefined_procedures::startdisp(interpreter, params);
            Ok(())
        }
        OpCode::FPUTPAD => {
            predefined_procedures::fputpad(interpreter, params);
            Ok(())
        }
        OpCode::HANGUP => predefined_procedures::hangup(interpreter),
        OpCode::GETUSER => predefined_procedures::getuser(interpreter),
        OpCode::PUTUSER => predefined_procedures::putuser(interpreter),
        OpCode::DEFCOLOR => predefined_procedures::defcolor(interpreter, params),
        OpCode::DELETE => predefined_procedures::delete(interpreter, params),
        OpCode::DELUSER => predefined_procedures::deluser(interpreter, params),
        OpCode::ADJTIME => predefined_procedures::adjtime(interpreter, params),
        OpCode::LOG => predefined_procedures::log(interpreter, params),
        OpCode::INPUTSTR => predefined_procedures::inputstr(interpreter, params),
        OpCode::INPUTYN => predefined_procedures::inputyn(interpreter, params),
        OpCode::INPUTMONEY => predefined_procedures::inputmoney(interpreter, params),
        OpCode::INPUTINT => predefined_procedures::inputint(interpreter, params),
        OpCode::INPUTCC => predefined_procedures::inputcc(interpreter, params),
        OpCode::INPUTDATE => predefined_procedures::inputdate(interpreter, params),
        OpCode::INPUTTIME => predefined_procedures::inputtime(interpreter, params),
        OpCode::PROMPTSTR => predefined_procedures::promptstr(interpreter, params),
        OpCode::DTRON => {
            predefined_procedures::dtron(interpreter);
            Ok(())
        }
        OpCode::DTROFF => predefined_procedures::dtroff(interpreter),
        OpCode::CDCHKON => predefined_procedures::cdchkon(interpreter, params),
        OpCode::CDCHKOFF => predefined_procedures::cdchkoff(interpreter, params),
        OpCode::DELAY => predefined_procedures::delay(interpreter, params),
        OpCode::SENDMODEM => predefined_procedures::sendmodem(interpreter, params),
        OpCode::INC => predefined_procedures::inc(interpreter, params),
        OpCode::DEC => predefined_procedures::dec(interpreter, params),
        OpCode::NEWLINE => predefined_procedures::newline(interpreter),
        OpCode::NEWLINES => predefined_procedures::newlines(interpreter, params),
        OpCode::TOKENIZE => {
            let s = get_string(&evaluate_exp(interpreter, &params[0])?);
            predefined_procedures::tokenize(interpreter, &s);
            Ok(())
        }
        OpCode::GETTOKEN => predefined_procedures::gettoken(interpreter, params),
        OpCode::SHELL => predefined_procedures::shell(interpreter, params),
        OpCode::DISPTEXT => predefined_procedures::disptext(interpreter, params),
        OpCode::STOP => predefined_procedures::stop(interpreter, params),
        OpCode::INPUTTEXT => predefined_procedures::inputtext(interpreter, params),
        OpCode::BEEP => predefined_procedures::beep(interpreter, params),
        OpCode::PUSH => predefined_procedures::push(interpreter, params),
        OpCode::POP => predefined_procedures::pop(interpreter, params),
        OpCode::KBDSTUFF => predefined_procedures::kbdstuff(interpreter, params),
        OpCode::CALL => predefined_procedures::call(interpreter, params),
        OpCode::JOIN => predefined_procedures::join(interpreter, params),
        OpCode::QUEST => predefined_procedures::quest(interpreter, params),
        OpCode::BLT => predefined_procedures::blt(interpreter, params),
        OpCode::DIR => predefined_procedures::dir(interpreter, params),
        OpCode::KBDFILE => predefined_procedures::kbdfile(interpreter, params),
        OpCode::BYE => predefined_procedures::bye(interpreter),
        OpCode::GOODBYE => predefined_procedures::goodbye(interpreter),
        OpCode::BROADCAST => predefined_procedures::broadcast(interpreter, params),
        OpCode::WAITFOR => predefined_procedures::waitfor(interpreter, params),
        OpCode::KBDCHKON => predefined_procedures::kbdchkon(interpreter, params),
        OpCode::KBDCHKOFF => predefined_procedures::kbdchkoff(interpreter, params),
        OpCode::OPTEXT => predefined_procedures::optext(interpreter, params),
        OpCode::DISPSTR => predefined_procedures::dispstr(interpreter, params),
        OpCode::RDUNET => predefined_procedures::rdunet(interpreter, params),
        OpCode::WRUNET => predefined_procedures::wrunet(interpreter, params),
        OpCode::DOINTR => predefined_procedures::dointr(interpreter, params),
        OpCode::VARSEG => predefined_procedures::varseg(interpreter, params),
        OpCode::VAROFF => predefined_procedures::varoff(interpreter, params),
        OpCode::POKEB => predefined_procedures::pokeb(interpreter, params),
        OpCode::POKEW => predefined_procedures::pokew(interpreter, params),
        OpCode::VARADDR => predefined_procedures::varaddr(interpreter, params),
        OpCode::ANSIPOS => predefined_procedures::ansipos(interpreter, params),
        OpCode::BACKUP => predefined_procedures::backup(interpreter, params),
        OpCode::FORWARD => predefined_procedures::forward(interpreter, params),
        OpCode::FRESHLINE => predefined_procedures::freshline(interpreter, params),
        OpCode::WRUSYS => predefined_procedures::wrusys(interpreter, params),
        OpCode::RDUSYS => predefined_procedures::rdusys(interpreter, params),
        OpCode::NEWPWD => predefined_procedures::newpwd(interpreter, params),
        OpCode::OPENCAP => predefined_procedures::opencap(interpreter, params),
        OpCode::CLOSECAP => predefined_procedures::closecap(interpreter, params),
        OpCode::MESSAGE => predefined_procedures::message(interpreter, params),
        OpCode::SAVESCRN => predefined_procedures::savescrn(interpreter, params),
        OpCode::RESTSCRN => predefined_procedures::restscrn(interpreter, params),
        OpCode::SOUND => predefined_procedures::sound(interpreter, params),
        OpCode::CHAT => predefined_procedures::chat(interpreter, params),
        OpCode::SPRINT => predefined_procedures::sprint(interpreter, params),
        OpCode::SPRINTLN => predefined_procedures::sprintln(interpreter, params),
        OpCode::MPRINT => predefined_procedures::mprint(interpreter, params),
        OpCode::MPRINTLN => predefined_procedures::mprintln(interpreter, params),
        OpCode::RENAME => predefined_procedures::rename(interpreter, params),
        OpCode::FREWIND => predefined_procedures::frewind(interpreter, params),
        OpCode::POKEDW => predefined_procedures::pokedw(interpreter, params),
        OpCode::DBGLEVEL => predefined_procedures::dbglevel(interpreter, params),
        OpCode::SHOWON => predefined_procedures::showon(interpreter, params),
        OpCode::SHOWOFF => predefined_procedures::showoff(interpreter, params),
        OpCode::PAGEON => predefined_procedures::pageon(interpreter, params),
        OpCode::PAGEOFF => predefined_procedures::pageoff(interpreter, params),
        OpCode::FSEEK => predefined_procedures::fseek(interpreter, params),
        OpCode::FFLUSH => predefined_procedures::fflush(interpreter, params),
        OpCode::FREAD => predefined_procedures::fread(interpreter, params),
        OpCode::FWRITE => predefined_procedures::fwrite(interpreter, params),
        OpCode::FDEFIN => predefined_procedures::fdefin(interpreter, params),
        OpCode::FDEFOUT => predefined_procedures::fdefout(interpreter, params),
        OpCode::FDGET => predefined_procedures::fdget(interpreter, params),
        OpCode::FDPUT => predefined_procedures::fdput(interpreter, params),
        OpCode::FDPUTLN => predefined_procedures::fdputln(interpreter, params),
        OpCode::FDPUTPAD => predefined_procedures::fdputpad(interpreter, params),
        OpCode::FDREAD => predefined_procedures::fdread(interpreter, params),
        OpCode::FDWRITE => predefined_procedures::fdwrite(interpreter, params),
        OpCode::ADJBYTES => predefined_procedures::adjbytes(interpreter, params),
        OpCode::KBDSTRING => predefined_procedures::kbdstring(interpreter, params),
        OpCode::ALIAS => predefined_procedures::alias(interpreter, params),
        OpCode::REDIM => predefined_procedures::redim(interpreter, params),
        OpCode::APPEND => predefined_procedures::append(interpreter, params),
        OpCode::COPY => predefined_procedures::copy(interpreter, params),
        OpCode::KBDFLUSH => {
            predefined_procedures::kbdflush(interpreter, params);
            Ok(())
        }
        OpCode::MDMFLUSH => {
            predefined_procedures::mdmflush(interpreter, params);
            Ok(())
        }
        OpCode::KEYFLUSH => {
            predefined_procedures::keyflush(interpreter, params);
            Ok(())
        }
        OpCode::LASTIN => predefined_procedures::lastin(interpreter, params),
        OpCode::FLAG => predefined_procedures::flag(interpreter, params),
        OpCode::DOWNLOAD => predefined_procedures::download(interpreter, params),
        OpCode::WRUSYSDOOR => predefined_procedures::wrusysdoor(interpreter, params),
        OpCode::GETALTUSER => predefined_procedures::getaltuser(interpreter, params),
        OpCode::ADJDBYTES => predefined_procedures::adjdbytes(interpreter, params),
        OpCode::ADJTBYTES => predefined_procedures::adjtbytes(interpreter, params),
        OpCode::ADJTFILES => predefined_procedures::adjtfiles(interpreter, params),
        OpCode::LANG => predefined_procedures::lang(interpreter, params),
        OpCode::SORT => predefined_procedures::sort(interpreter, params),
        OpCode::MOUSEREG => predefined_procedures::mousereg(interpreter, params),
        OpCode::SCRFILE => predefined_procedures::scrfile(interpreter, params),
        OpCode::SEARCHINIT => predefined_procedures::searchinit(interpreter, params),
        OpCode::SEARCHFIND => predefined_procedures::searchfind(interpreter, params),
        OpCode::SEARCHSTOP => predefined_procedures::searchstop(interpreter, params),
        OpCode::PRFOUND => predefined_procedures::prfound(interpreter, params),
        OpCode::PRFOUNDLN => predefined_procedures::prfoundln(interpreter, params),
        OpCode::TPAGET => predefined_procedures::tpaget(interpreter, params),
        OpCode::TPAPUT => predefined_procedures::tpaput(interpreter, params),
        OpCode::TPACGET => predefined_procedures::tpacget(interpreter, params),
        OpCode::TPACPUT => predefined_procedures::tpacput(interpreter, params),
        OpCode::TPAREAD => predefined_procedures::tparead(interpreter, params),
        OpCode::TPAWRITE => predefined_procedures::tpawrite(interpreter, params),
        OpCode::TPACREAD => predefined_procedures::tpacread(interpreter, params),
        OpCode::TPACWRITE => predefined_procedures::tpacwrite(interpreter, params),
        OpCode::BITSET => predefined_procedures::bitset(interpreter, params),
        OpCode::BITCLEAR => predefined_procedures::bitclear(interpreter, params),
        OpCode::BRAG => predefined_procedures::brag(interpreter, params),
        OpCode::FREALTUSER => predefined_procedures::frealtuser(interpreter, params),
        OpCode::SETLMR => predefined_procedures::setlmr(interpreter, params),
        OpCode::SETENV => predefined_procedures::setenv(interpreter, params),
        OpCode::FCLOSEALL => predefined_procedures::fcloseall(interpreter, params),
        // OpCode::STATIC => predefined_procedures::static(interpreter, params),
        OpCode::STACKABORT => predefined_procedures::stackabort(interpreter, params),
        OpCode::DCREATE => predefined_procedures::dcreate(interpreter, params),
        OpCode::DOPEN => predefined_procedures::dopen(interpreter, params),
        OpCode::DCLOSE => predefined_procedures::dclose(interpreter, params),
        OpCode::DSETALIAS => predefined_procedures::dsetalias(interpreter, params),
        OpCode::DPACK => predefined_procedures::dpack(interpreter, params),
        OpCode::DCLOSEALL => predefined_procedures::dcloseall(interpreter, params),
        OpCode::DLOCK => predefined_procedures::dlock(interpreter, params),
        OpCode::DLOCKR => predefined_procedures::dlockr(interpreter, params),
        OpCode::DLOCKG => predefined_procedures::dlockg(interpreter, params),
        OpCode::DUNLOCK => predefined_procedures::dunlock(interpreter, params),
        OpCode::DNCREATE => predefined_procedures::dncreate(interpreter, params),
        OpCode::DNOPEN => predefined_procedures::dnopen(interpreter, params),
        OpCode::DNCLOSE => predefined_procedures::dnclose(interpreter, params),
        OpCode::DNCLOSEALL => predefined_procedures::dncloseall(interpreter, params),
        OpCode::DNEW => predefined_procedures::dnew(interpreter, params),
        OpCode::DADD => predefined_procedures::dadd(interpreter, params),
        OpCode::DAPPEND => predefined_procedures::dappend(interpreter, params),
        OpCode::DTOP => predefined_procedures::dtop(interpreter, params),
        OpCode::DGO => predefined_procedures::dgo(interpreter, params),
        OpCode::DBOTTOM => predefined_procedures::dbottom(interpreter, params),
        OpCode::DSKIP => predefined_procedures::dskip(interpreter, params),
        OpCode::DBLANK => predefined_procedures::dblank(interpreter, params),
        OpCode::DDELETE => predefined_procedures::ddelete(interpreter, params),
        OpCode::DRECALL => predefined_procedures::drecall(interpreter, params),
        OpCode::DTAG => predefined_procedures::dtag(interpreter, params),
        OpCode::DSEEK => predefined_procedures::dseek(interpreter, params),
        OpCode::DFBLANK => predefined_procedures::dfblank(interpreter, params),
        OpCode::DGET => predefined_procedures::dget(interpreter, params),
        OpCode::DPUT => predefined_procedures::dput(interpreter, params),
        OpCode::DFCOPY => predefined_procedures::dfcopy(interpreter, params),

        OpCode::EVAL => predefined_procedures::eval(interpreter, params),
        OpCode::ACCOUNT => predefined_procedures::account(interpreter, params),
        OpCode::RECORDUSAGE => predefined_procedures::recordusage(interpreter, params),
        OpCode::MSGTOFILE => predefined_procedures::msgtofile(interpreter, params),
        OpCode::QWKLIMITS => predefined_procedures::qwklimits(interpreter, params),
        OpCode::COMMAND => predefined_procedures::command(interpreter, params),
        OpCode::USELMRS => predefined_procedures::uselmrs(interpreter, params),
        OpCode::CONFINFO => predefined_procedures::confinfo(interpreter, params),
        OpCode::ADJTUBYTES => predefined_procedures::adjtubytes(interpreter, params),
        OpCode::GRAFMODE => predefined_procedures::grafmode(interpreter, params),
        OpCode::ADDUSER => predefined_procedures::adduser(interpreter, params),
        OpCode::KILLMSG => predefined_procedures::killmsg(interpreter, params),
        OpCode::CHDIR => predefined_procedures::chdir(interpreter, params),
        OpCode::MKDIR => predefined_procedures::mkdir(interpreter, params),
        OpCode::REDIR => predefined_procedures::redir(interpreter, params),
        OpCode::FDOWRAKA => predefined_procedures::fdowraka(interpreter, params),
        OpCode::FDOADDAKA => predefined_procedures::fdoaddaka(interpreter, params),
        OpCode::FDOWRORG => predefined_procedures::fdowrorg(interpreter, params),
        OpCode::FDOADDORG => predefined_procedures::fdoaddorg(interpreter, params),
        OpCode::FDOQMOD => predefined_procedures::fdoqmod(interpreter, params),
        OpCode::FDOQADD => predefined_procedures::fdoqadd(interpreter, params),
        OpCode::FDOQDEL => predefined_procedures::fdoqdel(interpreter, params),
        OpCode::SOUNDDELAY => predefined_procedures::sounddelay(interpreter, params),
        _ => Err(Box::new(InterpreterError::UnsupportedOpCode(def.opcode))),
    }
}

#[allow(unused_variables)]
mod predefined_procedures;
