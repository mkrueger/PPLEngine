use crate::{
    ast::Expression,
    executable::{OpCode, StatementDefinition},
    Res,
};

use super::VirtualMachine;

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
    vm: &mut VirtualMachine,
    def: &'static StatementDefinition,
    params: &Vec<Expression>,
) -> Res<()> {
    match def.opcode {
        OpCode::CLS => predefined_procedures::cls(vm, params),
        OpCode::CLREOL => predefined_procedures::clreol(vm, params),
        OpCode::MORE => predefined_procedures::more(vm),
        OpCode::WAIT => predefined_procedures::wait(vm),
        OpCode::COLOR => predefined_procedures::color(vm, params),
        OpCode::PRINT => {
            for expr in params {
                let value = evaluate_exp(vm, expr)?;
                vm
                    .ctx
                    .print(super::TerminalTarget::Both, &value.as_string())?;
            }
            Ok(())
        }
        OpCode::PRINTLN => {
            for expr in params {
                let value = evaluate_exp(vm, expr)?;
                vm
                    .ctx
                    .print(super::TerminalTarget::Both, &value.as_string())?;
            }
            vm.ctx.print(super::TerminalTarget::Both, "\n")?;
            Ok(())
        }
        OpCode::CONFFLAG => predefined_procedures::confflag(vm, params),
        OpCode::CONFUNFLAG => predefined_procedures::confunflag(vm, params),
        OpCode::DISPFILE => {
            let file = evaluate_exp(vm, &params[0])?.as_string();
            let flags = evaluate_exp(vm, &params[1])?.as_int();
            predefined_procedures::dispfile(vm, &file, flags)
        }

        OpCode::INPUT => predefined_procedures::input(vm, params),
        OpCode::FCREATE => predefined_procedures::fcreate(vm, params),
        OpCode::FOPEN => predefined_procedures::fopen(vm, params),
        OpCode::FAPPEND => predefined_procedures::fappend(vm, params),
        OpCode::FCLOSE => predefined_procedures::fclose(vm, params),
        OpCode::FGET => predefined_procedures::fget(vm, params),
        OpCode::FPUT => predefined_procedures::fput(vm, params),
        OpCode::FPUTLN => predefined_procedures::fputln(vm, params),
        OpCode::RESETDISP => {
            predefined_procedures::resetdisp(vm, params);
            Ok(())
        }
        OpCode::STARTDISP => {
            predefined_procedures::startdisp(vm, params);
            Ok(())
        }
        OpCode::FPUTPAD => {
            predefined_procedures::fputpad(vm, params);
            Ok(())
        }
        OpCode::HANGUP => predefined_procedures::hangup(vm),
        OpCode::GETUSER => predefined_procedures::getuser(vm),
        OpCode::PUTUSER => predefined_procedures::putuser(vm),
        OpCode::DEFCOLOR => predefined_procedures::defcolor(vm, params),
        OpCode::DELETE => predefined_procedures::delete(vm, params),
        OpCode::DELUSER => predefined_procedures::deluser(vm, params),
        OpCode::ADJTIME => predefined_procedures::adjtime(vm, params),
        OpCode::LOG => predefined_procedures::log(vm, params),
        OpCode::INPUTSTR => predefined_procedures::inputstr(vm, params),
        OpCode::INPUTYN => predefined_procedures::inputyn(vm, params),
        OpCode::INPUTMONEY => predefined_procedures::inputmoney(vm, params),
        OpCode::INPUTINT => predefined_procedures::inputint(vm, params),
        OpCode::INPUTCC => predefined_procedures::inputcc(vm, params),
        OpCode::INPUTDATE => predefined_procedures::inputdate(vm, params),
        OpCode::INPUTTIME => predefined_procedures::inputtime(vm, params),
        OpCode::PROMPTSTR => predefined_procedures::promptstr(vm, params),
        OpCode::DTRON => {
            predefined_procedures::dtron(vm);
            Ok(())
        }
        OpCode::DTROFF => predefined_procedures::dtroff(vm),
        OpCode::CDCHKON => predefined_procedures::cdchkon(vm, params),
        OpCode::CDCHKOFF => predefined_procedures::cdchkoff(vm, params),
        OpCode::DELAY => predefined_procedures::delay(vm, params),
        OpCode::SENDMODEM => predefined_procedures::sendmodem(vm, params),
        OpCode::INC => predefined_procedures::inc(vm, params),
        OpCode::DEC => predefined_procedures::dec(vm, params),
        OpCode::NEWLINE => predefined_procedures::newline(vm),
        OpCode::NEWLINES => predefined_procedures::newlines(vm, params),
        OpCode::TOKENIZE => {
            let s = evaluate_exp(vm, &params[0])?.as_string();
            predefined_procedures::tokenize(vm, &s);
            Ok(())
        }
        OpCode::GETTOKEN => predefined_procedures::gettoken(vm, params),
        OpCode::SHELL => predefined_procedures::shell(vm, params),
        OpCode::DISPTEXT => predefined_procedures::disptext(vm, params),
        OpCode::STOP => predefined_procedures::stop(vm, params),
        OpCode::INPUTTEXT => predefined_procedures::inputtext(vm, params),
        OpCode::BEEP => predefined_procedures::beep(vm, params),
        OpCode::PUSH => predefined_procedures::push(vm, params),
        OpCode::POP => predefined_procedures::pop(vm, params),
        OpCode::KBDSTUFF => predefined_procedures::kbdstuff(vm, params),
        OpCode::CALL => predefined_procedures::call(vm, params),
        OpCode::JOIN => predefined_procedures::join(vm, params),
        OpCode::QUEST => predefined_procedures::quest(vm, params),
        OpCode::BLT => predefined_procedures::blt(vm, params),
        OpCode::DIR => predefined_procedures::dir(vm, params),
        OpCode::KBDFILE => predefined_procedures::kbdfile(vm, params),
        OpCode::BYE => predefined_procedures::bye(vm),
        OpCode::GOODBYE => predefined_procedures::goodbye(vm),
        OpCode::BROADCAST => predefined_procedures::broadcast(vm, params),
        OpCode::WAITFOR => predefined_procedures::waitfor(vm, params),
        OpCode::KBDCHKON => predefined_procedures::kbdchkon(vm, params),
        OpCode::KBDCHKOFF => predefined_procedures::kbdchkoff(vm, params),
        OpCode::OPTEXT => predefined_procedures::optext(vm, params),
        OpCode::DISPSTR => predefined_procedures::dispstr(vm, params),
        OpCode::RDUNET => predefined_procedures::rdunet(vm, params),
        OpCode::WRUNET => predefined_procedures::wrunet(vm, params),
        OpCode::DOINTR => predefined_procedures::dointr(vm, params),
        OpCode::VARSEG => predefined_procedures::varseg(vm, params),
        OpCode::VAROFF => predefined_procedures::varoff(vm, params),
        OpCode::POKEB => predefined_procedures::pokeb(vm, params),
        OpCode::POKEW => predefined_procedures::pokew(vm, params),
        OpCode::VARADDR => predefined_procedures::varaddr(vm, params),
        OpCode::ANSIPOS => predefined_procedures::ansipos(vm, params),
        OpCode::BACKUP => predefined_procedures::backup(vm, params),
        OpCode::FORWARD => predefined_procedures::forward(vm, params),
        OpCode::FRESHLINE => predefined_procedures::freshline(vm, params),
        OpCode::WRUSYS => predefined_procedures::wrusys(vm, params),
        OpCode::RDUSYS => predefined_procedures::rdusys(vm, params),
        OpCode::NEWPWD => predefined_procedures::newpwd(vm, params),
        OpCode::OPENCAP => predefined_procedures::opencap(vm, params),
        OpCode::CLOSECAP => predefined_procedures::closecap(vm, params),
        OpCode::MESSAGE => predefined_procedures::message(vm, params),
        OpCode::SAVESCRN => predefined_procedures::savescrn(vm, params),
        OpCode::RESTSCRN => predefined_procedures::restscrn(vm, params),
        OpCode::SOUND => predefined_procedures::sound(vm, params),
        OpCode::CHAT => predefined_procedures::chat(vm, params),
        OpCode::SPRINT => predefined_procedures::sprint(vm, params),
        OpCode::SPRINTLN => predefined_procedures::sprintln(vm, params),
        OpCode::MPRINT => predefined_procedures::mprint(vm, params),
        OpCode::MPRINTLN => predefined_procedures::mprintln(vm, params),
        OpCode::RENAME => predefined_procedures::rename(vm, params),
        OpCode::FREWIND => predefined_procedures::frewind(vm, params),
        OpCode::POKEDW => predefined_procedures::pokedw(vm, params),
        OpCode::DBGLEVEL => predefined_procedures::dbglevel(vm, params),
        OpCode::SHOWON => predefined_procedures::showon(vm, params),
        OpCode::SHOWOFF => predefined_procedures::showoff(vm, params),
        OpCode::PAGEON => predefined_procedures::pageon(vm, params),
        OpCode::PAGEOFF => predefined_procedures::pageoff(vm, params),
        OpCode::FSEEK => predefined_procedures::fseek(vm, params),
        OpCode::FFLUSH => predefined_procedures::fflush(vm, params),
        OpCode::FREAD => predefined_procedures::fread(vm, params),
        OpCode::FWRITE => predefined_procedures::fwrite(vm, params),
        OpCode::FDEFIN => predefined_procedures::fdefin(vm, params),
        OpCode::FDEFOUT => predefined_procedures::fdefout(vm, params),
        OpCode::FDGET => predefined_procedures::fdget(vm, params),
        OpCode::FDPUT => predefined_procedures::fdput(vm, params),
        OpCode::FDPUTLN => predefined_procedures::fdputln(vm, params),
        OpCode::FDPUTPAD => predefined_procedures::fdputpad(vm, params),
        OpCode::FDREAD => predefined_procedures::fdread(vm, params),
        OpCode::FDWRITE => predefined_procedures::fdwrite(vm, params),
        OpCode::ADJBYTES => predefined_procedures::adjbytes(vm, params),
        OpCode::KBDSTRING => predefined_procedures::kbdstring(vm, params),
        OpCode::ALIAS => predefined_procedures::alias(vm, params),
        OpCode::REDIM => predefined_procedures::redim(vm, params),
        OpCode::APPEND => predefined_procedures::append(vm, params),
        OpCode::COPY => predefined_procedures::copy(vm, params),
        OpCode::KBDFLUSH => {
            predefined_procedures::kbdflush(vm, params);
            Ok(())
        }
        OpCode::MDMFLUSH => {
            predefined_procedures::mdmflush(vm, params);
            Ok(())
        }
        OpCode::KEYFLUSH => {
            predefined_procedures::keyflush(vm, params);
            Ok(())
        }
        OpCode::LASTIN => predefined_procedures::lastin(vm, params),
        OpCode::FLAG => predefined_procedures::flag(vm, params),
        OpCode::DOWNLOAD => predefined_procedures::download(vm, params),
        OpCode::WRUSYSDOOR => predefined_procedures::wrusysdoor(vm, params),
        OpCode::GETALTUSER => predefined_procedures::getaltuser(vm, params),
        OpCode::ADJDBYTES => predefined_procedures::adjdbytes(vm, params),
        OpCode::ADJTBYTES => predefined_procedures::adjtbytes(vm, params),
        OpCode::ADJTFILES => predefined_procedures::adjtfiles(vm, params),
        OpCode::LANG => predefined_procedures::lang(vm, params),
        OpCode::SORT => predefined_procedures::sort(vm, params),
        OpCode::MOUSEREG => predefined_procedures::mousereg(vm, params),
        OpCode::SCRFILE => predefined_procedures::scrfile(vm, params),
        OpCode::SEARCHINIT => predefined_procedures::searchinit(vm, params),
        OpCode::SEARCHFIND => predefined_procedures::searchfind(vm, params),
        OpCode::SEARCHSTOP => predefined_procedures::searchstop(vm, params),
        OpCode::PRFOUND => predefined_procedures::prfound(vm, params),
        OpCode::PRFOUNDLN => predefined_procedures::prfoundln(vm, params),
        OpCode::TPAGET => predefined_procedures::tpaget(vm, params),
        OpCode::TPAPUT => predefined_procedures::tpaput(vm, params),
        OpCode::TPACGET => predefined_procedures::tpacget(vm, params),
        OpCode::TPACPUT => predefined_procedures::tpacput(vm, params),
        OpCode::TPAREAD => predefined_procedures::tparead(vm, params),
        OpCode::TPAWRITE => predefined_procedures::tpawrite(vm, params),
        OpCode::TPACREAD => predefined_procedures::tpacread(vm, params),
        OpCode::TPACWRITE => predefined_procedures::tpacwrite(vm, params),
        OpCode::BITSET => predefined_procedures::bitset(vm, params),
        OpCode::BITCLEAR => predefined_procedures::bitclear(vm, params),
        OpCode::BRAG => predefined_procedures::brag(vm, params),
        OpCode::FREALTUSER => predefined_procedures::frealtuser(vm, params),
        OpCode::SETLMR => predefined_procedures::setlmr(vm, params),
        OpCode::SETENV => predefined_procedures::setenv(vm, params),
        OpCode::FCLOSEALL => predefined_procedures::fcloseall(vm, params),
        // OpCode::STATIC => predefined_procedures::static(interpreter, params),
        OpCode::STACKABORT => predefined_procedures::stackabort(vm, params),
        OpCode::DCREATE => predefined_procedures::dcreate(vm, params),
        OpCode::DOPEN => predefined_procedures::dopen(vm, params),
        OpCode::DCLOSE => predefined_procedures::dclose(vm, params),
        OpCode::DSETALIAS => predefined_procedures::dsetalias(vm, params),
        OpCode::DPACK => predefined_procedures::dpack(vm, params),
        OpCode::DCLOSEALL => predefined_procedures::dcloseall(vm, params),
        OpCode::DLOCK => predefined_procedures::dlock(vm, params),
        OpCode::DLOCKR => predefined_procedures::dlockr(vm, params),
        OpCode::DLOCKG => predefined_procedures::dlockg(vm, params),
        OpCode::DUNLOCK => predefined_procedures::dunlock(vm, params),
        OpCode::DNCREATE => predefined_procedures::dncreate(vm, params),
        OpCode::DNOPEN => predefined_procedures::dnopen(vm, params),
        OpCode::DNCLOSE => predefined_procedures::dnclose(vm, params),
        OpCode::DNCLOSEALL => predefined_procedures::dncloseall(vm, params),
        OpCode::DNEW => predefined_procedures::dnew(vm, params),
        OpCode::DADD => predefined_procedures::dadd(vm, params),
        OpCode::DAPPEND => predefined_procedures::dappend(vm, params),
        OpCode::DTOP => predefined_procedures::dtop(vm, params),
        OpCode::DGO => predefined_procedures::dgo(vm, params),
        OpCode::DBOTTOM => predefined_procedures::dbottom(vm, params),
        OpCode::DSKIP => predefined_procedures::dskip(vm, params),
        OpCode::DBLANK => predefined_procedures::dblank(vm, params),
        OpCode::DDELETE => predefined_procedures::ddelete(vm, params),
        OpCode::DRECALL => predefined_procedures::drecall(vm, params),
        OpCode::DTAG => predefined_procedures::dtag(vm, params),
        OpCode::DSEEK => predefined_procedures::dseek(vm, params),
        OpCode::DFBLANK => predefined_procedures::dfblank(vm, params),
        OpCode::DGET => predefined_procedures::dget(vm, params),
        OpCode::DPUT => predefined_procedures::dput(vm, params),
        OpCode::DFCOPY => predefined_procedures::dfcopy(vm, params),

        OpCode::EVAL => predefined_procedures::eval(vm, params),
        OpCode::ACCOUNT => predefined_procedures::account(vm, params),
        OpCode::RECORDUSAGE => predefined_procedures::recordusage(vm, params),
        OpCode::MSGTOFILE => predefined_procedures::msgtofile(vm, params),
        OpCode::QWKLIMITS => predefined_procedures::qwklimits(vm, params),
        OpCode::COMMAND => predefined_procedures::command(vm, params),
        OpCode::USELMRS => predefined_procedures::uselmrs(vm, params),
        OpCode::CONFINFO => predefined_procedures::confinfo(vm, params),
        OpCode::ADJTUBYTES => predefined_procedures::adjtubytes(vm, params),
        OpCode::GRAFMODE => predefined_procedures::grafmode(vm, params),
        OpCode::ADDUSER => predefined_procedures::adduser(vm, params),
        OpCode::KILLMSG => predefined_procedures::killmsg(vm, params),
        OpCode::CHDIR => predefined_procedures::chdir(vm, params),
        OpCode::MKDIR => predefined_procedures::mkdir(vm, params),
        OpCode::REDIR => predefined_procedures::redir(vm, params),
        OpCode::FDOWRAKA => predefined_procedures::fdowraka(vm, params),
        OpCode::FDOADDAKA => predefined_procedures::fdoaddaka(vm, params),
        OpCode::FDOWRORG => predefined_procedures::fdowrorg(vm, params),
        OpCode::FDOADDORG => predefined_procedures::fdoaddorg(vm, params),
        OpCode::FDOQMOD => predefined_procedures::fdoqmod(vm, params),
        OpCode::FDOQADD => predefined_procedures::fdoqadd(vm, params),
        OpCode::FDOQDEL => predefined_procedures::fdoqdel(vm, params),
        OpCode::SOUNDDELAY => predefined_procedures::sounddelay(vm, params),
        _ => Err(Box::new(InterpreterError::UnsupportedOpCode(def.opcode))),
    }
}

#[allow(unused_variables)]
mod predefined_procedures;
