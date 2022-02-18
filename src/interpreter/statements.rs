use crate::{tables::StatementDefinition, ast::program::Program};
use super::*;

pub fn call_predefined_procedure(
    prg: &Program,
    cur_frame: &StackFrame,
    ctx: &mut dyn ExecutionContext,
    def: &'static StatementDefinition,
    params: &Vec<Expression>
)
{
    match def.opcode {
        OpCode::CLS => predefined_procedures::cls(prg, cur_frame, ctx, params),
        OpCode::CLREOL => predefined_procedures::clreol(prg, cur_frame, ctx, params),
        OpCode::MORE => predefined_procedures::more(prg, cur_frame, ctx, params),
        OpCode::WAIT => predefined_procedures::wait(prg, cur_frame, ctx, params),
        OpCode::COLOR => predefined_procedures::color(prg, cur_frame, ctx, params),
        OpCode::GOTO => predefined_procedures::goto(prg, cur_frame, ctx, params),
        OpCode::PRINT => {
            for expr in params {
                let value = evaluate_exp(prg, cur_frame, ctx, expr);
                ctx.print(value.to_string());
            }
        }
        OpCode::PRINTLN => {
            for expr in params {
                let value = evaluate_exp(prg, cur_frame, ctx, expr);
                ctx.print(value.to_string());
            }
            ctx.print("\n".to_string());
        }
        OpCode::CONFFLAG => predefined_procedures::confflag(prg, cur_frame, ctx, params),
        OpCode::CONFUNFLAG => predefined_procedures::confunflag(prg, cur_frame, ctx, params),
        OpCode::DISPFILE => predefined_procedures::dispfile(prg, cur_frame, ctx, params),
        OpCode::INPUT => predefined_procedures::input(prg, cur_frame, ctx, params),
        OpCode::FCREATE => predefined_procedures::fcreate(prg, cur_frame, ctx, params),
        OpCode::FOPEN => predefined_procedures::fopen(prg, cur_frame, ctx, params),
        OpCode::FAPPEND => predefined_procedures::fappend(prg, cur_frame, ctx, params),
        OpCode::FCLOSE => predefined_procedures::fclose(prg, cur_frame, ctx, params),
        OpCode::FGET => predefined_procedures::fget(prg, cur_frame, ctx, params),
        OpCode::FPUT => predefined_procedures::fput(prg, cur_frame, ctx, params),
        OpCode::FPUTLN => predefined_procedures::fputln(prg, cur_frame, ctx, params),
        OpCode::RESETDISP => predefined_procedures::resetdisp(prg, cur_frame, ctx, params),
        OpCode::STARTDISP => predefined_procedures::startdisp(prg, cur_frame, ctx, params),
        OpCode::FPUTPAD => predefined_procedures::fputpad(prg, cur_frame, ctx, params),
        OpCode::HANGUP => predefined_procedures::hangup(prg, cur_frame, ctx, params),
        OpCode::GETUSER => predefined_procedures::getuser(prg, cur_frame, ctx, params),
        OpCode::PUTUSER => predefined_procedures::putuser(prg, cur_frame, ctx, params),
        OpCode::DEFCOLOR => predefined_procedures::defcolor(prg, cur_frame, ctx, params),
        OpCode::DELETE => predefined_procedures::delete(prg, cur_frame, ctx, params),
        OpCode::DELUSER => predefined_procedures::deluser(prg, cur_frame, ctx, params),
        OpCode::ADJTIME => predefined_procedures::adjtime(prg, cur_frame, ctx, params),
        OpCode::LOG => predefined_procedures::log(prg, cur_frame, ctx, params),
        OpCode::INPUTSTR => predefined_procedures::inputstr(prg, cur_frame, ctx, params),
        OpCode::INPUTYN => predefined_procedures::inputyn(prg, cur_frame, ctx, params),
        OpCode::INPUTMONEY => predefined_procedures::inputmoney(prg, cur_frame, ctx, params),
        OpCode::INPUTINT => predefined_procedures::inputint(prg, cur_frame, ctx, params),
        OpCode::INPUTCC => predefined_procedures::inputcc(prg, cur_frame, ctx, params),
        OpCode::INPUTDATE => predefined_procedures::inputdate(prg, cur_frame, ctx, params),
        OpCode::INPUTTIME => predefined_procedures::inputtime(prg, cur_frame, ctx, params),
        OpCode::PROMPTSTR => predefined_procedures::promptstr(prg, cur_frame, ctx, params),
        OpCode::DTRON => predefined_procedures::dtron(prg, cur_frame, ctx, params),
        OpCode::DTROFF => predefined_procedures::dtroff(prg, cur_frame, ctx, params),
        OpCode::CDCHKON => predefined_procedures::cdchkon(prg, cur_frame, ctx, params),
        OpCode::CDCHKOFF => predefined_procedures::cdchkoff(prg, cur_frame, ctx, params),
        OpCode::DELAY => predefined_procedures::delay(prg, cur_frame, ctx, params),
        OpCode::SENDMODEM => predefined_procedures::sendmodem(prg, cur_frame, ctx, params),
        OpCode::INC => predefined_procedures::inc(prg, cur_frame, ctx, params),
        OpCode::DEC => predefined_procedures::dec(prg, cur_frame, ctx, params),
        OpCode::NEWLINE => predefined_procedures::newline(prg, cur_frame, ctx, params),
        OpCode::NEWLINES => predefined_procedures::newlines(prg, cur_frame, ctx, params),
        OpCode::TOKENIZE => predefined_procedures::tokenize(prg, cur_frame, ctx, params),
        OpCode::GETTOKEN => predefined_procedures::gettoken(prg, cur_frame, ctx, params),
        OpCode::SHELL => predefined_procedures::shell(prg, cur_frame, ctx, params),
        OpCode::DISPTEXT => predefined_procedures::disptext(prg, cur_frame, ctx, params),
        OpCode::STOP => predefined_procedures::stop(prg, cur_frame, ctx, params),
        OpCode::INPUTTEXT => predefined_procedures::inputtext(prg, cur_frame, ctx, params),
        OpCode::BEEP => predefined_procedures::beep(prg, cur_frame, ctx, params),
        OpCode::PUSH => predefined_procedures::push(prg, cur_frame, ctx, params),
        OpCode::POP => predefined_procedures::pop(prg, cur_frame, ctx, params),
        OpCode::KBDSTUFF => predefined_procedures::kbdstuff(prg, cur_frame, ctx, params),
        OpCode::CALL => predefined_procedures::call(prg, cur_frame, ctx, params),
        OpCode::JOIN => predefined_procedures::join(prg, cur_frame, ctx, params),
        OpCode::QUEST => predefined_procedures::quest(prg, cur_frame, ctx, params),
        OpCode::BLT => predefined_procedures::blt(prg, cur_frame, ctx, params),
        OpCode::DIR => predefined_procedures::dir(prg, cur_frame, ctx, params),
        OpCode::KBDFILE => predefined_procedures::kbdfile(prg, cur_frame, ctx, params),
        OpCode::BYE => predefined_procedures::bye(prg, cur_frame, ctx, params),
        OpCode::GOODBYE => predefined_procedures::goodbye(prg, cur_frame, ctx, params),
        OpCode::BROADCAST => predefined_procedures::broadcast(prg, cur_frame, ctx, params),
        OpCode::WAITFOR => predefined_procedures::waitfor(prg, cur_frame, ctx, params),
        OpCode::KBDCHKON => predefined_procedures::kbdchkon(prg, cur_frame, ctx, params),
        OpCode::KBDCHKOFF => predefined_procedures::kbdchkoff(prg, cur_frame, ctx, params),
        OpCode::OPTEXT => predefined_procedures::optext(prg, cur_frame, ctx, params),
        OpCode::DISPSTR => predefined_procedures::dispstr(prg, cur_frame, ctx, params),
        OpCode::RDUNET => predefined_procedures::rdunet(prg, cur_frame, ctx, params),
        OpCode::WRUNET => predefined_procedures::wrunet(prg, cur_frame, ctx, params),
        OpCode::DOINTR => predefined_procedures::dointr(prg, cur_frame, ctx, params),
        OpCode::VARSEG => predefined_procedures::varseg(prg, cur_frame, ctx, params),
        OpCode::VAROFF => predefined_procedures::varoff(prg, cur_frame, ctx, params),
        OpCode::POKEB => predefined_procedures::pokeb(prg, cur_frame, ctx, params),
        OpCode::POKEW => predefined_procedures::pokew(prg, cur_frame, ctx, params),
        OpCode::VARADDR => predefined_procedures::varaddr(prg, cur_frame, ctx, params),
        OpCode::ANSIPOS => predefined_procedures::ansipos(prg, cur_frame, ctx, params),
        OpCode::BACKUP => predefined_procedures::backup(prg, cur_frame, ctx, params),
        OpCode::FORWARD => predefined_procedures::forward(prg, cur_frame, ctx, params),
        OpCode::FRESHLINE => predefined_procedures::freshline(prg, cur_frame, ctx, params),
        OpCode::WRUSYS => predefined_procedures::wrusys(prg, cur_frame, ctx, params),
        OpCode::RDUSYS => predefined_procedures::rdusys(prg, cur_frame, ctx, params),
        OpCode::NEWPWD => predefined_procedures::newpwd(prg, cur_frame, ctx, params),
        OpCode::OPENCAP => predefined_procedures::opencap(prg, cur_frame, ctx, params),
        OpCode::CLOSECAP => predefined_procedures::closecap(prg, cur_frame, ctx, params),
        OpCode::MESSAGE => predefined_procedures::message(prg, cur_frame, ctx, params),
        OpCode::SAVESCRN => predefined_procedures::savescrn(prg, cur_frame, ctx, params),
        OpCode::RESTSCRN => predefined_procedures::restscrn(prg, cur_frame, ctx, params),
        OpCode::SOUND => predefined_procedures::sound(prg, cur_frame, ctx, params),
        OpCode::CHAT => predefined_procedures::chat(prg, cur_frame, ctx, params),
        OpCode::SPRINT => predefined_procedures::sprint(prg, cur_frame, ctx, params),
        OpCode::SPRINTLN => predefined_procedures::sprintln(prg, cur_frame, ctx, params),
        OpCode::MPRINT => predefined_procedures::mprint(prg, cur_frame, ctx, params),
        OpCode::MPRINTLN => predefined_procedures::mprintln(prg, cur_frame, ctx, params),
        OpCode::RENAME => predefined_procedures::rename(prg, cur_frame, ctx, params),
        OpCode::FREWIND => predefined_procedures::frewind(prg, cur_frame, ctx, params),
        OpCode::POKEDW => predefined_procedures::pokedw(prg, cur_frame, ctx, params),
        OpCode::DBGLEVEL => predefined_procedures::dbglevel(prg, cur_frame, ctx, params),
        OpCode::SHOWON => predefined_procedures::showon(prg, cur_frame, ctx, params),
        OpCode::SHOWOFF => predefined_procedures::showoff(prg, cur_frame, ctx, params),
        OpCode::PAGEON => predefined_procedures::pageon(prg, cur_frame, ctx, params),
        OpCode::PAGEOFF => predefined_procedures::pageoff(prg, cur_frame, ctx, params),
        OpCode::FSEEK => predefined_procedures::fseek(prg, cur_frame, ctx, params),
        OpCode::FFLUSH => predefined_procedures::fflush(prg, cur_frame, ctx, params),
        OpCode::FREAD => predefined_procedures::fread(prg, cur_frame, ctx, params),
        OpCode::FWRITE => predefined_procedures::fwrite(prg, cur_frame, ctx, params),
        OpCode::FDEFIN => predefined_procedures::fdefin(prg, cur_frame, ctx, params),
        OpCode::FDEFOUT => predefined_procedures::fdefout(prg, cur_frame, ctx, params),
        OpCode::FDGET => predefined_procedures::fdget(prg, cur_frame, ctx, params),
        OpCode::FDPUT => predefined_procedures::fdput(prg, cur_frame, ctx, params),
        OpCode::FDPUTLN => predefined_procedures::fdputln(prg, cur_frame, ctx, params),
        OpCode::FDPUTPAD => predefined_procedures::fdputpad(prg, cur_frame, ctx, params),
        OpCode::FDREAD => predefined_procedures::fdread(prg, cur_frame, ctx, params),
        OpCode::FDWRITE => predefined_procedures::fdwrite(prg, cur_frame, ctx, params),
        OpCode::ADJBYTES => predefined_procedures::adjbytes(prg, cur_frame, ctx, params),
        OpCode::KBDSTRING => predefined_procedures::kbdstring(prg, cur_frame, ctx, params),
        OpCode::ALIAS => predefined_procedures::alias(prg, cur_frame, ctx, params),
        OpCode::REDIM => predefined_procedures::redim(prg, cur_frame, ctx, params),
        OpCode::APPEND => predefined_procedures::append(prg, cur_frame, ctx, params),
        OpCode::COPY => predefined_procedures::copy(prg, cur_frame, ctx, params),
        OpCode::KBDFLUSH => predefined_procedures::kbdflush(prg, cur_frame, ctx, params),
        OpCode::MDMFLUSH => predefined_procedures::mdmflush(prg, cur_frame, ctx, params),
        OpCode::KEYFLUSH => predefined_procedures::keyflush(prg, cur_frame, ctx, params),
        OpCode::LASTIN => predefined_procedures::lastin(prg, cur_frame, ctx, params),
        OpCode::FLAG => predefined_procedures::flag(prg, cur_frame, ctx, params),
        OpCode::DOWNLOAD => predefined_procedures::download(prg, cur_frame, ctx, params),
        OpCode::WRUSYSDOOR => predefined_procedures::wrusysdoor(prg, cur_frame, ctx, params),
        OpCode::GETALTUSER => predefined_procedures::getaltuser(prg, cur_frame, ctx, params),
        OpCode::ADJDBYTES => predefined_procedures::adjdbytes(prg, cur_frame, ctx, params),
        OpCode::ADJTBYTES => predefined_procedures::adjtbytes(prg, cur_frame, ctx, params),
        OpCode::ADJTFILES => predefined_procedures::adjtfiles(prg, cur_frame, ctx, params),
        OpCode::LANG => predefined_procedures::lang(prg, cur_frame, ctx, params),
        OpCode::SORT => predefined_procedures::sort(prg, cur_frame, ctx, params),
        OpCode::MOUSEREG => predefined_procedures::mousereg(prg, cur_frame, ctx, params),
        OpCode::SCRFILE => predefined_procedures::scrfile(prg, cur_frame, ctx, params),
        OpCode::SEARCHINIT => predefined_procedures::searchinit(prg, cur_frame, ctx, params),
        OpCode::SEARCHFIND => predefined_procedures::searchfind(prg, cur_frame, ctx, params),
        OpCode::SEARCHSTOP => predefined_procedures::searchstop(prg, cur_frame, ctx, params),
        OpCode::PRFOUND => predefined_procedures::prfound(prg, cur_frame, ctx, params),
        OpCode::PRFOUNDLN => predefined_procedures::prfoundln(prg, cur_frame, ctx, params),
        OpCode::TPAGET => predefined_procedures::tpaget(prg, cur_frame, ctx, params),
        OpCode::TPAPUT => predefined_procedures::tpaput(prg, cur_frame, ctx, params),
        OpCode::TPACGET => predefined_procedures::tpacget(prg, cur_frame, ctx, params),
        OpCode::TPACPUT => predefined_procedures::tpacput(prg, cur_frame, ctx, params),
        OpCode::TPAREAD => predefined_procedures::tparead(prg, cur_frame, ctx, params),
        OpCode::TPAWRITE => predefined_procedures::tpawrite(prg, cur_frame, ctx, params),
        OpCode::TPACREAD => predefined_procedures::tpacread(prg, cur_frame, ctx, params),
        OpCode::TPACWRITE => predefined_procedures::tpacwrite(prg, cur_frame, ctx, params),
        OpCode::BITSET => predefined_procedures::bitset(prg, cur_frame, ctx, params),
        OpCode::BITCLEAR => predefined_procedures::bitclear(prg, cur_frame, ctx, params),
        OpCode::BRAG => predefined_procedures::brag(prg, cur_frame, ctx, params),
        OpCode::FREALTUSER => predefined_procedures::frealtuser(prg, cur_frame, ctx, params),
        OpCode::SETLMR => predefined_procedures::setlmr(prg, cur_frame, ctx, params),
        OpCode::SETENV => predefined_procedures::setenv(prg, cur_frame, ctx, params),
        OpCode::FCLOSEALL => predefined_procedures::fcloseall(prg, cur_frame, ctx, params),
        OpCode::DECLARE => predefined_procedures::declare(prg, cur_frame, ctx, params),
        OpCode::FUNCTION => predefined_procedures::function(prg, cur_frame, ctx, params),
        OpCode::PROCEDURE => predefined_procedures::procedure(prg, cur_frame, ctx, params),
        OpCode::PCALL => predefined_procedures::pcall(prg, cur_frame, ctx, params),
        OpCode::FPCLR => predefined_procedures::fpclr(prg, cur_frame, ctx, params),
        OpCode::BEGIN => predefined_procedures::begin(prg, cur_frame, ctx, params),
        OpCode::FEND => predefined_procedures::fend(prg, cur_frame, ctx, params),
       // OpCode::STATIC => predefined_procedures::static(prg, cur_frame, ctx, params),
        OpCode::STACKABORT => predefined_procedures::stackabort(prg, cur_frame, ctx, params),
        OpCode::DCREATE => predefined_procedures::dcreate(prg, cur_frame, ctx, params),
        OpCode::DOPEN => predefined_procedures::dopen(prg, cur_frame, ctx, params),
        OpCode::DCLOSE => predefined_procedures::dclose(prg, cur_frame, ctx, params),
        OpCode::DSETALIAS => predefined_procedures::dsetalias(prg, cur_frame, ctx, params),
        OpCode::DPACK => predefined_procedures::dpack(prg, cur_frame, ctx, params),
        OpCode::DCLOSEALL => predefined_procedures::dcloseall(prg, cur_frame, ctx, params),
        OpCode::DLOCK => predefined_procedures::dlock(prg, cur_frame, ctx, params),
        OpCode::DLOCKR => predefined_procedures::dlockr(prg, cur_frame, ctx, params),
        OpCode::DLOCKG => predefined_procedures::dlockg(prg, cur_frame, ctx, params),
        OpCode::DUNLOCK => predefined_procedures::dunlock(prg, cur_frame, ctx, params),
        OpCode::DNCREATE => predefined_procedures::dncreate(prg, cur_frame, ctx, params),
        OpCode::DNOPEN => predefined_procedures::dnopen(prg, cur_frame, ctx, params),
        OpCode::DNCLOSE => predefined_procedures::dnclose(prg, cur_frame, ctx, params),
        OpCode::DNCLOSEALL => predefined_procedures::dncloseall(prg, cur_frame, ctx, params),
        OpCode::DNEW => predefined_procedures::dnew(prg, cur_frame, ctx, params),
        OpCode::DADD => predefined_procedures::dadd(prg, cur_frame, ctx, params),
        OpCode::DAPPEND => predefined_procedures::dappend(prg, cur_frame, ctx, params),
        OpCode::DTOP => predefined_procedures::dtop(prg, cur_frame, ctx, params),
        OpCode::DGO => predefined_procedures::dgo(prg, cur_frame, ctx, params),
        OpCode::DBOTTOM => predefined_procedures::dbottom(prg, cur_frame, ctx, params),
        OpCode::DSKIP => predefined_procedures::dskip(prg, cur_frame, ctx, params),
        OpCode::DBLANK => predefined_procedures::dblank(prg, cur_frame, ctx, params),
        OpCode::DDELETE => predefined_procedures::ddelete(prg, cur_frame, ctx, params),
        OpCode::DRECALL => predefined_procedures::drecall(prg, cur_frame, ctx, params),
        OpCode::DTAG => predefined_procedures::dtag(prg, cur_frame, ctx, params),
        OpCode::DSEEK => predefined_procedures::dseek(prg, cur_frame, ctx, params),
        OpCode::DFBLANK => predefined_procedures::dfblank(prg, cur_frame, ctx, params),
        OpCode::DGET => predefined_procedures::dget(prg, cur_frame, ctx, params),
        OpCode::DPUT => predefined_procedures::dput(prg, cur_frame, ctx, params),
        OpCode::DFCOPY => predefined_procedures::dfcopy(prg, cur_frame, ctx, params),
    
        OpCode::EVAL => predefined_procedures::eval(prg, cur_frame, ctx, params),
        OpCode::ACCOUNT => predefined_procedures::account(prg, cur_frame, ctx, params),
        OpCode::RECORDUSAGE => predefined_procedures::recordusage(prg, cur_frame, ctx, params),
        OpCode::MSGTOFILE => predefined_procedures::msgtofile(prg, cur_frame, ctx, params),
        OpCode::QWKLIMITS => predefined_procedures::qwklimits(prg, cur_frame, ctx, params),
        OpCode::COMMAND => predefined_procedures::command(prg, cur_frame, ctx, params),
        OpCode::USELMRS => predefined_procedures::uselmrs(prg, cur_frame, ctx, params),
        OpCode::CONFINFO => predefined_procedures::confinfo(prg, cur_frame, ctx, params),
        OpCode::ADJTUBYTES => predefined_procedures::adjtubytes(prg, cur_frame, ctx, params),
        OpCode::GRAFMODE => predefined_procedures::grafmode(prg, cur_frame, ctx, params),
        OpCode::ADDUSER => predefined_procedures::adduser(prg, cur_frame, ctx, params),
        OpCode::KILLMSG => predefined_procedures::killmsg(prg, cur_frame, ctx, params),
        OpCode::CHDIR => predefined_procedures::chdir(prg, cur_frame, ctx, params),
        OpCode::MKDIR => predefined_procedures::mkdir(prg, cur_frame, ctx, params),
        OpCode::REDIR => predefined_procedures::redir(prg, cur_frame, ctx, params),
        OpCode::FDOWRAKA => predefined_procedures::fdowraka(prg, cur_frame, ctx, params),
        OpCode::FDOADDAKA => predefined_procedures::fdoaddaka(prg, cur_frame, ctx, params),
        OpCode::FDOWRORG => predefined_procedures::fdowrorg(prg, cur_frame, ctx, params),
        OpCode::FDOADDORG => predefined_procedures::fdoaddorg(prg, cur_frame, ctx, params),
        OpCode::FDOQMOD => predefined_procedures::fdoqmod(prg, cur_frame, ctx, params),
        OpCode::FDOQADD => predefined_procedures::fdoqadd(prg, cur_frame, ctx, params),
        OpCode::FDOQDEL => predefined_procedures::fdoqdel(prg, cur_frame, ctx, params),
        OpCode::SOUNDDELAY => predefined_procedures::sounddelay(prg, cur_frame, ctx, params),
            _ => { panic!("unsupported op code {:?}", def.opcode); }
    }
}

#[allow(unused_variables)]
mod predefined_procedures
{
    use crate::{ast::program::Program, interpreter::*};

    pub fn cls( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn clreol( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn more( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn wait( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn color( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn goto( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn confflag( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn confunflag( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dispfile( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn input( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fcreate( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fopen( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fappend( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fclose( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fget( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fput( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fputln( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn resetdisp( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn startdisp( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fputpad( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn hangup( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn getuser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn putuser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn defcolor( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn delete( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn deluser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjtime( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn log( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputstr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputyn( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputmoney( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputint( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputcc( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputdate( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputtime( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn promptstr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dtron( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dtroff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn cdchkon( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn cdchkoff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn delay( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sendmodem( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inc( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dec( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn newline( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn newlines( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tokenize( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn gettoken( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn shell( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn disptext( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn stop( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn inputtext( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn beep( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn push( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pop( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdstuff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn call( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn join( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn quest( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn blt( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dir( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdfile( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn bye( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn goodbye( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn broadcast( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn waitfor( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdchkon( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdchkoff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn optext( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dispstr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn rdunet( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn wrunet( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dointr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn varseg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn varoff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pokeb( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pokew( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn varaddr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn ansipos( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn backup( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn forward( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn freshline( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn wrusys( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn rdusys( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn newpwd( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn opencap( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn closecap( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn message( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn savescrn( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn restscrn( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sound( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn chat( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sprint( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sprintln( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn mprint( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn mprintln( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn rename( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn frewind( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pokedw( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dbglevel( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn showon( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn showoff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pageon( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pageoff( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fseek( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fflush( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fread( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fwrite( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdefin( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdefout( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdget( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdput( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdputln( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdputpad( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdread( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdwrite( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjbytes( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdstring( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn alias( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn redim( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn append( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn copy( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn kbdflush( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn mdmflush( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn keyflush( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn lastin( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn flag( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn download( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn wrusysdoor( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn getaltuser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjdbytes( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjtbytes( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjtfiles( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn lang( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sort( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn mousereg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn scrfile( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn searchinit( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn searchfind( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn searchstop( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn prfound( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn prfoundln( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpaget( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpaput( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpacget( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpacput( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tparead( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpawrite( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpacread( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn tpacwrite( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn bitset( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn bitclear( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn brag( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn frealtuser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn setlmr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn setenv( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fcloseall( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn declare( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn function( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn procedure( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn pcall( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fpclr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn begin( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fend( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn stackabort( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dcreate( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dopen( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dclose( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dsetalias( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dpack( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dcloseall( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dlock( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dlockr( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dlockg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dunlock( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dncreate( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dnopen( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dnclose( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dncloseall( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dnew( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dadd( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dappend( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dtop( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dgo( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dbottom( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dskip( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dblank( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn ddelete( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn drecall( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dtag( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dseek( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dfblank( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dget( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dput( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn dfcopy( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    
    pub fn eval( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn account( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn recordusage( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn msgtofile( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn qwklimits( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn command( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn uselmrs( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn confinfo( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adjtubytes( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn grafmode( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn adduser( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn killmsg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn chdir( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
      pub fn mkdir( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn redir( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdowraka( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdoaddaka( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdowrorg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdoaddorg( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdoqmod( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdoqadd( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn fdoqdel( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
    pub fn sounddelay( prg: &Program,
        cur_frame: &StackFrame,
        ctx: &mut dyn ExecutionContext,
        params: &Vec<Expression>) {
          panic!("TODO")
      }
}