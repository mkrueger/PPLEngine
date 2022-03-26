use crate::interpreter::{Expression, Interpreter, ProgramContext, VariableValue, convert_to, evaluate_exp, get_int, get_var_name};

pub fn cls(interpreter: &mut Interpreter, params: &[Expression]) {
    interpreter.ctx.print("\x1B[2J");
}

pub fn clreol(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn more(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn wait(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn color(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn goto(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn confflag(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn confunflag(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dispfile(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn input(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fcreate(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;
    let file = &evaluate_exp(interpreter,&params[1]).to_string();
    let am = get_int(&evaluate_exp(interpreter,&params[2]));
    let sm = get_int(&evaluate_exp(interpreter,&params[3]));
    interpreter.io.fcreate(channel, file, am, sm);
}

pub fn fopen(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;
    let file = &evaluate_exp(interpreter,&params[1]).to_string();
    let am = get_int(&evaluate_exp(interpreter,&params[2]));
    let sm = get_int(&evaluate_exp(interpreter,&params[3]));
    interpreter.io.fopen(channel, file, am, sm);
}

pub fn fappend(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;
    let file = &evaluate_exp(interpreter,&params[1]).to_string();
    let am = get_int(&evaluate_exp(interpreter,&params[2]));
    let sm = get_int(&evaluate_exp(interpreter,&params[3]));
    interpreter.io.fappend(channel, file, am, sm);
}

pub fn fclose(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;
    interpreter.io.fclose(channel);
}

pub fn fget(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;
    let value = VariableValue::String(interpreter.io.fget(channel));
    let var_name = get_var_name(&params[1]);
    let var_type = interpreter.prg.get_var_type(&var_name);
    interpreter.cur_frame.last_mut().unwrap().values.insert(var_name, convert_to(var_type, &value));
}

pub fn fput(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;

    for expr in &params[1..] {
        let value = evaluate_exp(interpreter, expr);
        interpreter.io.fput(channel, value.to_string());
    }
}

pub fn fputln(interpreter: &mut Interpreter, params: &[Expression]) {
    let channel = get_int(&evaluate_exp(interpreter,&params[0])) as usize;

    for expr in &params[1..] {
        let value = evaluate_exp(interpreter, expr);
        interpreter.io.fput(channel, value.to_string());
    }
    interpreter.io.fput(channel, "\n".to_string());
}

pub fn resetdisp(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn startdisp(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fputpad(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn hangup(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn getuser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn putuser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn defcolor(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn delete(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn deluser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjtime(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn log(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputstr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputyn(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputmoney(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputint(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputcc(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputdate(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputtime(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn promptstr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dtron(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dtroff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn cdchkon(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn cdchkoff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn delay(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sendmodem(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inc(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dec(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn newline(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn newlines(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tokenize(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn gettoken(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn shell(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn disptext(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn stop(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn inputtext(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn beep(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn push(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pop(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdstuff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn call(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn join(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn quest(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn blt(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dir(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdfile(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn bye(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn goodbye(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn broadcast(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn waitfor(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdchkon(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdchkoff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn optext(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dispstr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn rdunet(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn wrunet(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dointr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn varseg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn varoff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pokeb(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pokew(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn varaddr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn ansipos(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn backup(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn forward(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn freshline(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn wrusys(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn rdusys(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn newpwd(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn opencap(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn closecap(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn message(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn savescrn(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn restscrn(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sound(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn chat(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sprint(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sprintln(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn mprint(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn mprintln(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn rename(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn frewind(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pokedw(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dbglevel(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn showon(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn showoff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pageon(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pageoff(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fseek(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fflush(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fread(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fwrite(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdefin(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdefout(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdget(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdput(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdputln(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdputpad(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdread(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdwrite(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjbytes(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdstring(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn alias(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn redim(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn append(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn copy(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn kbdflush(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn mdmflush(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn keyflush(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn lastin(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn flag(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn download(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn wrusysdoor(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn getaltuser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjdbytes(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjtbytes(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjtfiles(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn lang(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sort(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn mousereg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn scrfile(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn searchinit(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn searchfind(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn searchstop(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn prfound(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn prfoundln(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpaget(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpaput(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpacget(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpacput(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tparead(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpawrite(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpacread(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn tpacwrite(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn bitset(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn bitclear(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn brag(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn frealtuser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn setlmr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn setenv(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fcloseall(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn declare(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn function(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn procedure(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn pcall(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fpclr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn begin(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fend(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn stackabort(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dcreate(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dopen(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dclose(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dsetalias(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dpack(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dcloseall(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dlock(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dlockr(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dlockg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dunlock(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dncreate(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dnopen(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dnclose(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dncloseall(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dnew(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dadd(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dappend(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dtop(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dgo(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dbottom(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dskip(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dblank(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn ddelete(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn drecall(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dtag(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dseek(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dfblank(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dget(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dput(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn dfcopy(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}

pub fn eval(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn account(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn recordusage(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn msgtofile(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn qwklimits(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn command(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn uselmrs(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn confinfo(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adjtubytes(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn grafmode(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn adduser(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn killmsg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn chdir(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn mkdir(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn redir(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdowraka(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdoaddaka(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdowrorg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdoaddorg(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdoqmod(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdoqadd(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn fdoqdel(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
pub fn sounddelay(interpreter: &Interpreter, params: &[Expression]) {
    panic!("TODO")
}
