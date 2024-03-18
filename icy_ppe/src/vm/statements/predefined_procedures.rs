use std::{fs, thread, time::Duration};

use crate::{
    ast::{Expression, Variable},
    icy_board::text_messages,
    vm::{VirtualMachine, TerminalTarget},
    Res,
};

use super::super::errors::IcyError;

pub fn cls(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    interpreter.ctx.print(TerminalTarget::Both, "\x1B[2J")
}

pub fn clreol(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    interpreter.ctx.print(TerminalTarget::Both, "\x1B[K")
}

pub fn more(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter.ctx.print(
        TerminalTarget::Both,
        &interpreter
            .icy_board_data
            .display_text
            .get_display_text(text_messages::MOREPROMPT)?,
    )?;
    loop {
        if let Some(ch) = interpreter.ctx.get_char()? {
            let ch = ch.to_uppercase().to_string();

            if ch == interpreter.icy_board_data.yes_char.to_string()
                || ch == interpreter.icy_board_data.no_char.to_string()
            {
                break;
            }
        }
    }
    Ok(())
}

pub fn wait(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter.ctx.print(
        TerminalTarget::Both,
        &interpreter
            .icy_board_data
            .display_text
            .get_display_text(text_messages::PRESSENTER)?,
    )?;
    loop {
        if let Some(ch) = interpreter.ctx.get_char()? {
            if ch == '\n' || ch == '\r' {
                break;
            }
        }
    }
    Ok(())
}

pub fn color(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let color = evaluate_exp(interpreter, &params[0])?.as_int();
    interpreter.ctx.set_color(color as u8);
    Ok(())
}

pub fn confflag(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn confunflag(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

/// # Errors
/// Errors if
pub fn dispfile(interpreter: &mut VirtualMachine, file: &str, flags: i32) -> Res<()> {
    let file = interpreter.io.resolve_file(file);

    let content = fs::read(&file);
    match content {
        Ok(content) => interpreter.ctx.write_raw(TerminalTarget::Both, &content),
        Err(err) => interpreter
            .ctx
            .print(TerminalTarget::Both, format!("{file} error {err}").as_str()),
    }
}

pub fn input(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fcreate(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;
    let file = evaluate_exp(interpreter, &params[1])?.as_string();
    let am = evaluate_exp(interpreter, &params[2])?.as_int();
    let sm = evaluate_exp(interpreter, &params[3])?.as_int();
    interpreter.io.fcreate(channel, &file, am, sm);
    Ok(())
}

pub fn fopen(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;
    let file = evaluate_exp(interpreter, &params[1])?.as_string();
    let am = evaluate_exp(interpreter, &params[2])?.as_int();
    let sm = evaluate_exp(interpreter, &params[3])?.as_int();
    interpreter.io.fopen(channel, &file, am, sm)?;
    Ok(())
}

pub fn fappend(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;
    let file = evaluate_exp(interpreter, &params[1])?.as_string();
    let am = evaluate_exp(interpreter, &params[2])?.as_int();
    let sm = evaluate_exp(interpreter, &params[3])?.as_int();
    interpreter.io.fappend(channel, &file, am, sm);
    Ok(())
}

pub fn fclose(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int();
    if channel == -1 {
        // READLINE uses -1 as a special value
        return Ok(());
    }
    if !(0..=7).contains(&channel) {
        return Err(Box::new(IcyError::FileChannelOutOfBounds(channel)));
    }
    interpreter.io.fclose(channel as usize);
    Ok(())
}

pub fn fget(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;
    let value = Variable::new_string(interpreter.io.fget(channel));
    interpreter.set_variable_value(&params[1], &value)?;
    Ok(())
}

pub fn fput(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;

    for expr in &params[1..] {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter.io.fput(channel, value.as_string());
    }
    Ok(())
}

pub fn fputln(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let channel = evaluate_exp(interpreter, &params[0])?.as_int() as usize;

    for expr in &params[1..] {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter.io.fput(channel, value.as_string());
    }
    interpreter.io.fput(channel, "\n".to_string());
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn resetdisp(interpreter: &VirtualMachine, params: &[Expression]) {
    // TODO?: unused
}

/// # Errors
/// Errors if the variable is not found.
pub fn startdisp(interpreter: &VirtualMachine, params: &[Expression]) {
    // TODO?: unused
}

/// # Errors
/// Errors if the variable is not found.
pub fn fputpad(interpreter: &VirtualMachine, params: &[Expression]) {
    panic!("TODO")
}

/// # Errors
/// Errors if the variable is not found.
pub fn hangup(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter
        .ctx
        .hangup(crate::vm::HangupType::Hangup)?;
    interpreter.is_running = false;
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn getuser(interpreter: &mut VirtualMachine) -> Res<()> {
    let user = if let Some(user) = interpreter.icy_board_data.users.get(interpreter.cur_user) {
        user.clone()
    } else {
        return Err(Box::new(IcyError::UserNotFound(
            interpreter.cur_user.to_string(),
        )));
    };

    interpreter.set_user_variables(&user);
    interpreter.current_user = Some(user);
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn putuser(interpreter: &mut VirtualMachine) -> Res<()> {
    if let Some(user) = &interpreter.current_user {
        interpreter.icy_board_data.users[interpreter.cur_user] = user.clone();
        Ok(())
    } else {
        Err(Box::new(IcyError::UserNotSet))
    }
}

pub fn defcolor(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn delete(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let file = &evaluate_exp(interpreter, &params[0])?.as_string();
    if let Err(err) = interpreter.io.delete(file) {
        log::error!("Error deleting file: {}", err);
    }
    Ok(())
}

pub fn deluser(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtime(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn log(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let msg = evaluate_exp(interpreter, &params[0])?.as_string();
    // let left = &evaluate_exp(interpreter, &params[0])?;
    log::info!("{}", msg);
    Ok(())
}

const TXT_STOPCHAR: char = '_';

pub fn inputstr(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    // 1 Output Variable
    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = evaluate_exp(interpreter, &params[3])?.as_int();
    let valid = evaluate_exp(interpreter, &params[4])?.as_string();
    let flags = evaluate_exp(interpreter, &params[5])?.as_int();
    let output = internal_input_string(interpreter, color, prompt, len, &valid)?;
    interpreter.set_variable_value(&params[1], &Variable::new_string(output))
}

fn internal_input_string(
    interpreter: &mut VirtualMachine,
    color: i32,
    prompt: String,
    len: i32,
    valid: &str,
) -> Res<String> {
    let mut prompt = prompt;
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }
    interpreter.ctx.set_color(color as u8);
    interpreter.ctx.print(TerminalTarget::Both, &prompt)?;
    let mut output = String::new();
    loop {
        let Some(ch) = interpreter.ctx.get_char()? else {
            continue;
        };
        if ch == '\n' || ch == '\r' {
            break;
        }
        if ch == '\x08' && !output.is_empty() {
            output.pop();
            interpreter.ctx.print(TerminalTarget::Both, "\x08 \x08")?;
            continue;
        }

        if (output.len() as i32) < len && valid.contains(ch) {
            output.push(ch);
            interpreter
                .ctx
                .print(TerminalTarget::Both, &ch.to_string())?;
        }
    }
    Ok(output)
}

pub fn inputyn(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 1;
    let valid = "YyNn";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    interpreter.set_variable_value(
        &params[1],
        &Variable::new_string(output.to_ascii_uppercase()),
    )
}

pub fn inputmoney(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 13;
    let valid = "01234567890+-$.";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    // TODO: Money conversion.
    interpreter.set_variable_value(&params[1], &Variable::new_string(output))
}

pub fn inputint(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 11;
    let valid = "01234567890+-";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    interpreter.set_variable_value(
        &params[1],
        &Variable::new_int(output.parse::<i32>().unwrap()),
    )
}
pub fn inputcc(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 16;
    let valid = "01234567890";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    interpreter.set_variable_value(&params[1], &Variable::new_string(output))
}
pub fn inputdate(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 8;
    let valid = "01234567890-/";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    // TODO: Date conversion
    interpreter.set_variable_value(&params[1], &Variable::new_string(output))
}
pub fn inputtime(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let mut prompt = evaluate_exp(interpreter, &params[0])?.as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = evaluate_exp(interpreter, &params[2])?.as_int();
    let len = 8;
    let valid = "01234567890:";
    let output = internal_input_string(interpreter, color, prompt, len, valid)?;
    // TODO: Time conversion
    interpreter.set_variable_value(&params[1], &Variable::new_string(output))
}
pub fn promptstr(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dtron(interpreter: &VirtualMachine) {
    // IGNORE
}

pub fn dtroff(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter
        .ctx
        .hangup(crate::vm::HangupType::Hangup)?;
    Ok(())
}

pub fn cdchkon(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn cdchkoff(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn delay(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    // 1 tick is ~1/18.2s
    let ticks = evaluate_exp(interpreter, &params[0])?.as_int();
    if ticks > 0 {
        thread::sleep(Duration::from_millis((ticks as f32 * 1000.0 / 18.2) as u64));
    }
    Ok(())
}

pub fn sendmodem(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn inc(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let cur_value = evaluate_exp(interpreter, &params[0])?;
    let new_value = cur_value + Variable::new_int(1);
    interpreter.set_variable_value(&params[0], &new_value)
}

pub fn dec(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let cur_value = evaluate_exp(interpreter, &params[0])?;
    let new_value = cur_value - Variable::new_int(1);
    interpreter.set_variable_value(&params[0], &new_value)
}

pub fn newline(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter.ctx.write_raw(TerminalTarget::Both, &[b'\n'])
}

pub fn newlines(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let count = evaluate_exp(interpreter, &params[0])?.as_int();
    for _ in 0..count {
        newline(interpreter)?;
    }
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn tokenize(interpreter: &mut VirtualMachine, str: &str) {
    let split = str
        .split(&[' ', ';'][..])
        .map(std::string::ToString::to_string);
    interpreter.cur_tokens = split.collect();
}

pub fn gettoken(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn shell(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn disptext(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn stop(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn inputtext(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn beep(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn push(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pop(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdstuff(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let value = evaluate_exp(interpreter, &params[0])?;
    interpreter
        .ctx
        .print(TerminalTarget::Both, &value.as_string())
}
pub fn call(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn join(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn quest(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn blt(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dir(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdfile(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn bye(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter
        .ctx
        .hangup(crate::vm::HangupType::Bye)?;
    interpreter.is_running = false;
    Ok(())
}

pub fn goodbye(interpreter: &mut VirtualMachine) -> Res<()> {
    interpreter
        .ctx
        .hangup(crate::vm::HangupType::Goodbye)?;
    interpreter.is_running = false;
    Ok(())
}

/// Broadcast a single line message to a range of nodes.
/// # Arguments
///  * `lonode` - The low node number to which the message should be broadcast.
///  * `hinode` - The high node number to which the message should be broadcast.
///  * `message` - The message text which should be broadcast to the specified nodes.
/// # Remarks
/// This statement allows you to programatically broadcast a message to a range of nodes
/// without giving users the ability to manually broadcast
/// at any time they choose.
pub fn broadcast(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let lonode = evaluate_exp(interpreter, &params[0])?.as_int();
    let hinode = evaluate_exp(interpreter, &params[1])?.as_int();
    let message = evaluate_exp(interpreter, &params[2])?.as_string();
    // TODO: Broadcast
    println!("Broadcasting message from {lonode} to {hinode}: {message}");
    Ok(())
}

pub fn waitfor(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdchkon(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdchkoff(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn optext(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dispstr(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let value = evaluate_exp(interpreter, &params[0])?.as_string();
    interpreter.ctx.print(TerminalTarget::Both, &value)
}

pub fn rdunet(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let value = evaluate_exp(interpreter, &params[0])?.as_int();

    if let Some(node) = interpreter.icy_board_data.nodes.get(value as usize) {
        interpreter.pcb_node = Some(node.clone());
    }
    Ok(())
}

pub fn wrunet(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let node = evaluate_exp(interpreter, &params[0])?.as_int();
    let stat = evaluate_exp(interpreter, &params[1])?.as_string();
    let name = evaluate_exp(interpreter, &params[2])?.as_string();
    let city = evaluate_exp(interpreter, &params[3])?.as_string();
    let operation = evaluate_exp(interpreter, &params[4])?.as_string();
    let broadcast = evaluate_exp(interpreter, &params[5])?.as_string();

    // Todo: Broadcast

    if !stat.is_empty() {
        interpreter.icy_board_data.nodes[node as usize].status = stat.as_bytes()[0] as char;
    }
    interpreter.icy_board_data.nodes[node as usize].name = name;
    interpreter.icy_board_data.nodes[node as usize].city = city;
    interpreter.icy_board_data.nodes[node as usize].operation = operation;

    Ok(())
}

pub fn dointr(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn varseg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn varoff(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pokeb(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pokew(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn varaddr(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn ansipos(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let x = evaluate_exp(interpreter, &params[0])?.as_int();
    let y = evaluate_exp(interpreter, &params[1])?.as_int();

    interpreter.ctx.gotoxy(TerminalTarget::Both, x, y)
}

pub fn backup(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn forward(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn freshline(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn wrusys(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn rdusys(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn newpwd(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn opencap(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn closecap(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn message(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn savescrn(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn restscrn(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn sound(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn chat(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn sprint(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    for expr in params {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter
            .ctx
            .print(TerminalTarget::Sysop, &value.as_string())?;
    }
    Ok(())
}

pub fn sprintln(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    for expr in params {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter
            .ctx
            .print(TerminalTarget::Sysop, &value.as_string())?;
    }
    interpreter.ctx.print(TerminalTarget::Sysop, "\n")?;
    Ok(())
}

pub fn mprint(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    for expr in params {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter
            .ctx
            .print(TerminalTarget::User, &value.as_string())?;
    }
    Ok(())
}

pub fn mprintln(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    for expr in params {
        let value = evaluate_exp(interpreter, expr)?;
        interpreter
            .ctx
            .print(TerminalTarget::User, &value.as_string())?;
    }
    interpreter.ctx.print(TerminalTarget::User, "\n")?;
    Ok(())
}

pub fn rename(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let old = &evaluate_exp(interpreter, &params[0])?.as_string();
    let new = &evaluate_exp(interpreter, &params[1])?.as_string();
    if let Err(err) = interpreter.io.rename(old, new) {
        log::error!("Error renaming file: {}", err);
    }
    Ok(())
}
pub fn frewind(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pokedw(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dbglevel(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn showon(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn showoff(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pageon(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn pageoff(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fseek(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fflush(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fread(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fwrite(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdefin(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdefout(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdget(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdput(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdputln(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdputpad(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdread(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdwrite(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adjbytes(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdstring(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn alias(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn redim(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn append(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn copy(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let old = &evaluate_exp(interpreter, &params[0])?.as_string();
    let new = &evaluate_exp(interpreter, &params[1])?.as_string();
    if let Err(err) = interpreter.io.copy(old, new) {
        log::error!("Error renaming file: {}", err);
    }
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn kbdflush(interpreter: &VirtualMachine, params: &[Expression]) {
    // TODO?
}

/// # Errors
/// Errors if the variable is not found.
pub fn mdmflush(interpreter: &VirtualMachine, params: &[Expression]) {
    // TODO?
}

/// # Errors
/// Errors if the variable is not found.
pub fn keyflush(interpreter: &VirtualMachine, params: &[Expression]) {
    // TODO?
}
pub fn lastin(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn flag(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn download(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn wrusysdoor(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn getaltuser(interpreter: &mut VirtualMachine, params: &[Expression]) -> Res<()> {
    let user_record = evaluate_exp(interpreter, &params[0])?.as_int() as usize;
    let user = interpreter.icy_board_data.users[user_record].clone();
    interpreter.set_user_variables(&user);
    Ok(())
}

pub fn adjdbytes(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtbytes(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtfiles(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn lang(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn sort(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn mousereg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn scrfile(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn searchinit(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn searchfind(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn searchstop(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn prfound(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn prfoundln(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpaget(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpaput(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacget(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacput(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tparead(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpawrite(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacread(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacwrite(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn bitset(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn bitclear(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn brag(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn frealtuser(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn setlmr(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn setenv(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fcloseall(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn stackabort(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dcreate(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dopen(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dclose(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dsetalias(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dpack(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dcloseall(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dlock(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dlockr(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dlockg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dunlock(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dncreate(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dnopen(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dnclose(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dncloseall(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dnew(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dadd(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dappend(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dtop(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dgo(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dbottom(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dskip(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dblank(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn ddelete(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn drecall(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dtag(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dseek(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dfblank(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dget(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dput(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn dfcopy(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}

pub fn eval(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn account(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn recordusage(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn msgtofile(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn qwklimits(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn command(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn uselmrs(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn confinfo(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtubytes(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn grafmode(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn adduser(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn killmsg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn chdir(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn mkdir(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn redir(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdowraka(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoaddaka(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdowrorg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoaddorg(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqmod(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqadd(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqdel(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
pub fn sounddelay(interpreter: &VirtualMachine, params: &[Expression]) -> Res<()> {
    panic!("TODO")
}
