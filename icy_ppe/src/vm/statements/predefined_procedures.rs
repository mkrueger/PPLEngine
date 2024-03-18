use std::{fs, thread, time::Duration};

use crate::{
    ast::Variable,
    icy_board::text_messages,
    vm::{TerminalTarget, VirtualMachine},
    Res,
};

use super::super::errors::IcyError;

/// Should never be called. But some op codes are invalid as statement call (like if or return)
/// and are handled by it's own `PPECommands` and will point to this function.
///
/// # Panics
///
/// Always
pub fn invalid(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("Invalid statement");
}

pub fn cls(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    vm.ctx.print(TerminalTarget::Both, "\x1B[2J")
}

pub fn clreol(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    vm.ctx.print(TerminalTarget::Both, "\x1B[K")
}

pub fn more(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.print(
        TerminalTarget::Both,
        &vm.icy_board_data
            .display_text
            .get_display_text(text_messages::MOREPROMPT)?,
    )?;
    loop {
        if let Some(ch) = vm.ctx.get_char()? {
            let ch = ch.to_uppercase().to_string();

            if ch == vm.icy_board_data.yes_char.to_string()
                || ch == vm.icy_board_data.no_char.to_string()
            {
                break;
            }
        }
    }
    Ok(())
}

pub fn wait(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.print(
        TerminalTarget::Both,
        &vm.icy_board_data
            .display_text
            .get_display_text(text_messages::PRESSENTER)?,
    )?;
    loop {
        if let Some(ch) = vm.ctx.get_char()? {
            if ch == '\n' || ch == '\r' {
                break;
            }
        }
    }
    Ok(())
}

pub fn color(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let color = params[0].as_int();
    vm.ctx.set_color(color as u8);
    Ok(())
}

pub fn confflag(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn confunflag(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

/// # Errors
/// Errors if
pub fn dispfile(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let file = vm.io.resolve_file(&params[0].as_string());

    let content = fs::read(&file);
    match content {
        Ok(content) => vm.ctx.write_raw(TerminalTarget::Both, &content),
        Err(err) => vm
            .ctx
            .print(TerminalTarget::Both, format!("{file} error {err}").as_str()),
    }
}

pub fn input(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fcreate(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;
    let file = params[1].as_string();
    let am = params[2].as_int();
    let sm = params[3].as_int();
    vm.io.fcreate(channel, &file, am, sm);
    Ok(())
}

pub fn fopen(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;
    let file = params[1].as_string();
    let am = params[2].as_int();
    let sm = params[3].as_int();
    vm.io.fopen(channel, &file, am, sm)?;
    Ok(())
}

pub fn fappend(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;
    let file = params[1].as_string();
    let am = params[2].as_int();
    let sm = params[3].as_int();
    vm.io.fappend(channel, &file, am, sm);
    Ok(())
}

pub fn fclose(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int();
    if channel == -1 {
        // READLINE uses -1 as a special value
        return Ok(());
    }
    if !(0..=7).contains(&channel) {
        return Err(Box::new(IcyError::FileChannelOutOfBounds(channel)));
    }
    vm.io.fclose(channel as usize);
    Ok(())
}

pub fn fget(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;
    params[1] = Variable::new_string(vm.io.fget(channel));
    Ok(())
}

pub fn fput(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;

    for value in &params[1..] {
        vm.io.fput(channel, value.as_string());
    }
    Ok(())
}

pub fn fputln(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let channel = params[0].as_int() as usize;

    for value in &params[1..] {
        vm.io.fput(channel, value.as_string());
    }
    vm.io.fput(channel, "\n".to_string());
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn resetdisp(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // TODO?: unused
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn startdisp(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // TODO?: unused
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn fputpad(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

/// # Errors
/// Errors if the variable is not found.
pub fn hangup(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.hangup(crate::vm::HangupType::Hangup)?;
    vm.is_running = false;
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn getuser(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    let user = if let Some(user) = vm.icy_board_data.users.get(vm.cur_user) {
        user.clone()
    } else {
        return Err(Box::new(IcyError::UserNotFound(vm.cur_user.to_string())));
    };

    vm.set_user_variables(&user);
    vm.current_user = Some(user);
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn putuser(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    if let Some(user) = &vm.current_user {
        vm.icy_board_data.users[vm.cur_user] = user.clone();
        Ok(())
    } else {
        Err(Box::new(IcyError::UserNotSet))
    }
}

pub fn defcolor(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn delete(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let file = &params[0].as_string();
    if let Err(err) = vm.io.delete(file) {
        log::error!("Error deleting file: {}", err);
    }
    Ok(())
}

pub fn deluser(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtime(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn log(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let msg = params[0].as_string();
    // let left = &params[0];
    log::info!("{}", msg);
    Ok(())
}

const TXT_STOPCHAR: char = '_';

pub fn inputstr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let prompt = params[0].as_string();
    // 1 Output Variable
    let color = params[2].as_int();
    let len = params[3].as_int();
    let valid = params[4].as_string();
    let flags = params[5].as_int();
    let output = internal_input_string(vm, color, prompt, len, &valid)?;
    params[1] = Variable::new_string(output);
    Ok(())
}

fn internal_input_string(
    vm: &mut VirtualMachine,
    color: i32,
    prompt: String,
    len: i32,
    valid: &str,
) -> Res<String> {
    let mut prompt = prompt;
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }
    vm.ctx.set_color(color as u8);
    vm.ctx.print(TerminalTarget::Both, &prompt)?;
    let mut output = String::new();
    loop {
        let Some(ch) = vm.ctx.get_char()? else {
            continue;
        };
        if ch == '\n' || ch == '\r' {
            break;
        }
        if ch == '\x08' && !output.is_empty() {
            output.pop();
            vm.ctx.print(TerminalTarget::Both, "\x08 \x08")?;
            continue;
        }

        if (output.len() as i32) < len && valid.contains(ch) {
            output.push(ch);
            vm.ctx.print(TerminalTarget::Both, &ch.to_string())?;
        }
    }
    Ok(output)
}

pub fn inputyn(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 1;
    let valid = "YyNn";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    params[1] = Variable::new_string(output.to_ascii_uppercase());
    Ok(())
}

pub fn inputmoney(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 13;
    let valid = "01234567890+-$.";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    // TODO: Money conversion.
    params[1] = Variable::new_string(output);
    Ok(())
}

pub fn inputint(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 11;
    let valid = "01234567890+-";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    params[1] = Variable::new_int(output.parse::<i32>().unwrap());
    Ok(())
}
pub fn inputcc(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 16;
    let valid = "01234567890";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    params[1] = Variable::new_string(output);
    Ok(())
}
pub fn inputdate(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 8;
    let valid = "01234567890-/";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    // TODO: Date conversion
    params[1] = Variable::new_string(output);
    Ok(())
}

pub fn inputtime(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let mut prompt = params[0].as_string();
    if prompt.ends_with(TXT_STOPCHAR) {
        prompt.pop();
    }

    let color = params[2].as_int();
    let len = 8;
    let valid = "01234567890:";
    let output = internal_input_string(vm, color, prompt, len, valid)?;
    // TODO: Time conversion
    params[1] = Variable::new_string(output);
    Ok(())
}
pub fn promptstr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dtron(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // IGNORE
    Ok(())
}

pub fn dtroff(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.hangup(crate::vm::HangupType::Hangup)?;
    Ok(())
}

pub fn cdchkon(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn cdchkoff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn delay(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // 1 tick is ~1/18.2s
    let ticks = params[0].as_int();
    if ticks > 0 {
        thread::sleep(Duration::from_millis((ticks as f32 * 1000.0 / 18.2) as u64));
    }
    Ok(())
}

pub fn sendmodem(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn inc(_vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let new_value = params[0].clone() + Variable::new_int(1);
    params[0] = new_value;
    Ok(())
}

pub fn dec(_vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let new_value = params[0].clone() - Variable::new_int(1);
    params[0] = new_value;
    Ok(())
}

pub fn newline(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.write_raw(TerminalTarget::Both, &[b'\n'])
}

pub fn newlines(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let count = params[0].as_int();
    for _ in 0..count {
        newline(vm, params)?;
    }
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn tokenize(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let str = params[0].to_string();
    let split = str
        .split(&[' ', ';'][..])
        .map(std::string::ToString::to_string);
    vm.cur_tokens = split.collect();
    Ok(())
}

pub fn gettoken(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn shell(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn disptext(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn stop(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn inputtext(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn beep(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn push(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pop(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdstuff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let value = params[0].as_string();
    vm.ctx.print(TerminalTarget::Both, &value)
}
pub fn call(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn join(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn quest(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn blt(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dir(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdfile(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn bye(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.hangup(crate::vm::HangupType::Bye)?;
    vm.is_running = false;
    Ok(())
}

pub fn goodbye(vm: &mut VirtualMachine, _params: &mut [Variable]) -> Res<()> {
    vm.ctx.hangup(crate::vm::HangupType::Goodbye)?;
    vm.is_running = false;
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
pub fn broadcast(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let lonode = params[0].as_int();
    let hinode = params[1].as_int();
    let message = params[2].as_string();
    // TODO: Broadcast
    println!("Broadcasting message from {lonode} to {hinode}: {message}");
    Ok(())
}

pub fn waitfor(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdchkon(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdchkoff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn optext(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dispstr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let value = params[0].as_string();
    crossterm::terminal::disable_raw_mode().unwrap();
    vm.ctx.print(TerminalTarget::Both, &value)
}

pub fn rdunet(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let value = params[0].as_int();

    if let Some(node) = vm.icy_board_data.nodes.get(value as usize) {
        vm.pcb_node = Some(node.clone());
    }
    Ok(())
}

pub fn wrunet(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let node = params[0].as_int();
    let stat = params[1].as_string();
    let name = params[2].as_string();
    let city = params[3].as_string();
    let operation = params[4].as_string();
    let broadcast = params[5].as_string();

    // Todo: Broadcast

    if !stat.is_empty() {
        vm.icy_board_data.nodes[node as usize].status = stat.as_bytes()[0] as char;
    }
    vm.icy_board_data.nodes[node as usize].name = name;
    vm.icy_board_data.nodes[node as usize].city = city;
    vm.icy_board_data.nodes[node as usize].operation = operation;

    Ok(())
}

pub fn dointr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn varseg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn varoff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pokeb(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pokew(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn varaddr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn ansipos(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let x = params[0].as_int();
    let y = params[1].as_int();

    vm.ctx.gotoxy(TerminalTarget::Both, x, y)
}

pub fn backup(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn forward(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn freshline(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn wrusys(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn rdusys(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn newpwd(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn opencap(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn closecap(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn message(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn savescrn(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn restscrn(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn sound(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn chat(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn sprint(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::Sysop, &value.as_string())?;
    }
    Ok(())
}

pub fn sprintln(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::Sysop, &value.as_string())?;
    }
    vm.ctx.print(TerminalTarget::Sysop, "\n")?;
    Ok(())
}

pub fn print(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::Both, &value.as_string())?;
    }
    Ok(())
}

pub fn println(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::Both, &value.as_string())?;
    }
    vm.ctx.print(TerminalTarget::User, "\n")?;
    Ok(())
}

pub fn mprint(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::User, &value.as_string())?;
    }
    Ok(())
}

pub fn mprintln(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    for value in params {
        vm.ctx.print(TerminalTarget::User, &value.as_string())?;
    }
    vm.ctx.print(TerminalTarget::User, "\n")?;
    Ok(())
}

pub fn rename(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let old = &params[0].as_string();
    let new = &params[1].as_string();
    if let Err(err) = vm.io.rename(old, new) {
        log::error!("Error renaming file: {}", err);
    }
    Ok(())
}
pub fn frewind(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pokedw(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dbglevel(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn showon(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn showoff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pageon(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn pageoff(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fseek(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fflush(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fread(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fwrite(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdefin(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdefout(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdget(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdput(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdputln(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdputpad(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdread(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdwrite(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn adjbytes(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn kbdstring(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn alias(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn redim(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn append(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn copy(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let old = &params[0].as_string();
    let new = &params[1].as_string();
    if let Err(err) = vm.io.copy(old, new) {
        log::error!("Error renaming file: {}", err);
    }
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn kbdflush(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // TODO?
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn mdmflush(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // TODO?
    Ok(())
}

/// # Errors
/// Errors if the variable is not found.
pub fn keyflush(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    // TODO?
    Ok(())
}
pub fn lastin(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn flag(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn download(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn wrusysdoor(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn getaltuser(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    let user_record = params[0].as_int() as usize;
    let user = vm.icy_board_data.users[user_record].clone();
    vm.set_user_variables(&user);
    Ok(())
}

pub fn adjdbytes(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtbytes(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn ayjtfiles(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn lang(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn sort(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn mousereg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn scrfile(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn searchinit(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn searchfind(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn searchstop(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn prfound(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn prfoundln(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpaget(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpaput(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacgea(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacput(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tparead(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpawrite(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacread(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn tpacwrite(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn bitset(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn bitclear(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn brag(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn frealtuser(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn setlmr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn setenv(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fcloseall(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn stackabort(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dcreate(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dopen(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dclose(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dsetalias(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dpack(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dcloseall(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dlock(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dlockr(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dlockg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dunlock(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dncreate(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dnopen(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dnclose(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dncloseall(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dnew(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dadd(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dappend(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dtop(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dgo(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dbottom(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dskip(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dblank(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn ddelete(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn drecall(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dtag(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dseek(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dfblank(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dget(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dput(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn dfcopy(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}

pub fn eval(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn account(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn recordusage(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn msgtofile(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn qwklimits(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn command(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn uselmrs(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn confinfo(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn adjtubytes(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn grafmode(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn adduser(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn killmsg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn chdir(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn mkdir(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn redir(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdowraka(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoaddaka(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdowrorg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoaddorg(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqmod(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqadd(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn fdoqdel(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
pub fn sounddelay(vm: &mut VirtualMachine, params: &mut [Variable]) -> Res<()> {
    panic!("TODO")
}
