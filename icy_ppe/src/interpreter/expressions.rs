use std::collections::HashMap;

use crate::{
    ast::{convert_to, BinOp, Expression, Implementations, UnaryOp, Variable, VariableValue},
    interpreter::{calc_table, errors::IcyError, execute_statement, StackFrame},
    tables::{FuncOpCode, FunctionDefinition},
    Res,
};

use super::Interpreter;

/// .
///
/// # Examples
///
/// ```
/// use icy_ppe::interpreter::expressions::evaluate_exp;
///
/// ```
///
/// # Panics
///
/// Panics if .
/// # Errors
pub fn evaluate_exp(interpreter: &mut Interpreter, expr: &Expression) -> Res<Variable> {
    match expr {
        Expression::Identifier(identifier_expr) => {
            let id = identifier_expr.get_identifier();
            for frame in &interpreter.cur_frame.last().unwrap().values {
                if frame.0 == id {
                    return Ok(frame.1.clone());
                }
            }

            if interpreter.cur_frame.len() > 1 {
                for frame in &interpreter.cur_frame[0].values {
                    if frame.0 == id {
                        return Ok(frame.1.clone());
                    }
                }
            }
            Err(Box::new(IcyError::VariableNotFound(
                identifier_expr.get_identifier().to_string(),
            )))
        }
        Expression::Const(constant) => Ok(constant.get_constant_value().get_value()),
        Expression::Parens(expr) => evaluate_exp(interpreter, expr.get_expression()),
        
        Expression::PredefinedFunctionCall(expr) => {
            // TODO: Check parameter signature
            return call_function(
                interpreter,
                expr.get_func(),
                expr.get_arguments(),
            );
        }

        Expression::FunctionCall(expr) => {
            let func_id = expr.get_identifier();

            for imp in &interpreter.prg.implementations {
                let Implementations::Function(f) = imp else {
                    continue;
                };

                if func_id != f.get_identifier() {
                    continue;
                }
                let label_table = calc_table(f.get_statements());

                let mut prg_frame = StackFrame {
                    values: HashMap::new(),
                    gosub_stack: vec![],
                    cur_ptr: 0,
                    label_table,
                };

                // insert empty return value!
                prg_frame
                    .values
                    .insert(func_id.clone(), f.get_return_type().create_empty_value());
                for (i, param) in f.get_parameters().iter().enumerate() {
                    let value = evaluate_exp(interpreter, &expr.get_arguments()[i])?;
                    prg_frame.values.insert(
                        param.get_variable().get_identifier().clone(),
                        convert_to(param.get_variable_type(), &value),
                    );
                }

                interpreter.cur_frame.push(prg_frame);

                while interpreter.cur_frame.last().unwrap().cur_ptr < f.get_statements().len() {
                    let stmt = &f.get_statements()[interpreter.cur_frame.last().unwrap().cur_ptr];
                    execute_statement(interpreter, stmt)?;
                    interpreter.cur_frame.last_mut().unwrap().cur_ptr += 1;
                }
                let prg_frame = interpreter.cur_frame.pop().unwrap();

                if let Some(val) = prg_frame.values.get(func_id) {
                    return Ok(val.clone());
                }
                panic!("function didn't return a value  {}", expr.get_identifier());
            }
            let first_arg_expr = &evaluate_exp(interpreter, &expr.get_arguments()[0])?.clone();

            let var_value = interpreter.get_variable_mut(expr.get_identifier());
            if let Some(var_value) = var_value {
                match expr.get_arguments().len() {
                    1 => {
                        let vector = first_arg_expr.as_int();
                        if let VariableValue::Dim1(data) = &mut var_value.generic_data {
                            if vector < 0 || vector >= data.len() as i32 {
                                return Ok(var_value.get_type().create_empty_value());
                            }
                            return Ok(data[vector as usize].clone());
                        }
                        panic!("unsupported for {expr:?} value {var_value:?}");
                    }
                    2 => {
                        let vector = first_arg_expr.as_int();
                        let matrix = first_arg_expr.as_int();
                        if let VariableValue::Dim2(data) = &mut var_value.generic_data {
                            if vector < 0
                                || vector >= data.len() as i32
                                || matrix < 0
                                || matrix >= data[0].len() as i32
                            {
                                return Ok(var_value.get_type().create_empty_value());
                            }
                            return Ok(data[vector as usize][matrix as usize].clone());
                        }
                        panic!("unsupported for {expr:?} value {var_value:?}");
                    }
                    3 => {
                        let vector = first_arg_expr.as_int();
                        let matrix = first_arg_expr.as_int();
                        let cube = first_arg_expr.as_int();
                        if let VariableValue::Dim3(data) = &mut var_value.generic_data {
                            if vector < 0
                                || vector >= data.len() as i32
                                || matrix < 0
                                || matrix >= data[0].len() as i32
                                || cube < 0
                                || cube >= data[0][0].len() as i32
                            {
                                return Ok(var_value.get_type().create_empty_value());
                            }
                            return Ok(
                                data[vector as usize][matrix as usize][cube as usize].clone()
                            );
                        }
                        panic!("unsupported for {expr:?} value {var_value:?}");
                    }
                    len => {
                        panic!("unsupported parameter length {len}");
                    }
                }
            }
            panic!("function/array not found {}", expr.get_identifier());

            /*
            if parameters.len() == 2 {
                if let VariableValue::Dim2(t, data) = var_value {
                    return data[get_int(&evaluate_exp(interpreter,&parameters[0]))][get_int(&evaluate_exp(interpreter,&parameters[1]))];
                }
            }
            if parameters.len() == 3 {
                if let VariableValue::Dim3(t, data) = var_value {
                    return data[get_int(&evaluate_exp(interpreter,&parameters[0]))][get_int(&evaluate_exp(interpreter,&parameters[1]))][get_int(&evaluate_exp(interpreter,&parameters[2]))];
                }
            }*/
        }
        Expression::Unary(expr) => match expr.get_op() {
            UnaryOp::Plus => evaluate_exp(interpreter, expr.get_expression()),
            UnaryOp::Not => Ok(evaluate_exp(interpreter, expr.get_expression())?.not()),
            UnaryOp::Minus => Ok(-evaluate_exp(interpreter, expr.get_expression())?),
        },

        Expression::Binary(expr) => match expr.get_op() {
            BinOp::Add => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                + evaluate_exp(interpreter, expr.get_right_expression())?),
            BinOp::Sub => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                - evaluate_exp(interpreter, expr.get_right_expression())?),
            BinOp::Mul => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                * evaluate_exp(interpreter, expr.get_right_expression())?),
            BinOp::Div => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                / evaluate_exp(interpreter, expr.get_right_expression())?),
            BinOp::Mod => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                % evaluate_exp(interpreter, expr.get_right_expression())?),
            BinOp::PoW => Ok(evaluate_exp(interpreter, expr.get_left_expression())?
                .pow(evaluate_exp(interpreter, expr.get_right_expression())?)),
            BinOp::Eq => {
                let l = evaluate_exp(interpreter, expr.get_left_expression())?;
                let r = evaluate_exp(interpreter, expr.get_right_expression())?;
                Ok(Variable::new_bool(l == r))
            }
            BinOp::NotEq => {
                let l = evaluate_exp(interpreter, expr.get_left_expression())?;
                let r = evaluate_exp(interpreter, expr.get_right_expression())?;
                Ok(Variable::new_bool(l != r))
            }
            BinOp::Or => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?.as_bool()
                    || evaluate_exp(interpreter, expr.get_right_expression())?.as_bool(),
            )),
            BinOp::And => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?.as_bool()
                    && evaluate_exp(interpreter, expr.get_right_expression())?.as_bool(),
            )),
            BinOp::Lower => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?
                    < evaluate_exp(interpreter, expr.get_right_expression())?,
            )),
            BinOp::LowerEq => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?
                    <= evaluate_exp(interpreter, expr.get_right_expression())?,
            )),
            BinOp::Greater => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?
                    > evaluate_exp(interpreter, expr.get_right_expression())?,
            )),
            BinOp::GreaterEq => Ok(Variable::new_bool(
                evaluate_exp(interpreter, expr.get_left_expression())?
                    >= evaluate_exp(interpreter, expr.get_right_expression())?,
            )),
        },
    }
}

fn call_function(
    interpreter: &mut Interpreter,
    func_def: &'static FunctionDefinition,
    params: &[Expression],
) -> Res<Variable> {
    Ok(match func_def.opcode {
        FuncOpCode::LEN => predefined_functions::len(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::LOWER => predefined_functions::lower(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::UPPER => predefined_functions::upper(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::MID => predefined_functions::mid(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
            evaluate_exp(interpreter, &params[2])?,
        )?,
        FuncOpCode::LEFT => predefined_functions::left(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        )?,
        FuncOpCode::RIGHT => predefined_functions::right(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        )?,
        FuncOpCode::SPACE => predefined_functions::space(evaluate_exp(interpreter, &params[0])?)?,
        FuncOpCode::FERR => {
            let a = evaluate_exp(interpreter, &params[0])?;
            predefined_functions::ferr(interpreter, a)?
        }
        FuncOpCode::CHR => predefined_functions::chr(evaluate_exp(interpreter, &params[0])?)?,
        FuncOpCode::ASC => predefined_functions::asc(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::INSTR => predefined_functions::instr(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::ABORT => predefined_functions::abort(interpreter),
        FuncOpCode::LTRIM => predefined_functions::ltrim(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::REPLACE => predefined_functions::replace(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
            evaluate_exp(interpreter, &params[2])?,
        ),
        FuncOpCode::STRIP => predefined_functions::strip(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::REPLACESTR => predefined_functions::replace_string(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
            evaluate_exp(interpreter, &params[2])?,
        ),
        FuncOpCode::STRIPSTR => predefined_functions::strip_string(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::RTRIM => predefined_functions::rtrim(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::TRIM => predefined_functions::trim(
            evaluate_exp(interpreter, &params[0])?,
            evaluate_exp(interpreter, &params[1])?,
        ),
        FuncOpCode::RANDOM => predefined_functions::random(evaluate_exp(interpreter, &params[0])?)?,
        FuncOpCode::DATE => predefined_functions::date(),
        FuncOpCode::TIME => predefined_functions::time(),

        FuncOpCode::U_NAME => predefined_functions::u_name(interpreter),
        FuncOpCode::U_LDATE => predefined_functions::u_ldate(interpreter),
        FuncOpCode::U_LTIME => predefined_functions::u_ltime(interpreter),
        FuncOpCode::U_LDIR => predefined_functions::u_ldir(interpreter),
        FuncOpCode::U_LMR => predefined_functions::u_lmr(interpreter),
        FuncOpCode::U_LOGONS => predefined_functions::u_logons(interpreter),
        FuncOpCode::U_FUL => predefined_functions::u_ful(interpreter),
        FuncOpCode::U_FDL => predefined_functions::u_fdl(interpreter),
        FuncOpCode::U_BDLDAY => predefined_functions::u_bdlday(interpreter),
        FuncOpCode::U_TIMEON => predefined_functions::u_timeon(interpreter),
        FuncOpCode::U_BDL => predefined_functions::u_bdl(interpreter),
        FuncOpCode::U_BUL => predefined_functions::u_bul(interpreter),
        FuncOpCode::U_MSGRD => predefined_functions::u_msgrd(interpreter),
        FuncOpCode::U_MSGWR => predefined_functions::u_msgwr(interpreter),

        FuncOpCode::YEAR => predefined_functions::year(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::MONTH => predefined_functions::month(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DAY => predefined_functions::day(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DOW => predefined_functions::dow(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::HOUR => predefined_functions::hour(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::MIN => predefined_functions::min(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::SEC => predefined_functions::sec(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TIMEAP => predefined_functions::timeap(evaluate_exp(interpreter, &params[0])?),

        FuncOpCode::VER => predefined_functions::ver(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::NOCHAR => predefined_functions::nochar(interpreter),
        FuncOpCode::YESCHAR => predefined_functions::yeschar(interpreter),
        FuncOpCode::STRIPATX => {
            predefined_functions::strip_atx(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::INKEY => predefined_functions::inkey(interpreter)?,
        FuncOpCode::TOSTRING => {
            predefined_functions::tostring(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::MASK_PWD => predefined_functions::mask_pwd(),
        FuncOpCode::MASK_ALPHA => predefined_functions::mask_alpha(),
        FuncOpCode::MASK_NUM => predefined_functions::mask_num(),
        FuncOpCode::MASK_ALNUM => predefined_functions::mask_alnum(),
        FuncOpCode::MASK_FILE => predefined_functions::mask_file(),
        FuncOpCode::MASK_PATH => predefined_functions::mask_path(),
        FuncOpCode::MASK_ASCII => predefined_functions::mask_ascii(),

        FuncOpCode::CURCONF => {
            predefined_functions::curconf(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PCBDAT => predefined_functions::pcbdat(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::PPEPATH => predefined_functions::ppepath(interpreter),
        FuncOpCode::VALDATE => {
            predefined_functions::valdate(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::VALTIME => {
            predefined_functions::valtime(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PCBNODE => predefined_functions::pcbnode(interpreter),
        FuncOpCode::READLINE => {
            let file = evaluate_exp(interpreter, &params[0])?;
            let line = evaluate_exp(interpreter, &params[1])?;
            predefined_functions::readline(interpreter, file, line)?
        }
        FuncOpCode::SYSOPSEC => predefined_functions::sysopsec(interpreter),
        FuncOpCode::ONLOCAL => predefined_functions::onlocal(interpreter),
        FuncOpCode::UN_STAT => predefined_functions::un_stat(interpreter),
        FuncOpCode::UN_NAME => predefined_functions::un_name(interpreter),
        FuncOpCode::UN_CITY => predefined_functions::un_city(interpreter),
        FuncOpCode::UN_OPER => predefined_functions::un_oper(interpreter),
        FuncOpCode::CURSEC => predefined_functions::cursec(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::GETTOKEN => predefined_functions::gettoken(interpreter),
        FuncOpCode::MINLEFT => {
            predefined_functions::minleft(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::MINON => predefined_functions::minon(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::GETENV => predefined_functions::getenv(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::CALLID => predefined_functions::callid(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGAL => predefined_functions::regal(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGAH => predefined_functions::regah(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGBL => predefined_functions::regbl(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGBH => predefined_functions::regbh(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGCL => predefined_functions::regcl(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGCH => predefined_functions::regch(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGDL => predefined_functions::regdl(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGDH => predefined_functions::regdh(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGAX => predefined_functions::regax(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGBX => predefined_functions::regbx(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGCX => predefined_functions::regcx(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGDX => predefined_functions::regdx(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGSI => predefined_functions::regsi(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGDI => predefined_functions::regdi(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGF => predefined_functions::regf(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGCF => predefined_functions::regcf(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGDS => predefined_functions::regds(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::REGES => predefined_functions::reges(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::B2W => predefined_functions::b2w(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::PEEKB => predefined_functions::peekb(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::PEEKW => predefined_functions::peekw(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::MKADDR => predefined_functions::mkaddr(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::EXIST => {
            let file_name = evaluate_exp(interpreter, &params[0])?.as_string();
            predefined_functions::exist(interpreter, file_name.as_str())
        }
        FuncOpCode::I2S => {
            let int = evaluate_exp(interpreter, &params[0])?.as_int();
            let base = evaluate_exp(interpreter, &params[1])?.as_int();
            predefined_functions::i2s(int, base)
        }
        FuncOpCode::S2I => {
            let s = evaluate_exp(interpreter, &params[0])?.as_string();
            let base = evaluate_exp(interpreter, &params[1])?.as_int();
            predefined_functions::s2i(&s, base)?
        }
        FuncOpCode::CARRIER => predefined_functions::carrier(interpreter),
        FuncOpCode::TOKENSTR => {
            predefined_functions::tokenstr(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CDON => predefined_functions::cdon(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::LANGEXT => {
            predefined_functions::langext(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ANSION => predefined_functions::ansion(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::VALCC => predefined_functions::valcc(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::FMTCC => predefined_functions::fmtcc(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::CCTYPE => predefined_functions::cctype(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::GETX => predefined_functions::getx(interpreter),
        FuncOpCode::GETY => predefined_functions::gety(interpreter),
        FuncOpCode::BAND => predefined_functions::band(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::BOR => predefined_functions::bor(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::BXOR => predefined_functions::bxor(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::BNOT => predefined_functions::bnot(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::U_PWDHIST => {
            predefined_functions::u_pwdhist(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::U_PWDLC => {
            predefined_functions::u_pwdlc(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::U_PWDTC => {
            predefined_functions::u_pwdtc(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::U_STAT => predefined_functions::u_stat(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DEFCOLOR => {
            predefined_functions::defcolor(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ABS => evaluate_exp(interpreter, &params[0])?.abs(),
        FuncOpCode::GRAFMODE => {
            predefined_functions::grafmode(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PSA => predefined_functions::psa(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::FILEINF => {
            let file = evaluate_exp(interpreter, &params[0])?.as_string();
            let item = evaluate_exp(interpreter, &params[1])?.as_int();
            predefined_functions::fileinf(interpreter, &file, item)?
        }
        FuncOpCode::PPENAME => predefined_functions::ppename(interpreter),
        FuncOpCode::MKDATE => predefined_functions::mkdate(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::CURCOLOR => {
            predefined_functions::curcolor(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::KINKEY => predefined_functions::kinkey(interpreter)?,
        FuncOpCode::MINKEY => predefined_functions::minkey(interpreter)?,
        FuncOpCode::MAXNODE => predefined_functions::maxnode(interpreter),
        FuncOpCode::SLPATH => predefined_functions::slpath(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::HELPPATH => {
            predefined_functions::helppath(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TEMPPATH => {
            predefined_functions::temppath(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::MODEM => predefined_functions::modem(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::LOGGEDON => {
            predefined_functions::loggedon(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CALLNUM => {
            predefined_functions::callnum(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::MGETBYTE => {
            predefined_functions::mgetbyte(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOKCOUNT => predefined_functions::tokcount(interpreter),
        FuncOpCode::U_RECNUM => {
            let user_name = evaluate_exp(interpreter, &params[0])?;
            predefined_functions::u_recnum(interpreter, user_name)
        }
        FuncOpCode::U_INCONF => {
            predefined_functions::u_inconf(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PEEKDW => predefined_functions::peekdw(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DBGLEVEL => {
            predefined_functions::dbglevel(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::SCRTEXT => {
            predefined_functions::scrtext(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::SHOWSTAT => {
            predefined_functions::showstat(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PAGESTAT => {
            predefined_functions::pagestat(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOBIGSTR => {
            predefined_functions::tobigstr(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOBOOLEAN => {
            predefined_functions::toboolean(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOBYTE => predefined_functions::tobyte(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TODATE => predefined_functions::todate(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TODREAL => {
            predefined_functions::todreal(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOEDATE => {
            predefined_functions::toedate(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOINTEGER => {
            predefined_functions::tointeger(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOMONEY => {
            predefined_functions::tomoney(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOREAL => predefined_functions::toreal(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TOSBYTE => {
            predefined_functions::tosbyte(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOSWORD => {
            predefined_functions::tosword(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOTIME => predefined_functions::totime(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TOUNSIGNED => {
            predefined_functions::tounsigned(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TOWORD => predefined_functions::toword(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::MIXED => predefined_functions::mixed(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::ALIAS => predefined_functions::alias(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::CONFREG => {
            predefined_functions::confreg(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFEXP => {
            predefined_functions::confexp(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFSEL => {
            predefined_functions::confsel(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFSYS => {
            predefined_functions::confsys(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFMW => predefined_functions::confmw(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::LPRINTED => {
            predefined_functions::lprinted(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ISNONSTOP => {
            predefined_functions::isnonstop(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ERRCORRECT => {
            predefined_functions::errcorrect(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFALIAS => {
            predefined_functions::confalias(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::USERALIAS => {
            predefined_functions::useralias(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CURUSER => {
            predefined_functions::curuser(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CHATSTAT => {
            predefined_functions::chatstat(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DEFANS => predefined_functions::defans(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::LASTANS => {
            predefined_functions::lastans(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::MEGANUM => {
            predefined_functions::meganum(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::EVTTIMEADJ => {
            predefined_functions::evttimeadj(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ISBITSET => {
            predefined_functions::isbitset(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FMTREAL => {
            predefined_functions::fmtreal(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FLAGCNT => {
            predefined_functions::flagcnt(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::KBDBUFSIZE => {
            predefined_functions::kbdbufsize(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PPLBUFSIZE => {
            predefined_functions::pplbufsize(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::KBDFILUSED => {
            predefined_functions::kbdfilused(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::LOMSGNUM => {
            predefined_functions::lomsgnum(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::HIMSGNUM => {
            predefined_functions::himsgnum(evaluate_exp(interpreter, &params[0])?)
        }

        FuncOpCode::DRIVESPACE => {
            predefined_functions::drivespace(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::OUTBYTES => predefined_functions::outbytes(),
        FuncOpCode::HICONFNUM => {
            predefined_functions::hiconfnum(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::INBYTES => predefined_functions::inbytes(interpreter),
        FuncOpCode::CRC32 => predefined_functions::crc32(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::PCBMAC => predefined_functions::pcbmac(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::ACTMSGNUM => {
            predefined_functions::actmsgnum(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::STACKLEFT => {
            predefined_functions::stackleft(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::STACKERR => {
            predefined_functions::stackerr(evaluate_exp(interpreter, &params[0])?)
        }

        FuncOpCode::DGETALIAS => {
            predefined_functions::dgetalias(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DBOF => predefined_functions::dbof(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DCHANGED => {
            predefined_functions::dchanged(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DDECIMALS => {
            predefined_functions::ddecimals(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DDELETED => {
            predefined_functions::ddeleted(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DEOF => predefined_functions::deof(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DERR => predefined_functions::derr(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DFIELDS => {
            predefined_functions::dfields(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DLENGTH => {
            predefined_functions::dlength(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DNAME => predefined_functions::dname(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DRECCOUNT => {
            predefined_functions::dreccount(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DRECNO => predefined_functions::drecno(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DTYPE => predefined_functions::dtype(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::FNEXT => predefined_functions::fnext(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DNEXT => predefined_functions::dnext(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::TODDATE => {
            predefined_functions::toddate(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DCLOSEALL => {
            predefined_functions::dcloseall(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DOPEN => predefined_functions::dopen(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DCLOSE => predefined_functions::dclose(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DSETALIAS => {
            predefined_functions::dsetalias(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DPACK => predefined_functions::dpack(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DLOCKF => predefined_functions::dlockf(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DLOCK => predefined_functions::dlock(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DLOCKR => predefined_functions::dlockr(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DUNLOCK => {
            predefined_functions::dunlock(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DNOPEN => predefined_functions::dnopen(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DNCLOSE => {
            predefined_functions::dnclose(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DNCLOSEALL => {
            predefined_functions::dncloseall(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DNEW => predefined_functions::dnew(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DADD => predefined_functions::dadd(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DAPPEND => {
            predefined_functions::dappend(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DTOP => predefined_functions::dtop(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DGO => predefined_functions::dgo(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DBOTTOM => {
            predefined_functions::dbottom(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DSKIP => predefined_functions::dskip(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DBLANK => predefined_functions::dblank(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DDELETE => {
            predefined_functions::ddelete(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DRECALL => {
            predefined_functions::drecall(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DTAG => predefined_functions::dtag(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DSEEK => predefined_functions::dseek(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DFBLANK => {
            predefined_functions::dfblank(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DGET => predefined_functions::dget(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DPUT => predefined_functions::dput(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DFCOPY => predefined_functions::dfcopy(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::DSELECT => {
            predefined_functions::dselect(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DCHKSTAT => {
            predefined_functions::dchkstat(evaluate_exp(interpreter, &params[0])?)
        }

        FuncOpCode::PCBACCOUNT => {
            predefined_functions::pcbaccount(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::PCBACCSTAT => {
            predefined_functions::pcbaccstat(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::DERRMSG => {
            predefined_functions::derrmsg(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::ACCOUNT => {
            predefined_functions::account(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::SCANMSGHDR => {
            predefined_functions::scanmsghdr(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CHECKRIP => {
            predefined_functions::checkrip(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::RIPVER => predefined_functions::ripver(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::QWKLIMITS => {
            predefined_functions::qwklimits(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FINDFIRST => {
            predefined_functions::findfirst(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FINDNEXT => {
            predefined_functions::findnext(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::USELMRS => {
            predefined_functions::uselmrs(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::CONFINFO => {
            predefined_functions::confinfo(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::TINKEY => predefined_functions::tinkey(interpreter)?,
        FuncOpCode::CWD => predefined_functions::cwd(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::INSTRR => predefined_functions::instrr(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::FDORDAKA => {
            predefined_functions::fdordaka(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FDORDORG => {
            predefined_functions::fdordorg(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FDORDAREA => {
            predefined_functions::fdordarea(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::FDOQRD => predefined_functions::fdoqrd(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::GETDRIVE => {
            predefined_functions::getdrive(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::SETDRIVE => {
            predefined_functions::setdrive(evaluate_exp(interpreter, &params[0])?)
        }
        FuncOpCode::BS2I => predefined_functions::bs2i(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::BD2I => predefined_functions::bd2i(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::I2BS => predefined_functions::i2bs(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::I2BD => predefined_functions::i2bd(evaluate_exp(interpreter, &params[0])?),
        FuncOpCode::FTELL => predefined_functions::ftell(evaluate_exp(interpreter, &params[0])?),
        _ => panic!("Unsupported function {func_def:?}"),
    })
}

#[allow(unused_variables)]
mod predefined_functions;
