use super::*;
use crate::{
    ast::{expression::Expression, program::Program},
    tables::{FuncOpCode, FunctionDefinition},
};

pub fn evaluate_exp(
    interpreter: &Interpreter, 
    cur_frame: &StackFrame,
    ctx: &dyn ExecutionContext,
    expr: &Expression,
) -> VariableValue {
    match expr {
        Expression::Identifier(str) => {
            let res = cur_frame.values.get(str).unwrap().clone();
            res
        }
        Expression::Const(constant) => match constant {
            Constant::Boolean(true) => VariableValue::Integer(PPL_TRUE),
            Constant::Boolean(false) => VariableValue::Integer(PPL_FALSE),
            Constant::Money(x) => VariableValue::Money(*x),
            Constant::Integer(x) => VariableValue::Integer(*x),
            Constant::Unsigned(x) => VariableValue::Unsigned(*x),
            Constant::String(x) => VariableValue::String(x.clone()),
            Constant::Real(x) => VariableValue::Real(*x),
            Constant::Builtin(x) => {
                for val in &CONSTANT_VALUES {
                    if *x == val.0 {
                        return val.1.clone();
                    }
                }
                panic!("unknown built in const {}", x)
            }
        },
        Expression::Parens(pexpr) => evaluate_exp(interpreter, cur_frame, ctx, pexpr),
        Expression::PredefinedFunctionCall(func_def, params) => {
            call_function(interpreter, cur_frame, ctx, func_def, params)
        }
        Expression::FunctionCall(func_name, _params) => {
            panic!("not supported function call {}", func_name);
        }
        Expression::Not(expr) => {
            let value = evaluate_exp(interpreter, cur_frame, ctx, expr);
            match value {
                VariableValue::Integer(x) => {
                    VariableValue::Integer(if x == PPL_FALSE { PPL_TRUE } else { PPL_FALSE })
                }
                VariableValue::Boolean(x) => VariableValue::Boolean(!x),
                _ => {
                    panic!("unsupported for minus {:?} value {:?}", expr, value);
                }
            }
        }
        Expression::Minus(expr) => {
            let value = evaluate_exp(interpreter, cur_frame, ctx, expr);
            match value {
                VariableValue::Integer(x) => VariableValue::Integer(-x),
                _ => {
                    panic!("unsupported for minus {:?} value {:?}", expr, value);
                }
            }
        }
        Expression::Plus(expr) => evaluate_exp(interpreter, cur_frame, ctx, expr),
        Expression::BinaryExpression(op, lvalue, rvalue) => match op {
            BinOp::Add => {
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    + evaluate_exp(interpreter, cur_frame, ctx, rvalue)
            }
            BinOp::Sub => {
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    - evaluate_exp(interpreter, cur_frame, ctx, rvalue)
            }
            BinOp::Mul => {
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    * evaluate_exp(interpreter, cur_frame, ctx, rvalue)
            }
            BinOp::Div => {
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    / evaluate_exp(interpreter, cur_frame, ctx, rvalue)
            }
            BinOp::Mod => evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                .modulo(evaluate_exp(interpreter, cur_frame, ctx, rvalue)),
            BinOp::PoW => evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                .pow(evaluate_exp(interpreter, cur_frame, ctx, rvalue)),
            BinOp::Eq => {
                let l = evaluate_exp(interpreter, cur_frame, ctx, lvalue);
                let r = evaluate_exp(interpreter, cur_frame, ctx, rvalue);
                VariableValue::Boolean(l == r)
            }
            BinOp::NotEq => {
                let l = evaluate_exp(interpreter, cur_frame, ctx, lvalue);
                let r = evaluate_exp(interpreter, cur_frame, ctx, rvalue);
                VariableValue::Boolean(l != r)
            }
            BinOp::Or => evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                .or(evaluate_exp(interpreter, cur_frame, ctx, rvalue)),
            BinOp::And => evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                .and(evaluate_exp(interpreter, cur_frame, ctx, rvalue)),
            BinOp::Lower => VariableValue::Boolean(
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    < evaluate_exp(interpreter, cur_frame, ctx, rvalue),
            ),
            BinOp::LowerEq => VariableValue::Boolean(
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    <= evaluate_exp(interpreter, cur_frame, ctx, rvalue),
            ),
            BinOp::Greater => VariableValue::Boolean(
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    > evaluate_exp(interpreter, cur_frame, ctx, rvalue),
            ),
            BinOp::GreaterEq => VariableValue::Boolean(
                evaluate_exp(interpreter, cur_frame, ctx, lvalue)
                    >= evaluate_exp(interpreter, cur_frame, ctx, rvalue),
            ),
        },
        Expression::Dim1(_expr, _vec) => {
            panic!("not supported");
        }
        Expression::Dim2(_expr, _vec, _mat) => {
            panic!("not supported");
        }
        Expression::Dim3(_expr, _vec, _mat, _cube) => {
            panic!("not supported");
        }
    }
}

fn get_int(val: &VariableValue) -> i32 {
    if let VariableValue::Integer(i) = convert_to(VariableType::Integer, val) {
        i
    } else {
        panic!("can't get int from {:?}", val);
    }
}

fn call_function(
    interpreter: &Interpreter,
    cur_frame: &StackFrame,
    ctx: &dyn ExecutionContext,
    func_def: &'static FunctionDefinition,
    params: &Vec<Expression>,
) -> VariableValue {
    match func_def.opcode {
        FuncOpCode::LEN => predefined_functions::len(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::LOWER => {
            predefined_functions::lower(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::UPPER => {
            predefined_functions::upper(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MID => predefined_functions::mid(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[2]),
        ),
        FuncOpCode::LEFT => predefined_functions::left(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::RIGHT => predefined_functions::right(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::SPACE => {
            predefined_functions::space(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FERR => {
            predefined_functions::ferr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CHR => predefined_functions::chr(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::ASC => predefined_functions::asc(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::INSTR => predefined_functions::instr(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::ABORT => predefined_functions::abort(ctx),
        FuncOpCode::LTRIM => predefined_functions::ltrim(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::REPLACE => predefined_functions::replace(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[2]),
        ),
        FuncOpCode::STRIP => predefined_functions::strip(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::REPLACESTR => predefined_functions::replace_string(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[2]),
        ),
        FuncOpCode::STRIPSTR => predefined_functions::strip_string(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::RTRIM => predefined_functions::rtrim(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::TRIM => predefined_functions::trim(
            evaluate_exp(interpreter, cur_frame, ctx, &params[0]),
            evaluate_exp(interpreter, cur_frame, ctx, &params[1]),
        ),
        FuncOpCode::RANDOM => {
            predefined_functions::random(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DATE => predefined_functions::date(),
        FuncOpCode::TIME => predefined_functions::time(),

        FuncOpCode::U_NAME => predefined_functions::u_name(ctx),
        FuncOpCode::U_LDATE => predefined_functions::u_ldate(ctx),
        FuncOpCode::U_LTIME => predefined_functions::u_ltime(ctx),
        FuncOpCode::U_LDIR => predefined_functions::u_ldir(ctx),
        FuncOpCode::U_LMR => predefined_functions::u_lmr(ctx),
        FuncOpCode::U_LOGONS => predefined_functions::u_logons(ctx),
        FuncOpCode::U_FUL => predefined_functions::u_ful(ctx),
        FuncOpCode::U_FDL => predefined_functions::u_fdl(ctx),
        FuncOpCode::U_BDLDAY => predefined_functions::u_bdlday(ctx),
        FuncOpCode::U_TIMEON => predefined_functions::u_timeon(ctx),
        FuncOpCode::U_BDL => predefined_functions::u_bdl(ctx),
        FuncOpCode::U_BUL => predefined_functions::u_bul(ctx),
        FuncOpCode::U_MSGRD => predefined_functions::u_msgrd(ctx),
        FuncOpCode::U_MSGWR => predefined_functions::u_msgwr(ctx),

        FuncOpCode::YEAR => {
            predefined_functions::year(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MONTH => {
            predefined_functions::month(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DAY => predefined_functions::day(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::DOW => predefined_functions::dow(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::HOUR => {
            predefined_functions::hour(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MIN => predefined_functions::min(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::SEC => predefined_functions::sec(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::TIMEAP => {
            predefined_functions::timeap(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }

        FuncOpCode::VER => predefined_functions::ver(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::NOCHAR => {
            predefined_functions::nochar(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::YESCHAR => {
            predefined_functions::yeschar(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::STRIPATX => {
            predefined_functions::strip_atx(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::INKEY => {
            predefined_functions::inkey(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOSTRING => {
            predefined_functions::tostring(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_PWD => {
            predefined_functions::mask_pwd(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_ALPHA => {
            predefined_functions::mask_alpha(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_NUM => {
            predefined_functions::mask_num(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_ALNUM => {
            predefined_functions::mask_alnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_FILE => {
            predefined_functions::mask_file(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_PATH => {
            predefined_functions::mask_path(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MASK_ASCII => {
            predefined_functions::mask_ascii(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CURCONF => {
            predefined_functions::curconf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PCBDAT => {
            predefined_functions::pcbdat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PPEPATH => {
            predefined_functions::ppepath(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::VALDATE => {
            predefined_functions::valdate(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::VALTIME => {
            predefined_functions::valtime(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PCBNODE => {
            predefined_functions::pcbnode(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::READLINE => {
            predefined_functions::readline(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SYSOPSEC => {
            predefined_functions::sysopsec(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ONLOCAL => {
            predefined_functions::onlocal(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::UN_STAT => {
            predefined_functions::un_stat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::UN_NAME => {
            predefined_functions::un_name(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::UN_CITY => {
            predefined_functions::un_city(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::UN_OPER => {
            predefined_functions::un_oper(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CURSEC => {
            predefined_functions::cursec(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::GETTOKEN => {
            predefined_functions::gettoken(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MINLEFT => {
            predefined_functions::minleft(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MINON => {
            predefined_functions::minon(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::GETENV => {
            predefined_functions::getenv(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CALLID => {
            predefined_functions::callid(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGAL => {
            predefined_functions::regal(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGAH => {
            predefined_functions::regah(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGBL => {
            predefined_functions::regbl(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGBH => {
            predefined_functions::regbh(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGCL => {
            predefined_functions::regcl(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGCH => {
            predefined_functions::regch(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGDL => {
            predefined_functions::regdl(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGDH => {
            predefined_functions::regdh(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGAX => {
            predefined_functions::regax(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGBX => {
            predefined_functions::regbx(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGCX => {
            predefined_functions::regcx(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGDX => {
            predefined_functions::regdx(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGSI => {
            predefined_functions::regsi(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGDI => {
            predefined_functions::regdi(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGF => {
            predefined_functions::regf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGCF => {
            predefined_functions::regcf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGDS => {
            predefined_functions::regds(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::REGES => {
            predefined_functions::reges(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::B2W => predefined_functions::b2w(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::PEEKB => {
            predefined_functions::peekb(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PEEKW => {
            predefined_functions::peekw(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MKADDR => {
            predefined_functions::mkaddr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::EXIST => {
            predefined_functions::exist(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::I2S => predefined_functions::i2s(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::S2I => predefined_functions::s2i(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::CARRIER => {
            predefined_functions::carrier(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOKENSTR => {
            predefined_functions::tokenstr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CDON => {
            predefined_functions::cdon(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::LANGEXT => {
            predefined_functions::langext(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ANSION => {
            predefined_functions::ansion(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::VALCC => {
            predefined_functions::valcc(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FMTCC => {
            predefined_functions::fmtcc(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CCTYPE => {
            predefined_functions::cctype(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::GETX => {
            predefined_functions::getx(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::GETY => {
            predefined_functions::gety(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::BAND => {
            predefined_functions::band(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::BOR => predefined_functions::bor(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::BXOR => {
            predefined_functions::bxor(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::BNOT => {
            predefined_functions::bnot(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_PWDHIST => {
            predefined_functions::u_pwdhist(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_PWDLC => {
            predefined_functions::u_pwdlc(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_PWDTC => {
            predefined_functions::u_pwdtc(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_STAT => {
            predefined_functions::u_stat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DEFCOLOR => {
            predefined_functions::defcolor(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ABS => predefined_functions::abs(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::GRAFMODE => {
            predefined_functions::grafmode(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PSA => predefined_functions::psa(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::FILEINF => {
            predefined_functions::fileinf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PPENAME => {
            predefined_functions::ppename(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MKDATE => {
            predefined_functions::mkdate(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CURCOLOR => {
            predefined_functions::curcolor(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::KINKEY => {
            predefined_functions::kinkey(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MINKEY => {
            predefined_functions::minkey(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MAXNODE => {
            predefined_functions::maxnode(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SLPATH => {
            predefined_functions::slpath(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::HELPPATH => {
            predefined_functions::helppath(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TEMPPATH => {
            predefined_functions::temppath(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MODEM => {
            predefined_functions::modem(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::LOGGEDON => {
            predefined_functions::loggedon(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CALLNUM => {
            predefined_functions::callnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MGETBYTE => {
            predefined_functions::mgetbyte(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOKCOUNT => {
            predefined_functions::tokcount(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_RECNUM => {
            predefined_functions::u_recnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::U_INCONF => {
            predefined_functions::u_inconf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PEEKDW => {
            predefined_functions::peekdw(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DBGLEVEL => {
            predefined_functions::dbglevel(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SCRTEXT => {
            predefined_functions::scrtext(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SHOWSTAT => {
            predefined_functions::showstat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PAGESTAT => {
            predefined_functions::pagestat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOBIGSTR => {
            predefined_functions::tobigstr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOBOOLEAN => {
            predefined_functions::toboolean(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOBYTE => {
            predefined_functions::tobyte(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TODATE => {
            predefined_functions::todate(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TODREAL => {
            predefined_functions::todreal(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOEDATE => {
            predefined_functions::toedate(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOINTEGER => {
            predefined_functions::tointeger(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOMONEY => {
            predefined_functions::tomoney(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOREAL => {
            predefined_functions::toreal(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOSBYTE => {
            predefined_functions::tosbyte(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOSWORD => {
            predefined_functions::tosword(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOTIME => {
            predefined_functions::totime(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOUNSIGNED => {
            predefined_functions::tounsigned(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TOWORD => {
            predefined_functions::toword(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MIXED => {
            predefined_functions::mixed(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ALIAS => {
            predefined_functions::alias(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFREG => {
            predefined_functions::confreg(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFEXP => {
            predefined_functions::confexp(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFSEL => {
            predefined_functions::confsel(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFSYS => {
            predefined_functions::confsys(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFMW => {
            predefined_functions::confmw(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::LPRINTED => {
            predefined_functions::lprinted(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ISNONSTOP => {
            predefined_functions::isnonstop(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ERRCORRECT => {
            predefined_functions::errcorrect(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFALIAS => {
            predefined_functions::confalias(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::USERALIAS => {
            predefined_functions::useralias(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CURUSER => {
            predefined_functions::curuser(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CHATSTAT => {
            predefined_functions::chatstat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DEFANS => {
            predefined_functions::defans(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::LASTANS => {
            predefined_functions::lastans(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::MEGANUM => {
            predefined_functions::meganum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::EVTTIMEADJ => {
            predefined_functions::evttimeadj(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ISBITSET => {
            predefined_functions::isbitset(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FMTREAL => {
            predefined_functions::fmtreal(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FLAGCNT => {
            predefined_functions::flagcnt(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::KBDBUFSIZE => {
            predefined_functions::kbdbufsize(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PPLBUFSIZE => {
            predefined_functions::pplbufsize(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::KBDFILUSED => {
            predefined_functions::kbdfilused(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::LOMSGNUM => {
            predefined_functions::lomsgnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::HIMSGNUM => {
            predefined_functions::himsgnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }

        FuncOpCode::DRIVESPACE => {
            predefined_functions::drivespace(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::OUTBYTES => {
            predefined_functions::outbytes(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::HICONFNUM => {
            predefined_functions::hiconfnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::INBYTES => {
            predefined_functions::inbytes(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CRC32 => {
            predefined_functions::crc32(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PCBMAC => {
            predefined_functions::pcbmac(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ACTMSGNUM => {
            predefined_functions::actmsgnum(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::STACKLEFT => {
            predefined_functions::stackleft(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::STACKERR => {
            predefined_functions::stackerr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }

        FuncOpCode::DGETALIAS => {
            predefined_functions::dgetalias(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DBOF => {
            predefined_functions::dbof(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DCHANGED => {
            predefined_functions::dchanged(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DDECIMALS => {
            predefined_functions::ddecimals(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DDELETED => {
            predefined_functions::ddeleted(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DEOF => {
            predefined_functions::deof(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DERR => {
            predefined_functions::derr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DFIELDS => {
            predefined_functions::dfields(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DLENGTH => {
            predefined_functions::dlength(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNAME => {
            predefined_functions::dname(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DRECCOUNT => {
            predefined_functions::dreccount(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DRECNO => {
            predefined_functions::drecno(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DTYPE => {
            predefined_functions::dtype(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FNEXT => {
            predefined_functions::fnext(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNEXT => {
            predefined_functions::dnext(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TODDATE => {
            predefined_functions::toddate(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DCLOSEALL => {
            predefined_functions::dcloseall(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DOPEN => {
            predefined_functions::dopen(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DCLOSE => {
            predefined_functions::dclose(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DSETALIAS => {
            predefined_functions::dsetalias(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DPACK => {
            predefined_functions::dpack(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DLOCKF => {
            predefined_functions::dlockf(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DLOCK => {
            predefined_functions::dlock(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DLOCKR => {
            predefined_functions::dlockr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DUNLOCK => {
            predefined_functions::dunlock(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNOPEN => {
            predefined_functions::dnopen(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNCLOSE => {
            predefined_functions::dnclose(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNCLOSEALL => {
            predefined_functions::dncloseall(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DNEW => {
            predefined_functions::dnew(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DADD => {
            predefined_functions::dadd(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DAPPEND => {
            predefined_functions::dappend(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DTOP => {
            predefined_functions::dtop(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DGO => predefined_functions::dgo(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::DBOTTOM => {
            predefined_functions::dbottom(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DSKIP => {
            predefined_functions::dskip(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DBLANK => {
            predefined_functions::dblank(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DDELETE => {
            predefined_functions::ddelete(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DRECALL => {
            predefined_functions::drecall(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DTAG => {
            predefined_functions::dtag(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DSEEK => {
            predefined_functions::dseek(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DFBLANK => {
            predefined_functions::dfblank(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DGET => {
            predefined_functions::dget(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DPUT => {
            predefined_functions::dput(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DFCOPY => {
            predefined_functions::dfcopy(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DSELECT => {
            predefined_functions::dselect(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DCHKSTAT => {
            predefined_functions::dchkstat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }

        FuncOpCode::PCBACCOUNT => {
            predefined_functions::pcbaccount(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::PCBACCSTAT => {
            predefined_functions::pcbaccstat(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::DERRMSG => {
            predefined_functions::derrmsg(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::ACCOUNT => {
            predefined_functions::account(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SCANMSGHDR => {
            predefined_functions::scanmsghdr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CHECKRIP => {
            predefined_functions::checkrip(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::RIPVER => {
            predefined_functions::ripver(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::QWKLIMITS => {
            predefined_functions::qwklimits(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FINDFIRST => {
            predefined_functions::findfirst(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FINDNEXT => {
            predefined_functions::findnext(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::USELMRS => {
            predefined_functions::uselmrs(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CONFINFO => {
            predefined_functions::confinfo(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::TINKEY => {
            predefined_functions::tinkey(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::CWD => predefined_functions::cwd(evaluate_exp(interpreter, cur_frame, ctx, &params[0])),
        FuncOpCode::INSTRR => {
            predefined_functions::instrr(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FDORDAKA => {
            predefined_functions::fdordaka(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FDORDORG => {
            predefined_functions::fdordorg(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FDORDAREA => {
            predefined_functions::fdordarea(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FDOQRD => {
            predefined_functions::fdoqrd(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::GETDRIVE => {
            predefined_functions::getdrive(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::SETDRIVE => {
            predefined_functions::setdrive(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::BS2I => {
            predefined_functions::bs2i(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::BD2I => {
            predefined_functions::bd2i(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::I2BS => {
            predefined_functions::i2bs(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::I2BD => {
            predefined_functions::i2bd(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        FuncOpCode::FTELL => {
            predefined_functions::ftell(evaluate_exp(interpreter, cur_frame, ctx, &params[0]))
        }
        _ => panic!("Unsupported function {:?}", func_def),
    }
}

mod predefined_functions {
    use substring::Substring;

    use crate::interpreter::{ExecutionContext, VariableValue};

    use super::get_int;

    /// Returns the length of a strning
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::Integer - the length of `str`
    /// # Remarks
    /// 0 means empty string
    /// According to specs 256 is the maximum returned
    pub fn len(str: VariableValue) -> VariableValue {
        VariableValue::Integer(str.to_string().len() as i32)
    }

    /// Returns the lowercase equivalent of a string
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::String - lowercase equivalent of `str`
    pub fn lower(str: VariableValue) -> VariableValue {
        VariableValue::String(str.to_string().to_lowercase())
    }

    /// Returns the uppercase equivalent of a string
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    ///  VariableValue::String - uppercase equivalent of `str`
    pub fn upper(str: VariableValue) -> VariableValue {
        VariableValue::String(str.to_string().to_uppercase())
    }

    /// Returns a substring
    /// # Arguments
    ///  * `str` - A string value
    ///  * `pos` - An integer value with a position from str to begin the substring 1 == first char
    ///  * `chars` - An integer value with the number of chars to take from `str`
    /// # Returns
    ///  the substring of `str`, "" if chars <= 0, Will add padding up to the full length specified
    pub fn mid(str: VariableValue, pos: VariableValue, chars: VariableValue) -> VariableValue {
        let mut pos = get_int(&pos) - 1; // 1 based
        let mut chars = get_int(&chars);
        if chars <= 0 {
            return VariableValue::String(String::new());
        }

        let mut res = String::new();
        while pos < 0 {
            res.push(' ');
            pos += 1;
            chars -= 1;
        }

        if chars > 0 {
            res.push_str(
                str.to_string()
                    .substring(pos as usize, (pos + chars) as usize),
            );
        }
        VariableValue::String(res)
    }

    pub fn left(str: VariableValue, chars: VariableValue) -> VariableValue {
        let mut chars = get_int(&chars);
        if chars <= 0 {
            return VariableValue::String(String::new());
        }

        let mut res = String::new();
        if chars > 0 {
            let str = str.to_string();
            if chars < str.len() as i32 {
                res.push_str(str.substring(0, chars as usize));
            } else {
                res.push_str(str.as_str());
                chars -= str.len() as i32;
                while chars > 0 {
                    res.push(' ');
                    chars -= 1;
                }
            }
        }
        VariableValue::String(res)
    }

    pub fn right(str: VariableValue, chars: VariableValue) -> VariableValue {
        let chars = get_int(&chars);
        if chars <= 0 {
            return VariableValue::String(String::new());
        }
        let mut chars = chars as usize;

        let mut res = String::new();
        if chars > 0 {
            let str = str.to_string();
            while chars > str.len() {
                res.push(' ');
                chars -= 1;
            }
            res.push_str(str.substring(str.len() - chars, str.len()));
        }
        VariableValue::String(res)
    }

    pub fn space(chars: VariableValue) -> VariableValue {
        let mut chars = get_int(&chars);
        if chars <= 0 {
            return VariableValue::String(String::new());
        }
        let mut res = String::new();
        while chars > 0 {
            res.push(' ');
            chars -= 1;
        }
        VariableValue::String(res)
    }

    pub fn ferr(_str: VariableValue) -> VariableValue {
        panic!("TODO FILES") // TODO
    }

    pub fn chr(chr: VariableValue) -> VariableValue {
        let c = get_int(&chr);
        if c <= 0 {
            return VariableValue::String(String::new());
        }
        // undocumented: returns a space for c > 255
        if c > 255 {
            return VariableValue::String(" ".to_string());
        }
        let mut res = String::new();
        unsafe {
            res.push(char::from_u32_unchecked(c as u32));
        }
        VariableValue::String(res)
    }

    pub fn asc(chr: VariableValue) -> VariableValue {
        let c = chr.to_string();
        if c.len() <= 0 {
            return VariableValue::Integer(0);
        }
        VariableValue::Integer(c.as_bytes()[0] as i32)
    }

    /// Returns the position of a substring
    /// # Arguments
    ///  * `str` - A string value
    ///  * `sub` - A string expression to search for
    /// # Returns
    ///  A 1 based integer of the position of sub or 0 if sub is not found.
    pub fn instr(str: VariableValue, sub: VariableValue) -> VariableValue {
        let str = str.to_string();
        let sub = sub.to_string();
        if sub.len() <= 0 {
            return VariableValue::Integer(0);
        }

        match str.find(&sub) {
            Some(x) => VariableValue::Integer(1 + x as i32),
            _ => VariableValue::Integer(0),
        }
    }

    /// Returns a flag indicating if the user has aborted the display of information.
    pub fn abort(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO ABORT") // TODO
    }

    /// Trim specified characters from the beginning of a string
    /// # Arguments
    ///  * `str` - A string value
    ///  * `ch` - A string with the character to strip from the beginning of `str`
    /// # Returns
    ///  The trimmed `str`
    pub fn ltrim(str: VariableValue, ch: VariableValue) -> VariableValue {
        let mut ch = ch.to_string();
        if ch.len() == 0 {
            return str;
        }

        let res = str.to_string();
        let pat = ch.remove(0);
        VariableValue::String(res.trim_start_matches(pat).to_string())
    }

    /// Replaces all occurences of a given character to another character in a string.
    /// # Arguments
    ///  * `str` - A string value
    ///  * `old` - A string with the old character
    ///  * `new` - A string with the new character
    pub fn replace(_str: VariableValue, _old: VariableValue, _new: VariableValue) -> VariableValue {
        panic!("TODO")
    }

    /// Remove all occurences of a given character in a string
    /// # Arguments
    ///  * `str` - A string value
    ///  * `ch` - A string with the character to remove
    pub fn strip(_str: VariableValue, _ch: VariableValue) -> VariableValue {
        panic!("TODO")
    }

    /// Remove @X codes from a string
    /// # Arguments
    ///  * `str` - A string value
    /// # Returns
    /// A string without any @X codes
    pub fn strip_atx(_str: VariableValue) -> VariableValue {
        panic!("TODO")
    }

    pub fn replace_string(
        _str: VariableValue,
        _old: VariableValue,
        _new: VariableValue,
    ) -> VariableValue {
        panic!("TODO")
    }

    pub fn strip_string(_str: VariableValue, _strip: VariableValue) -> VariableValue {
        panic!("TODO")
    }

    /// Trim specified characters from the end of a string
    /// # Arguments
    ///  * `str` - A string value
    ///  * `ch` - A string with the character to strip from the end of `str`
    /// # Returns
    ///  The trimmed `str`
    pub fn rtrim(str: VariableValue, ch: VariableValue) -> VariableValue {
        let mut ch = ch.to_string();
        if ch.len() == 0 {
            return str;
        }

        let res = str.to_string();
        let pat = ch.remove(0);
        VariableValue::String(res.trim_end_matches(pat).to_string())
    }

    /// Trim specified characters from the beginning and end of a string
    /// # Arguments
    ///  * `str` - A string value
    ///  * `ch` - A string with the character to strip from the beginning and end of `str`
    /// # Returns
    ///  The trimmed `str`
    pub fn trim(str: VariableValue, ch: VariableValue) -> VariableValue {
        let mut ch = ch.to_string();
        if ch.len() == 0 {
            return str;
        }

        let res = str.to_string();
        let pat = ch.remove(0);
        VariableValue::String(res.trim_matches(pat).to_string())
    }

    pub fn random(_upper: VariableValue) -> VariableValue {
        panic!("TODO")
    }

    pub fn date() -> VariableValue {
        panic!("TODO")
    }

    pub fn time() -> VariableValue {
        panic!("TODO")
    }

    pub fn u_name(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_ldate(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_ltime(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_ldir(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_lmr(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_logons(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_ful(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_fdl(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_bdlday(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_timeon(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_bdl(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_bul(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_msgrd(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }
    pub fn u_msgwr(_ctx: &dyn ExecutionContext) -> VariableValue {
        panic!("TODO") // TODO
    }

    pub fn year(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn month(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn day(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dow(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn hour(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn min(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn sec(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn timeap(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ver(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn nochar(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn yeschar(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn inkey(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tostring(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_pwd(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_alpha(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_num(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_alnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_file(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_path(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mask_ascii(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn curconf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pcbdat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ppepath(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn valdate(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn valtime(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pcbnode(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn readline(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn sysopsec(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn onlocal(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn un_stat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn un_name(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn un_city(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn un_oper(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn cursec(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn gettoken(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn minleft(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn minon(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn getenv(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn callid(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regal(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regah(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regbl(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regbh(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regcl(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regch(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regdl(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regdh(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regax(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regbx(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regcx(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regdx(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regsi(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regdi(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regcf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn regds(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn reges(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn b2w(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn peekb(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn peekw(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mkaddr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn exist(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn i2s(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn s2i(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn carrier(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tokenstr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn cdon(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn langext(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ansion(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn valcc(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fmtcc(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn cctype(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn getx(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn gety(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn band(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn bor(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn bxor(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn bnot(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_pwdhist(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_pwdlc(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_pwdtc(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_stat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn defcolor(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn abs(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn grafmode(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn psa(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fileinf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ppename(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mkdate(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn curcolor(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn kinkey(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn minkey(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn maxnode(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn slpath(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn helppath(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn temppath(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn modem(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn loggedon(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn callnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mgetbyte(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tokcount(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_recnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn u_inconf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn peekdw(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dbglevel(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn scrtext(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn showstat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pagestat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tobigstr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn toboolean(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tobyte(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn todate(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn todreal(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn toedate(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tointeger(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tomoney(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn toreal(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tosbyte(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tosword(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn totime(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tounsigned(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn toword(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn mixed(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn alias(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confreg(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confexp(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confsel(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confsys(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confmw(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn lprinted(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn isnonstop(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn errcorrect(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confalias(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn useralias(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn curuser(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn chatstat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn defans(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn lastans(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn meganum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn evttimeadj(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn isbitset(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fmtreal(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn flagcnt(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn kbdbufsize(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pplbufsize(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn kbdfilused(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn lomsgnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn himsgnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    
    pub fn drivespace(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn outbytes(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn hiconfnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn inbytes(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn crc32(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pcbmac(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn actmsgnum(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn stackleft(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn stackerr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    
    pub fn dgetalias(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dbof(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dchanged(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ddecimals(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ddeleted(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn deof(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn derr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dfields(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dlength(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dname(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dreccount(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn drecno(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dtype(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fnext(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dnext(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn toddate(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dcloseall(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dopen(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dclose(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dsetalias(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dpack(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dlockf(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dlock(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dlockr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dunlock(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dnopen(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dnclose(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dncloseall(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dnew(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dadd(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dappend(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dtop(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dgo(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dbottom(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dskip(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dblank(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ddelete(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn drecall(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dtag(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dseek(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dfblank(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dget(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dput(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dfcopy(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dselect(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn dchkstat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    
    pub fn pcbaccount(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn pcbaccstat(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn derrmsg(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn account(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn scanmsghdr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn checkrip(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ripver(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn qwklimits(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn findfirst(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn findnext(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn uselmrs(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn confinfo(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn tinkey(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn cwd(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn instrr(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fdordaka(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fdordorg(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fdordarea(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn fdoqrd(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn getdrive(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn setdrive(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn bs2i(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn bd2i(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn i2bs(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn i2bd(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
    pub fn ftell(_x: VariableValue) -> VariableValue {
        panic!("TODO")
    }
}
