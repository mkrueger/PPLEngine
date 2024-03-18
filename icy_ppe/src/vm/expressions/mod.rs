use std::collections::HashMap;

use crate::{ast::Variable, tables::{FuncOpCode, FunctionDefinition}, Res};

use super::VirtualMachine;

pub mod predefined_functions;
pub use predefined_functions::*;


fn call_function(
    vm: &mut VirtualMachine,
    func_def: &'static FunctionDefinition,
    params: &[Variable],
) -> Res<Variable> {
    Ok(match func_def.opcode {
        FuncOpCode::LEN => len(evaluate_exp(vm, &params[0])?),
        FuncOpCode::LOWER => lower(evaluate_exp(vm, &params[0])?),
        FuncOpCode::UPPER => upper(evaluate_exp(vm, &params[0])?),
        FuncOpCode::MID => mid(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
            evaluate_exp(vm, &params[2])?,
        )?,
        FuncOpCode::LEFT => left(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        )?,
        FuncOpCode::RIGHT => right(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        )?,
        FuncOpCode::SPACE => space(evaluate_exp(vm, &params[0])?)?,
        FuncOpCode::FERR => {
            let a = evaluate_exp(vm, &params[0])?;
            ferr(vm, a)?
        }
        FuncOpCode::CHR => chr(evaluate_exp(vm, &params[0])?)?,
        FuncOpCode::ASC => asc(evaluate_exp(vm, &params[0])?),
        FuncOpCode::INSTR => instr(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::ABORT => abort(vm),
        FuncOpCode::LTRIM => ltrim(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::REPLACE => replace(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
            evaluate_exp(vm, &params[2])?,
        ),
        FuncOpCode::STRIP => strip(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::REPLACESTR => replace_string(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
            evaluate_exp(vm, &params[2])?,
        ),
        FuncOpCode::STRIPSTR => strip_string(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::RTRIM => rtrim(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::TRIM => trim(
            evaluate_exp(vm, &params[0])?,
            evaluate_exp(vm, &params[1])?,
        ),
        FuncOpCode::RANDOM => random(evaluate_exp(vm, &params[0])?)?,
        FuncOpCode::DATE => date(),
        FuncOpCode::TIME => time(),

        FuncOpCode::U_NAME => u_name(vm),
        FuncOpCode::U_LDATE => u_ldate(vm),
        FuncOpCode::U_LTIME => u_ltime(vm),
        FuncOpCode::U_LDIR => u_ldir(vm),
        FuncOpCode::U_LMR => u_lmr(vm),
        FuncOpCode::U_LOGONS => u_logons(vm),
        FuncOpCode::U_FUL => u_ful(vm),
        FuncOpCode::U_FDL => u_fdl(vm),
        FuncOpCode::U_BDLDAY => u_bdlday(vm),
        FuncOpCode::U_TIMEON => u_timeon(vm),
        FuncOpCode::U_BDL => u_bdl(vm),
        FuncOpCode::U_BUL => u_bul(vm),
        FuncOpCode::U_MSGRD => u_msgrd(vm),
        FuncOpCode::U_MSGWR => u_msgwr(vm),

        FuncOpCode::YEAR => year(evaluate_exp(vm, &params[0])?),
        FuncOpCode::MONTH => month(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DAY => day(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DOW => dow(evaluate_exp(vm, &params[0])?),
        FuncOpCode::HOUR => hour(evaluate_exp(vm, &params[0])?),
        FuncOpCode::MIN => min(evaluate_exp(vm, &params[0])?),
        FuncOpCode::SEC => sec(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TIMEAP => timeap(evaluate_exp(vm, &params[0])?),

        FuncOpCode::VER => ver(evaluate_exp(vm, &params[0])?),
        FuncOpCode::NOCHAR => nochar(vm),
        FuncOpCode::YESCHAR => yeschar(vm),
        FuncOpCode::STRIPATX => {
            strip_atx(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::INKEY => inkey(vm)?,
        FuncOpCode::TOSTRING => {
            tostring(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::MASK_PWD => mask_pwd(),
        FuncOpCode::MASK_ALPHA => mask_alpha(),
        FuncOpCode::MASK_NUM => mask_num(),
        FuncOpCode::MASK_ALNUM => mask_alnum(),
        FuncOpCode::MASK_FILE => mask_file(),
        FuncOpCode::MASK_PATH => mask_path(),
        FuncOpCode::MASK_ASCII => mask_ascii(),

        FuncOpCode::CURCONF => {
            curconf(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PCBDAT => pcbdat(evaluate_exp(vm, &params[0])?),
        FuncOpCode::PPEPATH => ppepath(vm),
        FuncOpCode::VALDATE => {
            valdate(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::VALTIME => {
            valtime(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PCBNODE => pcbnode(vm),
        FuncOpCode::READLINE => {
            let file = evaluate_exp(vm, &params[0])?;
            let line = evaluate_exp(vm, &params[1])?;
            readline(vm, file, line)?
        }
        FuncOpCode::SYSOPSEC => sysopsec(vm),
        FuncOpCode::ONLOCAL => onlocal(vm),
        FuncOpCode::UN_STAT => un_stat(vm),
        FuncOpCode::UN_NAME => un_name(vm),
        FuncOpCode::UN_CITY => un_city(vm),
        FuncOpCode::UN_OPER => un_oper(vm),
        FuncOpCode::CURSEC => cursec(evaluate_exp(vm, &params[0])?),
        FuncOpCode::GETTOKEN => gettoken(vm),
        FuncOpCode::MINLEFT => {
            minleft(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::MINON => minon(evaluate_exp(vm, &params[0])?),
        FuncOpCode::GETENV => getenv(evaluate_exp(vm, &params[0])?),
        FuncOpCode::CALLID => callid(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGAL => regal(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGAH => regah(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGBL => regbl(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGBH => regbh(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGCL => regcl(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGCH => regch(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGDL => regdl(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGDH => regdh(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGAX => regax(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGBX => regbx(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGCX => regcx(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGDX => regdx(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGSI => regsi(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGDI => regdi(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGF => regf(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGCF => regcf(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGDS => regds(evaluate_exp(vm, &params[0])?),
        FuncOpCode::REGES => reges(evaluate_exp(vm, &params[0])?),
        FuncOpCode::B2W => b2w(evaluate_exp(vm, &params[0])?),
        FuncOpCode::PEEKB => peekb(evaluate_exp(vm, &params[0])?),
        FuncOpCode::PEEKW => peekw(evaluate_exp(vm, &params[0])?),
        FuncOpCode::MKADDR => mkaddr(evaluate_exp(vm, &params[0])?),
        FuncOpCode::EXIST => {
            let file_name = evaluate_exp(vm, &params[0])?.as_string();
            exist(vm, file_name.as_str())
        }
        FuncOpCode::I2S => {
            let int = evaluate_exp(vm, &params[0])?.as_int();
            let base = evaluate_exp(vm, &params[1])?.as_int();
            i2s(int, base)
        }
        FuncOpCode::S2I => {
            let s = evaluate_exp(vm, &params[0])?.as_string();
            let base = evaluate_exp(vm, &params[1])?.as_int();
            s2i(&s, base)?
        }
        FuncOpCode::CARRIER => carrier(vm),
        FuncOpCode::TOKENSTR => {
            tokenstr(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CDON => cdon(evaluate_exp(vm, &params[0])?),
        FuncOpCode::LANGEXT => {
            langext(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ANSION => ansion(evaluate_exp(vm, &params[0])?),
        FuncOpCode::VALCC => valcc(evaluate_exp(vm, &params[0])?),
        FuncOpCode::FMTCC => fmtcc(evaluate_exp(vm, &params[0])?),
        FuncOpCode::CCTYPE => cctype(evaluate_exp(vm, &params[0])?),
        FuncOpCode::GETX => getx(vm),
        FuncOpCode::GETY => gety(vm),
        FuncOpCode::BAND => band(evaluate_exp(vm, &params[0])?),
        FuncOpCode::BOR => bor(evaluate_exp(vm, &params[0])?),
        FuncOpCode::BXOR => bxor(evaluate_exp(vm, &params[0])?),
        FuncOpCode::BNOT => bnot(evaluate_exp(vm, &params[0])?),
        FuncOpCode::U_PWDHIST => {
            u_pwdhist(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::U_PWDLC => {
            u_pwdlc(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::U_PWDTC => {
            u_pwdtc(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::U_STAT => u_stat(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DEFCOLOR => {
            defcolor(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ABS => evaluate_exp(vm, &params[0])?.abs(),
        FuncOpCode::GRAFMODE => {
            grafmode(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PSA => psa(evaluate_exp(vm, &params[0])?),
        FuncOpCode::FILEINF => {
            let file = evaluate_exp(vm, &params[0])?.as_string();
            let item = evaluate_exp(vm, &params[1])?.as_int();
            fileinf(vm, &file, item)?
        }
        FuncOpCode::PPENAME => ppename(vm),
        FuncOpCode::MKDATE => mkdate(evaluate_exp(vm, &params[0])?),
        FuncOpCode::CURCOLOR => {
            curcolor(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::KINKEY => kinkey(vm)?,
        FuncOpCode::MINKEY => minkey(vm)?,
        FuncOpCode::MAXNODE => maxnode(vm),
        FuncOpCode::SLPATH => slpath(evaluate_exp(vm, &params[0])?),
        FuncOpCode::HELPPATH => {
            helppath(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TEMPPATH => {
            temppath(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::MODEM => modem(evaluate_exp(vm, &params[0])?),
        FuncOpCode::LOGGEDON => {
            loggedon(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CALLNUM => {
            callnum(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::MGETBYTE => {
            mgetbyte(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOKCOUNT => tokcount(vm),
        FuncOpCode::U_RECNUM => {
            let user_name = evaluate_exp(vm, &params[0])?;
            u_recnum(vm, user_name)
        }
        FuncOpCode::U_INCONF => {
            u_inconf(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PEEKDW => peekdw(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DBGLEVEL => {
            dbglevel(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::SCRTEXT => {
            scrtext(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::SHOWSTAT => {
            showstat(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PAGESTAT => {
            pagestat(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOBIGSTR => {
            tobigstr(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOBOOLEAN => {
            toboolean(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOBYTE => tobyte(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TODATE => todate(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TODREAL => {
            todreal(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOEDATE => {
            toedate(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOINTEGER => {
            tointeger(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOMONEY => {
            tomoney(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOREAL => toreal(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TOSBYTE => {
            tosbyte(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOSWORD => {
            tosword(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOTIME => totime(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TOUNSIGNED => {
            tounsigned(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TOWORD => toword(evaluate_exp(vm, &params[0])?),
        FuncOpCode::MIXED => mixed(evaluate_exp(vm, &params[0])?),
        FuncOpCode::ALIAS => alias(evaluate_exp(vm, &params[0])?),
        FuncOpCode::CONFREG => {
            confreg(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFEXP => {
            confexp(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFSEL => {
            confsel(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFSYS => {
            confsys(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFMW => confmw(evaluate_exp(vm, &params[0])?),
        FuncOpCode::LPRINTED => {
            lprinted(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ISNONSTOP => {
            isnonstop(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ERRCORRECT => {
            errcorrect(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFALIAS => {
            confalias(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::USERALIAS => {
            useralias(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CURUSER => {
            curuser(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CHATSTAT => {
            chatstat(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DEFANS => defans(evaluate_exp(vm, &params[0])?),
        FuncOpCode::LASTANS => {
            lastans(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::MEGANUM => {
            meganum(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::EVTTIMEADJ => {
            evttimeadj(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ISBITSET => {
            isbitset(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FMTREAL => {
            fmtreal(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FLAGCNT => {
            flagcnt(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::KBDBUFSIZE => {
            kbdbufsize(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PPLBUFSIZE => {
            pplbufsize(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::KBDFILUSED => {
            kbdfilused(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::LOMSGNUM => {
            lomsgnum(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::HIMSGNUM => {
            himsgnum(evaluate_exp(vm, &params[0])?)
        }

        FuncOpCode::DRIVESPACE => {
            drivespace(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::OUTBYTES => outbytes(),
        FuncOpCode::HICONFNUM => {
            hiconfnum(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::INBYTES => inbytes(vm),
        FuncOpCode::CRC32 => crc32(evaluate_exp(vm, &params[0])?),
        FuncOpCode::PCBMAC => pcbmac(evaluate_exp(vm, &params[0])?),
        FuncOpCode::ACTMSGNUM => {
            actmsgnum(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::STACKLEFT => {
            stackleft(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::STACKERR => {
            stackerr(evaluate_exp(vm, &params[0])?)
        }

        FuncOpCode::DGETALIAS => {
            dgetalias(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DBOF => dbof(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DCHANGED => {
            dchanged(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DDECIMALS => {
            ddecimals(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DDELETED => {
            ddeleted(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DEOF => deof(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DERR => derr(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DFIELDS => {
            dfields(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DLENGTH => {
            dlength(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DNAME => dname(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DRECCOUNT => {
            dreccount(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DRECNO => drecno(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DTYPE => dtype(evaluate_exp(vm, &params[0])?),
        FuncOpCode::FNEXT => fnext(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DNEXT => dnext(evaluate_exp(vm, &params[0])?),
        FuncOpCode::TODDATE => {
            toddate(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DCLOSEALL => {
            dcloseall(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DOPEN => dopen(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DCLOSE => dclose(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DSETALIAS => {
            dsetalias(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DPACK => dpack(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DLOCKF => dlockf(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DLOCK => dlock(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DLOCKR => dlockr(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DUNLOCK => {
            dunlock(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DNOPEN => dnopen(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DNCLOSE => {
            dnclose(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DNCLOSEALL => {
            dncloseall(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DNEW => dnew(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DADD => dadd(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DAPPEND => {
            dappend(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DTOP => dtop(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DGO => dgo(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DBOTTOM => {
            dbottom(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DSKIP => dskip(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DBLANK => dblank(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DDELETE => {
            ddelete(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DRECALL => {
            drecall(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DTAG => dtag(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DSEEK => dseek(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DFBLANK => {
            dfblank(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DGET => dget(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DPUT => dput(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DFCOPY => dfcopy(evaluate_exp(vm, &params[0])?),
        FuncOpCode::DSELECT => {
            dselect(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DCHKSTAT => {
            dchkstat(evaluate_exp(vm, &params[0])?)
        }

        FuncOpCode::PCBACCOUNT => {
            pcbaccount(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::PCBACCSTAT => {
            pcbaccstat(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::DERRMSG => {
            derrmsg(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::ACCOUNT => {
            account(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::SCANMSGHDR => {
            scanmsghdr(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CHECKRIP => {
            checkrip(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::RIPVER => ripver(evaluate_exp(vm, &params[0])?),
        FuncOpCode::QWKLIMITS => {
            qwklimits(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FINDFIRST => {
            findfirst(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FINDNEXT => {
            findnext(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::USELMRS => {
            uselmrs(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::CONFINFO => {
            confinfo(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::TINKEY => tinkey(vm)?,
        FuncOpCode::CWD => cwd(evaluate_exp(vm, &params[0])?),
        FuncOpCode::INSTRR => instrr(evaluate_exp(vm, &params[0])?),
        FuncOpCode::FDORDAKA => {
            fdordaka(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FDORDORG => {
            fdordorg(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FDORDAREA => {
            fdordarea(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::FDOQRD => fdoqrd(evaluate_exp(vm, &params[0])?),
        FuncOpCode::GETDRIVE => {
            getdrive(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::SETDRIVE => {
            setdrive(evaluate_exp(vm, &params[0])?)
        }
        FuncOpCode::BS2I => bs2i(evaluate_exp(vm, &params[0])?),
        FuncOpCode::BD2I => bd2i(evaluate_exp(vm, &params[0])?),
        FuncOpCode::I2BS => i2bs(evaluate_exp(vm, &params[0])?),
        FuncOpCode::I2BD => i2bd(evaluate_exp(vm, &params[0])?),
        FuncOpCode::FTELL => ftell(evaluate_exp(vm, &params[0])?),
        _ => panic!("Unsupported function {func_def:?}"),
    })
}

#[allow(unused_variables)]
mod predefined_functions;
