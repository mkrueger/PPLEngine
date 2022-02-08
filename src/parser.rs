use nom::{branch::alt, bytes::complete::{ tag, tag_no_case, take_while}, character::complete::{alpha1, space1,  alphanumeric1, multispace0}, combinator::{cut, map, map_res, opt, recognize, value}, error::{ ParseError}, multi::*,  sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult};
use nom::bytes::complete::{ take_while_m_n};
use nom::character::complete::{ multispace1};
use nom::sequence::tuple;
use std::str::FromStr;
use crate::executable::VariableType;

#[derive(Debug, PartialEq)]
pub enum Declaration<'a> {
    Variable(VariableType, &'a str),
    Function(&'a str, Vec<Declaration<'a>>, VariableType),
    Procedure(&'a str, Vec<Declaration<'a>>)
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_"))))
        )
    )(input)
}

fn parse_vartype<'a>(input: &'a str) -> IResult<&'a str, VariableType> {
    alt((
        value(VariableType::Integer, tag_no_case("INTEGER")),
        value(VariableType::String, tag_no_case("STRING")),
        value(VariableType::Boolean, tag_no_case("BOOLEAN")),
        value(VariableType::Date, tag_no_case("DATE")),
        value(VariableType::Time, tag_no_case("TIME")),
        value(VariableType::Money, tag_no_case("MONEY"))
    ))(input)
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
    where
        F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(
        multispace0,
        inner,
        multispace0
    )
}

fn parse_variable<'a>(input : &'a str) -> nom::IResult<&'a str, Declaration>
{
    map(
        pair(ws(parse_vartype), ws(identifier)),
        |name : (VariableType, &'a str)| {
            Declaration::Variable(name.0, name.1)
        }
    )(input)
}

fn parse_functiondeclaration<'a>(line : &'a str) -> nom::IResult<&'a str, Declaration>
{
    let parse = tuple((
        separated_pair(tag_no_case("DECLARE"), space1, tag_no_case("FUNCTION")),
        multispace1,
        identifier,
        multispace0,
        delimited(tag("("),
                  separated_list0(ws(tag(",")), parse_variable),
                  tag(")")),
        multispace0,
        parse_vartype)
    )(line);

    match parse {
        Ok(z) =>  nom::IResult::Ok((z.0, Declaration::Function(z.1.2, z.1.4, z.1.6))),
        Err(e) => Err(e)
    }
}

fn parse_proceduredeclaration<'a>(line : &'a str) -> nom::IResult<&'a str, Declaration>
{
    let parse = tuple((
        separated_pair(tag_no_case("DECLARE"), space1, tag_no_case("PROCEDURE")),
        multispace1,
        identifier,
        multispace0,
        delimited(tag("("),
                  separated_list0(ws(tag(",")), parse_variable),
                  tag(")")),
        multispace0)
    )(line);

    match parse {
        Ok(z) =>  nom::IResult::Ok((z.0, Declaration::Procedure(z.1.2, z.1.4))),
        Err(e) => Err(e)
    }
}

fn parse_declaration<'a>(input : &'a str) -> nom::IResult<&'a str, Declaration>
{
    alt((parse_variable, parse_functiondeclaration, parse_proceduredeclaration))(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Constant<'a>
{
    Money(f64),
    Integer(i32),
    String(&'a str),
    AUTO,
    BELL,
    DEFS,
    ECHODOTS,
    ERASELINE,
    FALSE,
    FCL,
    FIELDLEN,
    FNS,
    F_EXP,
    F_MW,
    F_REG,
    F_SEL,
    F_SYS,
    GRAPH,
    GUIDE,
    HIGHASCII,
    LANG,
    LFAFTER,
    LFBEFORE,
    LOGIT,
    LOGITLEFT,
    NC,
    NEWLINE,
    NOCLEAR,
    O_RD,
    O_RW,
    O_WR,
    SEC,
    STACKED,
    S_DB,
    S_DN,
    S_DR,
    S_DW,
    TRUE,
    UPCASE,
    WORDWRAP,
    YESNO
}

fn predefined_constants1<'a>(input : &'a str) -> nom::IResult<&'a str, Constant> {
    alt ((
        map(tag_no_case("LOGITLEFT"), |_| Constant::LOGITLEFT),
        map(tag_no_case("NC"), |_| Constant::NC),
        map(tag_no_case("NEWLINE"), |_| Constant::NEWLINE),
        map(tag_no_case("NOCLEAR"), |_| Constant::NOCLEAR),
        map(tag_no_case("O_RD"), |_| Constant::O_RD),
        map(tag_no_case("O_RW"), |_| Constant::O_RW),
        map(tag_no_case("O_WR"), |_| Constant::O_WR),
        map(tag_no_case("SEC"), |_| Constant::SEC),
        map(tag_no_case("STACKED"), |_| Constant::STACKED),
        map(tag_no_case("S_DB"), |_| Constant::S_DB),
        map(tag_no_case("S_DN"), |_| Constant::S_DN),
        map(tag_no_case("S_DR"), |_| Constant::S_DR),
        map(tag_no_case("S_DW"), |_| Constant::S_DW),
        map(tag_no_case("TRUE"), |_| Constant::TRUE),
        map(tag_no_case("UPCASE"), |_| Constant::UPCASE),
        map(tag_no_case("WORDWRAP"), |_| Constant::WORDWRAP),
        map(tag_no_case("YESNO"), |_| Constant::YESNO)))(input)
}
fn predefined_constants2<'a>(input : &'a str) -> nom::IResult<&'a str, Constant> {
    alt((
        map(tag_no_case("AUTO"), |_| Constant::AUTO),
        map(tag_no_case("BELL"), |_| Constant::BELL),
        map(tag_no_case("DEFS"), |_| Constant::DEFS),
        map(tag_no_case("ECHODOTS"), |_| Constant::ECHODOTS),
        map(tag_no_case("ERASELINE"), |_| Constant::ERASELINE),
        map(tag_no_case("FALSE"), |_| Constant::FALSE),
        map(tag_no_case("FCL"), |_| Constant::FCL),
        map(tag_no_case("FIELDLEN"), |_| Constant::FIELDLEN),
        map(tag_no_case("FNS"), |_| Constant::FNS),
        map(tag_no_case("F_EXP"), |_| Constant::F_EXP),
        map(tag_no_case("F_MW"), |_| Constant::F_MW),
        map(tag_no_case("F_REG"), |_| Constant::F_REG),
        map(tag_no_case("F_SEL"), |_| Constant::F_SEL),
        map(tag_no_case("F_SYS"), |_| Constant::F_SYS),
        map(tag_no_case("GRAPH"), |_| Constant::GRAPH),
        map(tag_no_case("GUIDE"), |_| Constant::GUIDE),
        map(tag_no_case("HIGHASCII"), |_| Constant::HIGHASCII),
        map(tag_no_case("LANG"), |_| Constant::LANG),
        map(tag_no_case("LFAFTER"), |_| Constant::LFAFTER),
        map(tag_no_case("LFBEFORE"), |_| Constant::LFBEFORE),
        map(tag_no_case("LOGIT"), |_| Constant::LOGIT)
    ))(input)
}
fn parse_constant<'a>(line : &'a str) -> nom::IResult<&'a str, Constant> {
    alt ((
        predefined_constants1,
        predefined_constants2,
        money,
        map (
            terminated(
                take_while(|z| '0' <= z && z <= '9'),
                tag_no_case("D")
            ),
            |z : &str| Constant::Integer(i32::from_str_radix( z, 10).unwrap())
        ),
        map (
            terminated(
                take_while(|z| '0' <= z && z <= '1'),
                tag_no_case("B")
            ),
            |z : &str| Constant::Integer(i32::from_str_radix( z, 2).unwrap())
        ),
        map (
            terminated(
                take_while(|z| '0' <= z && z <= '8'),
                tag_no_case("O")
            ),
            |z : &str| Constant::Integer(i32::from_str_radix( z, 8).unwrap())
        ),
        map (
            terminated(
                take_while(|z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f'),
                tag_no_case("H")
            ),
            |z : &str| Constant::Integer(i32::from_str_radix( z, 16).unwrap())
        ),        map (
            preceded(
                tag_no_case("@X"),
                take_while_m_n(2, 2, |z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f')),
            |z : &str| Constant::Integer(i32::from_str_radix( z, 16).unwrap())
        ),
         // string \"(.)*\"
        map(delimited(
            tag("\""),
            take_while(|z| z != '"'),
            tag("\"")),
            |s: &'a str| Constant::String(s)),
        // integer [+-i]\d+
        map_res(recognize(pair(
            opt(alt((tag("+"), tag("-")))),
            alphanumeric1)
        ), |s| {
            match i32::from_str(s) {
                nom::lib::std::result::Result::Ok(i) => { nom::lib::std::result::Result::Ok(Constant::Integer(i)) },
                _ => { nom::lib::std::result::Result::Err("Error parsing number.")}
            }
        })
    ))(line)
}

pub fn money<'a>(input: &'a str) -> IResult<&'a str, Constant> {
     map_res(pair(
        tag("$"),
        take_while(|c| c == '.' || '0' <= c && c <= '9')
    ),
    |z| {
        let value = f64::from_str(z.1);
        match value {
            Ok(f) => Ok(Constant::Money(f)),
            Err(e) => Err(e)
        }
    })(input)
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum BinOp {
    PoW,
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Eq,
    NotEq,
    Lower,
    LowerEq,
    Greater,
    GreaterEq,
    And,
    Or
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression<'a>
{
    Const(Constant<'a>),
    Parens(Box<Expression<'a>>),
    FunctionCall(&'a str, Vec<Expression<'a>>),
    Not(Box<Expression<'a>>),
    BinaryExpression(BinOp, Box<Expression<'a>>, Box<Expression<'a>>)
}

fn parse_parenexpr<'a>(input: &'a str) -> nom::IResult<&'a str, Expression<'a>>
{
    map(delimited(
        tag("("),
        ws(parse_expression),
        tag(")")),
        |s: Expression<'a>| Expression::Parens(Box::new(s)))(input)
}

fn parse_exprlist<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Expression<'a>>>
{
    map(
    pair(opt(parse_expression),
         many0(preceded(ws(tag(",")), parse_expression))),
    | (head, mut tail) | {
        match head  {
            Some(t) => tail.insert(0, t),
            _ => {}
        }
        tail
    }
    )(input)
}

fn parse_parameterlist<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Expression<'a>>>
{
   delimited(
        tag("("),
        ws(parse_exprlist),
        tag(")")
    )(input)
}

fn parse_binop(line : &str) -> nom::IResult<&str, BinOp>
{
    alt((
        map(ws(tag("^")), |_| BinOp::PoW),
        map(ws(tag("*")), |_| BinOp::Mul),
        map(ws(tag("/")), |_| BinOp::Div),
        map(ws(tag("%")), |_| BinOp::Mod),
        map(ws(tag("+")), |_| BinOp::Add),
        map(ws(tag("-")), |_| BinOp::Sub),
        map(ws(tag("=")), |_| BinOp::Eq),
        map(ws(tag("<>")), |_| BinOp::NotEq),
        map(ws(tag("<=")), |_| BinOp::LowerEq),
        map(ws(tag(">=")), |_| BinOp::GreaterEq),
        map(ws(tag("<")), |_| BinOp::Lower),
        map(ws(tag(">")), |_| BinOp::Greater),
        map(ws(tag("&")), |_| BinOp::And),
        map(ws(tag("|")), |_| BinOp::Or)
    )
    )(line)
}
fn fold_binop<'a>(left : Expression<'a>, remainder: Vec<(BinOp, Box<Expression<'a>>)>) -> Expression<'a>
{
    if remainder.len() == 0 {
        return left;
    }

    remainder.into_iter().fold(left, |acc, pair| {
        match pair.0 {
            BinOp::PoW => Expression::BinaryExpression(BinOp::PoW, Box::new(acc), pair.1),
            BinOp::Mul | BinOp::Div | BinOp::Mod => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::Add | BinOp::Sub => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::Eq | BinOp::NotEq | BinOp::Lower | BinOp::LowerEq| BinOp::Greater | BinOp::GreaterEq => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::And | BinOp::Or => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1)
        }
    })
}

fn parse_expression<'a>(line : &'a str) -> nom::IResult<&'a str, Expression<'a>> {
    let unary = alt ((
        parse_parenexpr,
        // function call
        map(separated_pair(identifier, multispace0, parse_parameterlist),
            |z| Expression::FunctionCall(z.0, z.1)),
        // !expr
        map(separated_pair(tag("!"),  multispace0, parse_expression),
            |z| Expression::Not(Box::new(z.1))),

         map(parse_constant, |z| Expression::Const(z))

    ));

    map(pair(
        unary,
        many0(pair(parse_binop, map(parse_expression, |z| Box::new(z))))
    ),
    |z| fold_binop(z.0, z.1)
     )(line)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement<'a> {
    ADJTIME(Box<Expression<'a>>),
    ANSIPOS(Box<Expression<'a>>, Box<Expression<'a>>),
    BACKUP(Box<Expression<'a>>),
    BLT(Box<Expression<'a>>),
    BYE,
    BROADCAST(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    CALL(Box<Expression<'a>>),
    CDCHKOFF,
    CDCHKON,
    CHAT,
    CLOSECAP,
    CLREOL,
    CLS,
    COLOR(Box<Expression<'a>>),
    CONFFLAG(Box<Expression<'a>>, Box<Expression<'a>>),
    CONFUNFLAG(Box<Expression<'a>>, Box<Expression<'a>>),
    DBGLEVEL(Box<Expression<'a>>),
    DEC(Box<Expression<'a>>),
    DEFCOLOR,
    DELAY(Box<Expression<'a>>),
    DELETE(Box<Expression<'a>>),
    DELUSER,
    DIR(Box<Expression<'a>>),
    DISPFILE(Box<Expression<'a>>, Box<Expression<'a>>),
    DISPSTR(Box<Expression<'a>>),
    DISPTEXT(Box<Expression<'a>>, Box<Expression<'a>>),
    DOINTR([Box<Expression<'a>>;10]),
    DTROFF,
    DTRON,
    ELSE,
    ELSEIF(Box<Expression<'a>>),
    END,
    ENDIF,
    ENDWHILE,
    FAPPEND(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    FCLOSE(Box<Expression<'a>>),
    FCREATE(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    FGET(Box<Expression<'a>>, Box<Expression<'a>>),
    FOPEN(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    FOR(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    FORWARD(Box<Expression<'a>>),
    FPUT(Box<Expression<'a>>, Vec<Expression<'a>>),
    FPUTLN(Box<Expression<'a>>, Option<Vec<Expression<'a>>>),
    FPUTPAD(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    FRESHLINE,
    FREWIND(Box<Expression<'a>>),
    GETTOKEN(Box<Expression<'a>>),
    GETUSER(Box<Expression<'a>>),
    GOODBYE,
    GOSUB(Box<Expression<'a>>),
    GOTO(Box<Expression<'a>>),
    HANGUP,
    IF(Box<Expression<'a>>),
    INC(Box<Expression<'a>>),
    INPUT(Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTDATE(Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTINT(Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTMONEY(Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTSTR([Box<Expression<'a>>;6]),
    INPUTTEXT(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTTIME(Box<Expression<'a>>, Box<Expression<'a>>),
    INPUTYN(Box<Expression<'a>>, Box<Expression<'a>>),
    JOIN(Box<Expression<'a>>),
    KBDCHKOFF,
    KBDCHKON,
    KBDFILE(Box<Expression<'a>>),
    KBDSTUFF(Box<Expression<'a>>),
    LET(Box<Expression<'a>>, Box<Expression<'a>>),
    LOG(Box<Expression<'a>>, Box<Expression<'a>>),
    MESSAGE([Box<Expression<'a>>;9]),
    MORE,
    MPRINT(Vec<Expression<'a>>),
    MPRINTLN(Option<Vec<Expression<'a>>>),
    NEWLINE,
    NEWLINES(Box<Expression<'a>>),
    NEWPWD(Box<Expression<'a>>, Box<Expression<'a>>),
    NEXT,
    OPENCAP(Box<Expression<'a>>, Box<Expression<'a>>),
    OPTEXT(Box<Expression<'a>>),
    PAGEOFF,
    PAGEON,
    POKEB(Box<Expression<'a>>, Box<Expression<'a>>),
    POKEDW(Box<Expression<'a>>, Box<Expression<'a>>),
    POKEW(Box<Expression<'a>>, Box<Expression<'a>>),
    POP(Vec<Expression<'a>>),
    PRINT(Vec<Expression<'a>>),
    PRINTLN(Option<Vec<Expression<'a>>>),
    PROMPTSTR(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    PUSH(Vec<Expression<'a>>),
    PUTUSER,
    QUEST(Box<Expression<'a>>),
    RDUNET(Box<Expression<'a>>),
    RDUSYS,
    RENAME(Box<Expression<'a>>, Box<Expression<'a>>),
    RESETDISP,
    RESTSCRN,
    RETURN,
    SAVESCRN,
    SENDMODEM(Box<Expression<'a>>),
    SHELL(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    SHOWOFF,
    SHOWON,
    SOUND(Box<Expression<'a>>),
    SPRINT(Vec<Expression<'a>>),
    SPRINTLN(Option<Vec<Expression<'a>>>),
    STARTDISP(Box<Expression<'a>>),
    STOP,
    TOKENIZE(Box<Expression<'a>>),
    VARADDR(Box<Expression<'a>>, Box<Expression<'a>>),
    VAROFF(Box<Expression<'a>>, Box<Expression<'a>>),
    VARSEG(Box<Expression<'a>>, Box<Expression<'a>>),
    WAIT,
    WAITFOR(Box<Expression<'a>>, Box<Expression<'a>>, Box<Expression<'a>>),
    WHILE(Box<Expression<'a>>, Box<Statement<'a>>),
    DOWHILE(Box<Expression<'a>>),
    WRUNET([Box<Expression<'a>>;6]),
    WRUSYS
}

pub fn parse_statement5(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(tag_no_case("SAVESCRN"), |_| Statement::SAVESCRN),
        map(separated_pair(tag_no_case("SENDMODEM"), multispace0, parse_expression), |z| Statement::SENDMODEM(Box::new(z.1))),
        map(separated_pair(tag_no_case("SHELL"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::SHELL(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("SHOWOFF"), |_| Statement::SHOWOFF),
        map(tag_no_case("SHOWON"), |_| Statement::SHOWON),
        map(separated_pair(tag_no_case("SOUND"), multispace0, parse_expression), |z| Statement::SOUND(Box::new(z.1))),
        map(separated_pair(tag_no_case("SPRINTLN"), multispace0, opt(parse_exprlist)), |z| Statement::SPRINTLN(z.1)),
        map(separated_pair(tag_no_case("SPRINT"), multispace0, parse_exprlist), |z| Statement::SPRINT(z.1)),
        map(tag_no_case("STOP"), |_| Statement::STOP),
        map(separated_pair(tag_no_case("TOKENIZE"), multispace0, parse_expression), |z| Statement::TOKENIZE(Box::new(z.1))),
        map(separated_pair(tag_no_case("VARADDR"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VARADDR(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("VAROFF"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VAROFF(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("VARSEG"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VARSEG(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("WAITFOR"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::WAITFOR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("WAIT"), |_| Statement::WAIT),
// TODO: WHILE
        map(separated_pair(tag_no_case("WHILE"), multispace0, pair(ws(parse_expression), tag_no_case("DO"))), |z| Statement::DOWHILE(Box::new(z.1.0))),
// TODO: WRUNET
        map(tag_no_case("WRUSYS"), |_| Statement::STOP)
    ))(input)
}

pub fn parse_statement4(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(tag_no_case("PAGEOFF"), |_| Statement::PAGEOFF),
        map(tag_no_case("PAGEON"), |_| Statement::PAGEON),
        map(separated_pair(tag_no_case("POKEB"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEB(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POKEDW"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEDW(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POKEW"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEW(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POP"), multispace0, parse_exprlist), |z| Statement::POP(z.1)),
        map(separated_pair(tag_no_case("PRINTLN"), multispace0, opt(parse_exprlist)), |z| Statement::PRINTLN(z.1)),
        map(separated_pair(tag_no_case("PRINT"), multispace0, parse_exprlist), |z| Statement::PRINT(z.1)),
        map(separated_pair(tag_no_case("PROMPTSTR"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::PROMPTSTR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("PUSH"), multispace0, parse_exprlist), |z| Statement::PUSH(z.1)),
        map(tag_no_case("PUTUSER"), |_| Statement::PUTUSER),
        map(separated_pair(tag_no_case("QUEST"), multispace0, parse_expression), |z| Statement::QUEST(Box::new(z.1))),
        map(separated_pair(tag_no_case("RDUNET"), multispace0, parse_expression), |z| Statement::RDUNET(Box::new(z.1))),
        map(tag_no_case("RDUSYS"), |_| Statement::RDUSYS),
        map(separated_pair(tag_no_case("RENAME"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::RENAME(Box::new(z.1.0), Box::new(z.1.1))),
        map(tag_no_case("RESETDISP"), |_| Statement::RESETDISP),
        map(tag_no_case("RESTSCRN"), |_| Statement::RESTSCRN),
        map(tag_no_case("RETURN"), |_| Statement::RETURN)
    ))(input)
}

pub fn parse_statement3(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(separated_pair(tag_no_case("INPUTTEXT"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTTEXT(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        // TODO:INPUTSTR
        //map(separated_pair(tag_no_case("INPUTSTR"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUT(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("INPUTMONEY"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUTMONEY(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("INPUTINT"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUTINT(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("INPUTDATE"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUTDATE(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("INPUT"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUT(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("JOIN"), multispace0, parse_expression), |z| Statement::JOIN(Box::new(z.1))),
        map(tag_no_case("KBDCHKOFF"), |_| Statement::KBDCHKOFF),
        map(tag_no_case("KBDCHKON"), |_| Statement::KBDCHKON),
        map(separated_pair(tag_no_case("KBDFILE"), multispace0, parse_expression), |z| Statement::KBDFILE(Box::new(z.1))),
        map(separated_pair(tag_no_case("KBDSTUFF"), multispace0, parse_expression), |z| Statement::KBDSTUFF(Box::new(z.1))),
        map(separated_pair(tag_no_case("LET"), multispace0, separated_pair(parse_expression, ws(tag("=")), parse_expression)), |z| Statement::LET(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("LOG"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::LOG(Box::new(z.1.0), Box::new(z.1.1))),
        // TODO:MESSAGE
        //map(separated_pair(tag_no_case("MESSAGE"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUT(Box::new(z.1.0), Box::new(z.1.1))),
        map(tag_no_case("MORE"), |_| Statement::ELSE),
        map(separated_pair(tag_no_case("MPRINTLN"), multispace0, opt(parse_exprlist)), |z| Statement::MPRINTLN(z.1)),
        map(separated_pair(tag_no_case("MPRINT"), multispace0, parse_exprlist), |z| Statement::MPRINT(z.1)),
        map(separated_pair(tag_no_case("NEWLINES"), multispace0, parse_expression), |z| Statement::NEWLINES(Box::new(z.1))),
        map(tag_no_case("NEWLINE"), |_| Statement::NEWLINE),
        map(separated_pair(tag_no_case("NEWPWD"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::NEWPWD(Box::new(z.1.0), Box::new(z.1.1))),
        map(tag_no_case("NEXT"), |_| Statement::NEXT),
        map(separated_pair(tag_no_case("OPENCAP"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::OPENCAP(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("OPTEXT"), multispace0, parse_expression), |z| Statement::OPTEXT(Box::new(z.1)))/*,
        map(tag_no_case("PAGEOFF"), |_| Statement::PAGEOFF),
        map(tag_no_case("PAGEON"), |_| Statement::PAGEON),
        map(separated_pair(tag_no_case("POKEB"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEB(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POKEDW"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEDW(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POKEW"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::POKEW(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("POP"), multispace0, parse_exprlist), |z| Statement::POP(z.1)),
        map(separated_pair(tag_no_case("PRINTLN"), multispace0, opt(parse_exprlist)), |z| Statement::PRINTLN(z.1)),
        map(separated_pair(tag_no_case("PRINT"), multispace0, parse_exprlist), |z| Statement::PRINT(z.1)),
        map(separated_pair(tag_no_case("PROMPTSTR"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::PROMPTSTR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("PUSH"), multispace0, parse_exprlist), |z| Statement::PUSH(z.1)),
        map(tag_no_case("PUTUSER"), |_| Statement::PUTUSER),
        map(separated_pair(tag_no_case("QUEST"), multispace0, parse_expression), |z| Statement::QUEST(Box::new(z.1))),
        map(separated_pair(tag_no_case("RDUNET"), multispace0, parse_expression), |z| Statement::RDUNET(Box::new(z.1))),
        map(tag_no_case("RDUSYS"), |_| Statement::RDUSYS),
        map(separated_pair(tag_no_case("RENAME"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::RENAME(Box::new(z.1.0), Box::new(z.1.1))),
        map(tag_no_case("RESETDISP"), |_| Statement::RESETDISP),
        map(tag_no_case("RESTSCRN"), |_| Statement::RESTSCRN),
        map(tag_no_case("RETURN"), |_| Statement::RETURN),
        map(tag_no_case("SAVESCRN"), |_| Statement::SAVESCRN),
        map(separated_pair(tag_no_case("SENDMODEM"), multispace0, parse_expression), |z| Statement::SENDMODEM(Box::new(z.1))),
        map(separated_pair(tag_no_case("SHELL"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::SHELL(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("SHOWOFF"), |_| Statement::SHOWOFF),
        map(tag_no_case("SHOWON"), |_| Statement::SHOWON),
        map(separated_pair(tag_no_case("SOUND"), multispace0, parse_expression), |z| Statement::SOUND(Box::new(z.1))),
        map(separated_pair(tag_no_case("SPRINTLN"), multispace0, opt(parse_exprlist)), |z| Statement::SPRINTLN(z.1)),
        map(separated_pair(tag_no_case("SPRINT"), multispace0, parse_exprlist), |z| Statement::SPRINT(z.1)),
        map(tag_no_case("STOP"), |_| Statement::STOP),
        map(separated_pair(tag_no_case("TOKENIZE"), multispace0, parse_expression), |z| Statement::TOKENIZE(Box::new(z.1))),
        map(separated_pair(tag_no_case("VARADDR"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VARADDR(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("VAROFF"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VAROFF(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("VARSEG"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::VARSEG(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("WAITFOR"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::WAITFOR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("WAIT"), |_| Statement::WAIT),
// TODO: WHILE
        map(separated_pair(tag_no_case("WHILE"), multispace0, pair(ws(parse_expression), tag_no_case("DO"))), |z| Statement::DOWHILE(Box::new(z.1.0))),
// TODO: WRUNET
        map(tag_no_case("WRUSYS"), |_| Statement::STOP)*/
    ))(input)
}

pub fn parse_statement2(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(separated_pair(tag_no_case("FAPPEND"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::FAPPEND(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("FCLOSE"), multispace0, parse_expression), |z| Statement::FCLOSE(Box::new(z.1))),
        map(separated_pair(tag_no_case("FCREATE"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::FCREATE(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("FGET"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::FGET(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("FOPEN"), multispace0, pair(parse_expression, many_m_n(3, 3, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::FOPEN(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        // TODO: FOR
        map(separated_pair(tag_no_case("FORWARD"), multispace0, parse_expression), |z| Statement::FORWARD(Box::new(z.1))),
        map(separated_pair(tag_no_case("FPUTLN"), multispace0, parse_exprlist), |mut z| Statement::FPUTLN(Box::new(z.1.remove(0)), Some(z.1))),
        map(separated_pair(tag_no_case("FPUT"), multispace0, parse_exprlist), |mut z| Statement::FPUT(Box::new(z.1.remove(0)), z.1)),
        map(separated_pair(tag_no_case("FPUTPAD"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::FPUTPAD(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("FRESHLINE"), |_| Statement::END),
        map(separated_pair(tag_no_case("FREWIND"), multispace0, parse_expression), |z| Statement::FREWIND(Box::new(z.1))),
        map(separated_pair(tag_no_case("GETTOKEN"), multispace0, parse_expression), |z| Statement::GETTOKEN(Box::new(z.1))),
        map(separated_pair(tag_no_case("GETUSER"), multispace0, parse_expression), |z| Statement::GETUSER(Box::new(z.1))),
        map(tag_no_case("GOODBYE"), |_| Statement::END),
        map(separated_pair(tag_no_case("GOSUB"), multispace0, parse_expression), |z| Statement::GOSUB(Box::new(z.1))),
        map(separated_pair(tag_no_case("GOTO"), multispace0, parse_expression), |z| Statement::GOTO(Box::new(z.1))),
        map(tag_no_case("HANGUP"), |_| Statement::END),
        map(separated_pair(tag_no_case("IF"), multispace0, pair(ws(parse_expression), tag_no_case("THEN"))), |z| Statement::IF(Box::new(z.1.0))),
        map(separated_pair(tag_no_case("INC"), multispace0, parse_expression), |z| Statement::INC(Box::new(z.1))),
        map(separated_pair(tag_no_case("INPUTYN"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUTYN(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("INPUTTIME"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::INPUTTIME(Box::new(z.1.0), Box::new(z.1.1)))
    ))(input)
}

pub fn parse_statement1(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(separated_pair(tag_no_case("DBGLEVEL"), multispace0, parse_expression), |z| Statement::DBGLEVEL(Box::new(z.1))),
        map(separated_pair(tag_no_case("DEC"), multispace0, parse_expression), |z| Statement::DEC(Box::new(z.1))),
        map(tag_no_case("DEFCOLOR"), |_| Statement::DEFCOLOR),
        map(separated_pair(tag_no_case("DELAY"), multispace0, parse_expression), |z| Statement::DELAY(Box::new(z.1))),
        map(separated_pair(tag_no_case("DELETE"), multispace0, parse_expression), |z| Statement::DELETE(Box::new(z.1))),
        map(tag_no_case("DELUSER"), |_| Statement::DELUSER),
        map(separated_pair(tag_no_case("DIR"), multispace0, parse_expression), |z| Statement::DIR(Box::new(z.1))),
        map(separated_pair(tag_no_case("DISPFILE"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::DISPFILE(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("DISPSTR"), multispace0, parse_expression), |z| Statement::DISPSTR(Box::new(z.1))),
        map(separated_pair(tag_no_case("DISPTEXT"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::DISPTEXT(Box::new(z.1.0), Box::new(z.1.1))),

// TODO:DOINTR
// map(separated_pair(tag_no_case("DOINTR"), multispace0, pair(parse_expression, many_m_n(9, 9, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::DOINTR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("DTROFF"), |_| Statement::DTROFF),
        map(tag_no_case("DTRON"), |_| Statement::DTRON),
        map(tag_no_case("ELSE"), |_| Statement::ELSE),
        map(separated_pair(tag_no_case("ELSEIF"), multispace0, parse_expression), |z| Statement::ELSEIF(Box::new(z.1))),
        map(tag_no_case("ENDWHILE"), |_| Statement::ENDWHILE),
        map(tag_no_case("ENDIF"), |_| Statement::ENDIF),
        map(tag_no_case("END"), |_| Statement::END)
    ))(input)
}

pub fn parse_statement(input : &str) -> nom::IResult<&str, Statement>
{
    alt ((
        map(separated_pair(tag_no_case("ADJTIME"), multispace0, parse_expression), |z| Statement::ADJTIME(Box::new(z.1))),
        map(separated_pair(tag_no_case("ANSIPOS"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::ANSIPOS(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("BACKUP"), multispace0, parse_expression), |z| Statement::BACKUP(Box::new(z.1))),
        map(separated_pair(tag_no_case("BLT"), multispace0, parse_expression), |z| Statement::BLT(Box::new(z.1))),
        map(separated_pair(tag_no_case("BROADCAST"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::BROADCAST(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("BYE"), |_| Statement::BYE),
        map(separated_pair(tag_no_case("CALL"), multispace0, parse_expression), |z| Statement::CALL(Box::new(z.1))),
        map(tag_no_case("CDCHKOFF"), |_| Statement::CDCHKOFF),
        map(tag_no_case("CDCHKON"), |_| Statement::CDCHKON),
        map(tag_no_case("CHAT"), |_| Statement::CHAT),
        map(tag_no_case("CLOSECAP"), |_| Statement::CLOSECAP),
        map(tag_no_case("CLREOL"), |_| Statement::CLREOL),
        map(tag_no_case("CLS"), |_| Statement::CLS),
        map(separated_pair(tag_no_case("COLOR"), multispace0, parse_expression), |z| Statement::COLOR(Box::new(z.1))),
        map(separated_pair(tag_no_case("CONFFLAG"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::CONFFLAG(Box::new(z.1.0), Box::new(z.1.1))),
        map(separated_pair(tag_no_case("CONFUNFLAG"), multispace0, separated_pair(parse_expression, ws(tag(",")), parse_expression)), |z| Statement::CONFUNFLAG(Box::new(z.1.0), Box::new(z.1.1))),
        parse_statement1,
        parse_statement2,
        parse_statement3,
        parse_statement4,
        parse_statement5,
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_statement() {
        assert_eq!(Ok(("", Statement::ADJTIME(Box::new(Expression::Const(Constant::Integer(1)))))), parse_statement("ADJTIME 1"));
        assert_eq!(Ok(("", Statement::ANSIPOS(Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(2)))))), parse_statement("ANSIPOS 1, 2"));
        assert_eq!(Ok(("", Statement::BACKUP(Box::new(Expression::Const(Constant::Integer(1)))))), parse_statement("BACKUP 1"));
        assert_eq!(Ok(("", Statement::BLT(Box::new(Expression::Const(Constant::Integer(1)))))), parse_statement("BLT 1"));
        assert_eq!(Ok(("", Statement::BROADCAST(Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(3)))))), parse_statement("BROADCAST 1, 2, 3"));
        assert_eq!(Ok(("", Statement::BYE)), parse_statement("BYE"));
        assert_eq!(Ok(("", Statement::CALL(Box::new(Expression::Const(Constant::String("911")))))), parse_statement("CALL \"911\""));

        assert_eq!(Ok(("", Statement::CDCHKOFF)), parse_statement("CDCHKOFF"));
        assert_eq!(Ok(("", Statement::CDCHKON)), parse_statement("CDCHKON"));
        assert_eq!(Ok(("", Statement::CHAT)), parse_statement("CHAT"));
        assert_eq!(Ok(("", Statement::CLOSECAP)), parse_statement("CLOSECAP"));
        assert_eq!(Ok(("", Statement::CLREOL)), parse_statement("CLREOL"));
        assert_eq!(Ok(("", Statement::CLS)), parse_statement("CLS"));
        assert_eq!(Ok(("", Statement::COLOR(Box::new(Expression::Const(Constant::Integer(1)))))), parse_statement("COLOR 1"));
        assert_eq!(Ok(("", Statement::CONFFLAG(Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::F_REG))))), parse_statement("CONFFLAG 1, F_REG"));
        assert_eq!(Ok(("", Statement::CONFUNFLAG(Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::F_REG))))), parse_statement("CONFUNFLAG 1, F_REG"));


        assert_eq!(Ok(("", Statement::PRINT(vec![
            Expression::Const(Constant::Integer(1)),
            Expression::Const(Constant::Integer(2)),
            Expression::Const(Constant::Integer(3))
        ]))), parse_statement("PRINT 1, 2, 3"));
        assert_eq!(Ok(("", Statement::PRINTLN(Some(vec![])))), parse_statement("PRINTLN"));
        assert_eq!(Ok(("", Statement::PRINTLN(Some(
            vec![
                Expression::Const(Constant::String("Hello World")),
            ]
        )))), parse_statement("PRINTLN \"Hello World\""));

    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(Ok(("", Expression::Const(Constant::Money(42.42)))), parse_expression("$42.42"));
        assert_eq!(Ok(("", Expression::Parens(Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("(5)"));
        assert_eq!(Ok(("", Expression::Not(Box::new(Expression::Const(Constant::FALSE))))), parse_expression("!FALSE"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABORT", Vec::new()))), parse_expression("ABORT()"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABS", vec!(Expression::Const(Constant::Integer(5)))))), parse_expression("ABS(5)"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::PoW,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2^5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Mul,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2*5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Div,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2/5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Mod,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2%5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Add,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2+5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Sub,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2-5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Eq,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 = 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::NotEq,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 <> 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Lower,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 < 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::LowerEq,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 <= 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Greater,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 > 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::GreaterEq,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 >= 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::And,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 & 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Or,Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 | 5"));
    }

    #[test]
    fn test_parse_constants() {
        assert_eq!(Ok(("", Constant::Money(42.42))), parse_constant("$42.42"));
        assert_eq!(Ok(("", Constant::Integer(123))), parse_constant("123"));
        assert_eq!(Ok(("", Constant::Integer(123))), parse_constant("+123"));
        assert_eq!(Ok(("", Constant::Integer(-456))), parse_constant("-456"));
        assert_eq!(Ok(("", Constant::String("Hello World"))), parse_constant("\"Hello World\""));
        assert_eq!(Ok(("", Constant::Integer(0x1E))), parse_constant("@X1E"));
        assert_eq!(Ok(("", Constant::Integer(0b1010))), parse_constant("1010b"));
        assert_eq!(Ok(("", Constant::Integer(0o5234))), parse_constant("5234o"));
        assert_eq!(Ok(("", Constant::Integer(4711))), parse_constant("4711d"));
        assert_eq!(Ok(("", Constant::Integer(0xAFFE))), parse_constant("AFFEh"));
        assert_eq!(Ok(("", Constant::TRUE)), parse_constant("TRUE"));
        assert_eq!(Ok(("", Constant::FALSE)), parse_constant("FALSE"));
    }

    #[test]
    fn test_parse_declarations() {
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Boolean, "VAR001"))), parse_declaration("BOOLEAN VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Integer, "VAR001"))), parse_declaration("INTEGER VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Date, "VAR001"))), parse_declaration("DATE VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::String, "VAR001"))), parse_declaration("STRING VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Money, "VAR001"))), parse_declaration("MONEY VAR001"));

        assert_eq!(Ok(("", Declaration::Procedure("PROC001", vec![]))), parse_declaration("DECLARE PROCEDURE PROC001()"));
        assert_eq!(Ok(("", Declaration::Function("FUNC001", vec![Declaration::Variable(VariableType::Integer, "LOC001")], VariableType::Integer))), parse_declaration("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER"));
    }

}