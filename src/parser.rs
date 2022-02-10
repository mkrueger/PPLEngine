use nom::{branch::alt, bytes::complete::{ tag, tag_no_case, take_while}, character::complete::{alpha1, space1,  alphanumeric1, multispace0}, combinator::{cut, map, map_res, opt, recognize, value}, error::{ ParseError}, multi::*,  sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult};
use nom::bytes::complete::{ take_while_m_n};
use nom::character::complete::{ multispace1};
use nom::sequence::tuple;
use std::str::FromStr;
use std::string::String;

use crate::executable::VariableType;

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Variable(VariableType, String),
    Variable1(VariableType, String, i32),
    Variable2(VariableType, String, i32, i32),
    Variable3(VariableType, String, i32, i32, i32),
    Function(String, Vec<Declaration>, VariableType),
    Procedure(String, Vec<Declaration>)
}

impl Declaration {
    fn declr_vec_to_string(list : &Vec<Declaration>) -> String
    {
        let mut res = String::new();
        let mut first = false;
        for decl in list {
            if first {
                first = false;
            } else {
                res.push_str(", ");
            }
            res.push_str(decl.to_string().as_str());
        }

        res
    }

    pub fn to_string(&self) -> String
    {
        match self {
            Declaration::Variable(var_type, name) => format!("{} {}", var_type.to_string(), name),
            Declaration::Variable1(var_type, name, dim1) => format!("{} {}({})", var_type.to_string(), name, dim1),
            Declaration::Variable2(var_type, name, dim1, dim2) => format!("{} {}({}, {})", var_type.to_string(), name, dim1, dim2),
            Declaration::Variable3(var_type, name, dim1, dim2, dim3) => format!("{} {}({}, {}, {})", var_type.to_string(), name, dim1, dim2, dim3),
            Declaration::Procedure(name, parameters) => format!("DECLARE PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => format!("DECLARE FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type.to_string()),
        }
    }
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
            Declaration::Variable(name.0, String::from(name.1))
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
        Ok(z) =>  nom::IResult::Ok((z.0, Declaration::Function(String::from(z.1.2), z.1.4, z.1.6))),
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
        Ok(z) =>  nom::IResult::Ok((z.0, Declaration::Procedure(String::from(z.1.2), z.1.4))),
        Err(e) => Err(e)
    }
}

fn parse_declaration<'a>(input : &'a str) -> nom::IResult<&'a str, Declaration>
{
    alt((parse_variable, parse_functiondeclaration, parse_proceduredeclaration))(input)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Constant
{
    Money(f64),
    Integer(i32),
    String(String),
    Double(f64),
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
    SEEK_CUR,
    SEEK_END,
    SEEK_SET,
    STACKED,
    S_DB,
    S_DN,
    S_DR,
    S_DW,
    TRUE,
    UPCASE,
    WORDWRAP,
    YESNO,

    // Debug
    START_BAL,
    START_SESSION,
    DEB_CALL,
    DEB_TIME,
    DEB_MSGREAD,
    DEB_MSGCAP,
    DEB_MSGWRITE,
    DEB_MSGECHOED,
    DEB_MSGPRIVATE,
    DEB_DOWNFILE,
    DEB_DOWNBYTES,
    DEB_CHAT,
    DEB_TPU,
    DEB_SPECIAL,
    CRED_UPFILE,
    CRED_UPBYTES,
    CRED_SPECIAL,
    SEC_DROP
}
impl Constant
{
    pub fn to_string(&self) -> String
    {
        match self {
            Constant::Money(f) => format!("${}", f),
            Constant::Integer(i) => format!("{}", i),
            Constant::String(str) => format!("\"{}\"", str),
            Constant::Double(f) => format!("{}", f),

            Constant::AUTO => String::from("AUTO"),
            Constant::BELL => String::from("BELL"),
            Constant::DEFS => String::from("DEFS"),
            Constant::ECHODOTS => String::from("ECHODOTS"),
            Constant::ERASELINE => String::from("ERASELINE"),
            Constant::FALSE => String::from("FALSE"),
            Constant::FCL => String::from("FCL"),
            Constant::FIELDLEN => String::from("FIELDLEN"),
            Constant::FNS => String::from("FNS"),
            Constant::F_EXP => String::from("F_EXP"),
            Constant::F_MW => String::from("F_MW"),
            Constant::F_REG => String::from("F_REG"),
            Constant::F_SEL => String::from("F_SEL"),
            Constant::F_SYS => String::from("F_SYS"),
            Constant::GRAPH => String::from("GRAPH"),
            Constant::GUIDE => String::from("GUIDE"),
            Constant::HIGHASCII => String::from("HIGHASCII"),
            Constant::LANG => String::from("LANG"),
            Constant::LFAFTER => String::from("LFAFTER"),
            Constant::LFBEFORE => String::from("LFBEFORE"),
            Constant::LOGIT => String::from("LOGIT"),
            Constant::LOGITLEFT => String::from("LOGITLEFT"),
            Constant::NC => String::from("NC"),
            Constant::NEWLINE => String::from("NEWLINE"),
            Constant::NOCLEAR => String::from("NOCLEAR"),
            Constant::O_RD => String::from("O_RD"),
            Constant::O_RW => String::from("O_RW"),
            Constant::O_WR => String::from("O_WR"),
            Constant::SEC => String::from("SEC"),
            Constant::STACKED => String::from("STACKED"),
            Constant::S_DB => String::from("S_DB"),
            Constant::S_DN => String::from("S_DN"),
            Constant::S_DR => String::from("S_DR"),
            Constant::S_DW => String::from("S_DW"),
            Constant::TRUE => String::from("TRUE"),
            Constant::UPCASE => String::from("UPCASE"),
            Constant::WORDWRAP => String::from("WORDWRAP"),
            Constant::YESNO => String::from("YESNO"),

            Constant::SEEK_CUR => String::from("SEEK_CUR"),
            Constant::SEEK_END => String::from("SEEK_END"),
            Constant::SEEK_SET => String::from("SEEK_SET"),

            Constant::START_BAL => String::from("?"),
            Constant::START_SESSION => String::from("?"),
            Constant::DEB_CALL => String::from("?"),
            Constant::DEB_TIME => String::from("?"),
            Constant::DEB_MSGREAD => String::from("?"),
            Constant::DEB_MSGCAP => String::from("?"),
            Constant::DEB_MSGWRITE => String::from("?"),
            Constant::DEB_MSGECHOED => String::from("?"),
            Constant::DEB_MSGPRIVATE => String::from("?"),
            Constant:: DEB_DOWNFILE => String::from("?"),
            Constant::DEB_DOWNBYTES => String::from("?"),
            Constant::DEB_CHAT => String::from("?"),
            Constant::DEB_TPU => String::from("?"),
            Constant::DEB_SPECIAL => String::from("?"),
            Constant::CRED_UPFILE => String::from("?"),
            Constant::CRED_UPBYTES => String::from("?"),
            Constant::CRED_SPECIAL => String::from("?"),
            Constant::SEC_DROP => String::from("?")
        }
    }
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
        map(tag_no_case("SEEK_CUR"), |_| Constant::SEEK_CUR),
        map(tag_no_case("SEEK_END"), |_| Constant::SEEK_END),
        map(tag_no_case("SEEK_SET"), |_| Constant::SEEK_SET),
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
            |s: &'a str| Constant::String(s.to_string())),
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

impl BinOp
{
    pub fn to_string(&self) -> String
    {
        match self {
            BinOp::PoW => "^".to_string(),
            BinOp::Mul => "*".to_string(),
            BinOp::Div => "/".to_string(),
            BinOp::Mod => "%".to_string(),
            BinOp::Add => "+".to_string(),
            BinOp::Sub => "-".to_string(),
            BinOp::Eq => "=".to_string(),
            BinOp::NotEq => "<>".to_string(),
            BinOp::Lower => "<".to_string(),
            BinOp::LowerEq => "<=".to_string(),
            BinOp::Greater => ">".to_string(),
            BinOp::GreaterEq => ">=".to_string(),
            BinOp::And => "&".to_string(),
            BinOp::Or => "|".to_string()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression
{
    Identifier(String),
    Const(Constant),
    Parens(Box<Expression>),
    FunctionCall(String, Vec<Expression>),
    Not(Box<Expression>),
    Minus(Box<Expression>),
    BinaryExpression(BinOp, Box<Expression>, Box<Expression>)
}

impl Expression
{
    pub fn to_string(&self) -> String
    {
        match self {
            Expression::Identifier(id) => id.to_string(),
            Expression::Const(c) => c.to_string(),
            Expression::Parens(expr) => format!("({})", (**expr).to_string()),
            Expression::Not(expr) => format!("!{}", (**expr).to_string()),
            Expression::Minus(expr) => format!("-{}", (**expr).to_string()),
            Expression::BinaryExpression(op, lexpr, rexpr) => format!("{} {} {}", (**lexpr).to_string(), op.to_string(), (**rexpr).to_string()),
            Expression::FunctionCall(name, params) => format!("{}({})", name, Statement::param_list_to_string(params))
        }
    }
}

fn parse_parenexpr<'a>(input: &'a str) -> nom::IResult<&'a str, Expression>
{
    map(delimited(
        tag("("),
        ws(parse_expression),
        tag(")")),
        |s: Expression| Expression::Parens(Box::new(s)))(input)
}

fn parse_exprlist<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Expression>>
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

fn parse_parameterlist<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Expression>>
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
fn fold_binop<'a>(left : Expression, remainder: Vec<(BinOp, Box<Expression>)>) -> Expression
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

fn parse_expression<'a>(line : &'a str) -> nom::IResult<&'a str, Expression> {
    let unary = alt ((
        parse_parenexpr,
        // function call
        map(separated_pair(identifier, multispace0, parse_parameterlist),
            |z| Expression::FunctionCall(z.0.to_string(), z.1)),
        // !expr
        map(separated_pair(tag("!"),  multispace0, parse_expression),
            |z| Expression::Not(Box::new(z.1))),
        map(separated_pair(tag("-"),  multispace0, parse_expression),
            |z| Expression::Minus(Box::new(z.1))),
        map(parse_constant, |z| Expression::Const(z)),
        map(alphanumeric1, |z : &str| Expression::Identifier(z.to_string()))
    ));

    map(pair(
        unary,
        many0(pair(parse_binop, map(parse_expression, |z| Box::new(z))))
    ),
    |z| fold_binop(z.0, z.1)
     )(line)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    Comment(String),
    ADJTIME(Box<Expression>),
    ANSIPOS(Box<Expression>, Box<Expression>),
    BACKUP(Box<Expression>),
    BLT(Box<Expression>),
    BYE,
    BEEP,
    BROADCAST(Box<Expression>, Box<Expression>, Box<Expression>),
    CALL(Box<Expression>),
    CDCHKOFF,
    CDCHKON,
    CHAT,
    CLOSECAP,
    CLREOL,
    CLS,
    COLOR(Box<Expression>),
    CONFFLAG(Box<Expression>, Box<Expression>),
    CONFUNFLAG(Box<Expression>, Box<Expression>),
    DBGLEVEL(Box<Expression>),
    DEC(Box<Expression>),
    DEFCOLOR,
    DELAY(Box<Expression>),
    DELETE(Box<Expression>),
    DELUSER,
    DIR(Box<Expression>),
    DISPFILE(Box<Expression>, Box<Expression>),
    DISPSTR(Box<Expression>),
    DISPTEXT(Box<Expression>, Box<Expression>),
    DOINTR([Box<Expression>;10]),
    DTROFF,
    DTRON,
    ELSE,
    ELSEIF(Box<Expression>),
    END,
    ENDIF,
    ENDWHILE,
    FAPPEND(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    FCLOSE(Box<Expression>),
    FCREATE(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    FGET(Box<Expression>, Box<Expression>),
    FOPEN(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    FOR(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    FORWARD(Box<Expression>),
    FPUT(Box<Expression>, Vec<Expression>),
    FPUTLN(Box<Expression>, Option<Vec<Expression>>),
    FPUTPAD(Box<Expression>, Box<Expression>, Box<Expression>),
    FRESHLINE,
    FREWIND(Box<Expression>),
    GETTOKEN(Box<Expression>),
    GETUSER,
    GOODBYE,
    GOSUB(Box<Expression>),
    GOTO(Box<Expression>),
    HANGUP,
    IF(Box<Expression>),
    INC(Box<Expression>),
    INPUT(Box<Expression>, Box<Expression>),
    INPUTDATE(Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTINT(Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTMONEY(Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTSTR([Box<Expression>;6]),
    INPUTTEXT(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTTIME(Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTYN(Box<Expression>, Box<Expression>, Box<Expression>),
    INPUTCC(Box<Expression>, Box<Expression>, Box<Expression>),
    JOIN(Box<Expression>),
    KBDCHKOFF,
    KBDCHKON,
    KBDFILE(Box<Expression>),
    KBDSTUFF(Box<Expression>),
    LET(Box<Expression>, Box<Expression>),
    LOG(Box<Expression>, Box<Expression>),
    MESSAGE([Box<Expression>;9]),
    MORE,
    MPRINT(Vec<Expression>),
    MPRINTLN(Option<Vec<Expression>>),
    NEWLINE,
    NEWLINES(Box<Expression>),
    NEWPWD(Box<Expression>, Box<Expression>),
    NEXT,
    OPENCAP(Box<Expression>, Box<Expression>),
    OPTEXT(Box<Expression>),
    PAGEOFF,
    PAGEON,
    POKEB(Box<Expression>, Box<Expression>),
    POKEDW(Box<Expression>, Box<Expression>),
    POKEW(Box<Expression>, Box<Expression>),
    POP(Vec<Expression>),
    PRINT(Vec<Expression>),
    PRINTLN(Option<Vec<Expression>>),
    PROMPTSTR(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    PUSH(Vec<Expression>),
    PUTUSER,
    QUEST(Box<Expression>),
    RDUNET(Box<Expression>),
    RDUSYS,
    RENAME(Box<Expression>, Box<Expression>),
    RESETDISP,
    RESTSCRN,
    RETURN,
    SAVESCRN,
    SENDMODEM(Box<Expression>),
    SHELL(Box<Expression>, Box<Expression>, Box<Expression>, Box<Expression>),
    SHOWOFF,
    SHOWON,
    SOUND(Box<Expression>),
    SPRINT(Vec<Expression>),
    SPRINTLN(Option<Vec<Expression>>),
    STARTDISP(Box<Expression>),
    STOP,
    TOKENIZE(Box<Expression>),
    VARADDR(Box<Expression>, Box<Expression>),
    VAROFF(Box<Expression>, Box<Expression>),
    VARSEG(Box<Expression>, Box<Expression>),
    WAIT,
    WAITFOR(Box<Expression>, Box<Expression>, Box<Expression>),
    WHILE(Box<Expression>, Box<Statement>),
    DOWHILE(Box<Expression>),
    WRUNET([Box<Expression>;6]),
    WRUSYS
}

impl Statement
{
    pub fn param_list_to_string(l : &Vec<Expression>) -> String
    {
     /*   match list {
            Some(l) => {*/
                let mut res = String::new();
                for expr in l {
                    if res.len() > 0 {
                        res.push_str(", ");
                    }
                    res.push_str(expr.to_string().as_str());
                }
                res
  /*          },
            _ => "".to_string()
        }*/
    }

    pub fn to_string(&self) -> String
    {
        match self {
            Statement::Comment(str) => format!(";{}", str),
            Statement::ADJTIME(t) => format!("ADJTIME {}", (**t).to_string()),
            Statement::ANSIPOS(t, t1) => format!("ANSIPOS {}, {}", (**t).to_string(), (*t1).to_string()),
            Statement::BACKUP(t) => format!("BACKUP {}", (**t).to_string()),
            Statement::BLT(t) => format!("BLT {}", (**t).to_string()),
            Statement::BEEP => "BEEP".to_string(),
            Statement::BYE => "BYE".to_string(),
            Statement::BROADCAST(t, t1, t2) => format!("BROADCAST {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::CALL(t) => format!("CALL {}", (**t).to_string()),
            Statement::CDCHKOFF => "CDCHKOFF".to_string(),
            Statement::CDCHKON => "CDCHKON".to_string(),
            Statement::CHAT => "CHAT".to_string(),
            Statement::CLOSECAP => "CLOSECAP".to_string(),
            Statement::CLREOL => "CLREOL".to_string(),
            Statement::CLS => "CLS".to_string(),
            Statement::COLOR(t) => format!("COLOR {}", (**t).to_string()),
            Statement::CONFFLAG(t, t1) => format!("CONFFLAG {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::CONFUNFLAG(t, t1) => format!("CONFUNFLAG {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::DBGLEVEL(t) => format!("DBGLEVEL {}", (**t).to_string()),
            Statement::DEC(t) => format!("DEC {}", (**t).to_string()),
            Statement::DEFCOLOR => "DEFCOLOR".to_string(),
            Statement::DELAY(t) => format!("DELAY {}", (**t).to_string()),
            Statement::DELETE(t) => format!("DELETE {}", (**t).to_string()),
            Statement::DELUSER => "DELUSER".to_string(),
            Statement::DIR(t) => format!("DIR {}", (**t).to_string()),
            Statement::DISPFILE(t, t1) => format!("DISPFILE {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::DISPSTR(t) => format!("DISPSTR {}", (**t).to_string()),
            Statement::DISPTEXT(t, t1) => format!("DISPTEXT {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::DOINTR(t) => format!("DOINTR {}, {}, {}, {}, {}, {}, {}, {}, {}, {}", (t[0]).to_string(), (t[1]).to_string(), (t[2]).to_string(), (t[3]).to_string(), (t[4]).to_string(), (t[5]).to_string(), (t[6]).to_string(), (t[7]).to_string(), (t[8]).to_string(), (t[9]).to_string()),
            Statement::DTROFF => "DTROFF".to_string(),
            Statement::DTRON => "DTRON".to_string(),
            Statement::ELSE => "ELSE".to_string(),
            Statement::ELSEIF(t) => format!("ELSEIF {}", (**t).to_string()),
            Statement::END => "END".to_string(),
            Statement::ENDIF => "ENDIF".to_string(),
            Statement::ENDWHILE => "ENDWHILE".to_string(),
            Statement::FAPPEND(t, t1, t2, t3) => format!("FAPPEND {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string()),
            Statement::FCLOSE(t) => format!("FCLOSE {}", (**t).to_string()),
            Statement::FCREATE(t, t1, t2, t3) => format!("FCREATE {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string()),
            Statement::FGET(t, t1) => format!("FGET {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::FOPEN(t, t1, t2, t3) => format!("FOPEN {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string()),
            Statement::FOR(t, t1, t2, t3) => format!("FOR {} = {} TO {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::FORWARD(t) => format!("FORWARD {}", (**t).to_string()),
            Statement::FPUT(t, t1) => format!("FPUT {}, {}", (**t).to_string(), Statement::param_list_to_string(t1)),
            Statement::FPUTLN(t, t1) => match t1 { Some(l) =>  format!("FPUTLN {}, {}", (**t).to_string(), Statement::param_list_to_string(l)), None =>  format!("FPUTLN {}", (**t).to_string())},
            Statement::FPUTPAD(t, t1, t2) => format!("FPUTPAD {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::FRESHLINE => "FRESHLINE".to_string(),
            Statement::FREWIND(t) => format!("FREWIND {}", (**t).to_string()),
            Statement::GETTOKEN(t) => format!("GETTOKEN {}", (**t).to_string()),
            Statement::GETUSER => "GETUSER".to_string(),
            Statement::GOODBYE => "GOODBYE".to_string(),
            Statement::GOSUB(t) => format!("GOSUB {}", (**t).to_string()),
            Statement::GOTO(t) => format!("GOTO {}", (**t).to_string()),
            Statement::HANGUP => "HANGUP".to_string(),
            Statement::IF(t) => format!("IF ({})", (**t).to_string()),
            Statement::INC(t) => format!("INC {}", (**t).to_string()),
            Statement::INPUT(t, t1) => format!("INPUT {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::INPUTDATE(t, t1, t2) => format!("INPUTDATE {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::INPUTINT(t, t1, t2) => format!("INPUTINT {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::INPUTMONEY(t, t1, t2) => format!("INPUTMONEY {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::INPUTSTR(t) => format!("INPUTSTR {}, {}, {}, {}, {}, {}", (t[0]).to_string(), (t[1]).to_string(), (t[2]).to_string(), (t[3]).to_string(), (t[4]).to_string(), (t[5]).to_string()),
            Statement::INPUTTEXT(t, t1, t2, t3) => format!("INPUTTEXT {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string()),
            Statement::INPUTTIME(t, t1, t2) => format!("INPUTTIME {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::INPUTYN(t, t1, t2) => format!("INPUTYN {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::INPUTCC(t, t1, t2) => format!("INPUTCC {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::JOIN(t) => format!("JOIN {}", (**t).to_string()),
            Statement::KBDCHKOFF => "KBDCHKOFF".to_string(),
            Statement::KBDCHKON => "KBDCHKON".to_string(),
            Statement::KBDFILE(t) => format!("KBDFILE {}", (**t).to_string()),
            Statement::KBDSTUFF(t) => format!("KBDSTUFF {}", (**t).to_string()),
            Statement::LET(t, t1) => format!("LET {} = {}", (**t).to_string(), (**t1).to_string()),
            Statement::LOG(t, t1) => format!("LOG {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::MESSAGE(t) => format!("MESSAGE {}, {}, {}, {}, {}, {}, {}, {}, {}, {}", (t[0]).to_string(), (t[0]).to_string(), (t[1]).to_string(), (t[2]).to_string(), (t[3]).to_string(), (t[4]).to_string(), (t[5]).to_string(), (t[6]).to_string(), (t[7]).to_string(), (t[8]).to_string()),
            Statement::MORE => "MORE".to_string(),
            Statement::MPRINT(t) => format!("MPRINT {}", Statement::param_list_to_string(t)),
            Statement::MPRINTLN(t) => match t { Some(l) => format!("MPRINTLN {}", Statement::param_list_to_string(l)), None => "MPRINTLN".to_string() },
            Statement::NEWLINE => "NEWLINE".to_string(),
            Statement::NEWLINES(t) => format!("NEWLINES {}", (**t).to_string()),
            Statement::NEWPWD(t, t1) => format!("NEWPWD {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::NEXT => "NEXT".to_string(),
            Statement::OPENCAP(t, t1) => format!("OPENCAP {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::OPTEXT(t) => format!("OPTEXT {}", (**t).to_string()),
            Statement::PAGEOFF => "PAGEOFF".to_string(),
            Statement::PAGEON => "PAGEON".to_string(),
            Statement::POKEB(t, t1) => format!("POKEB {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::POKEDW(t, t1) => format!("POKEDW {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::POKEW(t, t1) => format!("POKEW {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::POP(t) => format!("POP {}", Statement::param_list_to_string(t)),
            Statement::PRINT(t) => format!("PRINT {}", Statement::param_list_to_string(t)),
            Statement::PRINTLN(t) => match t { Some(l) => format!("PRINTLN {}", Statement::param_list_to_string(l)), None => "PRINTLN".to_string() },
            Statement::PROMPTSTR(t, t1, t2, t3, t4) => format!("PROMPTSTR {}, {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string(), (**t4).to_string()),
            Statement::PUSH(t) => format!("PUSH {}", Statement::param_list_to_string(t)),
            Statement::PUTUSER => "PUTUSER".to_string(),
            Statement::QUEST(t) => format!("QUEST {}", (**t).to_string()),
            Statement::RDUNET(t) => format!("RDUNET {}", (**t).to_string()),
            Statement::RDUSYS => "RDUSYS".to_string(),
            Statement::RENAME(t, t1) => format!("RENAME {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::RESETDISP => "RESETDISP".to_string(),
            Statement::RESTSCRN => "RESTSCRN".to_string(),
            Statement::RETURN => "RETURN".to_string(),
            Statement::SAVESCRN => "SAVESCRN".to_string(),
            Statement::SENDMODEM(t) => format!("SENDMODEM {}", (**t).to_string()),
            Statement::SHELL(t, t1, t2, t3) => format!("SHELL {}, {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string(), (**t3).to_string()),
            Statement::SHOWOFF => "SHOWOFF".to_string(),
            Statement::SHOWON => "SHOWON".to_string(),
            Statement::SOUND(t) => format!("SOUND {}", (**t).to_string()),
            Statement::SPRINT(t) => format!("SPRINT {}", Statement::param_list_to_string(t)),
            Statement::SPRINTLN(t) => match t { Some(l) => format!("SPRINTLN {}", Statement::param_list_to_string(l)), None => "SPRINTLN".to_string() },
            Statement::STARTDISP(t) => format!("STARTDISP {}", (**t).to_string()),
            Statement::STOP => "STOP".to_string(),
            Statement::TOKENIZE(t) => format!("TOKENIZE {}", (**t).to_string()),
            Statement::VARADDR(t, t1) => format!("VARADDR {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::VAROFF(t, t1) => format!("VAROFF {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::VARSEG(t, t1) => format!("VARSEG {}, {}", (**t).to_string(), (**t1).to_string()),
            Statement::WAIT => "WAIT".to_string(),
            Statement::WAITFOR(t, t1, t2) => format!("WAITFOR {}, {}, {}", (**t).to_string(), (**t1).to_string(), (**t2).to_string()),
            Statement::WHILE(t, t1) => format!("WHILE {} {}", (**t).to_string(), (**t1).to_string()),
            Statement::DOWHILE(t) => format!("WHILE {} DO", (**t).to_string()),
            Statement::WRUNET(t) => format!("WRUNET {}, {}, {}, {}, {}, {}, {}", (t[0]).to_string(), (t[0]).to_string(), (t[1]).to_string(), (t[2]).to_string(), (t[3]).to_string(), (t[4]).to_string(), (t[5]).to_string()),
            Statement::WRUSYS => "WRUSYS".to_string()
        }
    }
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
        map(separated_pair(tag_no_case("INPUTCC"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTCC(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(tag_no_case("WRUSYS"), |_| Statement::STOP),
        map(tag_no_case("BEEP"), |_| Statement::BEEP)
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
        map(separated_pair(tag_no_case("PROMPTSTR"), multispace0, pair(parse_expression, many_m_n(4, 4, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::PROMPTSTR(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1),Box::new(z.1.1.remove(0).1))),
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
        map(separated_pair(tag_no_case("INPUTMONEY"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTMONEY(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("INPUTINT"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTINT(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("INPUTDATE"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTDATE(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
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
        map(tag_no_case("FRESHLINE"), |_| Statement::FRESHLINE),
        map(separated_pair(tag_no_case("FREWIND"), multispace0, parse_expression), |z| Statement::FREWIND(Box::new(z.1))),
        map(separated_pair(tag_no_case("GETTOKEN"), multispace0, parse_expression), |z| Statement::GETTOKEN(Box::new(z.1))),
        map(tag_no_case("GETUSER"), |_| Statement::GETUSER),
        map(tag_no_case("GOODBYE"), |_| Statement::GOODBYE),
        map(separated_pair(tag_no_case("GOSUB"), multispace0, parse_expression), |z| Statement::GOSUB(Box::new(z.1))),
        map(separated_pair(tag_no_case("GOTO"), multispace0, parse_expression), |z| Statement::GOTO(Box::new(z.1))),
        map(tag_no_case("HANGUP"), |_| Statement::HANGUP),
        map(separated_pair(tag_no_case("IF"), multispace0, pair(ws(parse_expression), tag_no_case("THEN"))), |z| Statement::IF(Box::new(z.1.0))),
        map(separated_pair(tag_no_case("INC"), multispace0, parse_expression), |z| Statement::INC(Box::new(z.1))),
        map(separated_pair(tag_no_case("INPUTYN"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTYN(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
        map(separated_pair(tag_no_case("INPUTTIME"), multispace0, pair(parse_expression, many_m_n(2, 2, pair(ws(tag(",")), parse_expression)))), |mut z| Statement::INPUTTIME(Box::new(z.1.0), Box::new(z.1.1.remove(0).1), Box::new(z.1.1.remove(0).1))),
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
        assert_eq!(Ok(("", Statement::CALL(Box::new(Expression::Const(Constant::String("911".to_string())))))), parse_statement("CALL \"911\""));

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
                Expression::Const(Constant::String("Hello World".to_string())),
            ]
        )))), parse_statement("PRINTLN \"Hello World\""));
    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(Ok(("", Expression::Const(Constant::Money(42.42)))), parse_expression("$42.42"));
        assert_eq!(Ok(("", Expression::Parens(Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("(5)"));
        assert_eq!(Ok(("", Expression::Not(Box::new(Expression::Const(Constant::FALSE))))), parse_expression("!FALSE"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABORT".to_string(), Vec::new()))), parse_expression("ABORT()"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABS".to_string(), vec!(Expression::Const(Constant::Integer(5)))))), parse_expression("ABS(5)"));
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
        assert_eq!(Ok(("", Constant::String("Hello World".to_string()))), parse_constant("\"Hello World\""));
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
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Boolean, "VAR001".to_string()))), parse_declaration("BOOLEAN VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Integer, "VAR001".to_string()))), parse_declaration("INTEGER VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Date, "VAR001".to_string()))), parse_declaration("DATE VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::String, "VAR001".to_string()))), parse_declaration("STRING VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Money, "VAR001".to_string()))), parse_declaration("MONEY VAR001"));

        assert_eq!(Ok(("", Declaration::Procedure("PROC001".to_string(), vec![]))), parse_declaration("DECLARE PROCEDURE PROC001()"));
        assert_eq!(Ok(("", Declaration::Function("FUNC001".to_string(), vec![Declaration::Variable(VariableType::Integer, "LOC001".to_string())], VariableType::Integer))), parse_declaration("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER"));
    }
}