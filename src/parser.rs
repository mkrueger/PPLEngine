use std::intrinsics::transmute;
use nom::{branch::alt, bytes::complete::{tag, tag_no_case, take_while}, character::complete::{alpha1, space1, alphanumeric1, multispace0}, combinator::{map, map_res, opt, recognize, value}, error::{ParseError}, multi::*, sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult};
use nom::bytes::complete::{take_while_m_n};
use nom::character::complete::{multispace1};
use nom::sequence::tuple;
use std::str::FromStr;
use std::string::String;

use crate::executable::VariableType;
use crate::interpreter::Program;
use crate::tables::{OpCode, StatementDefinition, STATEMENT_DEFINITIONS};

#[derive(Debug, PartialEq)]
pub enum Declaration {
    Variable(VariableType, String),
    Variable1(VariableType, String, i32),
    Variable2(VariableType, String, i32, i32),
    Variable3(VariableType, String, i32, i32, i32),
    Function(String, Vec<Declaration>, VariableType),
    Procedure(String, Vec<Declaration>),
}

impl Declaration {
    fn declr_vec_to_string(list: &Vec<Declaration>) -> String
    {
        let mut res = String::new();
        for decl in list {
            if res.len() > 0 {
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

    pub fn print_header(&self) -> String
    {
        match self {
            Declaration::Procedure(name, parameters) => format!("PROCEDURE {}({})", name, Declaration::declr_vec_to_string(parameters)),
            Declaration::Function(name, parameters, return_type) => format!("FUNCTION {}({}) {}", name, Declaration::declr_vec_to_string(parameters), return_type.to_string()),
            _ => { "ERR".to_string() }
        }
    }
}

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
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
        multispace0,
    )
}

fn parse_variable<'a>(input: &'a str) -> nom::IResult<&'a str, Declaration>
{
    map(
        pair(ws(parse_vartype), ws(identifier)),
        |name: (VariableType, &'a str)| {
            Declaration::Variable(name.0, String::from(name.1))
        },
    )(input)
}

fn parse_functiondeclaration<'a>(line: &'a str) -> nom::IResult<&'a str, Declaration>
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
        Ok(z) => nom::IResult::Ok((z.0, Declaration::Function(String::from(z.1.2), z.1.4, z.1.6))),
        Err(e) => Err(e)
    }
}

fn parse_proceduredeclaration<'a>(line: &'a str) -> nom::IResult<&'a str, Declaration>
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
        Ok(z) => nom::IResult::Ok((z.0, Declaration::Procedure(String::from(z.1.2), z.1.4))),
        Err(e) => Err(e)
    }
}

fn parse_declaration<'a>(input: &'a str) -> nom::IResult<&'a str, Declaration>
{
    alt((parse_variable, parse_functiondeclaration, parse_proceduredeclaration))(input)
}

#[allow(non_camel_case_types)]
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
    SEC_DROP,
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
            Constant::DEB_DOWNFILE => String::from("?"),
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

fn predefined_constants1<'a>(input: &'a str) -> nom::IResult<&'a str, Constant> {
    alt((
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

fn predefined_constants2<'a>(input: &'a str) -> nom::IResult<&'a str, Constant> {
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

fn parse_constant<'a>(line: &'a str) -> nom::IResult<&'a str, Constant> {
    alt((
        predefined_constants1,
        predefined_constants2,
        money,
        map(
            terminated(
                take_while(|z| '0' <= z && z <= '9'),
                tag_no_case("D"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 10).unwrap()),
        ),
        map(
            terminated(
                take_while(|z| '0' <= z && z <= '1'),
                tag_no_case("B"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 2).unwrap()),
        ),
        map(
            terminated(
                take_while(|z| '0' <= z && z <= '8'),
                tag_no_case("O"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 8).unwrap()),
        ),
        map(
            terminated(
                take_while(|z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f'),
                tag_no_case("H"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 16).unwrap()),
        ), map(
            preceded(
                tag_no_case("@X"),
                take_while_m_n(2, 2, |z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f')),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 16).unwrap()),
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
                nom::lib::std::result::Result::Ok(i) => { nom::lib::std::result::Result::Ok(Constant::Integer(i)) }
                _ => { nom::lib::std::result::Result::Err("Error parsing number.") }
            }
        })
    ))(line)
}

pub fn money<'a>(input: &'a str) -> IResult<&'a str, Constant> {
    map_res(pair(
        tag("$"),
        take_while(|c| c == '.' || '0' <= c && c <= '9'),
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
    Or,
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
    BinaryExpression(BinOp, Box<Expression>, Box<Expression>),
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
        |(head, mut tail)| {
            match head {
                Some(t) => tail.insert(0, t),
                _ => {}
            }
            tail
        },
    )(input)
}

fn parse_parameterlist<'a>(input: &'a str) -> nom::IResult<&'a str, Vec<Expression>>
{
    delimited(
        tag("("),
        ws(parse_exprlist),
        tag(")"),
    )(input)
}

fn parse_binop(line: &str) -> nom::IResult<&str, BinOp>
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

fn fold_binop<'a>(left: Expression, remainder: Vec<(BinOp, Box<Expression>)>) -> Expression
{
    if remainder.len() == 0 {
        return left;
    }

    remainder.into_iter().fold(left, |acc, pair| {
        match pair.0 {
            BinOp::PoW => Expression::BinaryExpression(BinOp::PoW, Box::new(acc), pair.1),
            BinOp::Mul | BinOp::Div | BinOp::Mod => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::Add | BinOp::Sub => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::Eq | BinOp::NotEq | BinOp::Lower | BinOp::LowerEq | BinOp::Greater | BinOp::GreaterEq => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::And | BinOp::Or => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1)
        }
    })
}

fn parse_expression<'a>(line: &'a str) -> nom::IResult<&'a str, Expression> {
    let unary = alt((
        parse_parenexpr,
        // function call
        map(separated_pair(identifier, multispace0, parse_parameterlist),
            |z| Expression::FunctionCall(z.0.to_string(), z.1)),
        // !expr
        map(separated_pair(tag("!"), multispace0, parse_expression),
            |z| Expression::Not(Box::new(z.1))),
        map(separated_pair(tag("-"), multispace0, parse_expression),
            |z| Expression::Minus(Box::new(z.1))),
        map(parse_constant, |z| Expression::Const(z)),
        map(alphanumeric1, |z: &str| Expression::Identifier(z.to_string()))
    ));

    map(pair(
        unary,
        many0(pair(parse_binop, map(parse_expression, |z| Box::new(z)))),
    ),
        |z| fold_binop(z.0, z.1),
    )(line)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Comment(String),
    While(Box<Expression>, Box<Statement>),
    If(Box<Expression>, Box<Statement>),
    DoWhile(Box<Expression>),
    EndWhile,
    For(String, Box<Expression>, Box<Expression>, Box<Expression>),
    Next,
    Label(String),
    ProcedureCall(String, Vec<Expression>),
    Call(&'static StatementDefinition<'static>, Vec<Expression>),
}

impl Statement
{
    pub fn param_list_to_string(l: &Vec<Expression>) -> String
    {
        let mut res = String::new();
        for expr in l {
            if res.len() > 0 {
                res.push_str(", ");
            }
            res.push_str(expr.to_string().as_str());
        }
        res
    }
    fn try_boolean_conversion(expr: &Expression) -> &Expression
    {
        match expr {
            Expression::Const(Constant::Integer(i)) => {
                if *i == 0 {
                    &Expression::Const(Constant::FALSE)
                } else {
                    &Expression::Const(Constant::TRUE)
                }
            }
            Expression::Not(notexpr) => {
                match Statement::try_boolean_conversion(notexpr) {
                    Expression::Const(Constant::FALSE) => &Expression::Const(Constant::TRUE),
                    Expression::Const(Constant::TRUE) => &Expression::Const(Constant::FALSE),
                    Expression::Not(notexpr2) => notexpr2,
                    _ => expr
                }
            }
            _ => { expr }
        }
    }
    pub fn to_string(&self, prg: &Program) -> String
    {
        match self {
            Statement::Comment(str) => format!(";{}", str),
            Statement::While(cond, stmt) => format!("WHILE ({}) {}", Statement::try_boolean_conversion(cond).to_string(), stmt.to_string(prg)),
            Statement::If(cond, stmt) => format!("IF ({}) {}", Statement::try_boolean_conversion(cond).to_string(), stmt.to_string(prg)),
            Statement::DoWhile(t) => {
                format!("WHILE ({}) DO", Statement::try_boolean_conversion(&Expression::Not(t.clone())).to_string())
            }
            Statement::EndWhile => "ENDWHILE".to_string(),
            Statement::For(var_name, from, to, step) => {
                let step = step.to_string();
                if step == "1" {
                    format!("FOR {} = {} TO {}", var_name, from.to_string(), to.to_string())
                } else {
                    format!("FOR {} = {} TO {} STEP {}", var_name, from.to_string(), to.to_string(), step)
                }
            }
            Statement::Next => "NEXT".to_string(),
            Statement::Label(str) => format!(":{}", str),
            Statement::ProcedureCall(name, params) => format!("{}({})", name, Statement::param_list_to_string(params)),
            Statement::Call(def, params) => {
                let op: OpCode = unsafe { transmute(def.opcode) };
                match op {
                    OpCode::LET => {
                        let var = params[0].to_string();
                        let expected_type = prg.get_variable(&var);
                        let mut expr = &params[1];
                        if expected_type == VariableType::Boolean {
                            expr = Statement::try_boolean_conversion(expr);
                        }
                        format!("LET {} = {}", var.as_str(), expr.to_string().as_str())
                    }
                    OpCode::IF => {
                        format!("IF ({})", params[0].to_string().as_str())
                    }
                    _ => {
                        if params.is_empty() {
                            def.name.to_string()
                        } else {
                            format!("{} {}", def.name, Statement::param_list_to_string(params))
                        }
                    }
                }
            }
        }
    }
}

fn get_statement_definition(name: &str) -> Option<&'static StatementDefinition>
{
    for def in &STATEMENT_DEFINITIONS {
        if def.name == name {
            return Some(def);
        }
    }
    None
}

pub fn parse_statement(input: &str) -> nom::IResult<&str, Statement>
{
    alt((
        map_res(separated_pair(alpha1, multispace0, parse_exprlist), |z| {
            for def in &STATEMENT_DEFINITIONS {
                if def.name == z.0 {
                    if (z.1.len() as i8) < def.min_args {
                        return Err(format!("{} has too few arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }
                    if (z.1.len() as i8) > def.max_args {
                        return Err(format!("{} has too many arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }

                    return Ok(Statement::Call(def, z.1));
                }
            }
            Err(format!("unknown statement {}", z.0))
        }),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_statement() {
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"ADJTIME").unwrap(), vec![Expression::Const(Constant::Integer(1))]))), parse_statement("ADJTIME 1"));
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"ANSIPOS").unwrap(), vec![Expression::Const(Constant::Integer(1)), Expression::Const(Constant::Integer(2))]))), parse_statement("ANSIPOS 1, 2"));
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"BROADCAST").unwrap(), vec![Expression::Const(Constant::Integer(1)), Expression::Const(Constant::Integer(2)), Expression::Const(Constant::Integer(3))]))), parse_statement("BROADCAST 1, 2, 3"));
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"BYE").unwrap(), vec![]))), parse_statement("BYE"));
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"PRINTLN").unwrap(), vec![]))), parse_statement("PRINTLN"));
        assert_eq!(Ok(("", Statement::Call(get_statement_definition(&"PRINTLN").unwrap(), vec![Expression::Const(Constant::String("Hello World".to_string()))]))), parse_statement("PRINTLN \"Hello World\""));
    }

    #[test]
    fn test_parse_expression() {
        assert_eq!(Ok(("", Expression::Const(Constant::Money(42.42)))), parse_expression("$42.42"));
        assert_eq!(Ok(("", Expression::Parens(Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("(5)"));
        assert_eq!(Ok(("", Expression::Not(Box::new(Expression::Const(Constant::FALSE))))), parse_expression("!FALSE"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABORT".to_string(), Vec::new()))), parse_expression("ABORT()"));
        assert_eq!(Ok(("", Expression::FunctionCall("ABS".to_string(), vec!(Expression::Const(Constant::Integer(5)))))), parse_expression("ABS(5)"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::PoW, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2^5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Mul, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2*5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Div, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2/5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Mod, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2%5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Add, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2+5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Sub, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2-5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Eq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 = 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::NotEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 <> 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Lower, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 < 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::LowerEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 <= 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Greater, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 > 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::GreaterEq, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 >= 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::And, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 & 5"));
        assert_eq!(Ok(("", Expression::BinaryExpression(BinOp::Or, Box::new(Expression::Const(Constant::Integer(2))), Box::new(Expression::Const(Constant::Integer(5)))))), parse_expression("2 | 5"));
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