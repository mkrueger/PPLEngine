use nom::{branch::alt, bytes::complete::{tag, tag_no_case, take_while}, character::complete::{alpha1, space1, alphanumeric1, multispace0}, combinator::{map, map_res, opt, recognize, value}, error::{ParseError}, multi::*, sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult};
use nom::bytes::complete::{take_while_m_n};
use nom::character::complete::{multispace1};
use nom::sequence::tuple;
use std::str::FromStr;
use std::string::String;
use crate::ast::*;

use crate::tables::{ StatementDefinition, STATEMENT_DEFINITIONS};

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

pub fn parse_program(input: &str) -> Program
{
    let stmt = parse_statement(input).unwrap();

    Program {
        variable_declarations: vec![],
        main_block: Block {
            statements: vec![stmt.1]
        },
        function_declarations: vec![],
        procedure_declarations: vec![],
    }
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