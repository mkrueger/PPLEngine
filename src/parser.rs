use nom::{branch::alt, bytes::complete::{tag, tag_no_case, take_while, take_while1}, character::complete::{alpha1, space1, alphanumeric1, multispace0}, combinator::{map, map_res, opt, recognize, value, eof}, error::{ParseError}, multi::*, sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult };
use nom::bytes::complete::{take_while_m_n,is_not};
use nom::character::complete::{multispace1};
use nom::sequence::tuple;
use std::str::FromStr;
use std::string::String;
use crate::{ast::*, tables::FUNCTION_DEFINITIONS};

use crate::tables::{ STATEMENT_DEFINITIONS};

fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";

    take_while(move |c| chars.contains(c))(i)
}
  
pub fn ppl_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
  value(
    (),
    pair(alt((tag(";"), tag("'"))), opt(is_not("\n\r")))
  )(i)
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
        value(VariableType::String, tag_no_case("STRING")), // make no difference between STRING and BIGSTR
        value(VariableType::String, tag_no_case("BIGSTR")), 
        value(VariableType::Boolean, tag_no_case("BOOLEAN")),
        value(VariableType::Date, tag_no_case("DATE")),
        value(VariableType::Time, tag_no_case("TIME")),
        value(VariableType::Money, tag_no_case("MONEY")),
        value(VariableType::Word, tag_no_case("WORD")),
        value(VariableType::SWord, tag_no_case("SWORD")),
        value(VariableType::Byte, tag_no_case("BYTE")),
        value(VariableType::Unsigned, tag_no_case("UNSIGNED")),
        value(VariableType::SByte, tag_no_case("SBYTE")),
        value(VariableType::Real, tag_no_case("REAL")), // just use f64 for both
        value(VariableType::Real, tag_no_case("DREAL")),
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


fn parse_constant<'a>(line: &'a str) -> nom::IResult<&'a str, Constant> {
    alt((
        money,
        map(
            tag_no_case("TRUE"),
            |_| Constant::Boolean(true),
        ),
        map(
            tag_no_case("FALSE"),
            |_| Constant::Boolean(false),
        ),
        // builtin constants
        map_res(identifier, |z| {
            let tag_name = z.to_uppercase();
            for cnst in &crate::tables::CONSTANT_VALUES {
                if cnst.0 == tag_name {
                    return Ok(Constant::Builtin(cnst.0));
                }
            }
            Err(format!("unknown constant {}", z))
        }),
        map(
            preceded(
                tag_no_case("@X"),
                take_while_m_n(2, 2, |z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f')),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 16).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| '0' <= z && z <= '9'),
                tag_no_case("D"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 10).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| '0' <= z && z <= '1'),
                tag_no_case("B"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 2).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| '0' <= z && z <= '8'),
                tag_no_case("O"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 8).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| '0' <= z && z <= '9' || 'A' <= z && z <= 'F' || 'a' <= z && z <= 'f'),
                tag_no_case("H"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 16).unwrap()),
        ), 
        // string \"(.)*\"
        map(delimited(
            tag("\""),
            take_while(|z| z != '"'),
            tag("\"")),
            |s: &'a str| Constant::String(s.to_string())),
        // integer [+-i]\d+
        map_res(recognize(tuple((
            opt(alt((tag("+"), tag("-")))),
            alphanumeric1)
        )
        ), |s| {
            match i64::from_str(s) {
                nom::lib::std::result::Result::Ok(i) => { 
                    if i > i32::MAX.into() {
                        nom::lib::std::result::Result::Ok(Constant::Unsigned(i as u32))
                    } else {
                        nom::lib::std::result::Result::Ok(Constant::Integer(i as i32))
                    }
                }
                _ => { nom::lib::std::result::Result::Err("Error parsing number.") }
            }
        }),
  //      map(double, |s| { Constant::Real(s)  }),
        
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
            |z| {
                for i in 0..FUNCTION_DEFINITIONS.len() {
                    if FUNCTION_DEFINITIONS[i].name.eq_ignore_ascii_case(z.0) {
                        return Expression::PredefinedFunctionCall(&FUNCTION_DEFINITIONS[i], z.1);
                    }
                }

                Expression::FunctionCall(z.0.to_string(), z.1)
            
        }),
        // !expr
        map(separated_pair(tag("!"), multispace0, parse_expression),
            |z| Expression::Not(Box::new(z.1))),
        map(separated_pair(tag("-"), multispace0, parse_expression),
            |z| Expression::Minus(Box::new(z.1))),
        map(parse_constant, |z| Expression::Const(z)),
        map(identifier, |z: &str| Expression::Identifier(z.to_string()))
    ));

    map(pair(
        unary,
        many0(pair(parse_binop, map(parse_expression, |z| Box::new(z)))),
    ),
        |z| fold_binop(z.0, z.1),
    )(line)
}


pub fn parse_if_statement(input: &str) -> nom::IResult<&str, Statement>
{
    alt((
        map(
            tuple((
                tag_no_case("IF"), 
                ws(tag("(")), 
                parse_expression,
                ws(tag(")")),
                alt((map(tag_no_case("THEN"), |_| None), map(parse_statement, |z| Some(z))))
            )),
            |z| match z.4 {
                Some(s) => Statement::If(Box::new(z.2), Box::new(s)),
                None => Statement::IfThen(Box::new(z.2))
            }),
            map(
                tuple((
                    tag_no_case("ELSEIF"), 
                    ws(tag("(")), 
                    parse_expression,
                    ws(tag(")")),
                    tag_no_case("THEN")
                )),
                |z| Statement::ElseIf(Box::new(z.2)))
    
    ))(input)
}

pub fn parse_while_statement(input: &str) -> nom::IResult<&str, Statement>
{
    map(
        tuple((
            tag_no_case("WHILE"), 
            ws(tag("(")), 
            parse_expression,
            ws(tag(")")),
            alt((map(tag_no_case("DO"), |_| None), map(parse_statement, |z| Some(z))))
        )),
        |z| match z.4 {
            Some(s) => Statement::While(Box::new(z.2), Box::new(s)),
            None => Statement::DoWhile(Box::new(z.2))
    })(input)
}

pub fn parse_for_statement(input: &str) -> nom::IResult<&str, Statement>
{
    map(
        tuple((
            tag_no_case("FOR"), 
            ws(identifier), 
            ws(tag("=")),
            parse_expression,
            ws(tag_no_case("TO")),
            parse_expression,
            opt(pair(ws(tag_no_case("STEP")),  parse_expression))
        )),
        |z| Statement::For(Box::new(Expression::Identifier(z.1.to_string())), Box::new(z.3), Box::new(z.5), if let Some(step) = z.6 { Some(Box::new(step.1)) } else { None })
    )(input)
}

pub fn parse_let_statement(input: &str) -> nom::IResult<&str, Statement>
{
    map(
        tuple((
            opt(pair(tag_no_case("LET"), multispace1)), 
            identifier, 
            ws(tag("=")),
            parse_expression
        )),
        |z| Statement::Let(Box::new(Expression::Identifier(z.1.to_string())), Box::new(z.3))
    )(input)
}

pub fn parse_statement(input: &str) -> nom::IResult<&str, Statement>
{
    alt((
        map(tuple((tag(":"), identifier)), |z| Statement::Label(z.1.to_string())),
        parse_if_statement,
        parse_while_statement,
        parse_for_statement,
        parse_let_statement,
        map(tuple((tag_no_case("GOTO"), sp, identifier)), |z| Statement::Goto(z.2.to_string())),
        map(tuple((tag_no_case("GOSUB"), sp, identifier)), |z| Statement::Gosub(z.2.to_string())),
        map(tuple((tag_no_case("INC"), sp, parse_expression)), |z| Statement::Inc(Box::new(z.2))),
        map(tuple((tag_no_case("DEC"), sp, parse_expression)), |z| Statement::Dec(Box::new(z.2))),
        map(tag_no_case("ENDWHILE"), |_| Statement::EndWhile),
        map(tag_no_case("ENDIF"), |_| Statement::EndIf),
        map(tag_no_case("END"), |_| Statement::End),
        map(tag_no_case("ELSE"), |_| Statement::Else),
        map(tag_no_case("NEXT"), |_| Statement::Next),
        map(tag_no_case("BREAK"), |_| Statement::Break),
        map(tag_no_case("CONTINUE"), |_| Statement::Continue),
        map(tag_no_case("RETURN"), |_| Statement::Return),
        map(tag_no_case("STOP"), |_| Statement::Stop),
        map(tuple((
            identifier, 
            ws(tag("(")), 
            parse_exprlist,
            ws(tag(")")),
        )), |z| Statement::ProcedureCall(z.0.to_string(), z.2)),
        map_res(separated_pair(alpha1, multispace0, parse_exprlist), |z| {
            let tag_name = z.0.to_uppercase();
            for def in &STATEMENT_DEFINITIONS {
                if def.name.to_uppercase() == tag_name {
              /*    if (z.1.len() as i8) < def.min_args {
                        return Err(format!("{} has too few arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }
                    if (z.1.len() as i8) > def.max_args {
                        return Err(format!("{} has too many arguments {} [{}:{}]", def.name, z.1.len(), def.min_args, def.max_args));
                    }*/

                    return Ok(Statement::Call(def, z.1));
                }
            }
            Err(format!("unknown statement {}", z.0))
        }),
    ))(input)
}

pub fn parse_lines(input: &str) -> nom::IResult<&str, Vec<(Option<Statement>, Option<Declaration>)>>
{
    terminated(many0(
        delimited(opt(sp),alt((
        map( ppl_comment, |_| (None, None)),
        map(parse_declaration, |z| (None, Some(z))),
        map(parse_statement, |z| (Some(z), None)),
        )), opt(sp))
    ), eof)(input)
}

pub fn parse_program(input: &str) -> Program
{

    let parse_result = parse_lines(input);

    match parse_result {
        Ok(parse_result) => {
            let mut result = Program::new();
            for line in parse_result.1 {
                if let Some(s) = line.0 {
                    result.main_block.statements.push(s);
                }

                if let Some(d) = line.1 {
                    match d {
                        Declaration::Function(_, _, _) => result.function_declarations.push(Box::new(FunctionDeclaration { 
                            id: -1,
                            declaration: d,
                            variable_declarations: vec![],
                            block: Block::new()})),
                        Declaration::Procedure(_, _) => result.procedure_declarations.push(Box::new(FunctionDeclaration { 
                            id: -1,
                            declaration: d,
                            variable_declarations: vec![],
                            block: Block::new()})),
                        Declaration::Variable(_, _) => result.variable_declarations.push(d),
                        Declaration::Variable1(_, _, _) => result.variable_declarations.push(d),
                        Declaration::Variable2(_, _, _, _) => result.variable_declarations.push(d),
                        Declaration::Variable3(_, _, _, _, _) => result.variable_declarations.push(d),
                    }
                }
            }
            result
        },
        Err(err) => {
            panic!("{}", err);
        }
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tables::{ StatementDefinition };

    fn check_statements(input: &str, statements: Vec<Statement>)
    {
        assert_eq!(statements, parse_program(input).main_block.statements);
    }

    #[test]
    fn test_parse_hello_world() {
        check_statements(";This is a comment\nPRINT \"Hello World\"\n\t\n\n", vec![Statement::Call(get_statement_definition(&"PRINT").unwrap(), vec![Expression::Const(Constant::String("Hello World".to_string()))])]);
    }

    #[test]
    fn test_parse_simple_noncalls() {
        check_statements("End ; Predifined End", vec![Statement::End]);
        check_statements("Else", vec![Statement::Else]);
        check_statements(";Endif\nENDIF", vec![Statement::EndIf]);
        check_statements("NEXT ; FOR", vec![Statement::Next]);
        check_statements("\nBREAK\n", vec![Statement::Break]);
        check_statements("     CONTINUE        ", vec![Statement::Continue]);
        check_statements("          RETURN", vec![Statement::Return]);
        check_statements("STOP      ", vec![Statement::Stop]);
        check_statements("ENDWHILE ", vec![Statement::EndWhile]);
    }

    #[test]
    fn test_parse_if() {
        check_statements("IF (FALSE) END", vec![Statement::If(Box::new(Expression::Const(Constant::Boolean(false))), Box::new(Statement::End))]);
        check_statements("IF (TRUE) THEN", vec![Statement::IfThen(Box::new(Expression::Const(Constant::Boolean(true))))]);
        check_statements("ELSEIF (TRUE) THEN", vec![Statement::ElseIf(Box::new(Expression::Const(Constant::Boolean(true))))]);
    }  
    
    #[test]
    fn test_parse_while() {
        check_statements("WHILE (FALSE) END", vec![Statement::While(Box::new(Expression::Const(Constant::Boolean(false))), Box::new(Statement::End))]);
        check_statements("WHILE (TRUE) DO", vec![Statement::DoWhile(Box::new(Expression::Const(Constant::Boolean(true))))]);
    }

    #[test]
    fn test_let() {
        check_statements("LET FOO = FALSE", vec![Statement::Let(Box::new(Expression::Identifier("FOO".to_string())), Box::new(Expression::Const(Constant::Boolean(false))))]);
    }

    #[test]
    fn test_gotogosub() {
        check_statements("GOTO LABEL1", vec![Statement::Goto("LABEL1".to_string())]);
        check_statements("GOSUB LABEL2", vec![Statement::Gosub("LABEL2".to_string())]);
        check_statements(":LABEL1", vec![Statement::Label("LABEL1".to_string())]);
    }

    #[test]
    fn test_incdec() {
        check_statements("INC VAR1", vec![Statement::Inc(Box::new(Expression::Identifier("VAR1".to_string())))]);
        check_statements("DEC VAR2", vec![Statement::Dec(Box::new(Expression::Identifier("VAR2".to_string())))]);
    }

    #[test]
    fn test_procedure_calls() {
        check_statements("PROC(TRUE)", vec![Statement::ProcedureCall("PROC".to_string(), vec![Expression::Const(Constant::Boolean(true))])]);
        check_statements("PROC(TRUE, FALSE)", vec![Statement::ProcedureCall("PROC".to_string(), vec![Expression::Const(Constant::Boolean(true)), Expression::Const(Constant::Boolean(false))])]);
    }

    #[test]
    fn test_for_next() {
        check_statements("FOR i = 1 TO 10", vec![Statement::For(Box::new(Expression::Identifier("i".to_string())), Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(10))), None)]);
        check_statements("FOR i = 1 TO 10 STEP 5", vec![Statement::For(Box::new(Expression::Identifier("i".to_string())), Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(10))), Some(Box::new(Expression::Const(Constant::Integer(5)))))]);
        check_statements("FOR i = 1 TO 10 STEP -1", vec![Statement::For(Box::new(Expression::Identifier("i".to_string())), Box::new(Expression::Const(Constant::Integer(1))), Box::new(Expression::Const(Constant::Integer(10))), Some(Box::new(Expression::Minus(Box::new(Expression::Const(Constant::Integer(1)))))))]);
    }

    fn get_statement_definition(name: &str) -> Option<&'static StatementDefinition>
    {
        let name = name.to_uppercase();
        for def in &STATEMENT_DEFINITIONS {
            if def.name.to_uppercase() == name {
                return Some(def);
            }
        }
        None
    }
    
    #[test]
    fn test_parse_statement() {
        assert_eq!(Ok(("", Statement::Let(Box::new(Expression::Identifier("foo_bar".to_string())), Box::new(Expression::Const(Constant::Integer(1)))))), parse_statement("foo_bar=1"));
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
        assert_eq!(Ok(("", Expression::Not(Box::new(Expression::Const(Constant::Boolean(false)))))), parse_expression("!FALSE"));
     //  assert_eq!(Ok(("", Expression::PredefinedFunctionCall("ABORT".to_string(), Vec::new()))), parse_expression("ABORT()"));
      //  assert_eq!(Ok(("", Expression::PredefinedFunctionCall("ABS".to_string(), vec!(Expression::Const(Constant::Integer(5)))))), parse_expression("ABS(5)"));
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
        assert_eq!(Ok(("", Constant::Boolean(true))), parse_constant("TRUE"));
        assert_eq!(Ok(("", Constant::Boolean(false))), parse_constant("FALSE"));
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