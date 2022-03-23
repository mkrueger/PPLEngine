use nom::{branch::alt, bytes::complete::{tag, tag_no_case, take_while, take_while1}, character::complete::{alpha1, space1, alphanumeric1, multispace0}, combinator::{map, map_res, opt, recognize, value, eof}, error::{ParseError}, multi::{many0, separated_list0}, sequence::{delimited, preceded, pair, separated_pair, terminated}, IResult };
use nom::bytes::complete::{take_while_m_n,is_not};
use nom::character::complete::{multispace1};
use nom::sequence::tuple;
use std::{str::FromStr, rc::Rc};
use std::string::String;
use crate::{ast::{BinOp, Block, Constant, Declaration, Expression, FunctionDeclaration, Program, Statement, VariableType, ElseIfBlock, VarInfo}, tables::FUNCTION_DEFINITIONS};

use crate::tables::{ STATEMENT_DEFINITIONS};

mod tokens;
mod expression;
mod statements;

fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n";

    take_while(move |c| chars.contains(c))(i)
}
  
/// .
///
/// # Errors
///
/// This function will return an error if .
pub fn ppl_comment<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, (), E>
{
  value(
    (),
    delimited(
        alt((tag(";"), tag("'"))), 
            opt(is_not("\n\r")),
            alt((tag("\n\r"), tag("\n")))
        )
  )(i)
}

/// .
///
/// # Errors
///
/// This function will return an error if .
pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(
        pair(
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )
    )(input)
}

fn parse_vartype(input: &str) -> IResult<&str, VariableType> {
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
        tuple((
            ws(parse_vartype),
            ws(identifier),
            many0(preceded(ws(tag(",")), identifier))
        )),
        |name| {
            let mut vars = Vec::new();
            vars.push(VarInfo::Var0(name.1.to_string()));
            for a in name.2 {
                vars.push(VarInfo::Var0(a.to_string()));
            }
            Declaration::Variable(name.0, vars)
        },
    )(input)
}

fn parse_functiondeclaration(line: &str) -> nom::IResult<&str, Declaration>
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

fn parse_proceduredeclaration(line: &str) -> nom::IResult<&str, Declaration>
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

fn parse_declaration(input: &str) -> nom::IResult<&str, Declaration>
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
                take_while_m_n(2, 2, |z| ('0'..='9').contains(&z) || ('A'..='F').contains(&z) || ('a'..='f').contains(&z))),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 16).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| ('0'..='9').contains(&z)),
                tag_no_case("D"),
            ),
            |z: &str| Constant::Integer(z.parse::<i32>().unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| ('0'..='1').contains(&z)),
                tag_no_case("B"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 2).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| ('0'..='8').contains(&z)),
                tag_no_case("O"),
            ),
            |z: &str| Constant::Integer(i32::from_str_radix(z, 8).unwrap()),
        ),
        map(
            terminated(
                take_while1(|z| ('0'..='9').contains(&z) || ('A'..='F').contains(&z) || ('a'..='f').contains(&z)),
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

/// .
///
/// # Errors
///
/// This function will return an error if .
pub fn money(input: &str) -> IResult<&str, Constant> {
    map_res(pair(
        tag("$"),
        take_while(|c| c == '.' || ('0'..='9').contains(&c)),
    ),
            |z| {
                let value = f64::from_str(z.1);
                match value {
                    Ok(f) => Ok(Constant::Money(f)),
                    Err(e) => Err(e)
                }
            })(input)
}

fn parse_parenexpr(input: &str) -> nom::IResult<&str, Expression>
{
    map(delimited(
        tag("("),
        ws(parse_expression),
        tag(")")),
        |s: Expression| Expression::Parens(Box::new(s)))(input)
}

fn parse_exprlist(input: &str) -> nom::IResult<&str, Vec<Expression>>
{
    map(
        pair(opt(parse_expression),
             many0(preceded(ws(tag(",")), parse_expression))),
        |(head, mut tail)| {
            if let Some(t) = head { tail.insert(0, t) }
            tail
        },
    )(input)
}

fn parse_parameterlist(input: &str) -> nom::IResult<&str, Vec<Expression>>
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

fn fold_binop(left: Expression, remainder: Vec<(BinOp, Box<Expression>)>) -> Expression
{
    if remainder.is_empty() {
        return left;
    }

    remainder.into_iter().fold(left, |acc, pair| {
        Expression::BinaryExpression(pair.0, Box::new(acc), pair.1)
        /* 
        match pair.0 {
            BinOp::PoW |
            BinOp::Mul | BinOp::Div | BinOp::Mod |
            BinOp::Add | BinOp::Sub |
            BinOp::And | BinOp::Or => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
            BinOp::Eq | BinOp::NotEq | BinOp::Lower | BinOp::LowerEq | BinOp::Greater | BinOp::GreaterEq => Expression::BinaryExpression(pair.0, Box::new(acc), pair.1),
        }*/
    })
}

fn parse_expression(line: &str) -> nom::IResult<&str, Expression> {
    let unary = alt((
        parse_parenexpr,
        // function call
        map(separated_pair(identifier, multispace0, parse_parameterlist),
            |z| {
                for item in &FUNCTION_DEFINITIONS {
                    if item.name.eq_ignore_ascii_case(z.0) {
                        return Expression::PredefinedFunctionCall(item, z.1);
                    }
                }
                Expression::FunctionCall(z.0.to_string(), z.1)
        }),
        // !expr
        map(separated_pair(tag("!"), multispace0, parse_expression),
            |z| Expression::Not(Box::new(z.1))),
        map(separated_pair(tag("-"), multispace0, parse_expression),
            |z| Expression::Minus(Box::new(z.1))),
        map(parse_constant, Expression::Const),
        map(identifier, |z: &str| Expression::Identifier(z.to_string()))
    ));

    map(pair(
        unary,
        many0(pair(parse_binop, map(parse_expression, Box::new))),
    ),
        |z| fold_binop(z.0, z.1),
    )(line)
}

enum IfType {
    IfThen(Vec<Statement>, Option<Vec<ElseIfBlock>>, Option<Vec<Statement>>),
    If(Statement)
}
/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_if_statement;
///
/// ```
///
/// # Errors
///
/// This function will return an error if .
pub fn parse_if_statement(input: &str) -> nom::IResult<&str, Statement>
{
    map(
        tuple((
            tag_no_case("IF"), 
            ws(tag("(")), 
            parse_expression,
            ws(tag(")")),
            alt((
                map(tuple((
                    ws(tag_no_case("THEN")), 
                    many0(parse_statement),
                    many0(
                        map(
                        tuple((
                            ws(tag_no_case("ELSEIF")), 
                            ws(tag("(")), 
                            parse_expression,
                            ws(tag(")")),
                            ws(tag_no_case("THEN")),
                            many0(parse_statement)
                        )), |z| { ElseIfBlock { cond: Box::new(z.2), block: z.5} }
                    )),
                    opt(preceded(
                        ws(tag_no_case("ELSE")), 
                        many0(parse_statement)
                    )),
                    ws(tag_no_case("ENDIF"))
                )), |z| IfType::IfThen(z.1, if z.2.is_empty() { None } else { Some(z.2) }, z.3)), 
                
                map(parse_statement, IfType::If)
            )),
         )),
        |z| match z.4 {
            IfType::If(s) => Statement::If(Box::new(z.2), Box::new(s)),
            IfType::IfThen(stmts, blocks, else_block) => Statement::IfThen(Box::new(z.2), stmts, blocks, else_block)
        })
    
    (input)
}



enum WhileType {
    WhileBlock(Vec<Statement>),
    While(Statement)
}


/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_while_statement;
///
/// ```
///
/// # Errors
///
/// This function will return an error if .
pub fn parse_while_statement(input: &str) -> nom::IResult<&str, Statement>
{
    map(
        tuple((
            tag_no_case("WHILE"), 
            ws(tag("(")), 
            parse_expression,
            ws(tag(")")),
            alt((
                map(tuple((
                    tag_no_case("DO"), 
                    multispace0,
                    many0(parse_statement),
                    multispace0,
                    tag_no_case("ENDWHILE")
                )), |z| WhileType::WhileBlock(z.2)), 
                map(parse_statement, WhileType::While)
            )),
        )),
        |z| match z.4 {
            WhileType::While(s) => Statement::While(Box::new(z.2), Box::new(s)),
            WhileType::WhileBlock(stmt) => Statement::DoWhile(Box::new(z.2), stmt)
    })(input)
}

pub fn parse_procedure (input: &str) -> nom::IResult<&str, FunctionDeclaration>
{

    map(
        tuple((
            tag_no_case("PROCEDURE"), 
            multispace1,
            identifier,
            multispace0,
            delimited(tag("("),
            separated_list0(ws(tag(",")), parse_variable),
            tag(")")),
            multispace0,
            many0(parse_statement),
            multispace0,
            tag_no_case("ENDPROC")
        )),
        |z| FunctionDeclaration {
            id: 0,
            declaration: Declaration::Procedure(z.2.to_string(), z.4),
            variable_declarations: Vec::new(),
            block: Block { statements: z.6 },
        }
    )(input)
}
/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_for_statement;
///
/// ```
///
/// # Errors
///
/// This function will return an error if .
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
            opt(pair(ws(tag_no_case("STEP")),  parse_expression)),
            many0(parse_statement),
            ws(tag_no_case("NEXT")),
        )),
        |z| Statement::For(Box::new(Expression::Identifier(z.1.to_string())), Box::new(z.3), Box::new(z.5), if let Some(step) = z.6 { Some(Box::new(step.1)) } else { None }, z.7)
    )(input)
}

/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_let_statement;
///
/// ```
///
/// # Errors
///
/// This function will return an error if .
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

/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_statement;
///
/// ```
///
/// # Errors
///
/// This function will return an error if .
pub fn parse_statement(input: &str) -> nom::IResult<&str, Statement>
{
    alt((
        map(tuple((tag(":"), identifier)), |z| Statement::Label(z.1.to_string())),
        parse_if_statement,
        parse_while_statement,
        parse_for_statement,
        parse_let_statement,
        map(ppl_comment, |_| Statement::Comment(String::new())),
        map(tuple((tag_no_case("GOTO"), sp, identifier)), |z| Statement::Goto(z.2.to_string())),
        map(tuple((tag_no_case("GOSUB"), sp, identifier)), |z| Statement::Gosub(z.2.to_string())),
        map(tuple((tag_no_case("INC"), sp, parse_expression)), |z| Statement::Inc(String::new())),
        map(tuple((tag_no_case("DEC"), sp, parse_expression)), |z| Statement::Dec(String::new())),
        map(tuple((tag_no_case("END"), multispace1 )), |_| Statement::End),
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

/// .
///
/// # Examples
///
/// ```
/// use ppl_engine::parser::parse_program;
///
/// ```
///
/// # Panics
///
/// Panics if .
#[must_use] pub fn parse_program(input: &str) -> Program
{
    let mut procedure_declarations = Vec::new();
    let mut function_declarations = Vec::new();
    let mut variable_declarations = Vec::new();
    let mut statements = Vec::new();

    terminated(many0(
        delimited(opt(sp),alt((
            map(parse_procedure, |proc| {
                procedure_declarations.push(proc);
            }),
            map(parse_declaration, |d| {
                variable_declarations.push(d)
            }),
            map(parse_statement, |z| statements.push(z) ),
        )), opt(sp))
    ), eof)(input).expect("error");

    Program {
        declarations: variable_declarations,
        main_block: Block {
            statements: statements
        },
        function_declarations,
        procedure_declarations: procedure_declarations.to_vec(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tables::{ StatementDefinition };

    #[allow(clippy::needless_pass_by_value)]
    fn check_statements(input: &str, statements: Vec<Statement>)
    {
        assert_eq!(statements, parse_program(input).main_block.statements);
    }

    #[test]
    fn test_procedure() {
        let prg = parse_program("Procedure Proc() PRINT 5 EndProc");

        assert_eq!(1, prg.procedure_declarations.len());

    }
    
    /*
    #[test]
    fn test_parse_declarations() {
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Boolean, "VAR001".to_string()))), parse_declaration("BOOLEAN VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Integer, "VAR001".to_string()))), parse_declaration("INTEGER VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Date, "VAR001".to_string()))), parse_declaration("DATE VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::String, "VAR001".to_string()))), parse_declaration("STRING VAR001"));
        assert_eq!(Ok(("", Declaration::Variable(VariableType::Money, "VAR001".to_string()))), parse_declaration("MONEY VAR001"));

        assert_eq!(Ok(("", Declaration::Procedure("PROC001".to_string(), vec![]))), parse_declaration("DECLARE PROCEDURE PROC001()"));
        assert_eq!(Ok(("", Declaration::Function("FUNC001".to_string(), vec![Declaration::Variable(VariableType::Integer, "LOC001".to_string())], VariableType::Integer))), parse_declaration("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER"));
    }*/
}