use crate::executable::VariableType;
use crate::parser;
use crate::parser::*;


struct Block<'a>
{
    statements: Vec<Statement<'a>>
}

struct FunctionDeclaration<'a>
{
    declaration: parser::Declaration<'a>,
    block: Block<'a>
}
trait ProgramContext
{
    fn print(&mut self, str : String);
}
struct Program<'a>
{
    variable_declarations: Vec<parser::Declaration<'a>>,
    main_block: Block<'a>,
    function_delclarations: Vec<FunctionDeclaration<'a>>,
    procedure_delclarations: Vec<FunctionDeclaration<'a>>
}

fn to_stringc(expr : &Constant) -> String
{
    match expr {
        Constant::Money(f) => format!("${}", f),
        Constant::Integer(i) => format!("{}", i),
        Constant::String(str) => String::from(*str),

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
        Constant::LFBEFORE => String::from("FLBEFORE"),
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
        Constant::YESNO => String::from("YESNO")
    }

}
fn to_stringe(expr : &Expression) -> String
{
    match expr {
        Expression::Const(c) => to_stringc(c),
        _ => String::new()
    }
}

struct TestContext
{
    output : String
}

impl ProgramContext for TestContext
{
    fn print(&mut self, str : String)
    {
        self.output.push_str(str.as_str());
    }
}

impl Program<'_>
{
    fn execute_statement(&self, ctx : &mut dyn ProgramContext, stmt : &Statement)
    {
        match stmt {
            Statement::PRINT(vec) => {
                for expr in vec {
                    ctx.print(to_stringe(expr));
                }
            },

            Statement::PRINTLN(vec) => {
                match vec {
                    Some(t) => {
                        for expr in t {
                            ctx.print(to_stringe(expr));
                        }
                        ctx.print("\n".to_string());
                    },
                    _ => {}
                }
            },
            _ => {}
        }
    }

    pub fn run(&self, ctx : &mut dyn ProgramContext)
    {
        for stmt in &self.main_block.statements {
            self.execute_statement(ctx, stmt);
        }
    }
}

fn parse_program(input : &str) -> Program
{
    let stmt = parse_statement(input).unwrap();

    Program {
        variable_declarations: vec![],
        main_block: Block {
            statements: vec![stmt.1]
        },
        function_delclarations: vec![],
        procedure_delclarations: vec![]
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_println() {
        let mut ctx = TestContext { output: String::new() };
        parse_program("PRINTLN 1, 2, 3, \"Hello World\"").run(&mut ctx);
        assert_eq!("123Hello World\n".to_string(), ctx.output);

        ctx = TestContext { output: String::new() };
        parse_program("PRINT TRUE,  \",\", $41.43, \",\", 10h").run(&mut ctx);
        assert_eq!("TRUE,$41.43,16".to_string(), ctx.output);
    }
}