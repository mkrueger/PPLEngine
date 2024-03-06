use argh::FromArgs;
use ppl_engine::{
    ast::{Program, Statement, VariableType, VariableValue},
    compiler::transform_ast,
    crypt::{encode_rle, encrypt},
    parser::parse_program,
    tables::{FuncOpCode, OpCode},
};
use std::{collections::HashMap, ffi::OsStr, fs, path::Path};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilationError {
    #[error("Too many declarations: {0}")]
    TooManyDeclarations(usize),
}

#[derive(FromArgs)]
/// original by Clark Development Company, Inc. rewritten in rust https://github.com/mkrueger/PPLEngine
struct Arguments {
    /// srcname[.ext] .ext defaults to .ppe
    #[argh(positional)]
    file_name: String,
}

struct VarInfo {
    pub id: usize,
    pub var_type: VariableType,
    pub dims: u8,
    pub vector_size: usize,
    pub matrix_size: usize,
    pub cube_size: usize,

    //  Bit 1 set: tell runtime mod to reinit the var
    pub flags: u8,
}

struct Variable {
    pub info: VarInfo,
    pub var_type: VariableType,
    pub value: VariableValue,
}

impl Variable {
    fn create_var_header(&self, version: u16) -> Vec<u8> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.info.id as u16 + 1));

        buffer.push(self.info.dims);

        buffer.extend(u16::to_le_bytes(self.info.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.cube_size as u16));

        buffer.push(self.info.var_type as u8);
        buffer.push(self.info.flags);
        encrypt(&mut buffer, version);

        if self.var_type == VariableType::String {
            let s = if self.info.dims == 0 {
                let VariableValue::String(s) = &self.value else {
                    panic!("Invalid value type {:?}", self.value);
                };
                if s.len() > u16::MAX as usize {
                    panic!("String too long");
                }
                s.clone()
            } else {
                String::new()
            };
            buffer.extend_from_slice(&u16::to_le_bytes(s.len() as u16 + 1));
            let mut string_buffer: Vec<u8> = s.bytes().collect();
            string_buffer.push(0);
            encrypt(&mut string_buffer, version);
            buffer.extend(string_buffer);
        } else {
            // VTABLE - get's ignored by PCBoard - pure garbage
            buffer.push(0);
            buffer.push(0);

            // variable type
            buffer.push(self.var_type as u8);
            buffer.push(0);

            buffer.extend_from_slice(&u64::to_le_bytes(self.value.get_u64_value()));
        }
        buffer
    }
}

struct Executable {
    variable_declarations: HashMap<String, Variable>,
    script_buffer: Vec<u16>,
}

impl Executable {
    pub fn new() -> Self {
        Self {
            variable_declarations: HashMap::new(),
            script_buffer: Vec::new(),
        }
    }

    fn add_variable(&mut self, name: &str, val: VariableValue) {
        let id = self.variable_declarations.len();
        self.variable_declarations.insert(
            name.to_string(),
            Variable {
                info: VarInfo {
                    id,
                    var_type: val.get_type(),
                    dims: val.get_dimensions(),
                    vector_size: val.get_vector_size(),
                    matrix_size: val.get_matrix_size(),
                    cube_size: val.get_cube_size(),
                    flags: 0,
                },
                var_type: val.get_type(),
                value: val,
            },
        );
    }

    fn initialize_variables(&mut self) {
        self.add_variable("U_EXPERT", VariableValue::Boolean(false));
        self.add_variable("U_FSE", VariableValue::Boolean(false));
        self.add_variable("U_FSEP", VariableValue::Boolean(false));
        self.add_variable("U_CLS", VariableValue::Boolean(false));

        self.add_variable("U_EXPDATE", VariableValue::Date(0));

        self.add_variable("U_SEC", VariableValue::Integer(0));
        self.add_variable("U_PAGELEN", VariableValue::Integer(0));
        self.add_variable("U_EXPSEC", VariableValue::Integer(0));

        self.add_variable("U_CITY", VariableValue::String(String::new()));
        self.add_variable("U_BDPHONE", VariableValue::String(String::new()));
        self.add_variable("U_HVPHONE", VariableValue::String(String::new()));
        self.add_variable("U_TRANS", VariableValue::String(String::new()));
        self.add_variable("U_CMNT1", VariableValue::String(String::new()));
        self.add_variable("U_CMNT2", VariableValue::String(String::new()));
        self.add_variable("U_PWD", VariableValue::String(String::new()));

        self.add_variable("U_SCROLL", VariableValue::Boolean(false));
        self.add_variable("U_LONGHDR", VariableValue::Boolean(false));
        self.add_variable("U_DEF79", VariableValue::Boolean(false));

        self.add_variable("U_ALIAS", VariableValue::String(String::new()));
        self.add_variable("U_VER", VariableValue::String(String::new()));
        self.add_variable(
            "U_ADDR",
            VariableValue::Dim1(
                VariableType::String,
                vec![VariableValue::String(String::new()); 5],
            ),
        );
        self.add_variable(
            "U_NOTES",
            VariableValue::Dim1(
                VariableType::String,
                vec![VariableValue::String(String::new()); 4],
            ),
        );

        self.add_variable("U_PWDEXP", VariableValue::Date(0));

        self.add_variable(
            "U_ACCOUNT",
            VariableValue::Dim1(VariableType::Integer, vec![VariableValue::Integer(0); 16]),
        );
    }

    pub fn compile(&mut self, prg: &Program) {
        self.initialize_variables();

        prg.main_block.statements.iter().for_each(|s| {
            self.compile_statement(s);
            if self.script_buffer.last() != Some(&(OpCode::END as u16)) {
                self.script_buffer.push(OpCode::END as u16);
            }
        });
    }

    fn compile_statement(&mut self, s: &Statement) {
        match s {
            Statement::Call(smt, pars) => {
                let op_code = smt.opcode as u8;
                self.script_buffer.push(op_code as u16);
                if (pars.len() as i8) < smt.min_args || (pars.len() as i8) > smt.max_args {
                    panic!("Invalid number of parameters for {}", smt.name);
                }
                self.script_buffer.push(pars.len() as u16);
                for expr in pars {
                    let expr_buffer = self.compile_expression(expr);
                    self.script_buffer.extend(expr_buffer);
                }
            }

            Statement::End => {
                self.script_buffer.push(OpCode::END as u16);
            }
            Statement::Comment(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::If(_, _) => todo!(),
            Statement::Break => todo!(),
            Statement::Continue => todo!(),
            Statement::Gosub(_) => todo!(),
            Statement::Return => todo!(),
            Statement::Stop => todo!(),
            Statement::Let(_, _) => todo!(),
            Statement::Goto(_) => todo!(),
            Statement::Label(_) => todo!(),
            Statement::ProcedureCall(_, _) => todo!(),
            Statement::While(_, _) => todo!(),
            Statement::IfThen(_, _, _, _) => todo!(),
            Statement::DoWhile(_, _) => todo!(),
            Statement::For(_, _, _, _, _) => todo!(),
        }
    }

    pub fn create_binary(&self, version: u16) -> Result<Vec<u8>, CompilationError> {
        let mut buffer = Vec::new();
        buffer.extend_from_slice(b"PCBoard Programming Language Executable  ");
        buffer.push(b'0' + (version / 100) as u8);
        buffer.push(b'.');
        let minor = version % 100;
        buffer.push(b'0' + (minor / 10) as u8);
        buffer.push(b'0' + (minor % 10) as u8);
        buffer.extend_from_slice(b"\x0D\x0A\x1A");

        if self.variable_declarations.len() > 65535 {
            return Err(CompilationError::TooManyDeclarations(
                self.variable_declarations.len(),
            ));
        }
        buffer.extend_from_slice(&u16::to_le_bytes(self.variable_declarations.len() as u16));

        let mut vars: Vec<&Variable> = self.variable_declarations.iter().map(|s| s.1).collect();
        vars.sort_by(|a, b| b.info.id.cmp(&a.info.id));

        for d in &vars {
            let var = d.create_var_header(version);
            buffer.extend(var);
        }
        let mut script_buffer = Vec::new();
        for s in &self.script_buffer {
            script_buffer.extend_from_slice(&s.to_le_bytes());
        }
        let mut code_data = encode_rle(&script_buffer);

        buffer.extend_from_slice(&u16::to_le_bytes(self.script_buffer.len() as u16 * 2));
        // in the very unlikely case the rle compressed buffer is larger than the original buffer
        if code_data.len() > script_buffer.len() || version < 300 {
            code_data = script_buffer;
        }

        let mut offset = 0;
        while offset < code_data.len() {
            let mut chunk_size = 2027;
            if offset + chunk_size < code_data.len() && code_data[offset + chunk_size] == 0 {
                chunk_size += 1;
            }
            let end = (offset + chunk_size).min(code_data.len());
            let chunk = &mut code_data[offset..end];
            offset += chunk_size;

            encrypt(chunk, version);
            buffer.extend_from_slice(chunk);
        }
        Ok(buffer)
    }

    fn compile_expression(&mut self, expr: &ppl_engine::ast::Expression) -> Vec<u16> {
        let mut stack = Vec::new();
        self.comp_expr(&mut stack, expr);
        stack.push(0); // push end of expression
        stack
    }

    fn comp_expr(&mut self, stack: &mut Vec<u16>, expr: &ppl_engine::ast::Expression) {
        match expr {
            ppl_engine::ast::Expression::Identifier(_) => todo!(),
            ppl_engine::ast::Expression::Const(constant) => {
                stack.push(self.lookup_constant(constant));
                stack.push(0);
            }
            ppl_engine::ast::Expression::Parens(expr) => {
                self.comp_expr(stack, expr);
                stack.push(0xFFFF);
            }
            ppl_engine::ast::Expression::FunctionCall(_, _) => {
                print!("Function call not implemented");
            }
            ppl_engine::ast::Expression::PredefinedFunctionCall(_, _) => todo!(),
            ppl_engine::ast::Expression::UnaryExpression(op, expr) => {
                self.comp_expr(stack, expr);
                stack.push(*op as u16);
            }
            ppl_engine::ast::Expression::BinaryExpression(op, left, right) => {
                self.comp_expr(stack, left);
                self.comp_expr(stack, right);
                stack.push(*op as u16);
            }
        }
    }

    fn lookup_constant(&mut self, constant: &ppl_engine::ast::Constant) -> u16 {
        let id = self.variable_declarations.len() as u16;
        self.variable_declarations.insert(
            self.variable_declarations.len().to_string(),
            Variable {
                info: VarInfo {
                    id: id as usize,
                    var_type: constant.get_var_type(),
                    dims: 0,
                    vector_size: 0,
                    matrix_size: 0,
                    cube_size: 0,
                    flags: 0,
                },
                var_type: constant.get_var_type(),
                value: constant.get_value(),
            },
        );
        id + 1
    }
}

fn main() {
    println!("PPLC Version 0.01 - PCBoard Programming Language Compiler reborn");
    let arguments: Arguments = argh::from_env();

    let mut file_name = arguments.file_name;

    let extension = Path::new(&file_name).extension().and_then(OsStr::to_str);
    if extension.is_none() {
        file_name.push_str(".pps");
    }

    let read_result = fs::read_to_string(file_name);
    match read_result {
        Ok(content) => {
            let mut prg = parse_program(&content);
            println!("---- Output:");
            transform_ast(&mut prg);
            println!("{}", prg);
            let mut exec = Executable::new();
            exec.compile(&prg);
            match exec.create_binary(200) {
                Ok(bin) => {
                    fs::write("out.ppe", bin).expect("Unable to write file");
                }
                Err(err) => {
                    println!("Error while creating binary {}", err);
                }
            }
        }
        Err(err) => {
            println!("Error while reading file {}", err);
        }
    }
}
