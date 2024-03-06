use argh::FromArgs;
use ppl_engine::{
    ast::{Program, Statement, VariableType, VariableValue},
    compiler::transform_ast,
    crypt::{encode_rle, encrypt},
    parser::parse_program,
    tables::OpCode,
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

#[derive(Debug)]
struct Function {
    pub index: usize,

    pub args: i32,
    pub total_var: i32,
    pub start: i32,
    pub first_var: i32,
    pub return_var: i32,

    pub usages: Vec<usize>,
}
impl Function {
    fn add_usage(&mut self, len: usize) {
        self.usages.push(len);
    }
}

impl Variable {
    fn create_var_header(&self, exe: &Executable, version: u16) -> Vec<u8> {
        let mut buffer = Vec::new();
        buffer.extend(u16::to_le_bytes(self.info.id as u16 + 1));

        buffer.push(self.info.dims);

        buffer.extend(u16::to_le_bytes(self.info.vector_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.matrix_size as u16));
        buffer.extend(u16::to_le_bytes(self.info.cube_size as u16));

        buffer.push(self.info.var_type as u8);
        buffer.push(self.info.flags);
        encrypt(&mut buffer, version);
        if self.var_type == VariableType::Procedure || self.var_type == VariableType::Function {
            let VariableValue::String(s) = &self.value else {
                panic!("Invalid value type {:?}", self.value);
            };
            let proc = exe.procedure_declarations.get(s).unwrap();

            buffer.push(0);
            buffer.push(0);
            buffer.push(0);
            buffer.push(0);

            buffer.push(proc.args as u8);
            buffer.push(proc.total_var as u8);
            buffer.extend(u16::to_le_bytes(proc.start as u16));
            buffer.extend(u16::to_le_bytes(proc.first_var as u16));
            buffer.extend(u16::to_le_bytes(proc.return_var as u16));
        } else if self.var_type == VariableType::String {
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
    procedure_declarations: HashMap<String, Function>,
    script_buffer: Vec<u16>,
}
const ENDPROC: u16 = 0x00a9;
const ENDFUNC: u16 = 0x00ab;

impl Executable {
    pub fn new() -> Self {
        Self {
            variable_declarations: HashMap::new(),
            procedure_declarations: HashMap::new(),
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

        for (index, d) in prg.declarations.iter().enumerate() {
            match d {
                ppl_engine::ast::Declaration::Function(name, params, retValue) => {
                    self.procedure_declarations.insert(
                        name.clone(),
                        Function {
                            index,
                            args: params.len() as i32,
                            total_var: 0,
                            start: 0,
                            first_var: 0,
                            return_var: 0,
                            usages: Vec::new(),
                        },
                    );
                }
                ppl_engine::ast::Declaration::Procedure(name, params) => {
                    self.procedure_declarations.insert(
                        name.clone(),
                        Function {
                            index,
                            args: params.len() as i32,
                            total_var: 0,
                            start: 0,
                            first_var: 0,
                            return_var: 0,
                            usages: Vec::new(),
                        },
                    );
                }
                ppl_engine::ast::Declaration::Variable(_, _) => {}
            }
        }

        prg.main_block.statements.iter().for_each(|s| {
            self.compile_statement(s);
            if self.script_buffer.last() != Some(&(OpCode::END as u16)) {
                self.script_buffer.push(OpCode::END as u16);
            }
        });

        for p in &prg.procedure_implementations {
            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.start = self.script_buffer.len() as i32 * 2;
                decl.first_var = self.variable_declarations.len() as i32 + 1;
                self.variable_declarations.insert(
                    p.declaration.get_name().clone(),
                    Variable {
                        info: VarInfo {
                            id: self.variable_declarations.len(),
                            var_type: VariableType::Procedure,
                            dims: 0,
                            vector_size: 0,
                            matrix_size: 0,
                            cube_size: 0,
                            flags: 0,
                        },
                        var_type: VariableType::Procedure,
                        value: VariableValue::String(p.declaration.get_name().clone()),
                    },
                );
            }

            p.block.statements.iter().for_each(|s| {
                self.compile_statement(s);
                self.script_buffer.push(ENDPROC);
            });

            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.total_var = self.variable_declarations.len() as i32 - decl.first_var;

                println!("{:?}", decl);
            }
        }

        for p in &prg.function_implementations {
            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.start = self.script_buffer.len() as i32 * 2;
                decl.first_var = p.variable_declarations.len() as i32 + 1;
                decl.return_var = p.declaration.get_return_vartype();

                self.variable_declarations.insert(
                    p.declaration.get_name().clone(),
                    Variable {
                        info: VarInfo {
                            id: decl.first_var as usize,
                            var_type: VariableType::Function,
                            dims: 0,
                            vector_size: 0,
                            matrix_size: 0,
                            cube_size: 0,
                            flags: 0,
                        },
                        var_type: VariableType::Function,
                        value: VariableValue::String(p.declaration.get_name().clone()),
                    },
                );
            }

            p.block.statements.iter().for_each(|s| {
                self.compile_statement(s);
                self.script_buffer.push(ENDFUNC);
            });
            {
                let decl = self
                    .procedure_declarations
                    .get_mut(p.declaration.get_name())
                    .unwrap();
                decl.total_var = self.variable_declarations.len() as i32 - decl.first_var;
            }
        }
        if self.script_buffer.last() != Some(&(OpCode::END as u16)) {
            self.script_buffer.push(OpCode::END as u16);
        }

        for decl in &self.procedure_declarations {
            for idx in &decl.1.usages {
                self.script_buffer[*idx] = decl.1.first_var as u16;
            }
        }
    }

    fn compile_statement(&mut self, s: &Statement) {
        println!("Compiling statement {:?}", s);
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
            Statement::Comment(_) => {
                // ignore
            }
            Statement::Block(block) => {
                block.iter().for_each(|s| self.compile_statement(s));
            }
            Statement::If(_, _) => todo!(),
            Statement::Continue => todo!(),
            Statement::Gosub(_) => todo!(),
            Statement::Return => todo!(),
            Statement::Stop => todo!(),
            Statement::Let(_, _) => todo!(),
            Statement::Goto(_) => todo!(),
            Statement::Label(_) => todo!(),
            Statement::ProcedureCall(name, parameters) => {
                self.script_buffer.push(OpCode::PCALL as u16);

                // will be filled later.
                self.procedure_declarations
                    .get_mut(name)
                    .unwrap()
                    .add_usage(self.script_buffer.len());
                self.script_buffer.push(0);

                for p in parameters {
                    let expr_buffer = self.compile_expression(p);
                    self.script_buffer.extend(expr_buffer);
                }
                self.script_buffer.push(0);
            }
            Statement::While(_, _) => todo!(),

            Statement::Break => panic!("Break not allowed in output AST."),
            Statement::IfThen(_, _, _, _) => panic!("if then not allowed in output AST."),
            Statement::DoWhile(_, _) => panic!("do while not allowed in output AST."),
            Statement::For(_, _, _, _, _) => panic!("for not allowed in output AST."),
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
            let var = d.create_var_header(self, version);
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
