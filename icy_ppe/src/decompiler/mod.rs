use std::collections::{HashMap, HashSet};

use crate::{
    ast::{
        Ast, AstNode, BinaryExpression, CommentAstNode, Constant, ConstantExpression, Expression,
        FunctionCallExpression, FunctionDeclarationAstNode, FunctionImplementation, GosubStatement,
        GotoStatement, IdentifierExpression, IfStatement, LabelStatement, LetStatement,
        ParameterSpecifier, ParensExpression, PredefinedCallStatement,
        PredefinedFunctionCallExpression, ProcedureCallStatement, ProcedureDeclarationAstNode,
        ProcedureImplementation, Statement, UnaryExpression, UnaryOp, VariableDeclarationStatement,
        VariableSpecifier,
    },
    executable::{
        read_file, DeserializationError, EntryType, Executable, OpCode, PPECommand, PPEExpr,
        PPEScript, TableEntry, VariableType,
    },
    Res,
};

pub mod constant_scan_visitor;
pub mod reconstruct;
pub mod rename_visitor;

/// # Errors
///
pub fn load_file(file_name: &str, print_header_information: bool) -> Res<Ast> {
    let executable = read_file(file_name, print_header_information)?;
    let mut d = Decompiler::new(executable).unwrap();

    let ast = d.decompile();

    /*
    d.do_pass1();
    d.generate_variable_declarations(&mut ast);
    d.do_pass2(&mut ast);

    ast.nodes
        .extend(d.statements.iter().map(|s| AstNode::Statement(s.clone())));

        */
    ast
}

#[derive(Default)]
pub struct Decompiler {
    executable: Executable,
    script: PPEScript,

    functions: Vec<AstNode>,
    label_lookup: HashMap<usize, usize>,
    function_lookup: HashMap<usize, usize>,
    cur_ptr: usize,
}

impl Decompiler {
    /// .
    ///
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn new(executable: Executable) -> Result<Self, DeserializationError> {
        let script = PPEScript::from_ppe_file(&executable)?;
        Ok(Self {
            executable,
            script,
            label_lookup: HashMap::new(),
            function_lookup: HashMap::new(),
            functions: Vec::new(),
            cur_ptr: 0,
        })
    }

    fn analyze_labels(&mut self) -> HashMap<usize, usize> {
        let mut labels = HashSet::new();

        for statement in &self.script.statements {
            match statement.command {
                PPECommand::Goto(label)
                | PPECommand::Gosub(label)
                | PPECommand::IfNot(_, label)
                | PPECommand::While(_, _, label) => {
                    labels.insert(label);
                }
                _ => {}
            }
        }

        let label_list = labels.into_iter().collect::<Vec<usize>>();
        let mut cur_label = 0;

        let mut label_offsets = HashMap::new();

        for label in label_list {
            for statement in &self.script.statements {
                if statement.span.start * 2 == label {
                    label_offsets.insert(label, cur_label);
                    cur_label += 1;
                    break;
                }
            }
        }

        label_offsets
    }

    /// Returns the decompile of this [`Decompiler`].
    /// # Errors
    ///
    /// This function will return an error if .
    pub fn decompile(&mut self) -> Res<Ast> {
        self.label_lookup = self.analyze_labels();
        self.executable.variable_table.analyze_usage(&self.script);
        self.executable.variable_table.generate_names();

        let mut ast = Ast::default();

        self.generate_function_declarations(&mut ast);
        self.generate_variable_declarations(&mut ast);

        while self.cur_ptr < self.script.statements.len() {
            let statement = &self.script.statements[self.cur_ptr];
            let byte_offset = statement.span.start * 2;

            if let Some(func) = self.function_lookup.get(&byte_offset) {
                self.parse_function(*func);
                continue;
            }

            if self.label_lookup.contains_key(&(byte_offset)) {
                let label = self.get_label_name(byte_offset);
                ast.nodes
                    .push(AstNode::Statement(LabelStatement::create_empty_statement(
                        label,
                    )));
            }
            if let Some(bugs) = self.script.bugged_offsets.get_mut(&statement.span.start) {
                for bug in bugs.drain(..) {
                    ast.nodes
                        .push(AstNode::Statement(CommentAstNode::create_empty_statement(
                            format!(" PPLC bug use detected in next statement: {bug}"),
                        )));
                }
            }
            let stmt = self.decompile_statement(&statement.command);
            ast.nodes.push(AstNode::Statement(stmt));
            self.cur_ptr += 1;
        }
        ast.nodes
            .push(AstNode::Statement(LabelStatement::create_empty_statement(
                unicase::Ascii::new("EXIT_LABEL".to_string()),
            )));
        ast.nodes.append(&mut self.functions);

        for (k, bugs) in &self.script.bugged_offsets {
            for bug in bugs {
                ast.nodes
                    .push(AstNode::Statement(CommentAstNode::create_empty_statement(
                        format!("{:04X}: statement: {}", k, bug),
                    )));
            }
        }

        if !self.script.bugged_offsets.is_empty() {
            ast.nodes
                .push(AstNode::Statement(CommentAstNode::create_empty_statement(
                    format!(
                        " {} error(s) detected while decompiling",
                        self.script.bugged_offsets.len(),
                    ),
                )));

            ast.nodes
                .push(AstNode::Statement(CommentAstNode::create_empty_statement(
                    String::new(),
                )));
            ast.nodes.push(AstNode::Statement(CommentAstNode::create_empty_statement(
                "Some PPEs got altered to avoid decompilation. PCBoard doesn't handle unary expressions correcty.".to_string(),
            )));
            ast.nodes
                .push(AstNode::Statement(CommentAstNode::create_empty_statement(
                    "Search for 'PPLC bug' and look out for !!!<expr> or !<expr>*!<expr> cases."
                        .to_string(),
                )));
        }
        Ok(ast)
    }

    fn generate_variable_declarations(&mut self, ast: &mut Ast) {
        for var in self.executable.variable_table.get_entries() {
            if let EntryType::Variable = var.entry_type {
                if var.header.flags & 0x1 != 0 {
                    continue;
                }
                let var_decl = generate_variable_declaration(var);
                ast.nodes.push(AstNode::Statement(var_decl));
            }
        }
    }

    fn generate_function_declarations(&mut self, ast: &mut Ast) {
        for entry in self.executable.variable_table.get_entries() {
            match entry.entry_type {
                EntryType::Function | EntryType::Procedure => {
                    // offset == 0 seems to be a bug used for preventing decompilation
                    if unsafe { entry.value.data.procedure_value.start_offset } == 0 {
                        continue;
                    }
                    println!("add func {:0X} for {:0X} ", entry.header.id, unsafe {
                        entry.value.data.procedure_value.start_offset
                    });
                    self.function_lookup.insert(
                        unsafe { entry.value.data.procedure_value.start_offset as usize },
                        entry.header.id,
                    );

                    if entry.header.variable_type == VariableType::Function {
                        let parameters = self.generate_parameter_list(entry);
                        let return_value = self.executable.variable_table.get_var_entry(unsafe {
                            entry.value.data.function_value.return_var as usize
                        });
                        let func_decl = FunctionDeclarationAstNode::empty(
                            unicase::Ascii::new(entry.name.clone()),
                            parameters,
                            return_value.header.variable_type,
                        );
                        ast.nodes.push(AstNode::FunctionDeclaration(func_decl));
                    } else {
                        let parameters = self.generate_parameter_list(entry);
                        let proc_decl = ProcedureDeclarationAstNode::empty(
                            unicase::Ascii::new(entry.name.clone()),
                            parameters,
                        );
                        ast.nodes.push(AstNode::ProcedureDeclaration(proc_decl));
                    }
                }
                _ => {}
            }
        }
    }

    fn decompile_expression(&self, expression: &PPEExpr) -> Expression {
        match expression {
            PPEExpr::Invalid => todo!(),
            PPEExpr::Value(id) => unsafe {
                let entry = self.executable.variable_table.get_var_entry(*id);
                if entry.entry_type == EntryType::Constant {
                    let constant = match entry.value.get_type() {
                        VariableType::BigStr | VariableType::String => {
                            Constant::String(entry.value.as_string())
                        }
                        VariableType::Float => {
                            Constant::Double(entry.value.data.float_value as f64)
                        }
                        VariableType::Double => Constant::Double(entry.value.data.double_value),
                        VariableType::Boolean => Constant::Boolean(entry.value.data.bool_value),
                        VariableType::Unsigned => {
                            Constant::Unsigned(entry.value.data.unsigned_value)
                        }
                        //VariableType::Integer |
                        _ => Constant::Integer(entry.value.as_int()),
                    };
                    ConstantExpression::create_empty_expression(constant)
                } else {
                    IdentifierExpression::create_empty_expression(unicase::Ascii::new(
                        entry.name.clone(),
                    ))
                }
            },
            PPEExpr::UnaryExpression(op, expr) => {
                UnaryExpression::create_empty_expression(*op, self.decompile_expression(expr))
            }
            PPEExpr::BinaryExpression(op, left, right) => {
                ParensExpression::create_empty_expression(
                    BinaryExpression::create_empty_expression(
                        *op,
                        self.decompile_expression(left),
                        self.decompile_expression(right),
                    ),
                )
            }
            PPEExpr::Dim(id, dims) => FunctionCallExpression::create_empty_expression(
                self.get_variable_name(*id),
                dims.iter().map(|e| self.decompile_expression(e)).collect(),
            ),
            PPEExpr::PredefinedFunctionCall(f, args) => {
                PredefinedFunctionCallExpression::create_empty_expression(
                    f,
                    args.iter().map(|e| self.decompile_expression(e)).collect(),
                )
            }
            PPEExpr::FunctionCall(f, args) => FunctionCallExpression::create_empty_expression(
                self.get_variable_name(*f),
                args.iter().map(|e| self.decompile_expression(e)).collect(),
            ),
        }
    }

    fn decompile_statement(&self, statement: &PPECommand) -> Statement {
        match statement {
            PPECommand::EndFunc | PPECommand::EndProc | PPECommand::End => {
                PredefinedCallStatement::create_empty_statement(
                    OpCode::END.get_definition(),
                    Vec::new(),
                )
            }
            PPECommand::Return => PredefinedCallStatement::create_empty_statement(
                OpCode::RETURN.get_definition(),
                Vec::new(),
            ),
            PPECommand::Stop => PredefinedCallStatement::create_empty_statement(
                OpCode::STOP.get_definition(),
                Vec::new(),
            ),
            PPECommand::Goto(label) => {
                GotoStatement::create_empty_statement(self.get_label_name(*label))
            }
            PPECommand::Gosub(label) => {
                GosubStatement::create_empty_statement(self.get_label_name(*label))
            }

            PPECommand::IfNot(expr, label) => {
                IfStatement::create_empty_statement(
                    //self.decompile_expression(expr),
                    UnaryExpression::create_empty_expression(
                        UnaryOp::Not,
                        self.decompile_expression(expr),
                    ),
                    GotoStatement::create_empty_statement(self.get_label_name(*label)),
                )
            }
            PPECommand::While(_, _, _) => todo!(),
            PPECommand::ProcedureCall(p, args) => ProcedureCallStatement::create_empty_statement(
                self.get_variable_name(*p),
                args.iter().map(|e| self.decompile_expression(e)).collect(),
            ),
            PPECommand::PredefinedCall(p, args) => PredefinedCallStatement::create_empty_statement(
                p,
                args.iter().map(|e| self.decompile_expression(e)).collect(),
            ),
            PPECommand::Let(id, expr) => {
                let (identifier, arguments) = match self.decompile_expression(id) {
                    Expression::FunctionCall(f) => {
                        (f.get_identifier().clone(), f.get_arguments().clone())
                    }
                    Expression::Identifier(id) => (id.get_identifier().clone(), Vec::new()),
                    x => panic!("Invalid expression {x:?}"),
                };
                LetStatement::create_empty_statement(
                    identifier,
                    arguments,
                    self.decompile_expression(expr),
                )
            }
        }
    }

    fn get_label_name(&self, label: usize) -> unicase::Ascii<String> {
        if let Some(name) = self.label_lookup.get(&label) {
            unicase::Ascii::new(format!("LABEL{:03}", *name))
        } else {
            unicase::Ascii::new("EXIT_LABEL".to_string())
        }
    }

    fn get_variable_name(&self, p: usize) -> unicase::Ascii<String> {
        unicase::Ascii::new(self.executable.variable_table.get_var_entry(p).name.clone())
    }

    fn parse_function(&mut self, func: usize) {
        let entry = self.executable.variable_table.get_var_entry(func);
        let mut func_body = self.generate_local_variable_declarations(entry);
        while self.cur_ptr < self.script.statements.len() {
            let statement = &self.script.statements[self.cur_ptr];
            let byte_offset = statement.span.start * 2;
            if matches!(statement.command, PPECommand::EndFunc)
                || matches!(statement.command, PPECommand::EndProc)
            {
                if entry.header.variable_type == VariableType::Function {
                    let parameters = self.generate_parameter_list(entry);
                    let return_value = self.executable.variable_table.get_var_entry(unsafe {
                        entry.value.data.function_value.return_var as usize
                    });
                    let func_impl = FunctionImplementation::empty(
                        func,
                        unicase::Ascii::new(entry.name.clone()),
                        parameters,
                        return_value.header.variable_type,
                        func_body,
                    );
                    self.functions.push(AstNode::Function(func_impl));
                } else {
                    let parameters = self.generate_parameter_list(entry);
                    let proc_impl = ProcedureImplementation::empty(
                        func,
                        unicase::Ascii::new(entry.name.clone()),
                        parameters,
                        func_body,
                    );
                    self.functions.push(AstNode::Procedure(proc_impl));
                }
                self.cur_ptr += 1;
                break;
            }

            if self.label_lookup.contains_key(&(byte_offset)) {
                func_body.push(LabelStatement::create_empty_statement(
                    self.get_label_name(byte_offset),
                ));
            }
            func_body.push(self.decompile_statement(&statement.command));
            self.cur_ptr += 1;
        }

        if self.script.statements[self.cur_ptr].command == PPECommand::End {
            self.cur_ptr += 1;
        }
    }

    fn generate_local_variable_declarations(&self, entry: &TableEntry) -> Vec<Statement> {
        unsafe {
            let mut decl = Vec::new();

            let parameters;
            let first_var;
            let locals;

            if entry.header.variable_type == VariableType::Function {
                parameters = entry.value.data.function_value.parameters as usize;
                first_var = entry.value.data.function_value.first_var_id as usize;
                locals = entry.value.data.function_value.local_variables as usize;
            } else {
                parameters = entry.value.data.procedure_value.parameters as usize;
                first_var = entry.value.data.procedure_value.first_var_id as usize;
                locals = entry.value.data.procedure_value.local_variables as usize;
            }

            for i in parameters..locals {
                let local_var = self
                    .executable
                    .variable_table
                    .get_var_entry(first_var + 1 + i);
                if local_var.entry_type == EntryType::Variable {
                    decl.push(generate_variable_declaration(local_var));
                }
            }

            decl
        }
    }

    fn generate_parameter_list(&self, entry: &TableEntry) -> Vec<ParameterSpecifier> {
        unsafe {
            let mut parameters = Vec::new();

            let to;
            let pass_flags;
            let first_var;

            if entry.header.variable_type == VariableType::Function {
                to = entry.value.data.function_value.parameters as usize;
                first_var = entry.value.data.function_value.first_var_id as usize;
                pass_flags = 0;
            } else {
                to = entry.value.data.procedure_value.parameters as usize;
                pass_flags = entry.value.data.procedure_value.pass_flags;
                first_var = entry.value.data.procedure_value.first_var_id as usize;
            }

            for i in 0..to {
                let param = self
                    .executable
                    .variable_table
                    .get_var_entry(first_var + 1 + i);
                let mut dimensions = Vec::new();
                match param.header.dim {
                    1 => {
                        dimensions.push(param.header.vector_size);
                    }
                    2 => {
                        dimensions.push(param.header.vector_size);
                        dimensions.push(param.header.matrix_size);
                    }
                    3 => {
                        dimensions.push(param.header.vector_size);
                        dimensions.push(param.header.matrix_size);
                        dimensions.push(param.header.cube_size);
                    }
                    _ => {}
                }
                let is_var = pass_flags & (1 << i) != 0;
                parameters.push(ParameterSpecifier::empty(
                    is_var,
                    param.header.variable_type,
                    VariableSpecifier::empty(unicase::Ascii::new(param.name.clone()), dimensions),
                ));
            }

            parameters
        }
    }
}

fn generate_variable_declaration(var: &TableEntry) -> Statement {
    let dims = match var.header.dim {
        1 => {
            vec![var.header.vector_size]
        }
        2 => {
            vec![var.header.vector_size, var.header.matrix_size]
        }
        3 => {
            vec![
                var.header.vector_size,
                var.header.matrix_size,
                var.header.cube_size,
            ]
        }
        _ => Vec::new(),
    };
    VariableDeclarationStatement::create_empty_statement(
        var.header.variable_type,
        vec![VariableSpecifier::empty(
            unicase::Ascii::new(var.name.clone()),
            dims,
        )],
    )
}

/// .
///
/// # Panics
///
/// Panics if .
#[must_use]
pub fn decompile(executable: Executable, raw: bool) -> Ast {
    let mut d = Decompiler::new(executable).unwrap();
    d.decompile().unwrap()
}
