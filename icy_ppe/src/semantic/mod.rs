use std::{
    collections::HashMap,
    sync::{Arc, Mutex},
};

use crate::{
    ast::{
        walk_function_call_expression, walk_function_implementation,
        walk_predefined_call_statement, walk_predefined_function_call_expression,
        walk_procedure_call_statement, walk_procedure_implementation, AstVisitor, CommentAstNode,
        Constant, ConstantExpression, Expression, FunctionCallExpression,
        FunctionDeclarationAstNode, FunctionImplementation, GosubStatement, GotoStatement,
        IdentifierExpression, LabelStatement, LetStatement, ParameterSpecifier,
        PredefinedCallStatement, PredefinedFunctionCallExpression, ProcedureCallStatement,
        ProcedureDeclarationAstNode, ProcedureImplementation, VariableDeclarationStatement,
    },
    compiler::CompilationErrorType,
    executable::{
        EntryType, FuncOpCode, GenericVariableData, OpCode, TableEntry, VarHeader, VariableTable,
        VariableType, VariableValue, USER_VARIABLES,
    },
    parser::{
        self,
        lexer::{Spanned, Token},
        ErrorRepoter, ParserErrorType,
    },
};

#[cfg(test)]
mod find_references_tests;

#[derive(Debug, Clone, PartialEq)]
pub enum ReferenceType {
    PredefinedFunc(FuncOpCode),
    PredefinedProc(OpCode),
    Label(usize),
    Variable(usize),

    Function(usize),
    Procedure(usize),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct References {
    pub variable_type: VariableType,

    pub header: Option<VarHeader>,

    pub declaration: Option<Spanned<String>>,
    pub implementation: Option<Spanned<String>>,
    pub return_types: Vec<Spanned<String>>,

    pub usages: Vec<Spanned<String>>,

    pub table_id: usize,
}

impl References {
    pub fn contains(&self, offset: usize) -> bool {
        for r in &self.usages {
            if r.span.contains(&offset) {
                return true;
            }
        }

        for r in &self.return_types {
            if r.span.contains(&offset) {
                return true;
            }
        }

        if let Some(decl) = &self.implementation {
            if decl.span.contains(&offset) {
                return true;
            }
        }
        if let Some(decl) = &self.declaration {
            decl.span.contains(&offset)
        } else {
            false
        }
    }
}

#[derive(Default)]
pub struct SemanticVisitor {
    version: u16,

    pub errors: Arc<Mutex<ErrorRepoter>>,
    pub references: Vec<(ReferenceType, References)>,

    pub expression_lookup: HashMap<core::ops::Range<usize>, usize>,
    pub require_user_variables: bool,

    // labels
    label_count: usize,
    label_lookup_table: HashMap<unicase::Ascii<String>, usize>,

    // variables
    local_lookup: bool,
    variable_lookup: HashMap<unicase::Ascii<String>, usize>,
    local_variable_lookup: HashMap<unicase::Ascii<String>, usize>,

    // constants
    constant_start_id: usize,
    constants: Vec<TableEntry>,
    const_lookup_table: HashMap<(VariableType, u64), usize>,
    string_lookup_table: HashMap<String, usize>,

    procedures: usize,

    functions: usize,
}

impl SemanticVisitor {
    pub fn new(version: u16, errors: Arc<Mutex<ErrorRepoter>>) -> Self {
        let mut result = Self {
            version,
            errors,
            references: Vec::new(),

            label_count: 0,
            label_lookup_table: HashMap::new(),
            expression_lookup: HashMap::new(),

            local_lookup: false,
            variable_lookup: HashMap::new(),
            local_variable_lookup: HashMap::new(),
            require_user_variables: false,
            procedures: 0,
            functions: 0,

            constant_start_id: 0,
            constants: Vec::new(),
            const_lookup_table: HashMap::new(),
            string_lookup_table: HashMap::new(),
        };
        for user_var in USER_VARIABLES.iter() {
            if user_var.version <= version {
                result.add_predefined_variable(user_var.name, &user_var.value);
            } else {
                break;
            }
        }
        result
    }

    /// Returns the generate variable table of this [`SemanticVisitor`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn generate_variable_table(&mut self) -> VariableTable {
        let mut res = VariableTable::default();
        let mut id = 0;
        if self.require_user_variables {
            for user_var in USER_VARIABLES.iter() {
                if user_var.version <= self.version {
                    self.references[id].1.table_id = id;
                    id += 1;

                    let header = VarHeader {
                        id,
                        variable_type: user_var.value.get_type(),
                        dim: user_var.value.get_dimensions(),
                        vector_size: user_var.value.get_vector_size(),
                        matrix_size: user_var.value.get_matrix_size(),
                        cube_size: user_var.value.get_cube_size(),
                        flags: 0,
                    };
                    let mut entry =
                        TableEntry::new(header, user_var.value.clone(), EntryType::UserVariable);
                    entry.set_name(user_var.name.to_string());
                    res.push(entry);
                } else {
                    break;
                }
            }
        }

        let start = id;
        for i in 0..self.variable_lookup.len() {
            let (rt, r) = &mut self.references[start + i];
            if matches!(rt, ReferenceType::Function(_)) || matches!(rt, ReferenceType::Procedure(_))
            {
                break;
            }

            if r.usages.is_empty() {
                continue;
            }
            id += 1;
            r.table_id = id;

            let mut header = r.header.as_ref().unwrap().clone();
            header.id = id;

            let mut entry = TableEntry::new(
                header,
                r.variable_type.create_empty_value(),
                EntryType::Variable,
            );
            entry.set_name(r.declaration.as_ref().unwrap().token.clone());
            res.push(entry);
        }

        self.constant_start_id = id;

        for c in &self.constants {
            res.push(c.clone());
        }
        res
    }

    fn add_constant(&mut self, constant: &Constant) -> usize {
        let value = constant.get_value();

        if let GenericVariableData::String(str) = &value.generic_data {
            if let Some(id) = self.string_lookup_table.get(str) {
                return *id;
            }
        } else {
            unsafe {
                let key = (constant.get_var_type(), value.data.u64_value);
                if let Some(id) = self.const_lookup_table.get(&key) {
                    return *id;
                }
            }
        }
        let id = self.constants.len();
        let header: VarHeader = VarHeader {
            id,
            variable_type: constant.get_var_type(),
            dim: 0,
            vector_size: 0,
            matrix_size: 0,
            cube_size: 0,
            flags: 0,
        };
        let mut entry = TableEntry::new(header, value.clone(), EntryType::Constant);
        entry.set_name(format!("CONST_{}", id + 1));
        self.constants.push(entry);
        if let GenericVariableData::String(str) = value.generic_data {
            self.string_lookup_table.insert(str, id);
        } else {
            unsafe {
                let key = (constant.get_var_type(), value.data.u64_value);
                self.const_lookup_table.insert(key, id);
            }
        }
        id
    }

    fn add_declaration(
        &mut self,
        reftype: ReferenceType,
        variable_type: VariableType,
        identifier_token: &Spanned<parser::lexer::Token>,
    ) {
        self.expression_lookup
            .insert(identifier_token.span.clone(), self.references.len());
        self.references.push((
            reftype,
            References {
                variable_type,
                implementation: None,
                header: None,
                return_types: vec![],
                declaration: Some(Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                )),
                usages: vec![],
                table_id: 0,
            },
        ));
    }

    fn add_reference(
        &mut self,
        reftype: ReferenceType,
        variable_type: VariableType,
        identifier_token: &Spanned<parser::lexer::Token>,
    ) {
        for (i, r) in &mut self.references.iter_mut().enumerate() {
            if r.0 == reftype {
                r.1.usages.push(Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                ));
                self.expression_lookup
                    .insert(identifier_token.span.clone(), i);
                return;
            }
        }
        log::info!("Label ref {:?}", identifier_token);
        self.expression_lookup
            .insert(identifier_token.span.clone(), self.references.len());
        self.references.push((
            reftype,
            References {
                declaration: None,
                implementation: None,
                header: None,
                return_types: vec![],

                variable_type,
                usages: vec![Spanned::new(
                    identifier_token.token.to_string(),
                    identifier_token.span.clone(),
                )],
                table_id: 0,
            },
        ));
    }

    fn add_label_usage(&mut self, label_token: &Spanned<Token>) {
        let Token::Identifier(identifier) = &label_token.token else {
            log::error!("Invalid label token {:?}", label_token);
            return;
        };
        let idx = if let Some(idx) = self.label_lookup_table.get_mut(identifier) {
            *idx
        } else {
            self.label_count += 1;
            self.label_lookup_table
                .insert(identifier.clone(), self.label_count);
            self.label_count
        };

        self.add_reference(
            ReferenceType::Label(idx),
            VariableType::Unknown,
            label_token,
        );
    }

    fn set_label_declaration(&mut self, label_token: &Spanned<Token>) {
        let Token::Label(identifier) = &label_token.token else {
            log::error!("Invalid label token {:?}", label_token);
            return;
        };
        let idx = if let Some(idx) = self.label_lookup_table.get_mut(identifier) {
            for r in &mut self.references {
                if r.0 == ReferenceType::Label(*idx) && r.1.declaration.is_some() {
                    self.errors.lock().unwrap().report_error(
                        label_token.span.clone(),
                        CompilationErrorType::LabelAlreadyDefined(identifier.to_string()),
                    );
                    return;
                }
            }
            *idx
        } else {
            self.label_count += 1;
            self.label_lookup_table
                .insert(identifier.clone(), self.label_count);
            self.label_count
        };

        log::info!(
            "Label declaration {:?} -> {:?}",
            identifier,
            label_token.span
        );

        let reftype = ReferenceType::Label(idx);

        for (i, r) in &mut self.references.iter_mut().enumerate() {
            if r.0 == reftype {
                r.1.declaration = Some(Spanned::new(
                    label_token.token.to_string(),
                    label_token.span.clone(),
                ));
                self.expression_lookup.insert(label_token.span.clone(), i);
                return;
            }
        }

        self.expression_lookup
            .insert(label_token.span.clone(), self.references.len());
        self.references.push((
            reftype,
            References {
                variable_type: VariableType::Unknown,
                implementation: None,
                header: None,
                return_types: vec![],
                declaration: Some(Spanned::new(
                    label_token.token.to_string(),
                    label_token.span.clone(),
                )),
                usages: vec![],
                table_id: 0,
            },
        ));
    }

    fn start_parse_function_body(&mut self) {
        self.label_lookup_table.clear();
        self.local_variable_lookup.clear();
        self.local_lookup = true;
    }

    fn end_parse_function_body(&mut self) {
        self.local_variable_lookup.clear();
        self.local_lookup = false;
    }

    fn has_variable_defined(&self, id: &unicase::Ascii<String>) -> bool {
        if self.local_lookup {
            return self.local_variable_lookup.contains_key(id);
        }
        self.variable_lookup.contains_key(id)
    }

    fn add_predefined_variable(&mut self, name: &str, val: &VariableValue) {
        let val = val.clone();
        let id = self.references.len();
        let header = VarHeader {
            id,
            variable_type: val.get_type(),
            dim: val.get_dimensions(),
            vector_size: val.get_vector_size(),
            matrix_size: val.get_matrix_size(),
            cube_size: val.get_cube_size(),
            flags: 0,
        };
        self.references.push((
            ReferenceType::Variable(id),
            References {
                variable_type: val.get_type(),
                header: Some(header),
                declaration: None,
                implementation: None,
                return_types: vec![],
                usages: vec![],
                table_id: 0,
            },
        ));
        self.variable_lookup
            .insert(unicase::Ascii::new(name.to_string()), id);
    }

    fn add_variable(
        &mut self,
        variable_type: VariableType,
        identifier: &Spanned<parser::lexer::Token>,
        dim: u8,
        vector_size: usize,
        matrix_size: usize,
        cube_size: usize,
    ) {
        let id = self.references.len();
        self.add_declaration(ReferenceType::Variable(id), variable_type, identifier);

        let header = VarHeader {
            id,
            variable_type,
            dim,
            vector_size,
            matrix_size,
            cube_size,
            flags: 0,
        };
        self.references.last_mut().unwrap().1.header = Some(header);
        if self.local_lookup {
            self.local_variable_lookup
                .insert(unicase::Ascii::new(identifier.token.to_string()), id);
        } else {
            self.variable_lookup
                .insert(unicase::Ascii::new(identifier.token.to_string()), id);
        }
    }

    fn lookup_variable(&mut self, id: &unicase::Ascii<String>) -> Option<usize> {
        if self.local_lookup {
            if let Some(idx) = self.local_variable_lookup.get(id) {
                return Some(*idx);
            }
        }

        if let Some(idx) = self.variable_lookup.get(id) {
            return Some(*idx);
        }
        None
    }

    fn add_reference_to(&mut self, identifier: &Spanned<Token>, idx: usize) {
        self.expression_lookup.insert(identifier.span.clone(), idx);
        self.references[idx].1.usages.push(Spanned::new(
            identifier.token.to_string(),
            identifier.span.clone(),
        ));
    }

    fn add_parameters(&mut self, parameters: &[ParameterSpecifier]) {
        for param in parameters {
            let id = self.references.len();
            self.add_declaration(
                ReferenceType::Variable(id),
                param.get_variable_type(),
                param.get_variable().get_identifier_token(),
            );
            self.local_variable_lookup.insert(
                unicase::Ascii::new(param.get_variable().get_identifier().to_string()),
                id,
            );
        }
    }

    fn check_argument_is_variable(&mut self, arg: usize, v: &Expression) -> bool {
        // that the identifier/dim is in the vtable is checked in argument evaluation
        if let Expression::Identifier(_) = v {
            return true;
        }
        if let Expression::FunctionCall(a) = v {
            if let Some(idx) = self.lookup_variable(a.get_identifier()) {
                let (rt, _) = &mut self.references[idx];
                return matches!(rt, ReferenceType::Variable(_));
            }
        }
        self.errors.lock().unwrap().report_error(
            v.get_span().clone(),
            CompilationErrorType::VariableExpected(arg + 1),
        );
        false
    }

    fn check_arg_count(
        &mut self,
        arg_count_expected: usize,
        arg_count: usize,
        identifier_token: &Spanned<Token>,
    ) -> bool {
        if arg_count_expected < arg_count {
            self.errors.lock().unwrap().report_error(
                identifier_token.span.clone(),
                ParserErrorType::TooFewArguments(
                    identifier_token.token.to_string(),
                    arg_count,
                    arg_count_expected as i8,
                ),
            );
            return false;
        }
        if arg_count_expected > arg_count {
            self.errors.lock().unwrap().report_error(
                identifier_token.span.clone(),
                ParserErrorType::TooManyArguments(
                    identifier_token.token.to_string(),
                    arg_count,
                    arg_count_expected as i8,
                ),
            );
            return false;
        }
        true
    }
}

impl AstVisitor<()> for SemanticVisitor {
    fn visit_identifier_expression(&mut self, identifier: &IdentifierExpression) {
        if let Some(idx) = self.lookup_variable(identifier.get_identifier()) {
            let (rt, r) = &mut self.references[idx];
            if matches!(rt, ReferenceType::Function(_)) {
                let identifier = identifier.get_identifier_token();
                self.expression_lookup.insert(identifier.span.clone(), idx);
                r.return_types.push(Spanned::new(
                    identifier.token.to_string(),
                    identifier.span.clone(),
                ));
            } else if matches!(rt, ReferenceType::Variable(_)) {
                let identifier = identifier.get_identifier_token();
                self.expression_lookup.insert(identifier.span.clone(), idx);
                r.usages.push(Spanned::new(
                    identifier.token.to_string(),
                    identifier.span.clone(),
                ));
            }
        } else {
            self.errors.lock().unwrap().report_error(
                identifier.get_identifier_token().span.clone(),
                CompilationErrorType::VariableNotFound(identifier.get_identifier().to_string()),
            );
        }
    }

    fn visit_constant_expression(&mut self, constant: &ConstantExpression) {
        self.add_constant(constant.get_constant_value());
    }

    fn visit_predefined_function_call_expression(
        &mut self,
        call: &PredefinedFunctionCallExpression,
    ) {
        self.add_reference(
            ReferenceType::PredefinedFunc(call.get_func().opcode),
            VariableType::Function,
            call.get_identifier_token(),
        );
        walk_predefined_function_call_expression(self, call);
    }

    fn visit_comment(&mut self, _comment: &CommentAstNode) {
        // nothing yet
    }

    fn visit_predefined_call_statement(&mut self, call_stmt: &PredefinedCallStatement) {
        let def = call_stmt.get_func();
        if def.opcode == OpCode::GETUSER
            || def.opcode == OpCode::PUTUSER
            || def.opcode == OpCode::GETALTUSER
            || def.opcode == OpCode::FREALTUSER
            || def.opcode == OpCode::DELUSER
            || def.opcode == OpCode::ADDUSER
        {
            self.require_user_variables = true;
        }

        match def.sig {
            crate::executable::StatementSignature::Invalid => panic!("Invalid signature"),
            crate::executable::StatementSignature::ArgumentsWithVariable(v, arg_count) => {
                if !self.check_arg_count(
                    arg_count,
                    call_stmt.get_arguments().len(),
                    call_stmt.get_identifier_token(),
                ) {
                    return;
                }
                if v > 0
                    && !self.check_argument_is_variable(v - 1, &call_stmt.get_arguments()[v - 1])
                {
                    return;
                }
            }
            crate::executable::StatementSignature::VariableArguments(_) => {}
            crate::executable::StatementSignature::SpecialCaseDlockg => {
                if !self.check_arg_count(
                    3,
                    call_stmt.get_arguments().len(),
                    call_stmt.get_identifier_token(),
                ) {
                    return;
                }
                if !self.check_argument_is_variable(2, &call_stmt.get_arguments()[2]) {
                    return;
                }
            }
            crate::executable::StatementSignature::SpecialCaseDcreate => {
                if !self.check_arg_count(
                    4,
                    call_stmt.get_arguments().len(),
                    call_stmt.get_identifier_token(),
                ) {
                    return;
                }
                if !self.check_argument_is_variable(3, &call_stmt.get_arguments()[3]) {
                    return;
                }
            }
            crate::executable::StatementSignature::SpecialCaseSort => {
                if !self.check_arg_count(
                    2,
                    call_stmt.get_arguments().len(),
                    call_stmt.get_identifier_token(),
                ) {
                    return;
                }

                for i in 0..=1 {
                    if let Expression::Identifier(a) = &call_stmt.get_arguments()[i] {
                        if let Some(idx) = self.lookup_variable(a.get_identifier()) {
                            let (_rt, r) = &mut self.references[idx];
                            if let Some(header) = &r.header {
                                if header.dim != 1 {
                                    self.errors.lock().unwrap().report_error(
                                        a.get_identifier_token().span.clone(),
                                        CompilationErrorType::SortArgumentDimensionError(
                                            header.dim,
                                        ),
                                    );
                                    return;
                                }
                            }
                        } else {
                            self.errors.lock().unwrap().report_error(
                                call_stmt.get_arguments()[i].get_span().clone(),
                                CompilationErrorType::VariableExpected(i + 1),
                            );
                        }
                    } else {
                        self.errors.lock().unwrap().report_error(
                            call_stmt.get_arguments()[i].get_span().clone(),
                            CompilationErrorType::VariableExpected(i + 1),
                        );
                    }
                }
            }
            crate::executable::StatementSignature::SpecialCaseVarSeg => {
                if !self.check_arg_count(
                    2,
                    call_stmt.get_arguments().len(),
                    call_stmt.get_identifier_token(),
                ) {
                    return;
                }
                for (v, arg) in call_stmt.get_arguments().iter().enumerate() {
                    if !self.check_argument_is_variable(v, arg) {
                        return;
                    }
                }
            }
            crate::executable::StatementSignature::SpecialCasePop => {
                for (v, arg) in call_stmt.get_arguments().iter().enumerate() {
                    if !self.check_argument_is_variable(v, arg) {
                        return;
                    }
                }
            }
        }

        self.add_reference(
            ReferenceType::PredefinedProc(call_stmt.get_func().opcode),
            VariableType::Procedure,
            call_stmt.get_identifier_token(),
        );
        walk_predefined_call_statement(self, call_stmt);
    }

    fn visit_function_call_expression(&mut self, call: &FunctionCallExpression) {
        let mut found = false;
        let mut arg_count = 0;
        if let Some(idx) = self.lookup_variable(call.get_identifier()) {
            let (rt, r) = &mut self.references[idx];
            if matches!(rt, ReferenceType::Function(_)) || matches!(rt, ReferenceType::Variable(_))
            {
                self.expression_lookup
                    .insert(call.get_identifier_token().span.clone(), idx);

                arg_count = if let Some(header) = &r.header {
                    header.dim as usize
                } else {
                    self.errors.lock().unwrap().report_error(
                        call.get_identifier_token().span.clone(),
                        CompilationErrorType::FunctionNotFound(call.get_identifier().to_string()),
                    );
                    0
                };
                r.usages.push(Spanned::new(
                    call.get_identifier().to_string(),
                    call.get_identifier_token().span.clone(),
                ));
                found = true;
            }
        }

        if found {
            self.check_arg_count(
                arg_count,
                call.get_arguments().len(),
                call.get_identifier_token(),
            );
        } else {
            self.errors.lock().unwrap().report_error(
                call.get_identifier_token().span.clone(),
                CompilationErrorType::FunctionNotFound(call.get_identifier().to_string()),
            );
        }
        walk_function_call_expression(self, call);
    }

    fn visit_goto_statement(&mut self, goto: &GotoStatement) {
        self.add_label_usage(goto.get_label_token());
    }

    fn visit_gosub_statement(&mut self, gosub: &GosubStatement) {
        self.add_label_usage(gosub.get_label_token());
    }

    fn visit_label_statement(&mut self, label: &LabelStatement) {
        self.set_label_declaration(label.get_label_token());
    }

    fn visit_let_statement(&mut self, let_stmt: &LetStatement) {
        if let Some(idx) = self.lookup_variable(let_stmt.get_identifier()) {
            if self.references[idx].1.variable_type == VariableType::Procedure {
                self.errors.lock().unwrap().report_error(
                    let_stmt.get_identifier_token().span.clone(),
                    CompilationErrorType::InvalidLetVariable,
                );
            } else {
                if let Some(header) = &self.references[idx].1.header {
                    self.check_arg_count(
                        header.dim as usize,
                        let_stmt.get_arguments().len(),
                        let_stmt.get_identifier_token(),
                    );
                } else {
                    self.errors.lock().unwrap().report_error(
                        let_stmt.get_identifier_token().span.clone(),
                        CompilationErrorType::InvalidLetVariable,
                    );
                }

                self.add_reference_to(let_stmt.get_identifier_token(), idx);
            }
        } else {
            self.errors.lock().unwrap().report_error(
                let_stmt.get_identifier_token().span.clone(),
                CompilationErrorType::VariableNotFound(let_stmt.get_identifier().to_string()),
            );
        }
        for arg in let_stmt.get_arguments() {
            arg.visit(self);
        }
        let_stmt.get_value_expression().visit(self);
    }

    fn visit_variable_declaration_statement(&mut self, var_decl: &VariableDeclarationStatement) {
        for v in var_decl.get_variables() {
            if self.has_variable_defined(v.get_identifier()) {
                self.errors.lock().unwrap().report_error(
                    v.get_identifier_token().span.clone(),
                    CompilationErrorType::VariableAlreadyDefined(v.get_identifier().to_string()),
                );
                continue;
            }

            self.add_variable(
                var_decl.get_variable_type(),
                v.get_identifier_token(),
                v.get_dimensions().len() as u8,
                v.get_vector_size(),
                v.get_matrix_size(),
                v.get_cube_size(),
            );
        }
    }

    fn visit_procedure_call_statement(&mut self, call: &ProcedureCallStatement) {
        let mut found = false;
        if let Some(idx) = self.lookup_variable(call.get_identifier()) {
            if matches!(self.references[idx].0, ReferenceType::Function(_))
                || matches!(self.references[idx].0, ReferenceType::Variable(_))
            {
                self.add_reference_to(call.get_identifier_token(), idx);
                found = true;
            }

            if matches!(self.references[idx].0, ReferenceType::Procedure(_)) {
                /* TODO: Check VAR arguments in procedure calls.
                if let Some(header) = self.references[idx].1.header {
                    for i in 0..call.get_arguments().len() {
                        unsafe {
                            if decl.value.data.procedure_value.pass_flags & (1 << i) != 0
                                && !self.check_argument_is_variable(i, &call_stmt.get_arguments()[i])
                            {
                                return None;
                            }
                        }
                    }
                }
                */

                self.add_reference_to(call.get_identifier_token(), idx);
                found = true;
            }
        }

        if !found {
            self.errors.lock().unwrap().report_error(
                call.get_identifier_token().span.clone(),
                CompilationErrorType::ProcedureNotFound(call.get_identifier().to_string()),
            );
        }

        walk_procedure_call_statement(self, call);
    }

    fn visit_procedure_declaration(&mut self, proc_decl: &ProcedureDeclarationAstNode) {
        if self.has_variable_defined(proc_decl.get_identifier()) {
            self.errors.lock().unwrap().report_error(
                proc_decl.get_identifier_token().span.clone(),
                CompilationErrorType::VariableAlreadyDefined(
                    proc_decl.get_identifier().to_string(),
                ),
            );
            return;
        }
        self.variable_lookup
            .insert(proc_decl.get_identifier().clone(), self.references.len());
        self.add_declaration(
            ReferenceType::Procedure(self.procedures),
            VariableType::Procedure,
            proc_decl.get_identifier_token(),
        );
        self.procedures += 1;
    }

    fn visit_function_declaration(&mut self, func_decl: &FunctionDeclarationAstNode) {
        if self.has_variable_defined(func_decl.get_identifier()) {
            self.errors.lock().unwrap().report_error(
                func_decl.get_identifier_token().span.clone(),
                CompilationErrorType::VariableAlreadyDefined(
                    func_decl.get_identifier().to_string(),
                ),
            );
            return;
        }
        self.variable_lookup
            .insert(func_decl.get_identifier().clone(), self.references.len());
        self.add_declaration(
            ReferenceType::Function(self.functions),
            VariableType::Function,
            func_decl.get_identifier_token(),
        );
        self.functions += 1;
    }

    fn visit_function_implementation(&mut self, function: &FunctionImplementation) {
        if let Some(idx) = self.lookup_variable(function.get_identifier()) {
            let identifier = function.get_identifier_token();
            self.expression_lookup.insert(identifier.span.clone(), idx);
            self.references[idx].1.implementation = Some(Spanned::new(
                identifier.token.to_string(),
                identifier.span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                function.get_identifier_token().span.clone(),
                CompilationErrorType::FunctionNotFound(function.get_identifier().to_string()),
            );
        }

        self.start_parse_function_body();
        self.add_parameters(function.get_parameters());
        walk_function_implementation(self, function);
        self.end_parse_function_body();
    }

    fn visit_procedure_implementation(&mut self, procedure: &ProcedureImplementation) {
        if let Some(idx) = self.lookup_variable(procedure.get_identifier()) {
            let identifier = procedure.get_identifier_token();
            self.expression_lookup.insert(identifier.span.clone(), idx);
            self.references[idx].1.implementation = Some(Spanned::new(
                identifier.token.to_string(),
                identifier.span.clone(),
            ));
        } else {
            self.errors.lock().unwrap().report_error(
                procedure.get_identifier_token().span.clone(),
                CompilationErrorType::ProcedureNotFound(procedure.get_identifier().to_string()),
            );
        }

        self.start_parse_function_body();
        self.add_parameters(procedure.get_parameters());
        walk_procedure_implementation(self, procedure);
        self.end_parse_function_body();
    }

    fn visit_program(&mut self, program: &crate::ast::Ast) {
        for node in &program.nodes {
            match node {
                crate::ast::AstNode::Function(_) | crate::ast::AstNode::Procedure(_) => {}
                _ => {
                    node.visit(self);
                }
            }
        }

        for node in &program.nodes {
            match node {
                crate::ast::AstNode::Function(_) | crate::ast::AstNode::Procedure(_) => {
                    node.visit(self);
                }
                _ => {}
            }
        }

        for (_i, r) in &mut self.references.iter() {
            let Some(decl) = &r.declaration else {
                continue;
            };

            if r.variable_type == VariableType::Function
                || r.variable_type == VariableType::Procedure
            {
                if r.implementation.is_none() {
                    self.errors.lock().unwrap().report_error(
                        decl.span.clone(),
                        CompilationErrorType::MissingImplementation(decl.token.to_string()),
                    );
                }
            } else if r.usages.is_empty() {
                self.errors.lock().unwrap().report_warning(
                    decl.span.clone(),
                    CompilationErrorType::UnusedVariable(decl.token.to_string()),
                );
            }
        }

        // search if any user variables are used.
        if !self.require_user_variables {
            for (i, user_var) in USER_VARIABLES.iter().enumerate() {
                if user_var.version <= self.version && !self.references[i].1.usages.is_empty() {
                    self.require_user_variables = true;
                    break;
                }
            }
        }
    }
}
