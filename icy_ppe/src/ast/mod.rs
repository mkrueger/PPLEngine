pub mod constant;
pub use self::constant::Constant;

pub mod expression;
pub use self::expression::*;

pub mod statement;
pub use self::statement::*;

pub mod declaration;
pub use self::declaration::*;

pub mod program;
pub use self::program::*;

pub mod variable_type;
pub use self::variable_type::*;

pub mod variable_value;
pub use self::variable_value::*;

pub mod visitor;
pub use self::visitor::*;

pub mod output_visitor;
pub use self::output_visitor::*;

use crate::parser::lexer::{SpannedToken, Token};

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Comment(CommentAstNode),
    Function(FunctionImplementation),
    Procedure(ProcedureImplementation),
    Statement(Statement),
    ProcedureDeclaration(ProcedureDeclarationAstNode),
    FunctionDeclaration(FunctionDeclarationAstNode),
}

impl AstNode {
    pub fn visit<T: Default, V: AstVisitor<T>>(&self, visitor: &mut V) -> T {
        match self {
            AstNode::Comment(c) => visitor.visit_comment(c),
            AstNode::Function(f) => visitor.visit_function_implementation(f),
            AstNode::Procedure(p) => visitor.visit_procedure_implementation(p),
            AstNode::Statement(s) => s.visit(visitor),
            AstNode::ProcedureDeclaration(p) => visitor.visit_procedure_declaration(p),
            AstNode::FunctionDeclaration(f) => visitor.visit_function_declaration(f),
        }
    }

    pub fn visit_mut<T: Default, V: AstVisitorMut<T>>(&mut self, visitor: &mut V) -> T {
        match self {
            AstNode::Comment(c) => visitor.visit_comment(c),
            AstNode::Function(f) => visitor.visit_function_implementation(f),
            AstNode::Procedure(p) => visitor.visit_procedure_implementation(p),
            AstNode::Statement(s) => s.visit_mut(visitor),
            AstNode::ProcedureDeclaration(p) => visitor.visit_procedure_declaration(p),
            AstNode::FunctionDeclaration(f) => visitor.visit_function_declaration(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionImplementation {
    pub id: i32,
    function_token: SpannedToken,
    identifier_token: SpannedToken,
    leftpar_token: SpannedToken,
    parameters: Vec<ParameterSpecifier>,
    rightpar_token: SpannedToken,

    return_type_token: SpannedToken,
    return_type: VariableType,

    statements: Vec<Statement>,
    endfunc_token: SpannedToken,
}

impl FunctionImplementation {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        id: i32,
        function_token: SpannedToken,
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        parameters: Vec<ParameterSpecifier>,
        rightpar_token: SpannedToken,
        return_type_token: SpannedToken,
        return_type: VariableType,
        statements: Vec<Statement>,
        endfunc_token: SpannedToken,
    ) -> Self {
        Self {
            id,
            function_token,
            identifier_token,
            leftpar_token,
            parameters,
            rightpar_token,
            return_type_token,
            return_type,
            statements,
            endfunc_token,
        }
    }

    pub fn empty(
        id: i32,
        identifier: impl Into<String>,
        parameters: Vec<ParameterSpecifier>,
        return_type: VariableType,
        statements: Vec<Statement>,
    ) -> Self {
        Self {
            id,
            function_token: SpannedToken::create_empty(Token::Function),
            identifier_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                identifier.into(),
            ))),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            parameters,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            return_type_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                return_type.to_string(),
            ))),
            return_type,
            statements,
            endfunc_token: SpannedToken::create_empty(Token::EndFunc),
        }
    }

    pub fn get_function_token(&self) -> &SpannedToken {
        &self.function_token
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &unicase::Ascii<String> {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn set_identifier(&mut self, new_id: unicase::Ascii<String>) {
        if let Token::Identifier(id) = &mut self.identifier_token.token {
            *id = new_id;
        }
    }

    pub fn get_leftpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_parameters(&self) -> &Vec<ParameterSpecifier> {
        &self.parameters
    }

    pub fn get_parameters_mut(&mut self) -> &mut Vec<ParameterSpecifier> {
        &mut self.parameters
    }

    pub fn get_rightpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_return_type_token(&self) -> &SpannedToken {
        &self.return_type_token
    }

    pub fn get_return_type(&self) -> &VariableType {
        &self.return_type
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_endfunc_token(&self) -> &SpannedToken {
        &self.endfunc_token
    }
    /*
    pub fn print_content(&self) -> String {
        let mut res = self.declaration.print_header();
        res.push('\n');
        let mut indent = 1;

        if !self.variable_declarations.is_empty() {
            for v in &self.variable_declarations {
                res.push_str("    ");
                res.push_str(&v.to_string());
                res.push('\n');
            }
            res.push('\n');
        }

        for stmt in &self.block.statements {
            let out = stmt.to_string(self, indent);
            if indent > out.1 {
                indent = out.1;
            }
            for _ in 0..(indent + out.2) {
                res.push_str("    ");
            }
            res.push_str(out.0.as_str());
            indent = out.1;
            res.push('\n');
        }
        res.push_str(format!("ENDFUNC ;--{}", self.get_identifier()).as_str());
        //res.push_str(format!("ENDPROC ;--{name}").as_str());

        res.push('\n');

        res
    }*/
}

#[derive(Debug, Clone, PartialEq)]
pub struct ProcedureImplementation {
    pub id: i32,
    procedure_token: SpannedToken,
    identifier_token: SpannedToken,
    leftpar_token: SpannedToken,
    parameters: Vec<ParameterSpecifier>,
    rightpar_token: SpannedToken,
    statements: Vec<Statement>,
    endproc_token: SpannedToken,
}

/*
impl fmt::Display for FunctionImplementation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.declaration)
    }
}
*/
impl ProcedureImplementation {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        id: i32,
        procedure_token: SpannedToken,
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        parameters: Vec<ParameterSpecifier>,
        rightpar_token: SpannedToken,
        statements: Vec<Statement>,
        endproc_token: SpannedToken,
    ) -> Self {
        Self {
            id,
            procedure_token,
            identifier_token,
            leftpar_token,
            parameters,
            rightpar_token,
            statements,
            endproc_token,
        }
    }

    pub fn empty(
        id: i32,
        identifier: impl Into<String>,
        parameters: Vec<ParameterSpecifier>,
        statements: Vec<Statement>,
    ) -> Self {
        Self {
            id,
            procedure_token: SpannedToken::create_empty(Token::Procedure),
            identifier_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                identifier.into(),
            ))),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            parameters,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            statements,
            endproc_token: SpannedToken::create_empty(Token::EndProc),
        }
    }

    pub fn get_procedure_token(&self) -> &SpannedToken {
        &self.procedure_token
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`IdentifierExpression`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_identifier(&self) -> &unicase::Ascii<String> {
        if let Token::Identifier(id) = &self.identifier_token.token {
            return id;
        }
        panic!("Expected identifier token")
    }

    pub fn set_identifier(&mut self, new_id: unicase::Ascii<String>) {
        if let Token::Identifier(id) = &mut self.identifier_token.token {
            *id = new_id;
        }
    }

    pub fn get_leftpar_token(&self) -> &SpannedToken {
        &self.leftpar_token
    }

    pub fn get_parameters(&self) -> &Vec<ParameterSpecifier> {
        &self.parameters
    }

    pub fn get_parameters_mut(&mut self) -> &mut Vec<ParameterSpecifier> {
        &mut self.parameters
    }

    pub fn get_rightpar_token(&self) -> &SpannedToken {
        &self.rightpar_token
    }

    pub fn get_statements(&self) -> &Vec<Statement> {
        &self.statements
    }

    pub fn get_statements_mut(&mut self) -> &mut Vec<Statement> {
        &mut self.statements
    }

    pub fn get_endproc_token(&self) -> &SpannedToken {
        &self.endproc_token
    }
    /*
    pub fn print_content(&self) -> String {
        let mut res = self.declaration.print_header();
        res.push('\n');
        let mut indent = 1;

        if !self.variable_declarations.is_empty() {
            for v in &self.variable_declarations {
                res.push_str("    ");
                res.push_str(&v.to_string());
                res.push('\n');
            }
            res.push('\n');
        }

        for stmt in &self.block.statements {
            let out = stmt.to_string(self, indent);
            if indent > out.1 {
                indent = out.1;
            }
            for _ in 0..(indent + out.2) {
                res.push_str("    ");
            }
            res.push_str(out.0.as_str());
            indent = out.1;
            res.push('\n');
        }
        res.push_str(format!("ENDPROC ;--{}", self.get_identifier()).as_str());
        res.push('\n');
        res
    }*/
}
