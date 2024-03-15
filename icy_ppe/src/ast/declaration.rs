use std::fmt;

use crate::parser::lexer::{SpannedToken, Token};

use super::{Constant, Statement, Variable, VariableType};
#[derive(Debug, PartialEq, Clone)]
pub struct DimensionSpecifier {
    dimension_token: SpannedToken,
}

impl DimensionSpecifier {
    /// Creates a new [`DimensionSpecifier`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn new(dimension_token: SpannedToken) -> Self {
        #[allow(clippy::manual_assert)]
        if !matches!(dimension_token.token, Token::Const(Constant::Integer(_))) {
            panic!("DimensionSpecifier::new: invalid token {dimension_token:?}");
        }
        Self { dimension_token }
    }

    pub fn empty(dimension: i32) -> Self {
        Self {
            dimension_token: SpannedToken::create_empty(Token::Const(Constant::Integer(dimension))),
        }
    }

    pub fn get_dimension_token(&self) -> &SpannedToken {
        &self.dimension_token
    }

    /// Returns the get dimension of this [`DimensionSpecifier`].
    ///
    /// # Panics
    ///
    /// Panics if .
    pub fn get_dimension(&self) -> usize {
        if let Token::Const(Constant::Integer(i)) = self.dimension_token.token {
            i as usize
        } else {
            panic!("DimensionSpecifier::new: invalid token")
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableSpecifier {
    identifier_token: SpannedToken,
    leftpar_token: Option<SpannedToken>,
    dimensions: Vec<DimensionSpecifier>,
    rightpar_token: Option<SpannedToken>,
}

impl VariableSpecifier {
    pub fn new(
        identifier_token: SpannedToken,
        leftpar_token: Option<SpannedToken>,
        dimensions: Vec<DimensionSpecifier>,
        rightpar_token: Option<SpannedToken>,
    ) -> Self {
        Self {
            identifier_token,
            leftpar_token,
            dimensions,
            rightpar_token,
        }
    }

    pub fn empty(identifier: unicase::Ascii<String>, dimensions: Vec<i32>) -> Self {
        Self {
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier)),
            leftpar_token: None,
            dimensions: dimensions
                .into_iter()
                .map(DimensionSpecifier::empty)
                .collect(),
            rightpar_token: None,
        }
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`ForStatement`].
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

    pub fn get_leftpar_token(&self) -> &Option<SpannedToken> {
        &self.leftpar_token
    }

    pub fn get_dimensions(&self) -> &Vec<DimensionSpecifier> {
        &self.dimensions
    }

    pub fn get_dimensions_mut(&mut self) -> &mut Vec<DimensionSpecifier> {
        &mut self.dimensions
    }

    pub fn get_rightpar_token(&self) -> &Option<SpannedToken> {
        &self.rightpar_token
    }

    pub fn create_empty_value(&self, variable_type: VariableType) -> Variable {
        let var_value = variable_type.create_empty_value();
        match self.dimensions.len() {
            0 => var_value,
            1 => Variable::new_vector(
                variable_type,
                vec![var_value; self.dimensions[0].get_dimension()],
            ),
            2 => Variable::new_matrix(
                variable_type,
                vec![
                    vec![var_value; self.dimensions[0].get_dimension()];
                    self.dimensions[1].get_dimension()
                ],
            ),
            _ => Variable::new_cube(
                variable_type,
                vec![
                    vec![
                        vec![var_value; self.dimensions[0].get_dimension()];
                        self.dimensions[1].get_dimension()
                    ];
                    self.dimensions[2].get_dimension()
                ],
            ),
        }
    }

    pub fn get_vector_size(&self) -> usize {
        if self.dimensions.is_empty() {
            return 0;
        }
        self.dimensions[0].get_dimension()
    }

    pub fn get_matrix_size(&self) -> usize {
        if self.dimensions.len() < 2 {
            return 0;
        }
        self.dimensions[1].get_dimension()
    }

    pub fn get_cube_size(&self) -> usize {
        if self.dimensions.len() < 3 {
            return 0;
        }
        self.dimensions[2].get_dimension()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct VariableDeclarationStatement {
    type_token: SpannedToken,
    variable_type: VariableType,
    variables: Vec<VariableSpecifier>,
}

impl VariableDeclarationStatement {
    pub fn new(
        type_token: SpannedToken,
        variable_type: VariableType,
        variables: Vec<VariableSpecifier>,
    ) -> Self {
        Self {
            type_token,
            variable_type,
            variables,
        }
    }

    pub fn empty(variable_type: VariableType, variables: Vec<VariableSpecifier>) -> Self {
        Self {
            type_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                variable_type.to_string(),
            ))),
            variable_type,
            variables,
        }
    }

    pub fn get_type_token(&self) -> &SpannedToken {
        &self.type_token
    }

    pub fn get_variable_type(&self) -> VariableType {
        self.variable_type
    }

    pub fn get_variables(&self) -> &Vec<VariableSpecifier> {
        &self.variables
    }

    pub fn get_variables_mut(&mut self) -> &mut Vec<VariableSpecifier> {
        &mut self.variables
    }

    pub fn create_empty_statement(
        variable_type: VariableType,
        variables: Vec<VariableSpecifier>,
    ) -> Statement {
        Statement::VariableDeclaration(VariableDeclarationStatement::empty(
            variable_type,
            variables,
        ))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ParameterSpecifier {
    var_token: Option<SpannedToken>,
    type_token: SpannedToken,
    variable_type: VariableType,
    variable: VariableSpecifier,
}

impl ParameterSpecifier {
    pub fn new(
        var_token: Option<SpannedToken>,
        type_token: SpannedToken,
        variable_type: VariableType,
        variable: VariableSpecifier,
    ) -> Self {
        Self {
            var_token,
            type_token,
            variable_type,
            variable,
        }
    }

    pub fn empty(is_var: bool, variable_type: VariableType, variable: VariableSpecifier) -> Self {
        Self {
            var_token: if is_var {
                Some(SpannedToken::create_empty(Token::Identifier(
                    unicase::Ascii::new("VAR".to_string()),
                )))
            } else {
                None
            },
            type_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                variable_type.to_string(),
            ))),
            variable_type,
            variable,
        }
    }

    pub fn get_var_token(&self) -> &Option<SpannedToken> {
        &self.var_token
    }

    pub fn is_var(&self) -> bool {
        self.var_token.is_some()
    }

    pub fn get_type_token(&self) -> &SpannedToken {
        &self.type_token
    }

    pub fn get_variable_type(&self) -> VariableType {
        self.variable_type
    }

    pub fn get_variable(&self) -> &VariableSpecifier {
        &self.variable
    }

    pub fn get_variable_mut(&mut self) -> &mut VariableSpecifier {
        &mut self.variable
    }

    pub fn create_empty_statement(
        variable_type: VariableType,
        variables: Vec<VariableSpecifier>,
    ) -> Statement {
        Statement::VariableDeclaration(VariableDeclarationStatement::empty(
            variable_type,
            variables,
        ))
    }
}

impl fmt::Display for VariableDeclarationStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}",
            self.get_variable_type(),
            Statement::variable_list_to_string(self.get_variables())
        )
    }
}
#[derive(Debug, PartialEq, Clone)]
pub struct ProcedureDeclarationAstNode {
    declare_token: SpannedToken,
    procedure_token: SpannedToken,
    identifier_token: SpannedToken,
    leftpar_token: SpannedToken,
    parameters: Vec<ParameterSpecifier>,
    rightpar_token: SpannedToken,
}

impl ProcedureDeclarationAstNode {
    pub fn new(
        declare_token: SpannedToken,
        procedure_token: SpannedToken,
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        parameters: Vec<ParameterSpecifier>,
        rightpar_token: SpannedToken,
    ) -> Self {
        Self {
            declare_token,
            procedure_token,
            identifier_token,
            leftpar_token,
            parameters,
            rightpar_token,
        }
    }

    pub fn empty(identifier: unicase::Ascii<String>, parameters: Vec<ParameterSpecifier>) -> Self {
        Self {
            declare_token: SpannedToken::create_empty(Token::Declare),
            procedure_token: SpannedToken::create_empty(Token::Procedure),
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier)),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            parameters,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
        }
    }
    pub fn get_declare_token(&self) -> &SpannedToken {
        &self.declare_token
    }

    pub fn get_procedure_token(&self) -> &SpannedToken {
        &self.procedure_token
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`ForStatement`].
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
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionDeclarationAstNode {
    declare_token: SpannedToken,
    function_token: SpannedToken,
    identifier_token: SpannedToken,
    leftpar_token: SpannedToken,
    parameters: Vec<ParameterSpecifier>,
    rightpar_token: SpannedToken,
    return_type_token: SpannedToken,
    return_type: VariableType,
}

impl FunctionDeclarationAstNode {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        declare_token: SpannedToken,
        function_token: SpannedToken,
        identifier_token: SpannedToken,
        leftpar_token: SpannedToken,
        parameters: Vec<ParameterSpecifier>,
        rightpar_token: SpannedToken,
        return_type_token: SpannedToken,
        return_type: VariableType,
    ) -> Self {
        Self {
            declare_token,
            function_token,
            identifier_token,
            leftpar_token,
            parameters,
            rightpar_token,
            return_type_token,
            return_type,
        }
    }

    pub fn empty(
        identifier: unicase::Ascii<String>,
        parameters: Vec<ParameterSpecifier>,
        return_type: VariableType,
    ) -> Self {
        Self {
            declare_token: SpannedToken::create_empty(Token::Declare),
            function_token: SpannedToken::create_empty(Token::Function),
            identifier_token: SpannedToken::create_empty(Token::Identifier(identifier)),
            leftpar_token: SpannedToken::create_empty(Token::LPar),
            parameters,
            rightpar_token: SpannedToken::create_empty(Token::RPar),
            return_type_token: SpannedToken::create_empty(Token::Identifier(unicase::Ascii::new(
                return_type.to_string(),
            ))),
            return_type,
        }
    }
    pub fn get_declare_token(&self) -> &SpannedToken {
        &self.declare_token
    }

    pub fn get_function_token(&self) -> &SpannedToken {
        &self.function_token
    }

    pub fn get_identifier_token(&self) -> &SpannedToken {
        &self.identifier_token
    }

    /// Returns a reference to the get identifier of this [`ForStatement`].
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

    pub fn get_return_type(&self) -> VariableType {
        self.return_type
    }
}
