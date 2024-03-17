use crate::{
    ast::AstVisitor,
    executable::PPEExpr,
    tables::{get_function_definition, FUNCTION_DEFINITIONS},
};

use super::{CompilationError, CompilationErrorType, PPECompiler};

pub struct ExpressionCompiler<'a> {
    pub compiler: &'a mut PPECompiler,
}

impl<'a> AstVisitor<PPEExpr> for ExpressionCompiler<'a> {
    fn visit_identifier_expression(
        &mut self,
        identifier: &crate::ast::IdentifierExpression,
    ) -> PPEExpr {
        if let Some(decl) = self.compiler.lookup_variable(identifier.get_identifier())
        {
            return PPEExpr::Value(decl.header.id);
        }
        self.compiler.errors.push(CompilationError {
            error: CompilationErrorType::VariableNotFound(identifier.get_identifier().to_string()),
            range: identifier.get_identifier_token().span.clone(),
        });
        PPEExpr::Value(0)
    }

    fn visit_constant_expression(&mut self, constant: &crate::ast::ConstantExpression) -> PPEExpr {
        let table_id = self.compiler.lookup_constant(constant.get_constant_value());
        PPEExpr::Value(table_id)
    }

    fn visit_binary_expression(&mut self, bin_expr: &crate::ast::BinaryExpression) -> PPEExpr {
        let left = bin_expr.get_left_expression().visit(self);
        let right = bin_expr.get_right_expression().visit(self);
        PPEExpr::BinaryExpression(bin_expr.get_op(), Box::new(left), Box::new(right))
    }

    fn visit_unary_expression(&mut self, unary: &crate::ast::UnaryExpression) -> PPEExpr {
        let expression = unary.get_expression().visit(self);
        PPEExpr::UnaryExpression(unary.get_op(), Box::new(expression))
    }

    fn visit_predefined_function_call_expression(
        &mut self,
        call: &crate::ast::PredefinedFunctionCallExpression,
    ) -> PPEExpr {
        let predef = get_function_definition(call.get_identifier());
        if predef < 0 {
            self.compiler.errors.push(CompilationError {
                error: CompilationErrorType::FunctionNotFound(call.get_identifier().to_string()),
                range: call.get_identifier_token().span.clone(),
            });
            return PPEExpr::Value(0);
        }
        // TODO: Check parameter signature
        PPEExpr::PredefinedFunctionCall(
            &FUNCTION_DEFINITIONS[predef as usize],
            call.get_arguments().iter().map(|e| e.visit(self)).collect(),
        )
    }

    fn visit_function_call_expression(
        &mut self,
        func_call: &crate::ast::FunctionCallExpression,
    ) -> PPEExpr {
        let arguments = func_call
            .get_arguments()
            .iter()
            .map(|e| e.visit(self))
            .collect();

        if self.compiler.has_variable(func_call.get_identifier()) {
            let Some(table_idx) = self
                .compiler
                .lookup_variable_index(func_call.get_identifier())
            else {
                self.compiler.errors.push(CompilationError {
                    error: CompilationErrorType::FunctionNotFound(
                        func_call.get_identifier().to_string(),
                    ),
                    range: func_call.get_identifier_token().span.clone(),
                });
                return PPEExpr::Value(0);
            };
            return PPEExpr::FunctionCall(table_idx + 1, arguments);
        }

        let Some(table_idx) = self
            .compiler
            .lookup_variable_index(func_call.get_identifier())
        else {
            self.compiler.errors.push(CompilationError {
                error: CompilationErrorType::VariableNotFound(
                    func_call.get_identifier().to_string(),
                ),
                range: func_call.get_identifier_token().span.clone(),
            });
            return PPEExpr::Value(0);
        };
        let var = &self.compiler.get_variable(table_idx);
        if var.header.dim as usize != arguments.len() {
            self.compiler.errors.push(CompilationError {
                error: CompilationErrorType::InvalidDimensions(
                    func_call.get_identifier().to_string(),
                    var.header.dim,
                    arguments.len(),
                ),
                range: func_call.get_identifier_token().span.clone(),
            });
            return PPEExpr::Value(0);
        }
        PPEExpr::Dim(table_idx, arguments)
    }

    fn visit_parens_expression(&mut self, parens: &crate::ast::ParensExpression) -> PPEExpr {
        parens.get_expression().visit(self)
    }
}
