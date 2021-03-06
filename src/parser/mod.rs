use crate::{ast::{Block, Declaration, FunctionImplementation, Program, VariableType, VarInfo}};

use self::tokens::{Tokenizer, Token};

mod tokens;
mod expression;
mod statements;

impl Tokenizer {
    pub fn get_variable_type(&self) -> Option<VariableType>
    {
        if let Some(Token::Identifier(id)) = &self.cur_token {
            match id.as_str() {
                "INTEGER"  => Some(VariableType::Integer),
                "STRING" | "BIGSTR" => Some(VariableType::String),
                "BOOLEAN" => Some(VariableType::Boolean),
                "DATE" => Some(VariableType::Date),
                "TIME" => Some(VariableType::Time),
                "MONEY" => Some(VariableType::Money),
                "WORD" => Some(VariableType::Word),
                "SWORD" => Some(VariableType::SWord),
                "BYTE" => Some(VariableType::Byte),
                "UNSIGNED" => Some(VariableType::Unsigned),
                "SBYTE" => Some(VariableType::SByte),
                "REAL" => Some(VariableType::Real),
                "DREAL" => Some(VariableType::Real),
                _ => None
            }
        } else {
            None
        }
    }

    pub fn parse_var_info(&mut self) -> VarInfo
    {
        let var_name;
        if let Some(Token::Identifier(id)) = self.cur_token.clone() {
            self.next_token();
            var_name = id;
        } else {
            panic!("expected identifier, got: {:?}", self.cur_token);
        }

        VarInfo::Var0(var_name)
    }

    pub fn parse_function_declaration(&mut self) -> Option<Declaration>
    {
        if Some(Token::Identifier("DECLARE".to_string())) == self.cur_token {
            self.next_token();

            let is_function = if Some(Token::Identifier("PROCEDURE".to_string())) == self.cur_token {
                false
            } else if Some(Token::Identifier("FUNCTION".to_string())) == self.cur_token {
                true
            } else {
                panic!("FUNCTION or PROCEDURE expected. got {:?}", self.cur_token);
            };
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            if self.cur_token != Some(Token::LPar) {
                panic!("'(' expected. got {:?}", self.cur_token);
            } 
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            if self.cur_token != Some(Token::RPar) {
                panic!("')' expected. got {:?}", self.cur_token);
            } 
            self.next_token();
            if !is_function {
                return Some(Declaration::Procedure(name, vars));
            }

            let func_t = if let Some(var_type) = self.get_variable_type() {
                self.next_token();
                var_type
            } else {
                panic!("variable type expeted got: {:?}", self.cur_token);
            };

            return Some(Declaration::Function(name, vars, func_t));
        }
        None
    }

    pub fn parse_procedure(&mut self) -> Option<FunctionImplementation>
    {
        if Some(Token::Identifier("PROCEDURE".to_string())) == self.cur_token {
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            if self.cur_token != Some(Token::LPar) {
                panic!("'(' expected. got {:?}", self.cur_token);
            } 
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            if self.cur_token != Some(Token::RPar) {
                panic!("')' expected. got {:?}", self.cur_token);
            } 
            self.next_token();
            self.skip_eol();

            let mut variable_declarations = Vec::new();
            let mut statements = Vec::new();

            while self.cur_token != Some(Token::Identifier("ENDPROC".to_string())) {
        
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();
                    
                    let mut vars = Vec::new();
                    
                    vars.push(self.parse_var_info());
                    while self.cur_token == Some(Token::Comma) {
                        self.next_token();
                        vars.push(self.parse_var_info());
                    }
                    variable_declarations.push(Declaration::Variable(var_type, vars));
                } else {
                    statements.push(self.parse_statement());
                }
                self.skip_eol();
            }
            self.next_token();

            return Some(FunctionImplementation {
                id: -1,
                declaration: Declaration::Procedure(name, vars),
                variable_declarations,
                block: Block {
                    statements
                }
            });
        }
        None
    }

    pub fn parse_function(&mut self) -> Option<FunctionImplementation>
    {
        if Some(Token::Identifier("FUNCTION".to_string())) == self.cur_token {
            self.next_token();

            let name = if let Some(Token::Identifier(id)) = self.cur_token.clone() {
                self.next_token();
                id
            } else {
                panic!("IDENTIFIER expected. got {:?}", self.cur_token);
            };

            if self.cur_token != Some(Token::LPar) {
                panic!("'(' expected. got {:?}", self.cur_token);
            } 
            self.next_token();

            let mut vars = Vec::new();

            while self.cur_token != Some(Token::RPar) {
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();

                    let info = self.parse_var_info();
                    vars.push(Declaration::Variable(var_type, vec![info]));
                } else {
                    panic!("variable type expeted got: {:?}", self.cur_token);
                }

                if self.cur_token == Some(Token::Comma) {
                    self.next_token();
                }
            }

            if self.cur_token != Some(Token::RPar) {
                panic!("')' expected. got {:?}", self.cur_token);
            } 
            self.next_token();

            let func_t = if let Some(var_type) = self.get_variable_type() {
                self.next_token();
                var_type
            } else {
                panic!("variable type expeted got: {:?}", self.cur_token);
            };
            self.skip_eol();

            let mut variable_declarations = Vec::new();
            let mut statements = Vec::new();

            while self.cur_token != Some(Token::Identifier("ENDFUNC".to_string())) {
        
                if let Some(var_type) = self.get_variable_type() {
                    self.next_token();
                    
                    let mut vars = Vec::new();
                    
                    vars.push(self.parse_var_info());
                    while self.cur_token == Some(Token::Comma) {
                        self.next_token();
                        vars.push(self.parse_var_info());
                    }
                    variable_declarations.push(Declaration::Variable(var_type, vars));
                } else {
                    statements.push(self.parse_statement());
                }
                self.skip_eol();
            }
            self.next_token();

            return Some(FunctionImplementation {
                id: -1,
                declaration: Declaration::Function(name, vars, func_t),
                variable_declarations,
                block: Block {
                    statements
                }
            });
        }
        None
    }
}

pub fn parse_program(input: &str) -> Program
{
    let mut declarations = Vec::new();
    let mut function_implementations = Vec::new();
    let mut procedure_implementations = Vec::new();
    let mut statements = Vec::new();

    let mut tokenizer = Tokenizer::new(input);
    tokenizer.next_token();
    tokenizer.skip_eol();

    while tokenizer.cur_token.is_some() {
        if let Some(var_type) = tokenizer.get_variable_type() {
            tokenizer.next_token();
            
            let mut vars = Vec::new();
            
            vars.push(tokenizer.parse_var_info());
            while tokenizer.cur_token == Some(Token::Comma) {
                tokenizer.next_token();
                vars.push(tokenizer.parse_var_info());
            }
            declarations.push(Declaration::Variable(var_type, vars));
        } else if let Some(decl) = tokenizer.parse_function_declaration() {
            declarations.push(decl);
        } else if let Some(func) = tokenizer.parse_function() {
            function_implementations.push(func);
        } else if let Some(func) = tokenizer.parse_procedure() {
            procedure_implementations.push(func);
        } else {
            statements.push(tokenizer.parse_statement());
        }

        tokenizer.skip_eol();
    }

    Program { 
        declarations,
        function_implementations, 
        procedure_implementations, 
        main_block: Block {
            statements
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_procedure() {
        let prg = parse_program("Procedure Proc() PRINT 5 EndProc");
        assert_eq!(1, prg.procedure_implementations.len());
    }

    #[test]
    fn test_function() {
        let prg = parse_program("Function Func() BOOLEAN PRINT 5 EndFunc");
        assert_eq!(1, prg.function_implementations.len());
    }
    
    #[test]
    fn test_var_declarations() {

        let prg = parse_program("BOOLEAN VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Boolean, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("INTEGER VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Integer, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("DATE VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Date, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("STRING VAR001");
        assert_eq!(Declaration::create_variable(VariableType::String, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("MONEY VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Money, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("BYTE VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Byte, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("SBYTE VAR001");
        assert_eq!(Declaration::create_variable(VariableType::SByte, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("WORD VAR001");
        assert_eq!(Declaration::create_variable(VariableType::Word, "VAR001".to_string()), prg.declarations[0]);
        let prg = parse_program("SWORD VAR001");
        assert_eq!(Declaration::create_variable(VariableType::SWord, "VAR001".to_string()), prg.declarations[0]);
    }

    #[test]
    fn test_func_declarations() {
        let prg = parse_program("DECLARE PROCEDURE PROC001()");
        assert_eq!(Declaration::Procedure("PROC001".to_string(), vec![]), prg.declarations[0]);

        let prg = parse_program("DECLARE FUNCTION FUNC001(INTEGER LOC001) INTEGER");
        assert_eq!(Declaration::Function("FUNC001".to_string(), vec![Declaration::create_variable(VariableType::Integer, "LOC001".to_string())], VariableType::Integer), prg.declarations[0]);
    }
}