pub mod constant;
pub use self::constant::Constant;

pub mod expression;
pub use self::expression::*;

pub mod statement;
pub use self::statement::*;

pub mod declaration;
pub use self::declaration::*;

pub mod block;
pub use self::block::*;

pub mod program;
pub use self::program::*;

pub mod function_implementation;
pub use self::function_implementation::*;

pub mod variable_type;
pub use self::variable_type::*;

pub mod variable_value;
pub use self::variable_value::*;

pub trait ProgramContext {
    fn get_var_type(&self, var_name: &str) -> VariableType;
    fn get_var_info(&self, var_name: &str) -> Option<&VarInfo>;
}
