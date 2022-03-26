use std::io;

use ppl_engine::interpreter::ExecutionContext;

pub struct ConsoleContext
{
}

impl ExecutionContext for ConsoleContext
{
    fn print(&mut self, str: &str)
    {
        print!("{}", str);
    }
    fn read(&mut self) -> String
    {
        let mut buf = String::new();
        io::stdin().read_line(&mut buf).expect("error reading from stdin");
        buf
    }
}
