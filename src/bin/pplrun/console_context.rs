use ppl_engine::interpreter::ExecutionContext;

pub struct ConsoleContext
{
}

impl ExecutionContext for ConsoleContext
{
    fn print(&mut self, str: String)
    {
        print!("{}", str);
    }
}
