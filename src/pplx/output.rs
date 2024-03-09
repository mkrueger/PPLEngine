use icy_ppe::{interpreter::ExecutionContext, Res};

#[derive(Default)]
pub struct Output {
}

impl ExecutionContext for Output {
    fn gotoxy(&mut self, x: i32, y: i32) -> Res<()> {
        Ok(())
    }
    
    fn print(&mut self, str: &str) -> Res<()> {
        Ok(())
    }

    fn write_raw(&mut self, data: &[u8]) -> Res<()> {
        Ok(())
    }

    fn read(&mut self) -> Res<String> {
        Ok("".to_string())
    }

    fn get_char(&mut self) -> Res<Option<char>> {
        Ok(None)
    }

    fn inbytes(&mut self) -> i32 {
        0
    }

    fn set_color(&mut self, color: u8) {

    }

    fn get_caret_position(&mut self) -> (i32, i32) {
        (0, 0)
    }

    /// simulate user input for later processing
    fn send_to_com(&mut self, _data: &str) -> Res<()> {
        Ok(())
    }
}