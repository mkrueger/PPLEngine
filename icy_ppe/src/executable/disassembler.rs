use std::{io::stdout, ops::Range};

use crossterm::{
    execute,
    style::{Attribute, Color, Print, SetAttribute, SetForegroundColor},
};

use crate::ast::VariableType;

use super::{OpCode, PPECommand, PPEExpr, PPEScript, PPEVisitor, StatementDefinition};

#[derive(Default)]
pub struct DisassembleVisitor {}

impl DisassembleVisitor {
    fn output_op_code(end: OpCode) {
        execute!(
            stdout(),
            SetForegroundColor(Color::White),
            Print(format!("{:02X} ", end as i16)),
            SetAttribute(Attribute::Reset),
            SetForegroundColor(Color::Yellow),
            Print(format!("{:<10} ", end.to_string())),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
    }

    fn print_arguments(&mut self, args: &[PPEExpr]) {
        for (i, d) in args.iter().enumerate() {
            if i > 0 {
                print!(", ");
            }
            d.visit(self);
        }
    }

    fn dump_script_data(ppe_file: &super::Executable, range: Range<usize>) {
        let offset = range.start;
        execute!(
            stdout(),
            SetForegroundColor(Color::Cyan),
            Print(format!("{:05X}: ", offset * 2)),
            SetForegroundColor(Color::White),
        )
        .unwrap();

        for (i, x) in ppe_file.script_buffer[range].iter().enumerate() {
            if i > 0 && (i % 16) == 0 {
                println!();
                execute!(
                    stdout(),
                    SetForegroundColor(Color::Cyan),
                    Print(format!("{:05X}: ", (offset + i) * 2)),
                    SetForegroundColor(Color::White),
                )
                .unwrap();
            }
            print!("{:04X} ", *x);
        }
        execute!(stdout(), SetForegroundColor(Color::Reset),).unwrap();
    }

    pub fn print_disassembler(&mut self, ppe_file: &super::Executable) {
        println!();
        println!("Offset  # OpCode      Parameters");
        match PPEScript::from_ppe_file(ppe_file) {
            Ok(script) => {
                script.visit(self);
                println!();
            }
            Err((script, e)) => {
                script.visit(self);
                println!();
                execute!(
                    stdout(),
                    SetAttribute(Attribute::Bold),
                    SetForegroundColor(Color::Red),
                    Print("ERROR: ".to_string()),
                    SetAttribute(Attribute::Reset),
                    SetAttribute(Attribute::Bold),
                    Print(format!("{}", e.error_type)),
                    SetAttribute(Attribute::Reset),
                )
                .unwrap();
                println!();
                Self::dump_script_data(ppe_file, e.span);
                println!();
            }
        }
    }

    pub(crate) fn print_variable_table(ppe_file: &super::Executable) {
        println!();
        println!(
            "--- Variable Table ({}/{0:02X}h variables) ---",
            ppe_file.variable_table.len()
        );
        println!("   # Type         Flags Role           Name        Value");
        for var in ppe_file.variable_table.iter().rev() {
            let ts = if var.header.dim > 0 {
                format!("{}({})", var.header.variable_type, var.header.dim)
            } else {
                var.header.variable_type.to_string()
            };

            print!("{:04X} {:<13}", var.header.id, ts);

            let ts = format!("{:?}", var.get_type());
            print!("{}     {:<15}", var.header.flags, ts);
            print!("{:<12}", var.get_name());

            if var.header.variable_type == VariableType::Function {
                unsafe {
                    print!("{:?}", var.value.data.function_value);
                }
            } else if var.header.variable_type == VariableType::Procedure {
                unsafe {
                    print!("{:?}", var.value.data.procedure_value);
                }
            } else {
                print!("{}", var.value);
                if var.header.dim > 0
                    || var.header.vector_size > 0
                    || var.header.matrix_size > 0
                    || var.header.cube_size > 0
                {
                    print!(
                        " ({}, {}, {})",
                        var.header.vector_size, var.header.matrix_size, var.header.cube_size
                    );
                }
            }
            println!();
        }
    }

    pub fn print_script_buffer_dump(ppe_file: &super::Executable) {
        println!(
            "Script buffer size: {} ({} bytes)",
            ppe_file.script_buffer.len(),
            ppe_file.script_buffer.len() * 2
        );
        Self::dump_script_data(ppe_file, 0..ppe_file.script_buffer.len());
    }
}

impl PPEVisitor<()> for DisassembleVisitor {
    fn visit_value(&mut self, id: usize) {
        execute!(
            stdout(),
            Print("["),
            SetForegroundColor(Color::White),
            Print(format!("{id:04X}")),
            SetAttribute(Attribute::Reset),
            Print("]"),
        )
        .unwrap();
    }

    fn visit_unary_expression(&mut self, op: crate::ast::UnaryOp, expr: &PPEExpr) {
        execute!(
            stdout(),
            SetForegroundColor(Color::Yellow),
            Print(format!("{op}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
        print!("(");
        expr.visit(self);
        print!(")");
    }

    fn visit_binary_expression(&mut self, op: crate::ast::BinOp, left: &PPEExpr, right: &PPEExpr) {
        left.visit(self);
        execute!(
            stdout(),
            SetForegroundColor(Color::Yellow),
            Print(format!(" {op} ")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
        right.visit(self);
    }

    fn visit_dim_expression(&mut self, id: usize, dim: &[PPEExpr]) {
        execute!(
            stdout(),
            Print("["),
            SetForegroundColor(Color::Yellow),
            Print("#"),
            SetForegroundColor(Color::White),
            Print(format!("{id:04X}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
        print!(", ");
        self.print_arguments(dim);
        print!("]");
    }

    fn visit_predefined_function_call(
        &mut self,
        def: &crate::tables::FunctionDefinition,
        arguments: &[PPEExpr],
    ) {
        print!("(");
        execute!(
            stdout(),
            SetForegroundColor(Color::White),
            Print(format!("{:02X}", def.opcode as i16)),
            SetAttribute(Attribute::Reset),
            SetForegroundColor(Color::DarkYellow),
            Print(format!("'{}'", def.name)),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
        if !arguments.is_empty() {
            print!(" ");
            self.print_arguments(arguments);
        }
        print!(")");
    }

    fn visit_function_call(&mut self, id: usize, arguments: &[PPEExpr]) {
        print!("FUNC[{id}](");
        self.print_arguments(arguments);
        print!(")");
    }

    fn visit_end(&mut self) {
        Self::output_op_code(OpCode::END);
    }

    fn visit_return(&mut self) {
        Self::output_op_code(OpCode::RETURN);
    }

    fn visit_if(&mut self, cond: &PPEExpr, label: &usize) {
        Self::output_op_code(OpCode::IF);
        print!(" (");
        cond.visit(self);
        print!(")");

        execute!(
            stdout(),
            SetForegroundColor(Color::Yellow),
            Print(" GOTO "),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();

        execute!(
            stdout(),
            SetForegroundColor(Color::Cyan),
            Print(format!("{{{label:04X}}}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
    }

    fn visit_while(&mut self, cond: &PPEExpr, stmt: &PPECommand, label: &usize) {
        Self::output_op_code(OpCode::WHILE);
        print!("(");
        cond.visit(self);
        print!(")");
        print!(" {{{label:04X}}}");
        println!("\t\t");
        stmt.visit(self);
    }

    fn visit_proc_call(&mut self, id: &usize, args: &[PPEExpr]) {
        Self::output_op_code(OpCode::PCALL);
        print!(" PROC[{id:04X}]: ");
        self.print_arguments(args);
    }

    fn visit_predefined_call(&mut self, def: &StatementDefinition, args: &[PPEExpr]) {
        let name = format!("'{}'", def.name);
        execute!(
            stdout(),
            SetForegroundColor(Color::White),
            Print(format!("{:02X} ", def.opcode as i16)),
            SetAttribute(Attribute::Reset),
            SetForegroundColor(Color::DarkYellow),
            Print(format!("{name:<12}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();

        self.print_arguments(args);
    }

    fn visit_goto(&mut self, label: &usize) {
        Self::output_op_code(OpCode::GOTO);
        execute!(
            stdout(),
            SetForegroundColor(Color::Cyan),
            Print(format!(" {{{label:04X}}}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
    }

    fn visit_gosub(&mut self, label: &usize) {
        Self::output_op_code(OpCode::GOSUB);
        execute!(
            stdout(),
            SetForegroundColor(Color::Cyan),
            Print(format!(" {{{label:04X}}}")),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
    }

    fn visit_end_func(&mut self) {
        Self::output_op_code(OpCode::FEND);
    }

    fn visit_end_proc(&mut self) {
        Self::output_op_code(OpCode::FPCLR);
    }

    fn visit_stop(&mut self) {
        Self::output_op_code(OpCode::STOP);
    }

    fn visit_let(&mut self, target: &PPEExpr, value: &PPEExpr) {
        Self::output_op_code(OpCode::LET);
        print!(" ");
        target.visit(self);
        execute!(
            stdout(),
            SetForegroundColor(Color::Yellow),
            Print(" <- "),
            SetAttribute(Attribute::Reset),
        )
        .unwrap();
        value.visit(self);
    }

    fn visit_script(&mut self, script: &PPEScript) {
        for stmt in &script.statements {
            execute!(
                stdout(),
                SetForegroundColor(Color::Cyan),
                Print(format!("{:05X}: ", stmt.span.start * 2)),
                SetForegroundColor(Color::Reset),
            )
            .unwrap();
            stmt.command.visit(self);
            println!();
        }
    }
}
