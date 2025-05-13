use crate::compiler_driver::compiler::{assembly_generator, visualize};

impl visualize::Visualizer for assembly_generator::AssemblyAbstractSyntaxTree {
    fn visualize(&self, _depth: u8) -> String {
        let assembly_generator::AssemblyAbstractSyntaxTree::Program(program) = self;
        program.visualize(0)
    }
}

impl visualize::Visualizer for assembly_generator::Program {
    fn visualize(&self, depth: u8) -> String {
        let assembly_generator::Program::Program(function_definition) = self;
        String::from(format!(
            "Program(\n\
		{}\n\
		)",
            function_definition.visualize(depth + 1)
        ))
    }
}

impl visualize::Visualizer for assembly_generator::FunctionDefinition {
    fn visualize(&self, depth: u8) -> String {
        let assembly_generator::FunctionDefinition::Function {
            identifier,
            instructions,
        } = self;
        let prefix = "    ".repeat(depth as usize);
        let instructions_str = instructions
            .iter()
            .map(|instruction| instruction.visualize(depth + 1))
            .collect::<Vec<String>>()
            .join(format!("\n{prefix}        ").as_str());

        format!(
            "{prefix}Function(\n\
		{prefix}    name={identifier}\n\
		{prefix}    instructions=\n\
		{prefix}        {}\n\
		{prefix})",
            instructions_str
        )
    }
}

impl visualize::Visualizer for assembly_generator::Operand {
    fn visualize(&self, depth: u8) -> String {
        match self {
            assembly_generator::Operand::Immediate(value) => format!("Imm({value})"),
            assembly_generator::Operand::Register(register) => register.visualize(depth + 1),
            assembly_generator::Operand::Pseudo { identifier } => format!("Pseudo({identifier})"),
            assembly_generator::Operand::Stack { offset } => format!("Stack({offset})"),
        }
    }
}

impl visualize::Visualizer for assembly_generator::Instruction {
    fn visualize(&self, depth: u8) -> String {
        match self {
            assembly_generator::Instruction::Mov(op1, op2) => format!(
                "Mov({}, {})",
                op1.visualize(depth + 1),
                op2.visualize(depth + 1),
            ),
            assembly_generator::Instruction::Unary(unary_operator, op) => format!(
                "Unary({}, {})",
                unary_operator.visualize(depth + 1),
                op.visualize(depth + 1),
            ),
            assembly_generator::Instruction::Binary(binary_operator, op1, op2) => format!(
                "Binary({}, {}, {})",
                binary_operator.visualize(depth + 1),
                op1.visualize(depth + 1),
                op2.visualize(depth + 1),
            ),
            assembly_generator::Instruction::Cmp(op1, op2) => format!(
                "Cmp({}, {})",
                op1.visualize(depth + 1),
                op2.visualize(depth + 1)
            ),
            assembly_generator::Instruction::Jmp(identifier) => format!("Jmp({identifier})"),
            assembly_generator::Instruction::JmpCC(condition_code, identifier) => format!(
                "JmpCC({}, {})",
                condition_code.visualize(depth + 1),
                identifier
            ),
            assembly_generator::Instruction::SetCC(condition_code, op) => format!(
                "SetCC({}, {})",
                condition_code.visualize(depth + 1),
                op.visualize(depth + 1)
            ),
            assembly_generator::Instruction::Label(identifier) => format!("{identifier}:"),
            assembly_generator::Instruction::Idiv(op) => {
                format!("Idiv({})", op.visualize(depth + 1))
            }
            assembly_generator::Instruction::Cdq => String::from("Cdq"),
            assembly_generator::Instruction::AllocateStack(int) => {
                format!("AllocateStack({})", int)
            }
            assembly_generator::Instruction::Ret => String::from("Ret"),
        }
    }
}

impl visualize::Visualizer for assembly_generator::Register {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            assembly_generator::Register::AX => String::from("Reg(AX)"),
            assembly_generator::Register::CX => String::from("Reg(CX)"),
            assembly_generator::Register::DX => String::from("Reg(DX)"),
            assembly_generator::Register::R10 => String::from("Reg(R10)"),
            assembly_generator::Register::R11 => String::from("Reg(R11)"),
        }
    }
}

impl visualize::Visualizer for assembly_generator::ConditionCode {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            assembly_generator::ConditionCode::E => String::from("E"),
            assembly_generator::ConditionCode::NE => String::from("NE"),
            assembly_generator::ConditionCode::L => String::from("L"),
            assembly_generator::ConditionCode::LE => String::from("LE"),
            assembly_generator::ConditionCode::GE => String::from("GE"),
            assembly_generator::ConditionCode::G => String::from("G"),
        }
    }
}

impl visualize::Visualizer for assembly_generator::UnaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            assembly_generator::UnaryOperator::Neg => String::from("Neg"),
            assembly_generator::UnaryOperator::Not => String::from("Not"),
        }
    }
}

impl visualize::Visualizer for assembly_generator::BinaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            assembly_generator::BinaryOperator::Add => String::from("Add"),
            assembly_generator::BinaryOperator::Sub => String::from("Sub"),
            assembly_generator::BinaryOperator::Mult => String::from("Mult"),
            assembly_generator::BinaryOperator::LShift => String::from("LShift"),
            assembly_generator::BinaryOperator::RShift => String::from("RShift"),
            assembly_generator::BinaryOperator::BitAnd => String::from("BitAnd"),
            assembly_generator::BinaryOperator::BitXOr => String::from("BitXOr"),
            assembly_generator::BinaryOperator::BitOr => String::from("BitOr"),
        }
    }
}
