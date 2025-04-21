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
            assembly_generator::Operand::Pseudo { identifier } => format!("Pseudo({identifier}"),
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
                op2.visualize(depth + 1)
            ),
            assembly_generator::Instruction::Unary(unary_operator, dst) => format!(
                "Unary({}, {})",
                unary_operator.visualize(depth + 1),
                dst.visualize(depth + 1)
            ),
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
            assembly_generator::Register::R10 => String::from("Reg(R10)"),
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
