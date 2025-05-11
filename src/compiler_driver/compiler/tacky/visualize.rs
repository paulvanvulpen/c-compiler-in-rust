use crate::compiler_driver::compiler::{tacky, visualize};

impl visualize::Visualizer for tacky::TackyAbstractSyntaxTree {
    fn visualize(&self, _depth: u8) -> String {
        let tacky::TackyAbstractSyntaxTree::Program(program) = self;
        program.visualize(0)
    }
}

impl visualize::Visualizer for tacky::Program {
    fn visualize(&self, depth: u8) -> String {
        let tacky::Program::Program(function_definition) = self;
        format!(
            "Program(\n\
		{}\n\
		)",
            function_definition.visualize(depth + 1)
        )
    }
}

impl visualize::Visualizer for tacky::FunctionDefinition {
    fn visualize(&self, depth: u8) -> String {
        let tacky::FunctionDefinition::Function {
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

impl visualize::Visualizer for tacky::Instruction {
    fn visualize(&self, depth: u8) -> String {
        match self {
            tacky::Instruction::Return(val) => format!("Return({})", val.visualize(depth + 1)),
            tacky::Instruction::Unary {
                unary_operator,
                source,
                destination,
            } => format!(
                "Unary({}, {}, {})",
                unary_operator.visualize(depth + 1),
                source.visualize(depth + 1),
                destination.visualize(depth + 1),
            ),
            tacky::Instruction::Binary {
                binary_operator,
                source1,
                source2,
                destination,
            } => format!(
                "Binary({}, {}, {}, {})",
                binary_operator.visualize(depth + 1),
                source1.visualize(depth + 1),
                source2.visualize(depth + 1),
                destination.visualize(depth + 1),
            ),
            tacky::Instruction::Jump { target } => format!("Jump(label: {target})"),
            tacky::Instruction::JumpIfZero { condition, target } => {
                format!(
                    "JumpIfZero({}, label: {})",
                    condition.visualize(depth + 1),
                    target
                )
            }
            tacky::Instruction::JumpIfNotZero { condition, target } => {
                format!(
                    "JumpIfNotZero({}, label: {})",
                    condition.visualize(depth + 1),
                    target
                )
            }
            tacky::Instruction::Label { identifier } => format!("Label {identifier}:"),
            tacky::Instruction::Copy {
                source,
                destination,
            } => format!(
                "Copy: {}, {}",
                source.visualize(depth + 1),
                destination.visualize(depth + 1)
            ),
        }
    }
}

impl visualize::Visualizer for tacky::UnaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            tacky::UnaryOperator::Complement => String::from("Complement"),
            tacky::UnaryOperator::Negate => String::from("Negate"),
            tacky::UnaryOperator::Not => String::from("Not"),
        }
    }
}

impl visualize::Visualizer for tacky::BinaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            tacky::BinaryOperator::Add => String::from("Add"),
            tacky::BinaryOperator::Subtract => String::from("Subtract"),
            tacky::BinaryOperator::Multiply => String::from("Multiply"),
            tacky::BinaryOperator::Divide => String::from("Divide"),
            tacky::BinaryOperator::Remainder => String::from("Remainder"),
            tacky::BinaryOperator::LeftShift => String::from("LeftShift"),
            tacky::BinaryOperator::RightShift => String::from("RightShift"),
            tacky::BinaryOperator::BitwiseAnd => String::from("BitwiseAnd"),
            tacky::BinaryOperator::BitwiseXOr => String::from("BitwiseXOr"),
            tacky::BinaryOperator::BitwiseOr => String::from("BitwiseOr"),
            tacky::BinaryOperator::And => String::from("And"),
            tacky::BinaryOperator::Or => String::from("Or"),
            tacky::BinaryOperator::Equal => String::from("Equal"),
            tacky::BinaryOperator::NotEqual => String::from("NotEqual"),
            tacky::BinaryOperator::LessThan => String::from("LessThan"),
            tacky::BinaryOperator::LessOrEqual => String::from("LessOrEqual"),
            tacky::BinaryOperator::GreaterThan => String::from("GreaterThan"),
            tacky::BinaryOperator::GreaterOrEqual => String::from("GreaterOrEqual"),
        }
    }
}

impl visualize::Visualizer for tacky::Val {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            tacky::Val::Constant(value) => format!("Constant({value})"),
            tacky::Val::Var(value) => format!("Var(\"{value}\")"),
        }
    }
}
