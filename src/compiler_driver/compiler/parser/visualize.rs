use crate::compiler_driver::compiler::parser::{self, Expression};
use crate::compiler_driver::compiler::visualize;

impl visualize::Visualizer for parser::Program {
    fn visualize(&self, depth: u8) -> String {
        let parser::Program::Program(function_definition) = self;
        format!(
            "Program(\n\
        {}\n\
        )",
            function_definition.visualize(depth + 1)
        )
    }
}

impl visualize::Visualizer for parser::AbstractSyntaxTree {
    fn visualize(&self, _depth: u8) -> String {
        let parser::AbstractSyntaxTree::Program(program) = self;
        program.visualize(0)
    }
}

impl visualize::Visualizer for parser::Block {
    fn visualize(&self, depth: u8) -> String {
        let parser::Block::Block(body) = self;
        format!(
            "{{{}}}",
            body.iter()
                .map(|block_item| block_item.visualize(depth + 1))
                .collect::<Vec<String>>()
                .join(format!("\n").as_str())
        )
    }
}

impl visualize::Visualizer for parser::FunctionDefinition {
    fn visualize(&self, depth: u8) -> String {
        let parser::FunctionDefinition::Function { identifier, body } = self;
        let indent = "    ";
        let prefix = indent.repeat(depth as usize);

        format!(
            "{prefix}Function(name={identifier}, body=\n\
            {}\n\
            {prefix})",
            body.visualize(depth)
        )
    }
}

impl visualize::Visualizer for parser::BlockItem {
    fn visualize(&self, depth: u8) -> String {
        match self {
            parser::BlockItem::Declaration(declaration) => declaration.visualize(depth),
            parser::BlockItem::Statement(statement) => statement.visualize(depth),
        }
    }
}

impl visualize::Visualizer for parser::Declaration {
    fn visualize(&self, depth: u8) -> String {
        let indent = "    ";
        let prefix = indent.repeat(depth as usize);
        let parser::Declaration::Declaration { identifier, init } = self;
        match init {
            Some(expression) => match expression {
                Expression::Constant(..) | Expression::Var { .. } => {
                    format!("{prefix}int {identifier} = {};", expression.visualize(0))
                }
                _ => format!(
                    "{prefix}int {identifier} =\n{};",
                    expression.visualize(depth + 1)
                ),
            },
            None => format!("{prefix}int {identifier};"),
        }
    }
}

impl visualize::Visualizer for parser::Statement {
    fn visualize(&self, depth: u8) -> String {
        let indent = "    ";
        let prefix = indent.repeat(depth as usize);
        match self {
            parser::Statement::Return(expression) => {
                format!(
                    "{prefix}Return(\n\
                    {}\n\
                    {prefix})",
                    expression.visualize(depth + 1)
                )
            }
            parser::Statement::Expression(expression) => expression.visualize(depth),
            parser::Statement::If {
                condition,
                then_statement,
                optional_else_statement,
            } => match optional_else_statement {
                Some(else_statement) => format!(
                    "{prefix}If(\n\
                    condition={}\n\
                    then={}\n\
                    else={}\n\
                    {prefix})",
                    condition.visualize(depth + 1),
                    (*then_statement).visualize(depth + 1),
                    (*else_statement).visualize(depth + 1)
                ),
                None => format!(
                    "If(\n\
                        {prefix}{indent}condition={}\n\
                        {prefix}{indent}then={}\n\
                        {prefix})",
                    condition.visualize(depth + 1),
                    (*then_statement).visualize(depth + 1)
                ),
            },
            parser::Statement::Goto(identifier) => format!("goto {identifier}"),
            parser::Statement::Label(identifier) => format!("{identifier}:"),
            parser::Statement::Null => format!("{prefix};"),
        }
    }
}

impl visualize::Visualizer for parser::Expression {
    fn visualize(&self, depth: u8) -> String {
        let indent = "    ";
        let prefix = indent.repeat(depth as usize);
        match self {
            parser::Expression::Constant(value) => {
                format!("{prefix}Constant({value})")
            }
            parser::Expression::Var { identifier } => {
                format!("{prefix}Var({identifier})")
            }
            parser::Expression::Unary(unary_operator, boxed_expression) => match unary_operator {
                parser::UnaryOperator::PostfixDecrement
                | parser::UnaryOperator::PostfixIncrement => format!(
                    "{}{}",
                    boxed_expression.visualize(depth + 1),
                    unary_operator.visualize(depth + 1),
                ),
                _ => format!(
                    "{}{}",
                    unary_operator.visualize(depth + 1),
                    boxed_expression.visualize(depth + 1),
                ),
            },
            parser::Expression::BinaryOperation {
                binary_operator,
                left_operand,
                right_operand,
            } => {
                format!(
                    "{prefix}BinaryOperation(\n\
                    {prefix}{indent}{}:\n\
                    {},\n\
                    {}\n\
                    {prefix})",
                    binary_operator.visualize(0),
                    left_operand.visualize(depth + 1),
                    right_operand.visualize(depth + 1),
                )
            }
            parser::Expression::Assignment(boxed_expression1, boxed_expression2) => {
                format!(
                    "{prefix}{} = {}",
                    boxed_expression1.visualize(depth + 1),
                    boxed_expression2.visualize(depth + 1),
                )
            }
            parser::Expression::Conditional(
                boxed_expression1,
                boxed_expression2,
                boxed_expression3,
            ) => {
                format!(
                    "{prefix}Conditional:\n{}\n{prefix}?\n{}\n{prefix}:\n{}",
                    boxed_expression1.visualize(depth + 1),
                    boxed_expression2.visualize(depth + 1),
                    boxed_expression3.visualize(depth + 1),
                )
            }
        }
    }
}

impl visualize::Visualizer for parser::BinaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            parser::BinaryOperator::Add => String::from("Add"),
            parser::BinaryOperator::Subtract => String::from("Subtract"),
            parser::BinaryOperator::Multiply => String::from("Multiply"),
            parser::BinaryOperator::Divide => String::from("Divide"),
            parser::BinaryOperator::Remainder => String::from("Remainder"),
            parser::BinaryOperator::LeftShift => String::from("LeftShift"),
            parser::BinaryOperator::RightShift => String::from("RightShift"),
            parser::BinaryOperator::BitwiseAnd => String::from("BitwiseAnd"),
            parser::BinaryOperator::BitwiseXOr => String::from("BitwiseXOr"),
            parser::BinaryOperator::BitwiseOr => String::from("BitwiseOr"),
            parser::BinaryOperator::And => String::from("And"),
            parser::BinaryOperator::Or => String::from("Or"),
            parser::BinaryOperator::Equal => String::from("Equal"),
            parser::BinaryOperator::NotEqual => String::from("NotEqual"),
            parser::BinaryOperator::LessThan => String::from("LessThan"),
            parser::BinaryOperator::LessOrEqual => String::from("LessOrEqual"),
            parser::BinaryOperator::GreaterThan => String::from("GreaterThan"),
            parser::BinaryOperator::GreaterOrEqual => String::from("GreaterOrEqual"),
            parser::BinaryOperator::Conditional => String::from("Conditional"),
            parser::BinaryOperator::Assign => String::from("Assign"),
            parser::BinaryOperator::SumAssign => String::from("SumAssign"),
            parser::BinaryOperator::DifferenceAssign => String::from("DifferenceAssign"),
            parser::BinaryOperator::ProductAssign => String::from("ProductAssign"),
            parser::BinaryOperator::QuotientAssign => String::from("QuotientAssign"),
            parser::BinaryOperator::RemainderAssign => String::from("RemainderAssign"),
            parser::BinaryOperator::BitwiseAndAssign => String::from("BitwiseAndAssign"),
            parser::BinaryOperator::BitwiseOrAssign => String::from("BitwiseOrAssign"),
            parser::BinaryOperator::BitwiseXOrAssign => String::from("BitwiseXOrAssign"),
            parser::BinaryOperator::LeftShiftAssign => String::from("LeftShiftAssign"),
            parser::BinaryOperator::RightShiftAssign => String::from("RightShiftAssign"),
        }
    }
}

impl visualize::Visualizer for parser::UnaryOperator {
    fn visualize(&self, _depth: u8) -> String {
        match self {
            parser::UnaryOperator::Complement => String::from("~"),
            parser::UnaryOperator::Negate => String::from("-"),
            parser::UnaryOperator::Not => String::from("!"),
            parser::UnaryOperator::PrefixDecrement | parser::UnaryOperator::PostfixDecrement => {
                String::from("--")
            }
            parser::UnaryOperator::PrefixIncrement | parser::UnaryOperator::PostfixIncrement => {
                String::from("++")
            }
        }
    }
}
