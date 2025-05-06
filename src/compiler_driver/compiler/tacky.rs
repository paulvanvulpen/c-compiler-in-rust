mod visualize;

use super::parser;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program (function_definition)
// function_definition = Function(identifier, instruction* body)
// instruction = Return(val) | Unary(unary_operator, val src, val dst)
// val = Constant(int) | Var(identifier)
// unary_operator = Complement | Negate
pub enum TackyAbstractSyntaxTree {
    Program(Program),
}

pub enum Program {
    Program(FunctionDefinition),
}

pub enum FunctionDefinition {
    Function {
        identifier: String,
        instructions: Vec<Instruction>,
    },
}

pub enum Instruction {
    Return(Val),
    Unary {
        unary_operator: UnaryOperator,
        source: Val,
        destination: Val,
    },
    Binary {
        binary_operator: BinaryOperator,
        source1: Val,
        source2: Val,
        destination: Val,
    },
}

pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
}

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseXOr,
    BitwiseOr,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

#[derive(Clone)]
pub enum Val {
    Constant(usize),
    Var(String),
}

fn convert_binary_operator(binary_operator: parser::BinaryOperator) -> BinaryOperator {
    match binary_operator {
        parser::BinaryOperator::Add => BinaryOperator::Add,
        parser::BinaryOperator::Subtract => BinaryOperator::Subtract,
        parser::BinaryOperator::Multiply => BinaryOperator::Multiply,
        parser::BinaryOperator::Divide => BinaryOperator::Divide,
        parser::BinaryOperator::Remainder => BinaryOperator::Remainder,
        parser::BinaryOperator::LeftShift => BinaryOperator::LeftShift,
        parser::BinaryOperator::RightShift => BinaryOperator::RightShift,
        parser::BinaryOperator::BitwiseAnd => BinaryOperator::BitwiseAnd,
        parser::BinaryOperator::BitwiseXOr => BinaryOperator::BitwiseXOr,
        parser::BinaryOperator::BitwiseOr => BinaryOperator::BitwiseOr,
        parser::BinaryOperator::And => BinaryOperator::And,
        parser::BinaryOperator::Or => BinaryOperator::Or,
        parser::BinaryOperator::Equal => BinaryOperator::Equal,
        parser::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
        parser::BinaryOperator::LessThan => BinaryOperator::LessThan,
        parser::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
        parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
        parser::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
    }
}

fn convert_unary_operator(unary_operator: parser::UnaryOperator) -> UnaryOperator {
    match unary_operator {
        parser::UnaryOperator::Complement => UnaryOperator::Complement,
        parser::UnaryOperator::Negate => UnaryOperator::Negate,
        parser::UnaryOperator::Not => UnaryOperator::Not,
    }
}

static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn make_temporary() -> String {
    let id = COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("tmp.{id}")
}

fn convert_boxed_expression(boxed_expression: Box<parser::Expression>) -> (Vec<Instruction>, Val) {
    match *boxed_expression {
        parser::Expression::Constant(value) => {
            let instructions: Vec<Instruction> = vec![];
            (instructions, Val::Constant(value))
        }
        parser::Expression::Unary(unary_operator, boxed_expression) => {
            let (mut instructions, source) = convert_boxed_expression(boxed_expression);
            let destination = make_temporary();
            let destination = Val::Var(destination);
            let unary_operator = convert_unary_operator(unary_operator);
            instructions.push(Instruction::Unary {
                unary_operator,
                source,
                destination: destination.clone(),
            });
            (instructions, destination)
        }
        parser::Expression::BinaryOperation {
            binary_operator,
            left_operand,
            right_operand,
        } => {
            let binary_operator = convert_binary_operator(binary_operator);
            let (mut instructions1, source1) = convert_boxed_expression(left_operand);
            let (mut instructions2, source2) = convert_boxed_expression(right_operand);
            instructions1.append(&mut instructions2);
            let mut instructions = instructions1;

            let destination = make_temporary();
            let destination = Val::Var(destination);
            instructions.push(Instruction::Binary {
                binary_operator,
                source1,
                source2,
                destination: destination.clone(),
            });
            (instructions, destination)
        }
    }
}

fn convert_statement(statement: parser::Statement) -> Vec<Instruction> {
    match statement {
        parser::Statement::Return(expression) => {
            let (mut instructions, final_destination) =
                convert_boxed_expression(Box::new(expression));
            instructions.push(Instruction::Return(final_destination));
            instructions
        }
    }
}

fn convert_function_definition(
    function_definition: parser::FunctionDefinition,
) -> FunctionDefinition {
    match function_definition {
        parser::FunctionDefinition::Function { identifier, body } => FunctionDefinition::Function {
            identifier,
            instructions: convert_statement(body),
        },
    }
}

fn convert_program(program: parser::Program) -> Program {
    match program {
        parser::Program::Program(function_definition) => {
            Program::Program(convert_function_definition(function_definition))
        }
    }
}
fn convert_ast(ast: parser::AbstractSyntaxTree) -> TackyAbstractSyntaxTree {
    match ast {
        parser::AbstractSyntaxTree::Program(program) => {
            TackyAbstractSyntaxTree::Program(convert_program(program))
        }
    }
}

pub fn run_tacky_generator(
    ast: parser::AbstractSyntaxTree,
) -> anyhow::Result<TackyAbstractSyntaxTree> {
    let tacky_ast = convert_ast(ast);
    Ok(tacky_ast)
}
