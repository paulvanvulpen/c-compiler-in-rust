mod visualize;

use super::parser;
use crate::compiler_driver::compiler::tacky::Instruction::{JumpIfNotZero, JumpIfZero};

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program (function_definition)
// function_definition = Function(identifier, instruction* body)
// instruction = Return(val)
//      | Unary(unary_operator, val src, val dst)
//      | Binary(binary_operator, val src1, val src2, val dst)
//      | Copy(val src, val dst)
//      | Jump(identifier target)
//      | JumpIfZero(val condition, identifier target)
//      | JumpIfNotZero(val condition, identifier target)
//      | Label(identifier)
// val = Constant(int) | Var(identifier)
// unary_operator = Complement | Negate | Not
// binary_operator = Add | Subtract | Multiply | Divide | Remainder | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual
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
    Copy {
        source: Val,
        destination: Val,
    },
    Jump {
        target: String,
    },
    JumpIfZero {
        condition: Val,
        target: String,
    },
    JumpIfNotZero {
        condition: Val,
        target: String,
    },
    Label {
        identifier: String,
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
    GreaterOrEqual,
    GreaterThan,
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
        parser::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
    }
}

fn convert_unary_operator(unary_operator: parser::UnaryOperator) -> UnaryOperator {
    match unary_operator {
        parser::UnaryOperator::Complement => UnaryOperator::Complement,
        parser::UnaryOperator::Negate => UnaryOperator::Negate,
        parser::UnaryOperator::Not => UnaryOperator::Not,
    }
}

static TMP_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn make_temporary() -> String {
    let id = TMP_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("tmp.{id}")
}

static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
fn make_label(label: &str) -> String {
    let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("{label}{id}")
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
        } => match binary_operator {
            parser::BinaryOperator::Add
            | parser::BinaryOperator::Subtract
            | parser::BinaryOperator::Multiply
            | parser::BinaryOperator::Divide
            | parser::BinaryOperator::Remainder
            | parser::BinaryOperator::LeftShift
            | parser::BinaryOperator::RightShift
            | parser::BinaryOperator::BitwiseAnd
            | parser::BinaryOperator::BitwiseXOr
            | parser::BinaryOperator::BitwiseOr
            | parser::BinaryOperator::Equal
            | parser::BinaryOperator::NotEqual
            | parser::BinaryOperator::LessThan
            | parser::BinaryOperator::LessOrEqual
            | parser::BinaryOperator::GreaterThan
            | parser::BinaryOperator::GreaterOrEqual => {
                let binary_operator = convert_binary_operator(binary_operator);
                let (mut instructions1, destination_left_operand) =
                    convert_boxed_expression(left_operand);
                let (mut instructions2, destination_right_operand) =
                    convert_boxed_expression(right_operand);
                instructions1.append(&mut instructions2);
                let mut instructions = instructions1;

                let final_destination = make_temporary();
                let destination = Val::Var(final_destination);
                instructions.push(Instruction::Binary {
                    binary_operator,
                    source1: destination_left_operand,
                    source2: destination_right_operand,
                    destination: destination.clone(),
                });
                (instructions, destination)
            }
            parser::BinaryOperator::And => {
                // we conclude by an end result of 0 or 1. if either expression result is zero
                // we jump to the false_label where we set the end result to zero
                // when both results are non-zero then instead we set the result to one and jump
                // straight to the end_label
                let false_label = make_label("false");
                let end_label = make_label("end");

                let (left_instructions, destination_left_operand) =
                    convert_boxed_expression(left_operand);
                let left_expression_result = make_temporary();
                let left_expression_result = Val::Var(left_expression_result);
                let mut instructions = left_instructions;
                instructions.push(Instruction::Copy {
                    source: destination_left_operand,
                    destination: left_expression_result.clone(),
                });
                instructions.push(JumpIfZero {
                    condition: left_expression_result,
                    target: false_label.clone(),
                });

                let (mut right_instructions, destination_right_operand) =
                    convert_boxed_expression(right_operand);
                let right_expression_result = make_temporary();
                let right_expression_result = Val::Var(right_expression_result);
                instructions.append(&mut right_instructions);
                instructions.push(Instruction::Copy {
                    source: destination_right_operand,
                    destination: right_expression_result.clone(),
                });
                instructions.push(JumpIfZero {
                    condition: right_expression_result,
                    target: false_label.clone(),
                });

                let end_result = make_temporary();
                let end_result = Val::Var(end_result);

                instructions.push(Instruction::Copy {
                    source: Val::Constant(1),
                    destination: end_result.clone(),
                });

                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });
                instructions.push(Instruction::Label {
                    identifier: false_label,
                });
                instructions.push(Instruction::Copy {
                    source: Val::Constant(0),
                    destination: end_result.clone(),
                });
                instructions.push(Instruction::Label {
                    identifier: end_label,
                });

                (instructions, end_result)
            }
            parser::BinaryOperator::Or => {
                // return with an end result of 0 or 1. If either expression is non-zero
                // jump to a true_label where the end result is set to 1.
                // if no jump occurs the statement where the end result it set to 0, followed
                // by a jump to the end-label.
                let true_label = make_label("true");
                let end_label = make_label("end");
                let (left_instructions, destination_left_operand) =
                    convert_boxed_expression(left_operand);
                let mut instructions = left_instructions;
                let left_expression_result = make_temporary();
                let left_expression_result = Val::Var(left_expression_result);
                instructions.push(Instruction::Copy {
                    source: destination_left_operand,
                    destination: left_expression_result.clone(),
                });
                instructions.push(JumpIfNotZero {
                    condition: left_expression_result,
                    target: true_label.clone(),
                });

                let (mut right_instructions, destination_right_operand) =
                    convert_boxed_expression(right_operand);
                let right_expression_result = make_temporary();
                let right_expression_result = Val::Var(right_expression_result);
                instructions.append(&mut right_instructions);
                instructions.push(Instruction::Copy {
                    source: destination_right_operand,
                    destination: right_expression_result.clone(),
                });
                instructions.push(JumpIfNotZero {
                    condition: right_expression_result,
                    target: true_label.clone(),
                });

                let end_result = make_temporary();
                let end_result = Val::Var(end_result);

                instructions.push(Instruction::Copy {
                    source: Val::Constant(0),
                    destination: end_result.clone(),
                });

                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });

                instructions.push(Instruction::Label {
                    identifier: true_label,
                });
                instructions.push(Instruction::Copy {
                    source: Val::Constant(1),
                    destination: end_result.clone(),
                });
                instructions.push(Instruction::Label {
                    identifier: end_label,
                });

                (instructions, end_result)
            }
        },
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
