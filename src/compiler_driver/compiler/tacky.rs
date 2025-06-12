mod visualize;

use super::generator;
use super::parser;

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
    PrefixDecrement,
    PostfixDecrement,
    PrefixIncrement,
    PostfixIncrement,
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
        parser::BinaryOperator::Add | parser::BinaryOperator::SumAssign => BinaryOperator::Add,
        parser::BinaryOperator::Subtract | parser::BinaryOperator::DifferenceAssign => {
            BinaryOperator::Subtract
        }
        parser::BinaryOperator::Multiply | parser::BinaryOperator::ProductAssign => {
            BinaryOperator::Multiply
        }
        parser::BinaryOperator::Divide | parser::BinaryOperator::QuotientAssign => {
            BinaryOperator::Divide
        }
        parser::BinaryOperator::Remainder | parser::BinaryOperator::RemainderAssign => {
            BinaryOperator::Remainder
        }
        parser::BinaryOperator::LeftShift | parser::BinaryOperator::LeftShiftAssign => {
            BinaryOperator::LeftShift
        }
        parser::BinaryOperator::RightShift | parser::BinaryOperator::RightShiftAssign => {
            BinaryOperator::RightShift
        }
        parser::BinaryOperator::BitwiseAnd | parser::BinaryOperator::BitwiseAndAssign => {
            BinaryOperator::BitwiseAnd
        }
        parser::BinaryOperator::BitwiseXOr | parser::BinaryOperator::BitwiseXOrAssign => {
            BinaryOperator::BitwiseXOr
        }
        parser::BinaryOperator::BitwiseOr | parser::BinaryOperator::BitwiseOrAssign => {
            BinaryOperator::BitwiseOr
        }
        parser::BinaryOperator::And => BinaryOperator::And,
        parser::BinaryOperator::Or => BinaryOperator::Or,
        parser::BinaryOperator::Equal => BinaryOperator::Equal,
        parser::BinaryOperator::NotEqual => BinaryOperator::NotEqual,
        parser::BinaryOperator::LessThan => BinaryOperator::LessThan,
        parser::BinaryOperator::LessOrEqual => BinaryOperator::LessOrEqual,
        parser::BinaryOperator::GreaterOrEqual => BinaryOperator::GreaterOrEqual,
        parser::BinaryOperator::GreaterThan => BinaryOperator::GreaterThan,
        parser::BinaryOperator::Assign => panic!(
            "parser should have converted the Assign operation into an Assignment Expressions"
        ),
        parser::BinaryOperator::Conditional => panic!(
            "parser should have converted the conditional operation into a Conditional Expression"
        ),
    }
}

fn convert_unary_operator(unary_operator: parser::UnaryOperator) -> UnaryOperator {
    match unary_operator {
        parser::UnaryOperator::Complement => UnaryOperator::Complement,
        parser::UnaryOperator::Negate => UnaryOperator::Negate,
        parser::UnaryOperator::Not => UnaryOperator::Not,
        parser::UnaryOperator::PrefixDecrement => UnaryOperator::PrefixDecrement,
        parser::UnaryOperator::PostfixDecrement => UnaryOperator::PostfixDecrement,
        parser::UnaryOperator::PrefixIncrement => UnaryOperator::PrefixIncrement,
        parser::UnaryOperator::PostfixIncrement => UnaryOperator::PostfixIncrement,
    }
}

fn make_temporary() -> String {
    let id = generator::generate_unique_id();
    format!("tmp.{id}")
}

fn make_label(label: &str) -> String {
    static LABEL_COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
    let id = LABEL_COUNTER.fetch_add(1, std::sync::atomic::Ordering::Relaxed);
    format!("{label}{id}")
}

fn convert_expression(expression: parser::Expression) -> (Vec<Instruction>, Val) {
    match expression {
        parser::Expression::Constant(value) => {
            let instructions: Vec<Instruction> = vec![];
            (instructions, Val::Constant(value))
        }
        parser::Expression::Var { identifier } => {
            let instructions: Vec<Instruction> = vec![];
            (instructions, Val::Var(identifier))
        }
        parser::Expression::Unary(unary_operator, boxed_expression) => {
            let unary_operator = convert_unary_operator(unary_operator);
            match unary_operator {
                UnaryOperator::Complement | UnaryOperator::Negate | UnaryOperator::Not => {
                    let (mut instructions, source) = convert_expression(*boxed_expression);
                    let destination = Val::Var(make_temporary());
                    instructions.push(Instruction::Unary {
                        unary_operator,
                        source,
                        destination: destination.clone(),
                    });
                    (instructions, destination)
                }
                UnaryOperator::PrefixDecrement => {
                    convert_expression(parser::Expression::BinaryOperation {
                        binary_operator: parser::BinaryOperator::DifferenceAssign,
                        left_operand: boxed_expression,
                        right_operand: Box::new(parser::Expression::Constant(1)),
                    })
                }
                UnaryOperator::PrefixIncrement => {
                    convert_expression(parser::Expression::BinaryOperation {
                        binary_operator: parser::BinaryOperator::SumAssign,
                        left_operand: boxed_expression,
                        right_operand: Box::new(parser::Expression::Constant(1)),
                    })
                }
                UnaryOperator::PostfixDecrement | UnaryOperator::PostfixIncrement => {
                    let (mut instructions, destination_operand) =
                        convert_expression(*boxed_expression);

                    let unmodified_rhs = Val::Var(make_temporary());

                    instructions.push(Instruction::Copy {
                        source: destination_operand.clone(),
                        destination: unmodified_rhs.clone(),
                    });

                    let destination = Val::Var(make_temporary());
                    let binary_operator = match unary_operator {
                        UnaryOperator::PostfixDecrement => BinaryOperator::Subtract,
                        UnaryOperator::PostfixIncrement => BinaryOperator::Add,
                        _ => panic!("Expected postfix operator"),
                    };

                    instructions.push(Instruction::Binary {
                        binary_operator: binary_operator,
                        source1: destination_operand.clone(),
                        source2: Val::Constant(1),
                        destination: destination.clone(),
                    });

                    instructions.push(Instruction::Copy {
                        source: destination,
                        destination: destination_operand,
                    });

                    (instructions, unmodified_rhs)
                }
            }
        }
        parser::Expression::BinaryOperation {
            binary_operator,
            left_operand,
            right_operand,
        } => match binary_operator {
            parser::BinaryOperator::Assign => panic!(
                "parser should have converted the Assign operation into an Assignment Expressions"
            ),
            parser::BinaryOperator::Conditional => panic!(
                "parser should have converted the conditional operation into a Conditional Expression"
            ),
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
                    convert_expression(*left_operand);
                let (instructions2, destination_right_operand) = convert_expression(*right_operand);
                instructions1.extend(instructions2);
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
            parser::BinaryOperator::SumAssign
            | parser::BinaryOperator::DifferenceAssign
            | parser::BinaryOperator::ProductAssign
            | parser::BinaryOperator::QuotientAssign
            | parser::BinaryOperator::RemainderAssign
            | parser::BinaryOperator::BitwiseAndAssign
            | parser::BinaryOperator::BitwiseOrAssign
            | parser::BinaryOperator::BitwiseXOrAssign
            | parser::BinaryOperator::LeftShiftAssign
            | parser::BinaryOperator::RightShiftAssign => {
                let binary_operator = convert_binary_operator(binary_operator);
                let (mut instructions1, destination_left_operand) =
                    convert_expression(*left_operand);
                let (instructions2, destination_right_operand) = convert_expression(*right_operand);
                instructions1.extend(instructions2);
                let mut instructions = instructions1;

                let final_destination = make_temporary();
                let destination = Val::Var(final_destination);
                instructions.push(Instruction::Binary {
                    binary_operator,
                    source1: destination_left_operand.clone(),
                    source2: destination_right_operand,
                    destination: destination.clone(),
                });

                instructions.push(Instruction::Copy {
                    source: destination.clone(),
                    destination: destination_left_operand,
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
                    convert_expression(*left_operand);
                let left_expression_result = make_temporary();
                let left_expression_result = Val::Var(left_expression_result);
                let mut instructions = left_instructions;
                instructions.push(Instruction::Copy {
                    source: destination_left_operand,
                    destination: left_expression_result.clone(),
                });
                instructions.push(Instruction::JumpIfZero {
                    condition: left_expression_result,
                    target: false_label.clone(),
                });

                let (right_instructions, destination_right_operand) =
                    convert_expression(*right_operand);
                let right_expression_result = make_temporary();
                let right_expression_result = Val::Var(right_expression_result);
                instructions.extend(right_instructions);
                instructions.push(Instruction::Copy {
                    source: destination_right_operand,
                    destination: right_expression_result.clone(),
                });
                instructions.push(Instruction::JumpIfZero {
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
                    convert_expression(*left_operand);
                let mut instructions = left_instructions;
                let left_expression_result = make_temporary();
                let left_expression_result = Val::Var(left_expression_result);
                instructions.push(Instruction::Copy {
                    source: destination_left_operand,
                    destination: left_expression_result.clone(),
                });
                instructions.push(Instruction::JumpIfNotZero {
                    condition: left_expression_result,
                    target: true_label.clone(),
                });

                let (right_instructions, destination_right_operand) =
                    convert_expression(*right_operand);
                let right_expression_result = make_temporary();
                let right_expression_result = Val::Var(right_expression_result);
                instructions.extend(right_instructions);
                instructions.push(Instruction::Copy {
                    source: destination_right_operand,
                    destination: right_expression_result.clone(),
                });
                instructions.push(Instruction::JumpIfNotZero {
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
        parser::Expression::Assignment(lhs_expression, rhs_expression) => {
            let (.., lvalue) = convert_expression(*lhs_expression);
            let (mut instructions, rvalue) = convert_expression(*rhs_expression);
            instructions.push(Instruction::Copy {
                source: rvalue,
                destination: lvalue.clone(),
            });
            (instructions, lvalue)
        }
        parser::Expression::Conditional(lhs_expression, middle_expression, right_expression) => {
            todo!()
        }
    }
}

fn convert_statement(statement: parser::Statement) -> Vec<Instruction> {
    match statement {
        parser::Statement::Return(expression) => {
            let (mut instructions, final_destination) = convert_expression(expression);
            instructions.push(Instruction::Return(final_destination));
            instructions
        }
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => match optional_else_statement {
            Some(else_statement) => {
                let end_label: String = make_label("end");
                let else_label: String = make_label("else");
                let (mut instructions, result) = convert_expression(condition);
                instructions.push(Instruction::JumpIfZero {
                    condition: result,
                    target: else_label.clone(),
                });
                let then_instructions = convert_statement(*then_statement);
                instructions.extend(then_instructions);
                instructions.push(Instruction::Jump {
                    target: end_label.clone(),
                });
                instructions.push(Instruction::Label {
                    identifier: else_label,
                });
                let else_instructions = convert_statement(*else_statement);
                instructions.extend(else_instructions);
                instructions.push(Instruction::Label {
                    identifier: end_label,
                });
                instructions
            }
            None => {
                let end_label: String = make_label("end");
                let (mut instructions, result) = convert_expression(condition);
                instructions.push(Instruction::JumpIfZero {
                    condition: result,
                    target: end_label.clone(),
                });
                let then_instructions = convert_statement(*then_statement);
                instructions.extend(then_instructions);

                instructions.push(Instruction::Label {
                    identifier: end_label,
                });
                instructions
            }
        },
        parser::Statement::Expression(expression) => {
            let (instructions, ..) = convert_expression(expression);
            instructions
        }
        parser::Statement::Null => {
            vec![]
        }
    }
}

fn convert_function_definition(
    function_definition: parser::FunctionDefinition,
) -> FunctionDefinition {
    match function_definition {
        parser::FunctionDefinition::Function {
            identifier,
            mut body,
        } => {
            let mut tacky_body: Vec<Instruction> = vec![];
            for block_item in &mut body {
                match block_item {
                    parser::BlockItem::Declaration(declaration) => {
                        let parser::Declaration::Declaration { identifier, init } = declaration;
                        if let Some(unpacked_init) = init {
                            let unpacked_init = std::mem::take(unpacked_init);
                            let identifier = std::mem::take(identifier);
                            let assignment_expression = parser::Expression::Assignment(
                                Box::new(parser::Expression::Var { identifier }),
                                Box::new(unpacked_init),
                            );
                            let (instructions, ..) = convert_expression(assignment_expression);
                            tacky_body.extend(instructions);
                        }
                    }
                    parser::BlockItem::Statement(statement) => {
                        let original = std::mem::take(statement);
                        let instructions = convert_statement(original);
                        tacky_body.extend(instructions);
                    }
                }
            }

            // expected behavior for main when not explicitly specified
            // and handles missing return statement. This line is bypassed when a return statement is already given
            tacky_body.push(Instruction::Return(Val::Constant(0)));

            FunctionDefinition::Function {
                identifier,
                instructions: tacky_body,
            }
        }
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
