use super::tacky;
use std::collections::HashMap;

mod visualize;

const PARAMETER_REGISTERS: [Register; 6] = [
    Register::DI,
    Register::SI,
    Register::DX,
    Register::CX,
    Register::R8,
    Register::R9,
];

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(top_level*)
// top_level = Function(identifier name, bool global, instruction* instructions)
//              | StaticVariable(identifier name, bool global, int init)
// instruction = Mov(operand src, operand dst)
//              | Unary(unary_operator, operand)
//              | Binary(binary_operator, operand, operand)
//              | Cmp(operand, operand)
//              | Idiv(operand)
//              | Cdq
//              | Jmp(identifier)
//              | JmpCC(cond_code, identifier)
//              | SetCC(cond_code, operand)
//              | Label(identifier)
//              | AllocateStack(int)
//              | DeallocateStack(int)
//              | Push(operand)
//              | Call(identifier)
//              | Ret
// unary_operator = Neg | Not
// binary_operator = Add | Sub | Mult
// operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
// cond_code = E | NE | G | GE | L | LE
// reg = AX | CX | DX | DI | SI | R8 | R8 | R10 | R11
pub enum AssemblyAbstractSyntaxTree {
    Program(Program),
}

pub enum Program {
    Program(Vec<TopLevel>),
}

pub enum TopLevel {
    Function {
        identifier: String,
        is_globally_visible: bool,
        instructions: Vec<Instruction>,
        stack_size: usize,
    },
    StaticVariable {
        identifier: String,
        is_globally_visible: bool,
        init: usize,
    },
}

pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    Binary(BinaryOperator, Operand, Operand),
    Cmp(Operand, Operand),
    Idiv(Operand),
    Cdq,
    Jmp(String),
    JmpCC(ConditionCode, String),
    SetCC(ConditionCode, Operand),
    Label(String),
    AllocateStack(usize),
    DeallocateStack(usize),
    Push(Operand),
    Call(String),
    Ret,
}

pub enum ConditionCode {
    E,
    NE,
    G,
    GE,
    L,
    LE,
}

pub enum UnaryOperator {
    Neg,
    Not,
}

pub enum BinaryOperator {
    Add,
    Sub,
    Mult,
    LShift,
    RShift,
    BitAnd,
    BitXOr,
    BitOr,
}

#[derive(Clone)]
pub enum Operand {
    Immediate(usize),
    Register(Register),
    Pseudo { identifier: String },
    Stack { offset: isize },
}

#[derive(Clone)]
pub enum Register {
    AX,
    CX,
    DX,
    DI,
    SI,
    R8,
    R9,
    R10,
    R11,
}

fn convert_value(value: tacky::Value) -> Operand {
    match value {
        tacky::Value::Constant(value) => Operand::Immediate(value),
        tacky::Value::Var(identifier) => Operand::Pseudo { identifier },
    }
}

fn convert_unary_operator(unary_operator: tacky::UnaryOperator) -> UnaryOperator {
    match unary_operator {
        tacky::UnaryOperator::Complement => UnaryOperator::Not,
        tacky::UnaryOperator::Negate => UnaryOperator::Neg,
        tacky::UnaryOperator::Not => UnaryOperator::Not,
        tacky::UnaryOperator::PrefixDecrement
        | tacky::UnaryOperator::PostfixDecrement
        | tacky::UnaryOperator::PrefixIncrement
        | tacky::UnaryOperator::PostfixIncrement => {
            panic!("these should have been converted by tacky")
        }
    }
}

fn convert_binary_operator(binary_operator: tacky::BinaryOperator) -> BinaryOperator {
    match binary_operator {
        tacky::BinaryOperator::Add => BinaryOperator::Add,
        tacky::BinaryOperator::Subtract => BinaryOperator::Sub,
        tacky::BinaryOperator::Multiply => BinaryOperator::Mult,
        tacky::BinaryOperator::LeftShift => BinaryOperator::LShift,
        tacky::BinaryOperator::RightShift => BinaryOperator::RShift,
        tacky::BinaryOperator::BitwiseAnd => BinaryOperator::BitAnd,
        tacky::BinaryOperator::BitwiseXOr => BinaryOperator::BitXOr,
        tacky::BinaryOperator::BitwiseOr => BinaryOperator::BitOr,
        _ => panic!(),
    }
}

fn find_associated_condition_code(binary_operator: tacky::BinaryOperator) -> ConditionCode {
    match binary_operator {
        tacky::BinaryOperator::Equal => ConditionCode::E,
        tacky::BinaryOperator::NotEqual => ConditionCode::NE,
        tacky::BinaryOperator::LessThan => ConditionCode::L,
        tacky::BinaryOperator::LessOrEqual => ConditionCode::LE,
        tacky::BinaryOperator::GreaterOrEqual => ConditionCode::GE,
        tacky::BinaryOperator::GreaterThan => ConditionCode::G,
        _ => panic!(),
    }
}

fn convert_instruction(instruction: tacky::Instruction) -> Vec<Instruction> {
    match instruction {
        tacky::Instruction::Return(val) => {
            vec![
                Instruction::Mov(convert_value(val), Operand::Register(Register::AX)),
                Instruction::Ret,
            ]
        }
        tacky::Instruction::Unary {
            unary_operator,
            source,
            destination,
        } => match unary_operator {
            tacky::UnaryOperator::Complement | tacky::UnaryOperator::Negate => vec![
                Instruction::Mov(convert_value(source), convert_value(destination.clone())),
                Instruction::Unary(
                    convert_unary_operator(unary_operator),
                    convert_value(destination),
                ),
            ],
            tacky::UnaryOperator::Not => vec![
                Instruction::Cmp(Operand::Immediate(0), convert_value(source)),
                Instruction::Mov(Operand::Immediate(0), convert_value(destination.clone())),
                Instruction::SetCC(ConditionCode::E, convert_value(destination)),
            ],
            tacky::UnaryOperator::PrefixDecrement
            | tacky::UnaryOperator::PostfixDecrement
            | tacky::UnaryOperator::PrefixIncrement
            | tacky::UnaryOperator::PostfixIncrement => {
                panic!("these should have been converted by tacky")
            }
        },
        tacky::Instruction::Binary {
            binary_operator,
            source1,
            source2,
            destination,
        } => match binary_operator {
            tacky::BinaryOperator::Add
            | tacky::BinaryOperator::Subtract
            | tacky::BinaryOperator::Multiply
            | tacky::BinaryOperator::LeftShift
            | tacky::BinaryOperator::RightShift
            | tacky::BinaryOperator::BitwiseAnd
            | tacky::BinaryOperator::BitwiseXOr
            | tacky::BinaryOperator::BitwiseOr => {
                vec![
                    Instruction::Mov(
                        convert_value(source1.clone()),
                        convert_value(destination.clone()),
                    ),
                    Instruction::Binary(
                        convert_binary_operator(binary_operator),
                        convert_value(source2),
                        convert_value(destination),
                    ),
                ]
            }
            tacky::BinaryOperator::Equal
            | tacky::BinaryOperator::NotEqual
            | tacky::BinaryOperator::LessThan
            | tacky::BinaryOperator::LessOrEqual
            | tacky::BinaryOperator::GreaterOrEqual
            | tacky::BinaryOperator::GreaterThan => {
                vec![
                    Instruction::Cmp(convert_value(source2), convert_value(source1)),
                    Instruction::Mov(Operand::Immediate(0), convert_value(destination.clone())),
                    Instruction::SetCC(
                        find_associated_condition_code(binary_operator),
                        convert_value(destination),
                    ),
                ]
            }
            tacky::BinaryOperator::Divide => {
                vec![
                    Instruction::Mov(convert_value(source1), Operand::Register(Register::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(convert_value(source2)),
                    Instruction::Mov(Operand::Register(Register::AX), convert_value(destination)),
                ]
            }
            tacky::BinaryOperator::Remainder => {
                vec![
                    Instruction::Mov(convert_value(source1), Operand::Register(Register::AX)),
                    Instruction::Cdq,
                    Instruction::Idiv(convert_value(source2)),
                    Instruction::Mov(Operand::Register(Register::DX), convert_value(destination)),
                ]
            }
            tacky::BinaryOperator::And | tacky::BinaryOperator::Or => {
                panic!("These should have been handled during the tacky emitting phase")
            }
        },
        tacky::Instruction::Copy {
            source,
            destination,
        } => {
            vec![Instruction::Mov(
                convert_value(source),
                convert_value(destination),
            )]
        }
        tacky::Instruction::JumpIfZero { condition, target } => {
            vec![
                Instruction::Cmp(Operand::Immediate(0), convert_value(condition)),
                Instruction::JmpCC(ConditionCode::E, target),
            ]
        }
        tacky::Instruction::JumpIfNotZero { condition, target } => {
            vec![
                Instruction::Cmp(Operand::Immediate(0), convert_value(condition)),
                Instruction::JmpCC(ConditionCode::NE, target),
            ]
        }
        tacky::Instruction::Jump { target } => {
            vec![Instruction::Jmp(target)]
        }
        tacky::Instruction::Label { identifier } => {
            vec![Instruction::Label(identifier)]
        }
        tacky::Instruction::FunCall {
            identifier,
            arguments,
            destination,
        } => {
            let mut instructions: Vec<Instruction> = vec![];
            let mut iter = arguments.into_iter();
            let register_arguments: Vec<tacky::Value> =
                iter.by_ref().take(PARAMETER_REGISTERS.len()).collect();
            let stack_arguments: Vec<tacky::Value> = iter.collect();
            let stack_arguments_length = stack_arguments.len();
            let stack_padding = if stack_arguments_length % 2 == 0 {
                0
            } else {
                8
            };
            if stack_padding != 0 {
                instructions.push(Instruction::AllocateStack(8))
            }
            for (register, tacky_argument) in PARAMETER_REGISTERS.iter().zip(register_arguments) {
                let assembly_argument = convert_value(tacky_argument);
                instructions.push(Instruction::Mov(
                    assembly_argument,
                    Operand::Register(register.clone()),
                ));
            }

            for tacky_argument in stack_arguments.into_iter().rev() {
                let assembly_argument = convert_value(tacky_argument);
                match assembly_argument {
                    Operand::Immediate(_) => {
                        instructions.push(Instruction::Push(assembly_argument));
                    }
                    Operand::Register(_) => panic!("TACKY has no concept of a register"),
                    Operand::Pseudo { .. } => {
                        // AX is safe as it's not a callee-saved registers
                        instructions.push(Instruction::Mov(
                            assembly_argument,
                            Operand::Register(Register::AX),
                        ));
                        instructions.push(Instruction::Push(Operand::Register(Register::AX)));
                    }
                    Operand::Stack { .. } => panic!(
                        "stack operations are only produced during the 'replace-pseudo-registers'-pass"
                    ),
                }
            }
            instructions.push(Instruction::Call(identifier));
            let bytes_to_remove = 8 * stack_arguments_length + stack_padding;
            if bytes_to_remove != 0 {
                instructions.push(Instruction::DeallocateStack(bytes_to_remove));
            }

            let assembly_destination = convert_value(destination);
            instructions.push(Instruction::Mov(
                Operand::Register(Register::AX),
                assembly_destination,
            ));
            instructions
        }
    }
}

fn convert_parameters(parameters: Vec<String>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = parameters
        .iter()
        .zip(PARAMETER_REGISTERS.iter())
        .map(|(param, reg)| {
            Instruction::Mov(
                Operand::Register(reg.clone()),
                Operand::Pseudo {
                    identifier: param.clone(),
                },
            )
        })
        .collect();

    // "call" pushes the current %RIP onto the stack (8 bytes)
    // then %RBP is put onto the stack next during the function prologue. (8 bytes)
    // so to reach the arguments of the previous stack frame,
    // it requires to travel up the stack 16 bytes to reach the last argument.
    let mut stack_offset = 16;
    for i in 6..parameters.len() {
        instructions.push(Instruction::Mov(
            Operand::Stack {
                offset: stack_offset,
            },
            Operand::Pseudo {
                identifier: parameters[i].clone(),
            },
        ));
        stack_offset += 8;
    }

    instructions
}

fn convert_toplevel_definition(toplevel_definitions: tacky::TopLevel) -> TopLevel {
    match toplevel_definitions {
        tacky::TopLevel::Function {
            identifier,
            is_globally_visible,
            parameters,
            instructions,
        } => {
            let copied_parameter_count = parameters.len();
            let mut converted_instructions: Vec<Instruction> = convert_parameters(parameters);

            TopLevel::Function {
                identifier,
                is_globally_visible: is_globally_visible,
                instructions: {
                    converted_instructions.extend(
                        instructions
                            .into_iter()
                            .flat_map(convert_instruction)
                            .collect::<Vec<Instruction>>(),
                    );
                    converted_instructions
                },
                stack_size: copied_parameter_count * 4,
            }
        }
        tacky::TopLevel::StaticVariable(static_variable) => {
            let tacky::StaticVariable {
                identifier,
                is_globally_visible,
                init,
            } = static_variable;
            TopLevel::StaticVariable {
                identifier,
                is_globally_visible,
                init,
            }
        }
    }
}

fn convert_program(program: tacky::Program) -> Program {
    match program {
        tacky::Program::Program(toplevel_definitions) => Program::Program(
            toplevel_definitions
                .into_iter()
                .map(|t| convert_toplevel_definition(t))
                .collect(),
        ),
    }
}

fn convert_ast(ast: tacky::TackyAbstractSyntaxTree) -> AssemblyAbstractSyntaxTree {
    match ast {
        tacky::TackyAbstractSyntaxTree::Program(program) => {
            AssemblyAbstractSyntaxTree::Program(convert_program(program))
        }
    }
}

pub fn replace_pseudo_registers(
    assembly_ast: &mut AssemblyAbstractSyntaxTree,
    symbol_table: &HashMap<String, symbol_table::SymbolState>,
) {
    let mut temporary_to_offset: HashMap<String, isize> = HashMap::new();

    let AssemblyAbstractSyntaxTree::Program(Program::Program(toplevel_definitions)) = assembly_ast;

    let mut replace_pseudo_with_stack = |operand: &mut Operand, alloc_size: &mut usize| {
        if let Operand::Pseudo { identifier } = operand {
            let identifier = std::mem::take(identifier);
            if !temporary_to_offset.contains_key(&identifier)
                && symbol_table.contains_key(&identifier)
                && let symbol_table::IdentifierAttributes::StaticStorageAttribute { .. } =
                    &symbol_table[&identifier].identifier_attributes
            {
                *operand = Operand::Data { identifier };
            } else {
                let offset: isize = *temporary_to_offset.entry(identifier).or_insert_with(|| {
                    *alloc_size += 4;
                    -(*alloc_size as isize)
                });
                *operand = Operand::Stack { offset };
            }
        }
    };

    for top_level_definition in toplevel_definitions {
        match top_level_definition {
            TopLevel::Function {
                identifier,
                is_globally_visible,
                instructions,
                stack_size,
            } => {
                for instruction in instructions.iter_mut() {
                    match instruction {
                        Instruction::Mov(op1, op2)
                        | Instruction::Binary(.., op1, op2)
                        | Instruction::Cmp(op1, op2) => {
                            replace_pseudo_with_stack(op1, stack_size);
                            replace_pseudo_with_stack(op2, stack_size);
                        }
                        Instruction::Unary(.., operand)
                        | Instruction::Idiv(operand)
                        | Instruction::SetCC(.., operand)
                        | Instruction::Push(operand) => {
                            replace_pseudo_with_stack(operand, stack_size);
                        }
                        Instruction::Ret
                        | Instruction::AllocateStack(_)
                        | Instruction::DeallocateStack(_)
                        | Instruction::Call(_)
                        | Instruction::Cdq
                        | Instruction::Jmp(_)
                        | Instruction::JmpCC(_, _)
                        | Instruction::Label(_) => {}
                    }
                }
            }
            TopLevel::StaticVariable { .. } => continue,
        }
    }
}

fn fix_invalid_instruction(instruction: Instruction) -> Vec<Instruction> {
    // use R10 to fix the first operand
    // use R11 to fix the second operand
    match instruction {
        // many instructions can not have both operands be memory addresses
        Instruction::Mov(op1 @ Operand::Stack { .. }, op2 @ Operand::Stack { .. }) => vec![
            Instruction::Mov(op1, Operand::Register(Register::R10)),
            Instruction::Mov(Operand::Register(Register::R10), op2),
        ],
        Instruction::Cmp(op1 @ Operand::Stack { .. }, op2 @ Operand::Stack { .. }) => vec![
            Instruction::Mov(op1.clone(), Operand::Register(Register::R10)),
            Instruction::Cmp(Operand::Register(Register::R10), op2),
        ],
        Instruction::Binary(
            binary_operator @ (BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::BitAnd
            | BinaryOperator::BitXOr
            | BinaryOperator::BitOr),
            op1 @ Operand::Stack { .. },
            op2 @ Operand::Stack { .. },
        ) => vec![
            Instruction::Mov(op1, Operand::Register(Register::R10)),
            Instruction::Binary(binary_operator, Operand::Register(Register::R10), op2),
        ],

        // multiply can not have a memory address as its destination, regardless of its source operand
        Instruction::Binary(BinaryOperator::Mult, op1, op2 @ Operand::Stack { .. }) => vec![
            Instruction::Mov(op2.clone(), Operand::Register(Register::R11)),
            Instruction::Binary(BinaryOperator::Mult, op1, Operand::Register(Register::R11)),
            Instruction::Mov(Operand::Register(Register::R11), op2),
        ],
        // compare instructions cannot have an immediate as the second operand
        Instruction::Cmp(op1, op2 @ Operand::Immediate(_)) => vec![
            Instruction::Mov(op2, Operand::Register(Register::R11)),
            Instruction::Cmp(op1, Operand::Register(Register::R11)),
        ],
        // On x86 the shift instructions can only take their count
        // from an 8-bit immediate or from the CL register (the low byte of CX)
        Instruction::Binary(
            binary_operator @ (BinaryOperator::LShift | BinaryOperator::RShift),
            op1,
            op2,
        ) if !matches!(op1, Operand::Immediate(_) | Operand::Register(Register::CX)) => vec![
            Instruction::Mov(op1, Operand::Register(Register::CX)),
            Instruction::Binary(binary_operator, Operand::Register(Register::CX), op2),
        ],
        // cannot divide by an immediate
        Instruction::Idiv(op1 @ Operand::Immediate { .. }) => vec![
            Instruction::Mov(op1, Operand::Register(Register::R10)),
            Instruction::Idiv(Operand::Register(Register::R10)),
        ],
        other => vec![other],
    }
}

fn fix_up_instructions(instructions: Vec<Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(fix_invalid_instruction)
        .collect()
}

pub fn fix_up_invalid_instructions(assembly_ast: &mut AssemblyAbstractSyntaxTree) {
    let AssemblyAbstractSyntaxTree::Program(Program::Program(function_definitions)) = assembly_ast;

    for function in function_definitions {
        let FunctionDefinition::Function {
            instructions,
            stack_size,
            ..
        } = function;
        if *stack_size > 0 {
            instructions.insert(0, Instruction::AllocateStack((*stack_size + 15) & !15))
        }
        let old = std::mem::take(instructions);
        *instructions = fix_up_instructions(old)
    }
}

pub fn run_assembly_generator(
    tacky_ast: tacky::TackyAbstractSyntaxTree,
) -> anyhow::Result<AssemblyAbstractSyntaxTree> {
    let assembly_ast = convert_ast(tacky_ast);
    Ok(assembly_ast)
}
