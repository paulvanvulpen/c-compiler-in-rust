use super::tacky;
use std::collections::HashMap;

mod visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, instruction* instructions)
// instruction = Mov(operand src, operand dst)
//				| Unary(unary_operator, operand)
//				| AllocateStack(int)
//				| Ret
// unary_operator = Neg | Not
// operand = Imm(int) | Reg(reg) | Pseudo(identifier) | Stack(int)
// reg = AX | R10
pub enum AssemblyAbstractSyntaxTree {
    Program(Program),
}

// <program>
pub enum Program {
    Program(FunctionDefinition),
}

// <function>
pub enum FunctionDefinition {
    Function {
        identifier: String,
        instructions: Vec<Instruction>,
    },
}

pub enum Instruction {
    Mov(Operand, Operand),
    Unary(UnaryOperator, Operand),
    AllocateStack(usize),
    Ret,
}

pub enum UnaryOperator {
    Neg,
    Not,
}

pub enum Operand {
    Immediate(usize),
    Register(Register),
    Pseudo { identifier: String },
    Stack { offset: isize },
}

pub enum Register {
    AX,
    R10,
}

fn convert_val(val: tacky::Val) -> Operand {
    match val {
        tacky::Val::Constant(value) => Operand::Immediate(value),
        tacky::Val::Var(identifier) => Operand::Pseudo { identifier },
    }
}

fn convert_unary_operator(unary_operator: tacky::UnaryOperator) -> UnaryOperator {
    match unary_operator {
        tacky::UnaryOperator::Complement => UnaryOperator::Not,
        tacky::UnaryOperator::Negate => UnaryOperator::Neg,
    }
}

fn convert_instruction(instruction: tacky::Instruction) -> Vec<Instruction> {
    match instruction {
        tacky::Instruction::Return(val) => {
            vec![
                Instruction::Mov(convert_val(val), Operand::Register(Register::AX)),
                Instruction::Ret,
            ]
        }
        tacky::Instruction::Unary {
            unary_operator,
            src,
            dst,
        } => {
            vec![
                Instruction::Mov(convert_val(src), convert_val(dst.clone())),
                Instruction::Unary(convert_unary_operator(unary_operator), convert_val(dst)),
            ]
        }
    }
}

fn convert_function_definition(
    function_definition: tacky::FunctionDefinition,
) -> FunctionDefinition {
    match function_definition {
        tacky::FunctionDefinition::Function {
            identifier,
            instructions,
        } => FunctionDefinition::Function {
            identifier,
            instructions: {
                instructions
                    .into_iter()
                    .flat_map(convert_instruction)
                    .collect()
            },
        },
    }
}

fn convert_program(program: tacky::Program) -> Program {
    match program {
        tacky::Program::Program(function_definition) => {
            Program::Program(convert_function_definition(function_definition))
        }
    }
}

fn convert_ast(ast: tacky::TackyAbstractSyntaxTree) -> AssemblyAbstractSyntaxTree {
    match ast {
        tacky::TackyAbstractSyntaxTree::Program(program) => {
            AssemblyAbstractSyntaxTree::Program(convert_program(program))
        }
    }
}

pub fn run_assembly_generator(
    tacky_ast: tacky::TackyAbstractSyntaxTree,
) -> std::io::Result<AssemblyAbstractSyntaxTree> {
    let assembly_ast = convert_ast(tacky_ast);
    Ok(assembly_ast)
}

pub fn replace_pseudo_registers(assembly_ast: &mut AssemblyAbstractSyntaxTree) -> usize {
    let mut temporary_to_offset: HashMap<String, isize> = HashMap::new();
    let mut alloc_size: usize = 0;

    let AssemblyAbstractSyntaxTree::Program(Program::Program(FunctionDefinition::Function {
        instructions,
        ..
    })) = assembly_ast;

    let mut replace_pseudo_with_stack = |operand: &mut Operand| {
        if let Operand::Pseudo { identifier } = operand {
            let id = std::mem::take(identifier);
            let offset: isize = *temporary_to_offset.entry(id).or_insert_with(|| {
                alloc_size += 4;
                -(alloc_size as isize)
            });
            *operand = Operand::Stack { offset };
        }
    };

    for instruction in instructions.iter_mut() {
        match instruction {
            Instruction::Unary(.., operand) => {
                replace_pseudo_with_stack(operand);
            }
            Instruction::Mov(op1, op2) => {
                replace_pseudo_with_stack(op1);
                replace_pseudo_with_stack(op2);
            }
            Instruction::Ret | Instruction::AllocateStack(_) => {}
        }
    }
    alloc_size
}

fn split_mem_to_mem_instruction(instruction: Instruction) -> Vec<Instruction> {
    match instruction {
        Instruction::Mov(op1 @ Operand::Stack { .. }, op2 @ Operand::Stack { .. }) => vec![
            Instruction::Mov(op1, Operand::Register(Register::R10)),
            Instruction::Mov(Operand::Register(Register::R10), op2),
        ],
        other => vec![other],
    }
}

fn fix_up_unpack_mem_to_mem_mov_instruction(instructions: Vec<Instruction>) -> Vec<Instruction> {
    instructions
        .into_iter()
        .flat_map(split_mem_to_mem_instruction)
        .collect()
}

pub fn fix_up_invalid_instructions(
    assembly_ast: &mut AssemblyAbstractSyntaxTree,
    allocate_stack_size: usize,
) {
    let AssemblyAbstractSyntaxTree::Program(Program::Program(FunctionDefinition::Function {
        instructions,
        ..
    })) = assembly_ast;

    if allocate_stack_size > 0 {
        instructions.insert(0, Instruction::AllocateStack(allocate_stack_size));
    }

    let old = std::mem::take(instructions);
    *instructions = fix_up_unpack_mem_to_mem_mov_instruction(old);
}
