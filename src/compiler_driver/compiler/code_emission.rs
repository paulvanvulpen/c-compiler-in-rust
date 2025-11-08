use super::assembly_generator;
use super::symbol_table::{IdentifierAttributes, SymbolState};
use std::collections::HashMap;
use std::io::Write;

fn write_assembler_ast(
    assembler_ast: assembly_generator::AssemblyAbstractSyntaxTree,
    symbol_table: &HashMap<String, SymbolState>,
) -> String {
    match assembler_ast {
        assembly_generator::AssemblyAbstractSyntaxTree::Program(program) => {
            String::from(write_program(program, symbol_table))
        }
    }
}
fn write_program(
    program: assembly_generator::Program,
    symbol_table: &HashMap<String, SymbolState>,
) -> String {
    match program {
        assembly_generator::Program::Program(function_definitions) => format!(
            "{}\n\
            .section .note.GNU-stack,\"\",@progbits\n",
            function_definitions
                .into_iter()
                .map(|f| write_function(f, symbol_table))
                .collect::<Vec<String>>()
                .join("\n")
        ),
    }
}

fn write_function(
    toplevel_definition: assembly_generator::TopLevel,
    symbol_table: &HashMap<String, SymbolState>,
) -> String {
    let prefix = "    ";
    match toplevel_definition {
        assembly_generator::TopLevel::Function {
            identifier,
            is_globally_visible,
            instructions,
            ..
        } => {
            let instructions_str = instructions
                .into_iter()
                .map(|instruction| write_instruction(instruction, symbol_table))
                .collect::<Vec<String>>()
                .join(format!("{prefix}").as_str());
            let global_directive = if is_globally_visible {
                format!("{prefix}.globl {identifier}\n")
            } else {
                String::new()
            };
            format!(
                "{global_directive}{prefix}.text\n\
                {identifier}:\n\
                {prefix}pushq	%rbp\n\
                {prefix}movq	%rsp, %rbp\n\
                {prefix}{instructions_str}"
            )
        }
        assembly_generator::TopLevel::StaticVariable {
            identifier,
            is_globally_visible,
            init,
        } => {
            let global_directive = if is_globally_visible {
                format!("{prefix}.globl {identifier}\n")
            } else {
                String::new()
            };
            let (section_directive, section_instruction) = if init != 0 {
                (String::from(".data"), format!(".long {init}"))
            } else {
                (String::from(".bss"), String::from(".zero 4"))
            };
            format!(
                "{global_directive}{prefix}{section_directive}\n\
                {prefix}.balign 4\n\
                {identifier}:\n\
                {prefix}{section_instruction}"
            )
        }
    }
}

fn write_unary_operator(unary_operator: assembly_generator::UnaryOperator) -> String {
    match unary_operator {
        assembly_generator::UnaryOperator::Neg => String::from("negl"),
        assembly_generator::UnaryOperator::Not => String::from("notl"),
    }
}

fn write_binary_operator(binary_operator: assembly_generator::BinaryOperator) -> String {
    match binary_operator {
        assembly_generator::BinaryOperator::Add => String::from("addl"),
        assembly_generator::BinaryOperator::Sub => String::from("subl"),
        assembly_generator::BinaryOperator::Mult => String::from("imull"),

        //https://c9x.me/x86/html/file_module_x86_id_285.html
        // in current version assuming all values are signed 32-bit values. (using SAR, not SHR)
        assembly_generator::BinaryOperator::LShift => String::from("sall"),
        assembly_generator::BinaryOperator::RShift => String::from("sarl"),

        assembly_generator::BinaryOperator::BitAnd => String::from("andl"),
        assembly_generator::BinaryOperator::BitXOr => String::from("xorl"),
        assembly_generator::BinaryOperator::BitOr => String::from("orl "),
    }
}

fn write_instruction(
    instruction: assembly_generator::Instruction,
    symbol_table: &HashMap<String, SymbolState>,
) -> String {
    let prefix = "    ";
    match instruction {
        assembly_generator::Instruction::Mov(src, dst) => {
            format!(
                "movl\t{}, {}\n",
                write_operand(src, 4),
                write_operand(dst, 4)
            )
        }
        assembly_generator::Instruction::Unary(unary_operator, operand) => {
            format!(
                "{}	{}\n",
                write_unary_operator(unary_operator),
                write_operand(operand, 4)
            )
        }
        assembly_generator::Instruction::Binary(binary_operator, source, destination) => {
            // shifts have either been fixed to use the cl register, or an immediate.
            // in its second form it can be parsed like any other binary operator at this point.
            if matches!(
                (&binary_operator, &source),
                (
                    assembly_generator::BinaryOperator::LShift
                        | assembly_generator::BinaryOperator::RShift,
                    assembly_generator::Operand::Register(assembly_generator::Register::CX)
                )
            ) {
                format!(
                    "{}\t{}, {}\n",
                    write_binary_operator(binary_operator),
                    write_operand(source, 1),
                    write_operand(destination, 4)
                )
            } else {
                format!(
                    "{}\t{}, {}\n",
                    write_binary_operator(binary_operator),
                    write_operand(source, 4),
                    write_operand(destination, 4)
                )
            }
        }
        assembly_generator::Instruction::Idiv(operand) => {
            format!("idivl\t{}\n", write_operand(operand, 4))
        }
        assembly_generator::Instruction::Cdq => String::from("cdq\n"),
        assembly_generator::Instruction::AllocateStack(size) => format!("subq\t${size}, %rsp\n"),
        assembly_generator::Instruction::DeallocateStack(size) => format!("addq\t${size}, %rsp\n"),
        assembly_generator::Instruction::Push(operand) => {
            format!("pushq\t{}\n", write_operand(operand, 8))
        }
        assembly_generator::Instruction::Call(identifier) => {
            let SymbolState {
                identifier_attributes,
                ..
            } = &symbol_table[&identifier];
            if let IdentifierAttributes::FuncAttribute { is_defined, .. } = identifier_attributes {
                return format!(
                    "call\t{identifier}{}\n",
                    if !is_defined { "@PLT" } else { "" },
                );
            }
            panic!("Trying to a call an identifier that isn't a function")
        }
        assembly_generator::Instruction::Ret => format!(
            "movq\t%rbp, %rsp\n\
            {prefix}popq\t%rbp\n\
            {prefix}ret\n"
        ),
        assembly_generator::Instruction::Cmp(op1, op2) => {
            format!(
                "cmpl\t{}, {}\n",
                write_operand(op1, 4),
                write_operand(op2, 4)
            )
        }
        assembly_generator::Instruction::Jmp(label) => {
            format!("jmp\t.L{label}\n")
        }
        assembly_generator::Instruction::JmpCC(condition_code, label) => {
            format!("j{}\t.L{label}\n", write_condition_code(condition_code))
        }
        assembly_generator::Instruction::SetCC(condition_code, operand) => {
            format!(
                "set{}\t{}\n",
                write_condition_code(condition_code),
                write_operand(operand, 1)
            )
        }
        assembly_generator::Instruction::Label(label) => {
            format!(".L{label}:\n")
        }
    }
}

fn write_operand(operand: assembly_generator::Operand, byte_count: u8) -> String {
    match operand {
        assembly_generator::Operand::Register(register) => write_register(register, byte_count),
        assembly_generator::Operand::Immediate(int) => format!("${int}"),
        assembly_generator::Operand::Pseudo { .. } => {
            panic!("should have been cleaned up in the assembly_generator")
        }
        assembly_generator::Operand::Stack { offset } => format!("{offset}(%rbp)"),
        assembly_generator::Operand::Data { identifier } => format!("{identifier}(%rip)"),
    }
}

fn write_register(register: assembly_generator::Register, byte_count: u8) -> String {
    match register {
        assembly_generator::Register::AX => match byte_count {
            1 => String::from("%al"),
            4 => String::from("%eax"),
            8 => String::from("%rax"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::CX => match byte_count {
            1 => String::from("%cl"),
            4 => String::from("%ecx"),
            8 => String::from("%rcx"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::DX => match byte_count {
            1 => String::from("%dl"),
            4 => String::from("%edx"),
            8 => String::from("%rdx"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::DI => match byte_count {
            1 => String::from("%dil"),
            4 => String::from("%edi"),
            8 => String::from("%rdi"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::SI => match byte_count {
            1 => String::from("%sil"),
            4 => String::from("%esi"),
            8 => String::from("%rsi"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::R8 => match byte_count {
            1 => String::from("%r8b"),
            4 => String::from("%r8d"),
            8 => String::from("%r8"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::R9 => match byte_count {
            1 => String::from("%r9b"),
            4 => String::from("%r9d"),
            8 => String::from("%r9"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::R10 => match byte_count {
            1 => String::from("%r10b"),
            4 => String::from("%r10d"),
            8 => String::from("%r10"),
            _ => panic!("unknown register"),
        },
        assembly_generator::Register::R11 => match byte_count {
            1 => String::from("%r11b"),
            4 => String::from("%r11d"),
            8 => String::from("%r11"),
            _ => panic!("unknown register"),
        },
    }
}

fn write_condition_code(condition_code: assembly_generator::ConditionCode) -> String {
    match condition_code {
        assembly_generator::ConditionCode::E => String::from("e"),
        assembly_generator::ConditionCode::NE => String::from("ne"),
        assembly_generator::ConditionCode::L => String::from("l"),
        assembly_generator::ConditionCode::LE => String::from("le"),
        assembly_generator::ConditionCode::GE => String::from("ge"),
        assembly_generator::ConditionCode::G => String::from("g"),
    }
}

pub fn run_code_emission(
    assembly_ast: assembly_generator::AssemblyAbstractSyntaxTree,
    symbol_table: &HashMap<String, SymbolState>,
    input_file_path: &std::path::Path,
) -> anyhow::Result<()> {
    let mut assembly_file = std::fs::File::create(input_file_path.with_extension("s"))?;
    assembly_file.write(write_assembler_ast(assembly_ast, symbol_table).as_bytes())?;
    Ok(())
}
