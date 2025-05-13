use super::assembly_generator;
use std::io::Write;

fn write_assembler_ast(assembler_ast: assembly_generator::AssemblyAbstractSyntaxTree) -> String {
    match assembler_ast {
        assembly_generator::AssemblyAbstractSyntaxTree::Program(program) => {
            String::from(write_program(program))
        }
    }
}
fn write_program(program: assembly_generator::Program) -> String {
    match program {
        assembly_generator::Program::Program(function_definition) => String::from(format!(
            "{}\n\
			.section .note.GNU-stack,\"\",@progbits\n",
            write_function(function_definition)
        )),
    }
}
fn write_function(function_definition: assembly_generator::FunctionDefinition) -> String {
    let prefix = "    ";
    match function_definition {
        assembly_generator::FunctionDefinition::Function {
            identifier,
            instructions,
        } => {
            let instructions_str = instructions
                .into_iter()
                .map(|instruction| write_instruction(instruction))
                .collect::<Vec<String>>()
                .join(format!("{prefix}").as_str());
            String::from(format!(
                "{prefix}.globl {identifier}\n\
                {identifier}:\n\
                {prefix}pushq	%rbp\n\
                {prefix}movq	%rsp, %rbp\n\
                {prefix}{}",
                instructions_str
            ))
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

fn write_instruction(instruction: assembly_generator::Instruction) -> String {
    let prefix = "    ";
    match instruction {
        assembly_generator::Instruction::Mov(src, dst) => {
            format!("movl\t{}, {}\n", write_operand(src), write_operand(dst))
        }
        assembly_generator::Instruction::Unary(unary_operator, operand) => {
            format!(
                "{}	{}\n",
                write_unary_operator(unary_operator),
                write_operand(operand)
            )
        }
        assembly_generator::Instruction::Binary(binary_operator, source, destination) => {
            if matches!(
                (&binary_operator, &source),
                (
                    assembly_generator::BinaryOperator::LShift
                        | assembly_generator::BinaryOperator::RShift,
                    assembly_generator::Operand::Register(assembly_generator::Register::CX)
                )
            ) {
                format!(
                    "{}\t%cl, {}\n",
                    write_binary_operator(binary_operator),
                    write_operand(destination)
                )
            } else {
                format!(
                    "{}\t{}, {}\n",
                    write_binary_operator(binary_operator),
                    write_operand(source),
                    write_operand(destination)
                )
            }
        }
        assembly_generator::Instruction::Idiv(operand) => {
            format!("idivl\t{}\n", write_operand(operand))
        }
        assembly_generator::Instruction::Cdq => String::from("cdq\n"),
        assembly_generator::Instruction::AllocateStack(size) => format!("subq\t${size}, %rsp\n"),
        assembly_generator::Instruction::Ret => format!(
            "movq\t%rbp, %rsp\n\
            {prefix}popq\t%rbp\n\
            {prefix}ret\n"
        ),
        _ => todo!(),
    }
}

fn write_operand(operand: assembly_generator::Operand) -> String {
    match operand {
        assembly_generator::Operand::Register(register) => write_register(register),
        assembly_generator::Operand::Immediate(int) => format!("${int}"),
        assembly_generator::Operand::Pseudo { .. } => {
            panic!("should have been cleaned up in the assembly_generator")
        }
        assembly_generator::Operand::Stack { offset } => format!("{offset}(%rbp)"),
    }
}

fn write_register(register: assembly_generator::Register) -> String {
    match register {
        assembly_generator::Register::AX => String::from("%eax"),
        assembly_generator::Register::CX => String::from("%ecx"),
        assembly_generator::Register::DX => String::from("%edx"),
        assembly_generator::Register::R10 => String::from("%r10d"),
        assembly_generator::Register::R11 => String::from("%r11d"),
    }
}

pub fn run_code_emission(
    assembly_ast: assembly_generator::AssemblyAbstractSyntaxTree,
    input_file_path: &std::path::Path,
) -> anyhow::Result<()> {
    let mut assembly_file = std::fs::File::create(input_file_path.with_extension("s"))?;
    assembly_file.write(write_assembler_ast(assembly_ast).as_bytes())?;
    Ok(())
}
