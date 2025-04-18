use std::io::Write;
use super::assembly_generator;

fn write_assembler_ast(assembler_ast : assembly_generator::AssemblyAbstractSyntaxTree) -> String {
	match assembler_ast {
		assembly_generator::AssemblyAbstractSyntaxTree::Program(program) => String::from(write_program(program))
	}
}
fn write_program(program: assembly_generator::Program) -> String {
	match program {
		assembly_generator::Program::Program(function_definition) => {
			String::from(format!("{}\n\
			.section .note.GNU-stack,\"\",@progbits\n", write_function(function_definition)))
		}
	}
}
fn write_function(function_definition : assembly_generator::FunctionDefinition) -> String {
	let prefix = "    ";
	match function_definition {
		assembly_generator::FunctionDefinition::Function { identifier, instructions } => {
			let instructions_str = instructions.into_iter()
				.map(|instruction| write_instruction(instruction))
				.collect::<Vec<String>>()
				.join(format!("{prefix}").as_str());
			String::from(format!(
				"{prefix}.globl {identifier}\n\
				{identifier}:\n\
				{prefix}{}", instructions_str))
		}
	}
}
fn write_instruction(instruction: assembly_generator::Instruction) -> String {
	match instruction {
		assembly_generator::Instruction::Mov(src, dst) => String::from(format!("movl\t {}, {}\n", write_operand(src), write_operand(dst))),
		assembly_generator::Instruction::Ret => String::from("ret\n")
	}
}

fn write_operand(operand : assembly_generator::Operand) -> String {
	match operand {
		assembly_generator::Operand::Register(register) => write_register(register),
		assembly_generator::Operand::Immediate(int) => String::from(format!("${int}")),
	}
}

fn write_register(register :assembly_generator::Register) -> String {
	match register {
		assembly_generator::Register::EAX => String::from("%eax"),
	}
}

pub fn run_code_emission(assembly_ast : assembly_generator::AssemblyAbstractSyntaxTree, input_file_path : &std::path::Path) -> std::io::Result<()>
{
	let mut assembly_file = std::fs::File::create(input_file_path.with_extension("s"))?;
	assembly_file.write(write_assembler_ast(assembly_ast).as_bytes())?;
	Ok(())
}
