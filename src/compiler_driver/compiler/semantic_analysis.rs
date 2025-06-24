mod label_resolution;
mod variable_resolution;

use super::generator;
use super::parser;
use anyhow::Context;

pub fn run_semantic_analysis(
    parser_ast: parser::AbstractSyntaxTree,
) -> anyhow::Result<parser::AbstractSyntaxTree> {
    let parser::AbstractSyntaxTree::Program(parser::Program::Program(
        parser::FunctionDefinition::Function {
            identifier,
            body: function_body,
        },
    )) = parser_ast;

    let parser::Block::Block(function_body) = function_body;
    let function_body = variable_resolution::analyse(&identifier, function_body)
        .with_context(|| format!("running semantic analysis in function: {}", identifier))?;
    let function_body = label_resolution::analyse(function_body)
        .with_context(|| format!("running semantic analysis in function: {}", identifier))?;

    Ok(parser::AbstractSyntaxTree::Program(
        parser::Program::Program(parser::FunctionDefinition::Function {
            body: parser::Block::Block(function_body),
            identifier,
        }),
    ))
}
