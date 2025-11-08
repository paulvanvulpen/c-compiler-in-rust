mod identifier_resolution;
mod label_resolution;
mod loop_labeling;
mod type_checking;

use super::generator;
use super::parser;
use super::symbol_table::SymbolState;
use anyhow::Context;
use std::collections::HashMap;

pub fn run_semantic_analysis(
    parser_ast: parser::AbstractSyntaxTree,
) -> anyhow::Result<(parser::AbstractSyntaxTree, HashMap<String, SymbolState>)> {
    let parser::AbstractSyntaxTree::Program(parser::Program::Program(mut declarations)) =
        parser_ast;

    declarations = identifier_resolution::analyse(declarations);
    let symbol_table = type_checking::analyse(&declarations);

    for declaration in declarations.iter_mut() {
        if let parser::Declaration::FunctionDeclaration(function) = declaration {
            match &mut function.body {
                Some(function_body) => {
                    let function_body = std::mem::take(function_body);
                    let function_body =
                        label_resolution::analyse(&function.identifier, function_body)
                            .with_context(|| {
                                format!(
                                    "running semantic analysis in function: {}",
                                    &function.identifier
                                )
                            })?;
                    function.body = Some(function_body);
                }
                None => {
                    continue;
                }
            }
        }
    }

    for declaration in declarations.iter_mut() {
        if let parser::Declaration::FunctionDeclaration(function) = declaration {
            match &mut function.body {
                Some(function_body) => {
                    let function_body = std::mem::take(function_body);
                    let function_body = loop_labeling::analyse(&function.identifier, function_body)
                        .with_context(|| {
                            format!(
                                "running semantic analysis in function: {}",
                                &function.identifier
                            )
                        })?;
                    function.body = Some(function_body);
                }
                None => {
                    continue;
                }
            }
        }
    }

    Ok((
        parser::AbstractSyntaxTree::Program(parser::Program::Program(declarations)),
        symbol_table,
    ))
}
