use std::collections::HashMap;

use anyhow::{Context, anyhow};

use super::generator;
use super::parser;

fn make_temporary_from_identifier(name: &str) -> String {
    let id = generator::generate_unique_id();
    format!("{name}.{id}")
}

fn resolve_declaration(
    declaration: parser::Declaration,
    variable_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::Declaration> {
    let parser::Declaration::Declaration { identifier, init } = declaration;
    if variable_map.contains_key(&identifier) {
        return Err(anyhow!("Duplicate variable declaration!"));
    }
    let unique_name = make_temporary_from_identifier(&identifier);
    variable_map.insert(identifier.clone(), unique_name);

    let mut updated_initialiser: Option<parser::Expression> = None;
    if let Some(init) = init {
        updated_initialiser = Some(resolve_expression(init, variable_map)?);
    }

    Ok(parser::Declaration::Declaration {
        identifier,
        init: updated_initialiser,
    })
}

fn resolve_statement(
    statement: parser::Statement,
    variable_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Return(expression) => Ok(parser::Statement::Return(resolve_expression(
            expression,
            variable_map,
        )?)),
        parser::Statement::Expression(expression) => Ok(parser::Statement::Expression(
            resolve_expression(expression, variable_map)?,
        )),
        parser::Statement::Null => Ok(statement),
    }
}

fn resolve_expression(
    expression: parser::Expression,
    variable_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::Expression> {
    match expression {
        parser::Expression::Assignment(left, right) => {
            if !matches!(*left, parser::Expression::Var { .. }) {
                return Err(anyhow!("invalid lvalue!"));
            }
            Ok(parser::Expression::Assignment(
                Box::new(resolve_expression(*left, variable_map)?),
                Box::new(resolve_expression(*right, variable_map)?),
            ))
        }
        parser::Expression::Var { identifier } => {
            if variable_map.contains_key(&identifier) {
                Ok(parser::Expression::Var {
                    identifier: variable_map[&identifier].clone(),
                })
            } else {
                Err(anyhow!("undeclared identifier"))
            }
        }
        parser::Expression::Constant(..) => Ok(expression),
        parser::Expression::BinaryOperation {
            binary_operator,
            left_operand,
            right_operand,
        } => Ok(parser::Expression::BinaryOperation {
            binary_operator,
            left_operand: Box::new(resolve_expression(*left_operand, variable_map)?),
            right_operand: Box::new(resolve_expression(*right_operand, variable_map)?),
        }),
        parser::Expression::Unary(unary_operator, expression) => Ok(parser::Expression::Unary(
            unary_operator,
            Box::new(resolve_expression(*expression, variable_map)?),
        )),
    }
}

fn analyse_block_item(
    block_item: parser::BlockItem,
    variable_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::BlockItem> {
    match block_item {
        parser::BlockItem::Statement(statement) => {
            let resolved_statement = resolve_statement(statement, variable_map)
                .context("analysed statement block item")?;
            Ok(parser::BlockItem::Statement(resolved_statement))
        }
        parser::BlockItem::Declaration(declaration) => {
            let resolved_declaration = resolve_declaration(declaration, variable_map)
                .context("analysed declaration block item")?;
            Ok(parser::BlockItem::Declaration(resolved_declaration))
        }
    }
}

pub fn run_semantic_analysis(
    parser_ast: parser::AbstractSyntaxTree,
) -> anyhow::Result<parser::AbstractSyntaxTree> {
    let parser::AbstractSyntaxTree::Program(parser::Program::Program(
        parser::FunctionDefinition::Function {
            identifier,
            mut body,
        },
    )) = parser_ast;

    let mut variable_map: HashMap<String, String> = HashMap::new();

    for block_item in &mut body {
        let original = std::mem::take(block_item);
        *block_item = analyse_block_item(original, &mut variable_map)?;
    }

    Ok(parser::AbstractSyntaxTree::Program(
        parser::Program::Program(parser::FunctionDefinition::Function { body, identifier }),
    ))
}
