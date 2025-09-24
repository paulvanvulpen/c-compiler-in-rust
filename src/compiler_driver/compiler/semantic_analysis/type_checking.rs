use super::parser;
use crate::compiler_driver::compiler::symbol_table::{IdentifierAttributes, Symbol, SymbolState};
use anyhow::{Context, bail};
use std::collections::hash_map::{Entry, HashMap};

fn type_check_variable_declaration(
    variable_declaration: &parser::VariableDeclaration,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    let parser::VariableDeclaration { identifier, init } = variable_declaration;
    symbol_table.insert(
        identifier.clone(),
        SymbolState {
            symbol_type: Symbol::Int,
            is_defined: true,
        },
    );

    if let Some(init) = init {
        type_check_expression(init, symbol_table)
            .context("type checking a variable declaration")?;
    }

    Ok(())
}

fn type_check_function_declaration(
    function_declaration: &parser::FunctionDeclaration,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    let parser::FunctionDeclaration {
        identifier,
        parameters,
        body,
        storage_class,
    } = function_declaration;

    let this_declaration_symbol_type = Symbol::FuncType {
        param_count: parameters.len(),
    };

    let is_this_declaration_global = !matches!(storage_class, Some(parser::StorageClass::Static));

    match symbol_table.entry(identifier.clone()) {
        Entry::Occupied(mut entry) => {
            let old = entry.get();

            if old.symbol_type != this_declaration_symbol_type {
                bail!(
                    "Incompatible function declaration {:?} and {:?} for {}",
                    old.symbol_type,
                    this_declaration_symbol_type,
                    identifier
                )
            }

            if let IdentifierAttributes::FuncAttribute {
                is_defined: old_function_declaration_is_defined,
                is_global: old_function_declaration_is_global,
            } = old.identifier_attributes
            {
                if old_function_declaration_is_defined && body.is_some() {
                    bail!(
                        "Function with name {} is defined more than once",
                        identifier
                    )
                }

                if old_function_declaration_is_global
                    && matches!(storage_class, Some(parser::StorageClass::Static))
                {
                    bail!("Static function declaration follows non-static")
                }

                entry.insert(SymbolState {
                    symbol_type: this_declaration_symbol_type,
                    identifier_attributes: IdentifierAttributes::FuncAttribute {
                        is_defined: old_function_declaration_is_defined || body.is_some(),
                        is_global: old_function_declaration_is_global,
                    },
                });
            }
        }
        Entry::Vacant(e) => {
            e.insert(SymbolState {
                symbol_type: this_declaration_symbol_type,
                identifier_attributes: IdentifierAttributes::FuncAttribute {
                    is_defined: body.is_some(),
                    is_global: is_this_declaration_global,
                },
            });
        }
    }

    if body.is_some() {
        for p in parameters {
            symbol_table.insert(
                p.clone(),
                SymbolState {
                    symbol_type: Symbol::Int,
                    identifier_attributes: IdentifierAttributes::LocalAttribute,
                },
            );
        }
        type_check_block(body.as_ref().unwrap(), symbol_table)
            .context("type checking function declaration")?;
    }

    Ok(())
}

fn type_check_block(
    block: &parser::Block,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    let parser::Block::Block(block) = block;
    for block_item in block {
        type_check_block_item(block_item, symbol_table).context("type checking a block")?;
    }
    Ok(())
}

fn type_check_block_item(
    block_item: &parser::BlockItem,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    match block_item {
        parser::BlockItem::Statement(statement) => {
            type_check_statement(statement, symbol_table).context("type checking a block item")?
        }
        parser::BlockItem::Declaration(declaration) => {
            type_check_declaration(declaration, symbol_table)
                .context("type checking a block item")?
        }
    }
    Ok(())
}

fn type_check_statement(
    statement: &parser::Statement,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    match statement {
        parser::Statement::Return(expression) | parser::Statement::Expression(expression) => {
            type_check_expression(expression, symbol_table)
        }
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => {
            type_check_expression(condition, symbol_table).context("type checking a statement")?;
            type_check_statement(then_statement, symbol_table)
                .context("type checking a statement")?;
            if let Some(else_statement) = optional_else_statement {
                type_check_statement(else_statement, symbol_table)
                    .context("type checking a statement")?;
            }
            Ok(())
        }
        parser::Statement::Label(_, statement) => {
            type_check_statement(statement, symbol_table).context("type checking a statement")
        }
        parser::Statement::Compound(block) => {
            type_check_block(block, symbol_table).context("type checking a block")
        }
        parser::Statement::While {
            condition, body, ..
        }
        | parser::Statement::DoWhile {
            body, condition, ..
        } => {
            type_check_expression(condition, symbol_table).context("type checking a statement")?;
            type_check_statement(body, symbol_table).context("type checking a statement")
        }
        parser::Statement::For {
            init,
            condition,
            post,
            body,
            ..
        } => {
            type_check_for_init(init, symbol_table).context("type checking a statement")?;
            if let Some(condition) = condition {
                type_check_expression(condition, symbol_table)
                    .context("type checking a statement")?;
            }
            if let Some(post) = post {
                type_check_expression(post, symbol_table).context("type checking a statement")?;
            }
            type_check_statement(body, symbol_table).context("type checking a statement")
        }
        parser::Statement::Switch {
            condition, body, ..
        } => {
            type_check_expression(condition, symbol_table).context("type checking a statement")?;
            type_check_statement(body, symbol_table).context("type checking a statement")?;
            Ok(())
        }
        parser::Statement::Case {
            follow_statement, ..
        }
        | parser::Statement::Default {
            follow_statement, ..
        } => type_check_statement(follow_statement, symbol_table)
            .context("type checking a statement"),
        parser::Statement::Goto(..)
        | parser::Statement::Break { .. }
        | parser::Statement::Continue { .. }
        | parser::Statement::Null => Ok(()),
    }
}

fn type_check_for_init(
    for_init: &parser::ForInit,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    match for_init {
        parser::ForInit::InitialDeclaration(variable_declaration) => {
            type_check_variable_declaration(variable_declaration, symbol_table)
        }
        parser::ForInit::InitialOptionalExpression(optional_expression) => {
            if let Some(expression) = optional_expression {
                type_check_expression(expression, symbol_table)?
            }
            Ok(())
        }
    }
}

fn type_check_declaration(
    declaration: &parser::Declaration,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    match declaration {
        parser::Declaration::VariableDeclaration(variable_declaration) => {
            type_check_variable_declaration(variable_declaration, symbol_table)
                .context("type checking a declaration")?
        }
        parser::Declaration::FunctionDeclaration(function_declaration) => {
            type_check_function_declaration(function_declaration, symbol_table)
                .context("type checking a declaration")?
        }
    }

    Ok(())
}

fn type_check_expression(
    expression: &parser::Expression,
    symbol_table: &mut HashMap<String, SymbolState>,
) -> anyhow::Result<()> {
    match expression {
        parser::Expression::Var { identifier } => {
            if matches!(
                symbol_table[identifier].symbol_type,
                Symbol::FuncType { .. }
            ) {
                bail!("Function name used as a variable!");
            }
            Ok(())
        }
        parser::Expression::FunctionCall {
            identifier,
            arguments,
        } => {
            if matches!(symbol_table[identifier].symbol_type, Symbol::Int) {
                bail!("Variable name used as a function!");
            }
            if !matches!(
                symbol_table[identifier].symbol_type,
                Symbol::FuncType {
                    param_count
                } if param_count == arguments.len()
            ) {
                bail!("Function called with the wrong number of arguments");
            }
            for argument in arguments {
                type_check_expression(argument, symbol_table)
                    .context("type checking an expression")?
            }
            Ok(())
        }
        parser::Expression::Unary(_, expression) => type_check_expression(expression, symbol_table),
        parser::Expression::BinaryOperation {
            left_operand,
            right_operand,
            ..
        }
        | parser::Expression::Assignment(left_operand, right_operand) => {
            type_check_expression(left_operand, symbol_table)?;
            type_check_expression(right_operand, symbol_table)
        }
        parser::Expression::Conditional(left_operand, middle_operand, right_operand) => {
            type_check_expression(left_operand, symbol_table)?;
            type_check_expression(middle_operand, symbol_table)?;
            type_check_expression(right_operand, symbol_table)
        }
        parser::Expression::Constant(_) => Ok(()),
    }
}

pub fn analyse(
    function_declarations: &Vec<parser::FunctionDeclaration>,
) -> HashMap<String, SymbolState> {
    let mut symbol_table: HashMap<String, SymbolState> = HashMap::new();

    for function_declaration in function_declarations {
        type_check_function_declaration(function_declaration, &mut symbol_table)
            .context("type checking a function")
            .unwrap()
    }

    symbol_table
}
