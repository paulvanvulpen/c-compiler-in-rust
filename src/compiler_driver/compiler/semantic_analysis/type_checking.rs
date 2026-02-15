use super::parser;
use crate::compiler_driver::compiler::symbol_table;
use crate::compiler_driver::compiler::symbol_table::{Constant, Type};
use anyhow::{Context, bail};
use std::collections::hash_map::{Entry, HashMap};

fn type_check_variable_declaration(
    variable_declaration: &parser::VariableDeclaration,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<()> {
    let parser::VariableDeclaration {
        identifier,
        init,
        variable_type,
        storage_class,
    } = variable_declaration;

    match storage_class {
        Some(storage_class) => match storage_class {
            parser::StorageClass::Extern => {
                if init.is_some() {
                    bail!("Initializer on local extern variable declaration")
                }
                if let Some(old) = symbol_table.get(identifier) {
                    if old.symbol_type != symbol_table::Type::Int {
                        bail!("Function redeclared as variable")
                    }
                } else {
                    symbol_table.insert(
                        identifier.clone(),
                        symbol_table::Symbol {
                            symbol_type: symbol_table::Type::Int,
                            identifier_attributes:
                                symbol_table::IdentifierAttributes::StaticStorageAttribute {
                                    init: symbol_table::InitialValue::NoInitializer,
                                    is_globally_visible: true,
                                },
                        },
                    );
                }
            }
            parser::StorageClass::Static => {
                let initial_value = match init {
                    Some(expression) => {
                        if let parser::Expression::Constant(constant) = expression {
                            symbol_table::InitialValue::Initial(constant.clone())
                        } else {
                            bail!("Non-constant initializer")
                        }
                    }
                    None => {
                        todo!("perhaps need variable type here rather than just default constant")
                    } //symbol_table::InitialValue::Initial(),
                };
                symbol_table.insert(
                    identifier.clone(),
                    symbol_table::Symbol {
                        symbol_type: symbol_table::Type::Int,
                        identifier_attributes:
                            symbol_table::IdentifierAttributes::StaticStorageAttribute {
                                init: initial_value,
                                is_globally_visible: false,
                            },
                    },
                );
            }
        },
        None => {
            symbol_table.insert(
                identifier.clone(),
                symbol_table::Symbol {
                    symbol_type: symbol_table::Type::Int,
                    identifier_attributes: symbol_table::IdentifierAttributes::LocalAttribute,
                },
            );
            if let Some(init) = init {
                type_check_expression(init, symbol_table)
                    .context("type checking a variable declaration")?
            }
        }
    }

    Ok(())
}

fn type_check_file_scope_variable_declaration(
    variable_declaration: parser::VariableDeclaration,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::VariableDeclaration> {
    let parser::VariableDeclaration {
        identifier,
        init,
        variable_type,
        storage_class,
    } = variable_declaration;

    let mut initial_value = match &init {
        Some(typed_expression) => {
            if let parser::Expression::Constant(constant) = &typed_expression.expression {
                let static_init = match (constant, &variable_type) {
                    (Constant::ConstInt(const_int), Type::Int) => {
                        symbol_table::StaticInit::IntInit(*const_int)
                    }
                    (Constant::ConstInt(const_int), Type::Long) => {
                        symbol_table::StaticInit::LongInit(*const_int as i64)
                    }
                    (Constant::ConstLong(const_long), Type::Int) => {
                        symbol_table::StaticInit::IntInit(*const_long as i32)
                    }
                    (Constant::ConstLong(const_long), Type::Long) => {
                        symbol_table::StaticInit::LongInit(*const_long)
                    }
                    (_, Type::Undefined) => unreachable!(
                        "Undefined variable found when type checking a file scope variable declaration"
                    ),
                    (_, Type::FuncType { .. }) => unreachable!(
                        "Function type found when type checking a file scope variable declaration"
                    ),
                };
                symbol_table::InitialValue::Initial(static_init)
            } else {
                bail!("Non-constant initializer")
            }
        }
        None => {
            if matches!(&storage_class, Some(parser::StorageClass::Extern)) {
                symbol_table::InitialValue::NoInitializer
            } else {
                symbol_table::InitialValue::Tentative
            }
        }
    };

    match symbol_table.entry(identifier.clone()) {
        Entry::Occupied(entry) => {
            let old = entry.get();

            if old.symbol_type != variable_type {
                bail!(
                    "Conflicting types {:?} and {:?} found for {}",
                    old.symbol_type,
                    variable_type,
                    identifier
                )
            }

            // IF this is a variable that was previously recorded as having static storage duration
            // meaning it was either
            // at file scope:
            //  in which case it doesn't say anything about whether it was globally visible,
            // or at block scope:
            //  in which case it is NOT globally visible
            if let symbol_table::IdentifierAttributes::StaticStorageAttribute {
                init: old_initial_value,
                is_globally_visible: is_old_variable_globally_visible,
            } = &old.identifier_attributes
            {
                let is_this_declaration_globally_visible = match &storage_class {
                    Some(storage_class) => match storage_class {
                        parser::StorageClass::Extern => *is_old_variable_globally_visible,
                        parser::StorageClass::Static => {
                            // marks explicitly to not be globally visible
                            if *is_old_variable_globally_visible {
                                bail!("Conflicting variable linkage")
                            }
                            false
                        }
                    },
                    None => {
                        // marks explicitly to be globally visible
                        if !*is_old_variable_globally_visible {
                            bail!("Conflicting variable linkage")
                        }
                        true
                    }
                };

                match (old_initial_value, &mut initial_value) {
                    (
                        symbol_table::InitialValue::Initial(..),
                        symbol_table::InitialValue::Initial(..),
                    ) => {
                        bail!("Conflicting file scope variable definitions")
                    }
                    (
                        symbol_table::InitialValue::Initial(..),
                        symbol_table::InitialValue::Tentative
                        | symbol_table::InitialValue::NoInitializer,
                    ) => initial_value = old_initial_value.clone(),
                    (
                        symbol_table::InitialValue::Tentative,
                        symbol_table::InitialValue::NoInitializer,
                    ) => initial_value = symbol_table::InitialValue::Tentative,
                    _ => {}
                }

                symbol_table.insert(
                    identifier.clone(),
                    symbol_table::Symbol {
                        symbol_type: variable_type.clone(),
                        identifier_attributes:
                            symbol_table::IdentifierAttributes::StaticStorageAttribute {
                                init: initial_value,
                                is_globally_visible: is_this_declaration_globally_visible,
                            },
                    },
                );
            }
        }
        Entry::Vacant(e) => {
            e.insert(symbol_table::Symbol {
                symbol_type: variable_type.clone(),
                identifier_attributes: symbol_table::IdentifierAttributes::StaticStorageAttribute {
                    init: initial_value,
                    is_globally_visible: !matches!(
                        &storage_class,
                        Some(parser::StorageClass::Static)
                    ),
                },
            });
        }
    }

    Ok(parser::VariableDeclaration {
        identifier,
        init,
        variable_type,
        storage_class,
    })
}

fn type_check_function_declaration(
    function_declaration: parser::FunctionDeclaration,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::FunctionDeclaration> {
    let parser::FunctionDeclaration {
        identifier,
        parameters,
        body,
        function_type,
        storage_class,
    } = function_declaration;

    let (parameter_types, return_type) = match &function_type {
        Type::Undefined | Type::Int | Type::Long => unreachable!(
            "non-function-type found when type checking a file scope function declaration"
        ),
        Type::FuncType {
            parameter_types,
            return_type,
        } => (parameter_types, return_type),
    };

    let mut is_this_declaration_global =
        !matches!(storage_class, Some(parser::StorageClass::Static));

    match symbol_table.entry(identifier.clone()) {
        Entry::Occupied(mut entry) => {
            let old = entry.get();

            if old.symbol_type != function_type {
                bail!(
                    "Incompatible function declaration {:?} and {:?} for {}",
                    old.symbol_type,
                    function_type,
                    identifier
                )
            }

            if let symbol_table::IdentifierAttributes::FuncAttribute {
                is_defined: is_old_function_declaration_defined,
                is_globally_visible: is_old_function_declaration_global,
            } = old.identifier_attributes
            {
                if is_old_function_declaration_defined && body.is_some() {
                    bail!(
                        "Function with name {} is defined more than once",
                        identifier
                    )
                }

                if is_old_function_declaration_global
                    && matches!(storage_class, Some(parser::StorageClass::Static))
                {
                    bail!("Static function declaration follows non-static")
                }

                is_this_declaration_global = is_old_function_declaration_global;

                entry.insert(symbol_table::Symbol {
                    symbol_type: function_type.clone(),
                    identifier_attributes: symbol_table::IdentifierAttributes::FuncAttribute {
                        is_defined: is_old_function_declaration_defined || body.is_some(),
                        is_globally_visible: is_this_declaration_global,
                    },
                });
            }
        }
        Entry::Vacant(e) => {
            e.insert(symbol_table::Symbol {
                symbol_type: function_type.clone(),
                identifier_attributes: symbol_table::IdentifierAttributes::FuncAttribute {
                    is_defined: body.is_some(),
                    is_globally_visible: is_this_declaration_global,
                },
            });
        }
    }

    let body: Option<parser::Block> = if let Some(body) = body {
        parameters
            .iter()
            .zip(parameter_types)
            .for_each(|(parameter, parameter_type)| {
                symbol_table.insert(
                    parameter.clone(),
                    symbol_table::Symbol {
                        symbol_type: parameter_type.clone(),
                        identifier_attributes: symbol_table::IdentifierAttributes::LocalAttribute,
                    },
                );
            });
        Some(type_check_block(body, symbol_table).context("type checking function declaration")?)
    } else {
        None
    };

    Ok(parser::FunctionDeclaration {
        identifier,
        parameters,
        body,
        function_type,
        storage_class,
    })
}

fn type_check_block(
    block: parser::Block,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(block) = block;
    for block_item in block {
        type_check_block_item(block_item, symbol_table).context("type checking a block")?;
    }
    Ok(())
}

fn type_check_block_item(
    block_item: &parser::BlockItem,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
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
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
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
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<()> {
    match for_init {
        parser::ForInit::InitialDeclaration(variable_declaration) => {
            let parser::VariableDeclaration { storage_class, .. } = &variable_declaration;
            if storage_class.is_some() {
                bail!("for loop header should not contain a storage-class specifier")
            }
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
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
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
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<()> {
    match expression {
        parser::Expression::Var { identifier } => {
            if matches!(
                symbol_table[identifier].symbol_type,
                symbol_table::Type::FuncType { .. }
            ) {
                bail!("Function name used as a variable!");
            }
            Ok(())
        }
        parser::Expression::FunctionCall {
            identifier,
            arguments,
        } => {
            if matches!(
                symbol_table[identifier].symbol_type,
                symbol_table::Type::Int | symbol_table::Type::Long
            ) {
                bail!("Variable name used as a function!");
            }
            // todo!("I just cheated here to make it compile") it's no longer enough to compare
            //  argument count, we need to check the types and the return types.
            if !matches!(
                &symbol_table[identifier].symbol_type,
                symbol_table::Type::FuncType {
                    parameter_types,
                    ..
                } if parameter_types.len() == arguments.len()
            ) {
                bail!("Function called with the wrong number of arguments");
            }
            for argument in arguments {
                type_check_expression(argument, symbol_table)
                    .context("type checking an expression")?
            }
            Ok(())
        }
        parser::Expression::Cast { .. } => todo!("implement this"),
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
    declarations: Vec<parser::Declaration>,
) -> (
    Vec<parser::Declaration>,
    HashMap<String, symbol_table::Symbol>,
) {
    let mut symbol_table: HashMap<String, symbol_table::Symbol> = HashMap::new();
    (
        declarations
            .into_iter()
            .map(|declaration| match declaration {
                parser::Declaration::VariableDeclaration(variable_declaration) => {
                    parser::Declaration::VariableDeclaration(
                        type_check_file_scope_variable_declaration(
                            variable_declaration,
                            &mut symbol_table,
                        )
                        .context("Type checking a file scope variable declaration")
                        .unwrap(),
                    )
                }
                parser::Declaration::FunctionDeclaration(function_declaration) => {
                    parser::Declaration::FunctionDeclaration(
                        type_check_function_declaration(function_declaration, &mut symbol_table)
                            .context("type checking a function")
                            .unwrap(),
                    )
                }
            })
            .collect(),
        symbol_table,
    )
}
