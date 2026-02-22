use super::parser;
use crate::compiler_driver::compiler::symbol_table;
use crate::compiler_driver::compiler::symbol_table::{Constant, Type};
use anyhow::{Context, bail};
use std::collections::hash_map::{Entry, HashMap};

// I wonder if I should consider these methods implementations of their respective types.
// rather than a set of free functions, each type for which a type check is defined, has its
// own implementation of type-check.

fn type_check_variable_declaration(
    variable_declaration: parser::VariableDeclaration,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::VariableDeclaration> {
    let parser::VariableDeclaration {
        identifier,
        init,
        variable_type,
        storage_class,
    } = variable_declaration;

    let mut updated_init: Option<parser::TypedExpression> = None;
    match &storage_class {
        Some(storage_class) => match storage_class {
            parser::StorageClass::Extern => {
                if init.is_some() {
                    bail!("Initializer on local extern variable declaration")
                }
                if let Some(old) = symbol_table.get(&identifier) {
                    if old.symbol_type != variable_type {
                        bail!(
                            "Conflicting types {:?} and {:?} found for {}",
                            old.symbol_type,
                            variable_type.clone(),
                            identifier
                        )
                    }
                } else {
                    symbol_table.insert(
                        identifier.clone(),
                        symbol_table::Symbol {
                            symbol_type: variable_type.clone(),
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
                    Some(typed_expression) => {
                        if let parser::Expression::Constant(constant) = &typed_expression.expression
                        {
                            symbol_table::InitialValue::Initial(get_static_init(
                                &constant,
                                &variable_type,
                            ))
                        } else {
                            bail!("Non-constant initializer")
                        }
                    }
                    None => symbol_table::InitialValue::Initial(get_static_init(
                        &Constant::ConstInt(0),
                        &variable_type,
                    )),
                };

                symbol_table.insert(
                    identifier.clone(),
                    symbol_table::Symbol {
                        symbol_type: Type::Int,
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
                    symbol_type: variable_type.clone(),
                    identifier_attributes: symbol_table::IdentifierAttributes::LocalAttribute,
                },
            );
            if let Some(init) = init {
                updated_init = Some(
                    type_check_expression(init, symbol_table)
                        .context("type checking a variable declaration")?,
                )
            }
        }
    }

    Ok(parser::VariableDeclaration {
        identifier,
        init: updated_init,
        variable_type,
        storage_class,
    })
}

fn get_static_init(constant: &Constant, variable_type: &Type) -> symbol_table::StaticInit {
    match (constant, variable_type) {
        (Constant::ConstInt(const_int), Type::Int) => symbol_table::StaticInit::IntInit(*const_int),
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
        (_, Type::FuncType { .. }) => {
            unreachable!("Function type found when type checking a file scope variable declaration")
        }
    }
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
                symbol_table::InitialValue::Initial(get_static_init(constant, &variable_type))
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
                            // Static marks it explicitly to NOT be globally visible
                            if *is_old_variable_globally_visible {
                                bail!("Conflicting variable linkage")
                            }
                            false
                        }
                    },
                    None => {
                        // None marks it implicitly to be globally visible
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
        Some(
            type_check_block(body, symbol_table, &return_type)
                .context("type checking function declaration")?,
        )
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
    enclosing_function_return_type: &Type,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(block) = block;
    let block = block
        .into_iter()
        .map(|block_item| {
            type_check_block_item(block_item, symbol_table, enclosing_function_return_type)
                .context("type checking a block")
        })
        .collect::<anyhow::Result<_>>()?;

    Ok(parser::Block::Block(block))
}

fn type_check_block_item(
    block_item: parser::BlockItem,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
    enclosing_function_return_type: &Type,
) -> anyhow::Result<parser::BlockItem> {
    match block_item {
        parser::BlockItem::Statement(statement) => Ok(parser::BlockItem::Statement(
            type_check_statement(statement, symbol_table, enclosing_function_return_type)
                .context("type checking a block item")?,
        )),
        parser::BlockItem::Declaration(declaration) => Ok(parser::BlockItem::Declaration(
            type_check_declaration(declaration, symbol_table)
                .context("type checking a block item")?,
        )),
    }
}

fn type_check_statement(
    statement: parser::Statement,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
    enclosing_function_return_type: &Type,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Return(expression) => Ok(parser::Statement::Return(
            type_check_expression(expression, symbol_table)
                .context("type checking a statement")?
                .cast_to(enclosing_function_return_type),
        )),
        parser::Statement::Expression(expression) => Ok(parser::Statement::Return(
            type_check_expression(expression, symbol_table).context("type checking a statement")?,
        )),
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => Ok(parser::Statement::If {
            condition: type_check_expression(condition, symbol_table)
                .context("type checking a statement")?,
            then_statement: Box::new(
                type_check_statement(
                    *then_statement,
                    symbol_table,
                    enclosing_function_return_type,
                )
                .context("type checking a statement")?,
            ),
            optional_else_statement: if let Some(else_statement) = optional_else_statement {
                Some(Box::new(
                    type_check_statement(
                        *else_statement,
                        symbol_table,
                        enclosing_function_return_type,
                    )
                    .context("type checking a statement")?,
                ))
            } else {
                None
            },
        }),
        parser::Statement::Label(identifier, statement) => Ok(parser::Statement::Label(
            identifier,
            Box::new(
                type_check_statement(*statement, symbol_table, enclosing_function_return_type)
                    .context("type checking a statement")?,
            ),
        )),
        parser::Statement::Compound(block) => Ok(parser::Statement::Compound(
            type_check_block(block, symbol_table, enclosing_function_return_type)
                .context("type checking a block")?,
        )),
        parser::Statement::While {
            condition,
            body,
            label,
        } => Ok(parser::Statement::While {
            condition: type_check_expression(condition, symbol_table)
                .context("type checking a statement")?,
            body: Box::new(
                type_check_statement(*body, symbol_table, enclosing_function_return_type)
                    .context("type checking a statement")?,
            ),
            label,
        }),
        parser::Statement::DoWhile {
            body,
            condition,
            label,
        } => Ok(parser::Statement::DoWhile {
            body: Box::new(
                type_check_statement(*body, symbol_table, enclosing_function_return_type)
                    .context("type checking a statement")?,
            ),
            condition: type_check_expression(condition, symbol_table)
                .context("type checking a statement")?,
            label,
        }),
        parser::Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => Ok(parser::Statement::For {
            init: type_check_for_init(init, symbol_table).context("type checking a statement")?,
            condition: if let Some(condition) = condition {
                Some(
                    type_check_expression(condition, symbol_table)
                        .context("type checking a statement")?,
                )
            } else {
                None
            },
            post: if let Some(post) = post {
                Some(
                    type_check_expression(post, symbol_table)
                        .context("type checking a statement")?,
                )
            } else {
                None
            },
            body: Box::new(
                type_check_statement(*body, symbol_table, enclosing_function_return_type)
                    .context("type checking a statement")?,
            ),
            label,
        }),
        parser::Statement::Switch {
            condition,
            cases,
            body,
            label,
        } => Ok(parser::Statement::Switch {
            condition: type_check_expression(condition, symbol_table)
                .context("type checking a statement")?,
            cases,
            body: Box::new(
                type_check_statement(*body, symbol_table, enclosing_function_return_type)
                    .context("type checking a statement")?,
            ),
            label,
        }),
        parser::Statement::Case {
            match_value,
            follow_statement,
            break_label,
            label,
        } => Ok(parser::Statement::Case {
            match_value,
            follow_statement: Box::new(
                type_check_statement(
                    *follow_statement,
                    symbol_table,
                    enclosing_function_return_type,
                )
                .context("type checking a statement")?,
            ),
            break_label,
            label,
        }),
        parser::Statement::Default {
            break_label,
            follow_statement,
            label,
        } => Ok(parser::Statement::Default {
            break_label,
            follow_statement: Box::new(
                type_check_statement(
                    *follow_statement,
                    symbol_table,
                    enclosing_function_return_type,
                )
                .context("type checking a statement")?,
            ),
            label,
        }),

        parser::Statement::Goto(..)
        | parser::Statement::Break { .. }
        | parser::Statement::Continue { .. }
        | parser::Statement::Null => Ok(statement),
    }
}

fn type_check_for_init(
    for_init: parser::ForInit,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::ForInit> {
    match for_init {
        parser::ForInit::InitialDeclaration(variable_declaration) => {
            let parser::VariableDeclaration { storage_class, .. } = &variable_declaration;
            if storage_class.is_some() {
                bail!("for loop header should not contain a storage-class specifier")
            }
            Ok(parser::ForInit::InitialDeclaration(
                type_check_variable_declaration(variable_declaration, symbol_table)
                    .context("type checking a for init")?,
            ))
        }
        parser::ForInit::InitialOptionalExpression(optional_expression) => {
            Ok(parser::ForInit::InitialOptionalExpression(
                if let Some(expression) = optional_expression {
                    Some(
                        type_check_expression(expression, symbol_table)
                            .context("type checking a for init")?,
                    )
                } else {
                    None
                },
            ))
        }
    }
}

fn type_check_declaration(
    declaration: parser::Declaration,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::Declaration> {
    match declaration {
        parser::Declaration::VariableDeclaration(variable_declaration) => {
            Ok(parser::Declaration::VariableDeclaration(
                type_check_variable_declaration(variable_declaration, symbol_table)
                    .context("type checking a declaration")?,
            ))
        }
        parser::Declaration::FunctionDeclaration(function_declaration) => {
            Ok(parser::Declaration::FunctionDeclaration(
                type_check_function_declaration(function_declaration, symbol_table)
                    .context("type checking a declaration")?,
            ))
        }
    }
}

fn type_check_expression(
    typed_expression: parser::TypedExpression,
    symbol_table: &mut HashMap<String, symbol_table::Symbol>,
) -> anyhow::Result<parser::TypedExpression> {
    match typed_expression.expression {
        parser::Expression::Constant(ref constant) => Ok(parser::TypedExpression {
            expression_type: match constant {
                Constant::ConstInt(_) => Type::Int,
                Constant::ConstLong(_) => Type::Long,
            },
            expression: typed_expression.expression,
        }),
        parser::Expression::Var { ref identifier } => {
            if matches!(symbol_table[identifier].symbol_type, Type::FuncType { .. }) {
                bail!("Function name used as a variable!");
            }
            Ok(parser::TypedExpression {
                expression_type: symbol_table[identifier].symbol_type.clone(),
                expression: typed_expression.expression,
            })
        }
        parser::Expression::Cast {
            target_type,
            expression: inner_expression,
        } => {
            let typed_inner_expression = type_check_expression(*inner_expression, symbol_table)
                .context("type checking cast expression")?;
            Ok(parser::TypedExpression {
                expression_type: target_type.clone(),
                expression: parser::Expression::Cast {
                    target_type,
                    expression: Box::new(typed_inner_expression),
                },
            })
        }
        parser::Expression::Unary(unary_operator, inner_expression) => {
            let typed_inner_expression = type_check_expression(*inner_expression, symbol_table)
                .context("type checking unary expression")?;
            let expression_type = match unary_operator {
                parser::UnaryOperator::Not => Type::Int,
                _ => typed_inner_expression.expression_type.clone(),
            };
            Ok(parser::TypedExpression {
                expression_type,
                expression: parser::Expression::Unary(
                    unary_operator,
                    Box::new(typed_inner_expression),
                ),
            })
        }
        parser::Expression::BinaryOperation {
            binary_operator,
            left_operand,
            right_operand,
        } => {
            let typed_left_operand = type_check_expression(*left_operand, symbol_table)
                .context("type_checking an expression")?;
            let typed_right_operand = type_check_expression(*right_operand, symbol_table)
                .context("type_checking an expression")?;
            match binary_operator {
                parser::BinaryOperator::And | parser::BinaryOperator::Or => {
                    Ok(parser::TypedExpression {
                        expression_type: Type::Int,
                        expression: parser::Expression::BinaryOperation {
                            binary_operator,
                            left_operand: Box::new(typed_left_operand),
                            right_operand: Box::new(typed_right_operand),
                        },
                    })
                }
                _ => {
                    let common_type = typed_left_operand
                        .expression_type
                        .common_with(&typed_right_operand.expression_type)
                        .context("type_checking an expression")?;

                    let promoted_left_operand = typed_left_operand.cast_to(&common_type);
                    let promoted_right_operand = typed_right_operand.cast_to(&common_type);
                    let promoted_binary_expression = parser::Expression::BinaryOperation {
                        binary_operator: binary_operator.clone(),
                        left_operand: Box::new(promoted_left_operand),
                        right_operand: Box::new(promoted_right_operand),
                    };
                    match binary_operator {
                        parser::BinaryOperator::Add
                        | parser::BinaryOperator::Subtract
                        | parser::BinaryOperator::Multiply
                        | parser::BinaryOperator::Divide
                        | parser::BinaryOperator::Remainder => Ok(parser::TypedExpression {
                            expression_type: common_type,
                            expression: promoted_binary_expression,
                        }),
                        _ => Ok(parser::TypedExpression {
                            expression_type: Type::Int,
                            expression: promoted_binary_expression,
                        }),
                    }
                }
            }
        }
        parser::Expression::Assignment(left_operand, right_operand) => {
            let typed_left_operand = type_check_expression(*left_operand, symbol_table)
                .context("type_checking an expression")?;
            let typed_right_operand = type_check_expression(*right_operand, symbol_table)
                .context("type_checking an expression")?;
            let converted_right_operand =
                typed_right_operand.cast_to(&typed_left_operand.expression_type);
            Ok(parser::TypedExpression {
                expression_type: typed_left_operand.expression_type.clone(),
                expression: parser::Expression::Assignment(
                    Box::new(typed_left_operand),
                    Box::new(converted_right_operand),
                ),
            })
        }
        parser::Expression::Conditional(left_operand, middle_operand, right_operand) => {
            let typed_left_operand = type_check_expression(*left_operand, symbol_table)?;
            let typed_middle_operand = type_check_expression(*middle_operand, symbol_table)?;
            let typed_right_operand = type_check_expression(*right_operand, symbol_table)?;
            let common_type = typed_middle_operand
                .expression_type
                .common_with(&typed_right_operand.expression_type)
                .context("type_checking an expression")?;
            let promoted_middle_operand = typed_middle_operand.cast_to(&common_type);
            let promoted_right_operand = typed_right_operand.cast_to(&common_type);
            Ok(parser::TypedExpression {
                expression_type: common_type,
                expression: parser::Expression::Conditional(
                    Box::new(typed_left_operand),
                    Box::new(promoted_middle_operand),
                    Box::new(promoted_right_operand),
                ),
            })
        }
        parser::Expression::FunctionCall {
            identifier,
            arguments,
        } => match symbol_table[&identifier].symbol_type.clone() {
            Type::Int | Type::Long => bail!("Variable name used as a function!"),
            Type::FuncType {
                parameter_types,
                return_type,
            } => {
                if parameter_types.len() != arguments.len() {
                    bail!("Function called with the wrong number of arguments");
                }
                let typed_arguments = parameter_types
                    .iter()
                    .zip(arguments.into_iter())
                    .map(|(parameter_type, argument)| {
                        let typed_argument = type_check_expression(argument, symbol_table)
                            .context("type checking cast expression")
                            .unwrap();
                        typed_argument.cast_to(&parameter_type)
                    })
                    .collect();
                Ok(parser::TypedExpression {
                    expression_type: *return_type.clone(),
                    expression: parser::Expression::FunctionCall {
                        identifier,
                        arguments: typed_arguments,
                    },
                })
            }
            Type::Undefined => {
                unreachable!("A symbol ended up in the symbol table without a type!")
            }
        },
    }
}

pub fn analyse(
    declarations: Vec<parser::Declaration>,
) -> anyhow::Result<(
    Vec<parser::Declaration>,
    HashMap<String, symbol_table::Symbol>,
)> {
    let mut symbol_table: HashMap<String, symbol_table::Symbol> = HashMap::new();
    let resolved_declarations: Vec<_> = declarations
        .into_iter()
        .map(|declaration| match declaration {
            parser::Declaration::VariableDeclaration(variable_declaration) => {
                let checked = type_check_file_scope_variable_declaration(
                    variable_declaration,
                    &mut symbol_table,
                )
                .context("Type checking a file scope variable declaration")?;
                Ok(parser::Declaration::VariableDeclaration(checked))
            }
            parser::Declaration::FunctionDeclaration(function_declaration) => {
                let checked =
                    type_check_function_declaration(function_declaration, &mut symbol_table)
                        .context("type checking a function")?;
                Ok(parser::Declaration::FunctionDeclaration(checked))
            }
        })
        .collect::<anyhow::Result<_>>()?;

    Ok((resolved_declarations, symbol_table))
}
