use std::collections::HashMap;

use super::generator;
use super::parser;
use crate::compiler_driver::compiler::visualize::Visualizer;
use anyhow::{Context, anyhow};

fn resolve_local_variable_declaration(
    variable_declaration: parser::VariableDeclaration,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::VariableDeclaration> {
    let parser::VariableDeclaration { identifier, init } = variable_declaration;
    if identifier_map.contains_key(&identifier) && identifier_map[&identifier].from_current_scope {
        return Err(anyhow!("Duplicate declaration: {}!", &identifier));
    }
    let unique_name = generator::make_temporary_from_identifier(&identifier);
    identifier_map.insert(
        identifier.clone(),
        NameAndScope {
            unique_name: unique_name.clone(),
            from_current_scope: true,
            has_linkage: false,
        },
    );

    let mut updated_initialiser: Option<parser::Expression> = None;
    if let Some(init) = init {
        updated_initialiser = Some(resolve_expression(init, identifier_map)?);
    }

    Ok(parser::VariableDeclaration {
        identifier: unique_name,
        init: updated_initialiser,
    })
}

fn resolve_local_function_declaration(
    function_declaration: parser::FunctionDeclaration,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::FunctionDeclaration> {
    let parser::FunctionDeclaration {
        identifier, body, ..
    } = &function_declaration;
    if body.is_some() {
        return Err(anyhow!(
            "Function definitions found inside function scope {}!",
            identifier
        ));
    }

    resolve_function_declaration(function_declaration, identifier_map)
}

fn resolve_function_declaration(
    function_declaration: parser::FunctionDeclaration,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::FunctionDeclaration> {
    let parser::FunctionDeclaration {
        identifier,
        parameters: params,
        body,
    } = function_declaration;
    if identifier_map.contains_key(&identifier)
        && identifier_map[&identifier].from_current_scope
        && !identifier_map[&identifier].has_linkage
    {
        return Err(anyhow!("Duplicate declaration: {}!", &identifier));
    }
    identifier_map.insert(
        identifier.clone(),
        NameAndScope {
            unique_name: identifier.clone(),
            from_current_scope: true,
            has_linkage: true,
        },
    );

    let mut copy_of_identifier_map: HashMap<String, NameAndScope> = identifier_map.clone();
    let params: Vec<String> = params
        .into_iter()
        .map(|p| {
            resolve_parameter(p, &mut copy_of_identifier_map)
                .context("resolving a function declaration")
                .unwrap()
        })
        .collect();

    Ok(parser::FunctionDeclaration {
        identifier: identifier.clone(),
        parameters: params,
        body: match body {
            Some(body) => Some(resolve_block(body, &mut copy_of_identifier_map)?),
            None => None,
        },
    })
}

fn resolve_parameter(
    identifier: String,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<String> {
    if identifier_map.contains_key(&identifier) && identifier_map[&identifier].from_current_scope {
        return Err(anyhow!("Duplicate declaration: {}!", &identifier));
    }
    let unique_name = generator::make_temporary_from_identifier(&identifier);
    identifier_map.insert(
        identifier.clone(),
        NameAndScope {
            unique_name: unique_name.clone(),
            from_current_scope: true,
            has_linkage: false,
        },
    );

    Ok(unique_name)
}

fn resolve_declaration(
    declaration: parser::Declaration,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::Declaration> {
    match declaration {
        parser::Declaration::VariableDeclaration(variable_declaration) => {
            Ok(parser::Declaration::VariableDeclaration(
                resolve_local_variable_declaration(variable_declaration, identifier_map)
                    .context("resolving a declaration")?,
            ))
        }
        parser::Declaration::FunctionDeclaration(function_declaration) => {
            Ok(parser::Declaration::FunctionDeclaration(
                resolve_local_function_declaration(function_declaration, identifier_map)
                    .context("resolving a declaration")?,
            ))
        }
    }
}

fn resolve_for_init(
    for_init: parser::ForInit,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::ForInit> {
    match for_init {
        parser::ForInit::InitialDeclaration(declaration) => {
            Ok(parser::ForInit::InitialDeclaration(
                resolve_local_variable_declaration(declaration, identifier_map)?,
            ))
        }
        parser::ForInit::InitialOptionalExpression(optional_expression) => {
            match optional_expression {
                Some(expression) => Ok(parser::ForInit::InitialOptionalExpression(Some(
                    resolve_expression(expression, identifier_map)?,
                ))),
                None => Ok(parser::ForInit::InitialOptionalExpression(None)),
            }
        }
    }
}

fn resolve_statement(
    statement: parser::Statement,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Return(expression) => Ok(parser::Statement::Return(resolve_expression(
            expression,
            identifier_map,
        )?)),
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => Ok(parser::Statement::If {
            condition: resolve_expression(condition, identifier_map)?,
            then_statement: Box::new(resolve_statement(*then_statement, identifier_map)?),
            optional_else_statement: if let Some(else_statement) = optional_else_statement {
                Some(Box::new(resolve_statement(
                    *else_statement,
                    identifier_map,
                )?))
            } else {
                None
            },
        }),
        parser::Statement::Compound(block) => {
            let mut copy_of_identifier_map: HashMap<String, NameAndScope> = identifier_map.clone();
            Ok(parser::Statement::Compound(resolve_block(
                block,
                &mut copy_of_identifier_map,
            )?))
        }
        parser::Statement::While {
            condition,
            body,
            label,
        } => {
            let condition = resolve_expression(condition, identifier_map)
                .context("resolving a while statement")?;
            let body =
                resolve_statement(*body, identifier_map).context("resolving a while statement")?;
            Ok(parser::Statement::While {
                condition,
                body: Box::new(body),
                label,
            })
        }
        parser::Statement::DoWhile {
            body,
            condition,
            label,
        } => {
            let body =
                resolve_statement(*body, identifier_map).context("resolving a while statement")?;
            let condition = resolve_expression(condition, identifier_map)
                .context("resolving a while statement")?;
            Ok(parser::Statement::DoWhile {
                body: Box::new(body),
                condition,
                label,
            })
        }
        parser::Statement::For {
            init,
            condition,
            post,
            body,
            label,
        } => {
            let mut copy_of_identifier_map: HashMap<String, NameAndScope> = identifier_map.clone();
            let init = resolve_for_init(init, &mut copy_of_identifier_map)
                .context("resolving a for statement")?;
            let condition = condition
                .map(|c| resolve_expression(c, &mut copy_of_identifier_map))
                .transpose()
                .context("resolving a for statement")?;
            let post = post
                .map(|c| resolve_expression(c, &mut copy_of_identifier_map))
                .transpose()
                .context("resolving a for statement")?;
            let body = resolve_statement(*body, &mut copy_of_identifier_map)
                .context("resolving a while statement")?;

            Ok(parser::Statement::For {
                init,
                condition,
                post,
                body: Box::new(body),
                label,
            })
        }
        parser::Statement::Switch {
            condition,
            cases,
            body,
            label,
        } => {
            let condition = resolve_expression(condition, identifier_map)
                .context("resolving a switch statement")?;
            let body =
                resolve_statement(*body, identifier_map).context("resolving a switch statement")?;
            Ok(parser::Statement::Switch {
                condition,
                cases,
                body: Box::new(body),
                label,
            })
        }
        parser::Statement::Case {
            match_value,
            follow_statement,
            break_label,
            label,
        } => {
            let follow_statement = resolve_statement(*follow_statement, identifier_map)
                .context("resolving a switch-case statement")?;
            Ok(parser::Statement::Case {
                match_value,
                follow_statement: Box::new(follow_statement),
                break_label,
                label,
            })
        }
        parser::Statement::Default {
            follow_statement,
            break_label,
            label,
        } => {
            let follow_statement = resolve_statement(*follow_statement, identifier_map)
                .context("resolving a switch-default statement")?;
            Ok(parser::Statement::Default {
                follow_statement: Box::new(follow_statement),
                break_label,
                label,
            })
        }
        parser::Statement::Expression(expression) => Ok(parser::Statement::Expression(
            resolve_expression(expression, identifier_map)
                .context("resolving an expression statement")?,
        )),
        parser::Statement::Label(label, following_statement) => Ok(parser::Statement::Label(
            label,
            Box::new(
                resolve_statement(*following_statement, identifier_map)
                    .context("resolving a label statement")?,
            ),
        )),

        parser::Statement::Goto(..)
        | parser::Statement::Break { .. }
        | parser::Statement::Continue { .. }
        | parser::Statement::Null => Ok(statement),
    }
}

fn resolve_expression(
    expression: parser::Expression,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::Expression> {
    match expression {
        parser::Expression::Assignment(left, right) => {
            if !matches!(*left, parser::Expression::Var { .. }) {
                return Err(anyhow!("invalid lvalue {}", (*left).visualize(0)));
            }
            Ok(parser::Expression::Assignment(
                Box::new(resolve_expression(*left, identifier_map)?),
                Box::new(resolve_expression(*right, identifier_map)?),
            ))
        }
        parser::Expression::Var { identifier } => {
            if identifier_map.contains_key(&identifier) {
                Ok(parser::Expression::Var {
                    identifier: identifier_map[&identifier].unique_name.clone(),
                })
            } else {
                Err(anyhow!("undeclared variable identifier {:?}", identifier))
            }
        }
        parser::Expression::Constant(..) => Ok(expression),
        parser::Expression::BinaryOperation {
            binary_operator,
            left_operand,
            right_operand,
        } => {
            match binary_operator {
                parser::BinaryOperator::SumAssign
                | parser::BinaryOperator::DifferenceAssign
                | parser::BinaryOperator::ProductAssign
                | parser::BinaryOperator::QuotientAssign
                | parser::BinaryOperator::RemainderAssign
                | parser::BinaryOperator::BitwiseAndAssign
                | parser::BinaryOperator::BitwiseOrAssign
                | parser::BinaryOperator::BitwiseXOrAssign
                | parser::BinaryOperator::LeftShiftAssign
                | parser::BinaryOperator::RightShiftAssign => {
                    if !matches!(*left_operand, parser::Expression::Var { .. }) {
                        return Err(anyhow!("invalid lvalue {:?}", (*left_operand).visualize(0)));
                    }
                }
                parser::BinaryOperator::Add
                | parser::BinaryOperator::Subtract
                | parser::BinaryOperator::Multiply
                | parser::BinaryOperator::Divide
                | parser::BinaryOperator::Remainder
                | parser::BinaryOperator::LeftShift
                | parser::BinaryOperator::RightShift
                | parser::BinaryOperator::BitwiseAnd
                | parser::BinaryOperator::BitwiseXOr
                | parser::BinaryOperator::BitwiseOr
                | parser::BinaryOperator::And
                | parser::BinaryOperator::Or
                | parser::BinaryOperator::Equal
                | parser::BinaryOperator::NotEqual
                | parser::BinaryOperator::LessThan
                | parser::BinaryOperator::LessOrEqual
                | parser::BinaryOperator::GreaterOrEqual
                | parser::BinaryOperator::GreaterThan => {}
                parser::BinaryOperator::Assign => panic!(
                    "parser should have converted the Assign operation into an Assignment Expressions"
                ),
                parser::BinaryOperator::Conditional => panic!(
                    "parser should have converted the conditional operation into a Conditional Expression"
                ),
            }
            Ok(parser::Expression::BinaryOperation {
                binary_operator,
                left_operand: Box::new(resolve_expression(*left_operand, identifier_map)?),
                right_operand: Box::new(resolve_expression(*right_operand, identifier_map)?),
            })
        }
        parser::Expression::Unary(unary_operator, expression) => match unary_operator {
            parser::UnaryOperator::Negate
            | parser::UnaryOperator::Not
            | parser::UnaryOperator::Complement => Ok(parser::Expression::Unary(
                unary_operator,
                Box::new(resolve_expression(*expression, identifier_map)?),
            )),
            parser::UnaryOperator::PrefixDecrement
            | parser::UnaryOperator::PostfixDecrement
            | parser::UnaryOperator::PrefixIncrement
            | parser::UnaryOperator::PostfixIncrement => {
                if !matches!(*expression, parser::Expression::Var { .. }) {
                    return Err(anyhow!("invalid lvalue {:?}", (*expression).visualize(0)));
                }
                Ok(parser::Expression::Unary(
                    unary_operator,
                    Box::new(resolve_expression(*expression, identifier_map)?),
                ))
            }
        },
        parser::Expression::Conditional(exp1, exp2, exp3) => Ok(parser::Expression::Conditional(
            Box::new(resolve_expression(*exp1, identifier_map)?),
            Box::new(resolve_expression(*exp2, identifier_map)?),
            Box::new(resolve_expression(*exp3, identifier_map)?),
        )),
        parser::Expression::FunctionCall {
            identifier,
            arguments,
        } => {
            if identifier_map.contains_key(&identifier) {
                Ok(parser::Expression::FunctionCall {
                    identifier: identifier_map[&identifier].unique_name.clone(),
                    arguments: arguments
                        .into_iter()
                        .map(|arg| {
                            resolve_expression(arg, identifier_map)
                                .context("resolving an expression")
                                .unwrap()
                        })
                        .collect(),
                })
            } else {
                Err(anyhow!("undeclared function identifier {:?}", identifier))
            }
        }
    }
}

fn resolve_block_item(
    block_item: parser::BlockItem,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::BlockItem> {
    match block_item {
        parser::BlockItem::Statement(statement) => {
            let resolved_statement = resolve_statement(statement, identifier_map)
                .context("analysed statement block item")?;
            Ok(parser::BlockItem::Statement(resolved_statement))
        }
        parser::BlockItem::Declaration(declaration) => {
            let resolved_declaration = resolve_declaration(declaration, identifier_map)
                .context("analysed declaration block item")?;
            Ok(parser::BlockItem::Declaration(resolved_declaration))
        }
    }
}

fn resolve_block(
    block: parser::Block,
    identifier_map: &mut HashMap<String, NameAndScope>,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;
    for block_item in &mut block {
        let original = std::mem::take(block_item);
        *block_item = resolve_block_item(original, identifier_map).context("resolving_block")?;
    }

    Ok(parser::Block::Block(block))
}

struct NameAndScope {
    unique_name: String,
    from_current_scope: bool,
    has_linkage: bool,
}

impl Clone for NameAndScope {
    // Clone always assumes the use of a new scope.
    fn clone(&self) -> Self {
        NameAndScope {
            unique_name: self.unique_name.clone(),
            from_current_scope: false,
            has_linkage: self.has_linkage,
        }
    }
}

pub fn analyse(
    function_declarations: Vec<parser::FunctionDeclaration>,
) -> Vec<parser::FunctionDeclaration> {
    let mut identifier_map: HashMap<String, NameAndScope> = HashMap::new();

    function_declarations
        .into_iter()
        .map(|f| resolve_function_declaration(f, &mut identifier_map).unwrap())
        .collect()
}
