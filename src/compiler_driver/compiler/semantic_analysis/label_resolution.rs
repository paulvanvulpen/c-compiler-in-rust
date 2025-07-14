use super::generator;
use super::parser;
use anyhow::{Context, anyhow};
use std::collections::HashMap;

fn update_label_map_in_block(
    block: &parser::Block,
    label_map: &mut HashMap<String, String>,
) -> anyhow::Result<()> {
    let parser::Block::Block(block) = block;
    for block_item in block {
        if let parser::BlockItem::Statement(statement) = block_item {
            update_label_map(statement, label_map).context("updating label_map in block")?;
        }
    }

    Ok(())
}

fn update_label_map(
    statement: &parser::Statement,
    label_map: &mut HashMap<String, String>,
) -> anyhow::Result<()> {
    match statement {
        parser::Statement::Label(identifier, statement) => {
            if label_map.contains_key(identifier) {
                return Err(anyhow!("Duplicate label statement!"));
            }
            let unique_name = generator::make_temporary_from_identifier(&identifier);
            label_map.insert(identifier.clone(), unique_name);
            update_label_map(&*statement, label_map)?;
            Ok(())
        }
        parser::Statement::If {
            then_statement,
            optional_else_statement,
            ..
        } => {
            update_label_map(&*then_statement, label_map)?;
            if let Some(else_statement) = optional_else_statement {
                update_label_map(&*else_statement, label_map)?;
            }
            Ok(())
        }
        parser::Statement::While { body, .. }
        | parser::Statement::DoWhile { body, .. }
        | parser::Statement::For { body, .. }
        | parser::Statement::Switch { body, .. } => {
            update_label_map(&*body, label_map)?;
            Ok(())
        }
        parser::Statement::Compound(block) => update_label_map_in_block(block, label_map),
        parser::Statement::Case {
            follow_statement, ..
        }
        | parser::Statement::Default {
            follow_statement, ..
        } => update_label_map(follow_statement, label_map),
        parser::Statement::Expression(..)
        | parser::Statement::Return(..)
        | parser::Statement::Goto(..)
        | parser::Statement::Break { .. }
        | parser::Statement::Continue { .. }
        | parser::Statement::Null => Ok(()),
    }
}

fn resolve_statement(
    statement: parser::Statement,
    label_map: &HashMap<String, String>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Goto(identifier) => {
            if label_map.contains_key(&identifier) {
                Ok(parser::Statement::Goto(label_map[&identifier].clone()))
            } else {
                Err(anyhow!(
                    "goto statement to undeclared label {:?}",
                    identifier
                ))
            }
        }
        parser::Statement::Label(identifier, following_statement) => {
            let resolved_statement = resolve_statement(*following_statement, label_map)
                .context("resolving label statement")?;
            Ok(parser::Statement::Label(
                label_map[&identifier].clone(),
                Box::new(resolved_statement),
            ))
        }
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => Ok(parser::Statement::If {
            condition,
            then_statement: Box::new(resolve_statement(*then_statement, label_map)?),
            optional_else_statement: if let Some(else_statement) = optional_else_statement {
                Some(Box::new(resolve_statement(*else_statement, label_map)?))
            } else {
                None
            },
        }),
        parser::Statement::Compound(block) => Ok(parser::Statement::Compound(resolve_block(
            block, label_map,
        )?)),
        parser::Statement::While {
            condition,
            body,
            label,
        } => {
            let body =
                resolve_statement(*body, label_map).context("resolving a while statement")?;
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
                resolve_statement(*body, label_map).context("resolving a while statement")?;
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
            let body = resolve_statement(*body, label_map).context("resolving a for statement")?;

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
            let body =
                resolve_statement(*body, label_map).context("resolving a switch statement")?;
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
            let follow_statement = resolve_statement(*follow_statement, label_map)
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
            let follow_statement = resolve_statement(*follow_statement, label_map)
                .context("resolving a switch-default statement")?;
            Ok(parser::Statement::Default {
                follow_statement: Box::new(follow_statement),
                break_label,
                label,
            })
        }
        parser::Statement::Break { .. }
        | parser::Statement::Continue { .. }
        | parser::Statement::Expression(..)
        | parser::Statement::Return(..)
        | parser::Statement::Null => Ok(statement),
    }
}

fn resolve_block(
    block: parser::Block,
    label_map: &HashMap<String, String>,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;

    for block_item in &mut block {
        if let parser::BlockItem::Statement(statement) = block_item {
            let original = std::mem::take(statement);
            let resolved_statement =
                resolve_statement(original, label_map).context("analysed statement block item")?;
            *block_item = parser::BlockItem::Statement(resolved_statement);
        }
    }
    Ok(parser::Block::Block(block))
}

pub fn analyse(function_name: &str, function_body: parser::Block) -> anyhow::Result<parser::Block> {
    let mut label_map: HashMap<String, String> = HashMap::new();

    update_label_map_in_block(&function_body, &mut label_map)
        .with_context(|| format!("update label_map for function body of: {}", function_name))?;

    resolve_block(function_body, &label_map)
        .with_context(|| format!("analysing function body of: {}", function_name))
}
