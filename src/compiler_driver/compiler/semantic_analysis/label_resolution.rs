use super::generator;
use super::parser;
use anyhow::{Context, anyhow};
use std::collections::HashMap;

fn update_label_map(
    statement: &parser::Statement,
    label_map: &mut HashMap<String, String>,
) -> anyhow::Result<()> {
    match statement {
        parser::Statement::Label(identifier) => {
            if label_map.contains_key(identifier) {
                return Err(anyhow!("Duplicate label statement!"));
            }
            let unique_name = generator::make_temporary_from_identifier(&identifier);
            label_map.insert(identifier.clone(), unique_name);
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
        parser::Statement::Compound(..) => todo!(),
        parser::Statement::Expression(..)
        | parser::Statement::Return(..)
        | parser::Statement::Goto(..)
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
        parser::Statement::Label(identifier) => {
            Ok(parser::Statement::Label(label_map[&identifier].clone()))
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
        parser::Statement::Compound(..) => todo!(),
        parser::Statement::Expression(..)
        | parser::Statement::Return(..)
        | parser::Statement::Null => Ok(statement),
    }
}

fn resolve_block(
    block: parser::Block,
    label_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;

    for block_item in &mut block {
        if let parser::BlockItem::Statement(statement) = block_item {
            update_label_map(statement, label_map)?;
        }
    }

    let mut is_last_item_label_statement = false;
    for block_item in &mut block {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                let original = std::mem::take(statement);
                let resolved_statement = resolve_statement(original, &label_map)
                    .context("analysed statement block item")?;
                is_last_item_label_statement =
                    matches!(resolved_statement, parser::Statement::Label(..));
                *block_item = parser::BlockItem::Statement(resolved_statement);
            }
            parser::BlockItem::Declaration(..) => {
                if is_last_item_label_statement {
                    return Err(anyhow!(
                        "In C17, a label must precede a statement, not a declaration"
                    ));
                }
                is_last_item_label_statement = false;
            }
        }
    }
    if is_last_item_label_statement {
        return Err(anyhow!(
            "In C17, labels must be followed by a valid statement"
        ));
    }
    Ok(parser::Block::Block(block))
}

// TODO: "when adding scopes, reevaluate this,
//  inside nested ifs the condition that labels must be followed by a valid statement is not implicitly met"
pub fn analyse(function_name: &str, function_body: parser::Block) -> anyhow::Result<parser::Block> {
    let mut label_map: HashMap<String, String> = HashMap::new();

    resolve_block(function_body, &mut label_map)
        .with_context(|| format!("analysing function body of: {}", function_name))
}
