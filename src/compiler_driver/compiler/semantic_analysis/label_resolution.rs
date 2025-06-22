use super::generator;
use super::parser;
use anyhow::{Context, anyhow};
use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone)]
struct LabelNotFound;

impl fmt::Display for LabelNotFound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Label not found")
    }
}

impl std::error::Error for LabelNotFound {}

fn resolve_statement(
    statement: parser::Statement,
    label_map: &mut HashMap<String, String>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Label(identifier) => {
            if label_map.contains_key(&identifier) {
                return Err(anyhow!("Duplicate label statement!"));
            }
            let unique_name = generator::make_temporary_from_identifier(&identifier);
            label_map.insert(identifier.clone(), unique_name.clone());
            Ok(parser::Statement::Label(label_map[&identifier].clone()))
        }
        parser::Statement::Goto(identifier) => {
            if label_map.contains_key(&identifier) {
                Ok(parser::Statement::Goto(label_map[&identifier].clone()))
            } else {
                Err(anyhow::Error::new(LabelNotFound))
            }
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
        parser::Statement::Expression(..)
        | parser::Statement::Return(..)
        | parser::Statement::Null => Ok(statement),
    }
}

pub fn analyse(
    mut function_body: Vec<parser::BlockItem>,
) -> anyhow::Result<Vec<parser::BlockItem>> {
    let mut label_map: HashMap<String, String> = HashMap::new();
    let mut goto_requests: Vec<&mut parser::BlockItem> = Vec::new();

    let mut is_last_item_label_statement = false;
    for block_item in &mut function_body {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                let original = std::mem::take(statement);
                let resolved_statement = resolve_statement(original, &mut label_map)
                    .context("analysed statement block item");
                match resolved_statement {
                    Ok(resolved_statement) => {
                        is_last_item_label_statement =
                            matches!(resolved_statement, parser::Statement::Label(..));
                        *block_item = parser::BlockItem::Statement(resolved_statement);
                    }
                    Err(error) => {
                        if error.downcast_ref::<LabelNotFound>().is_some() {
                            goto_requests.push(block_item);
                            is_last_item_label_statement = false;
                        } else {
                            return Err(error);
                        }
                    }
                };
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

    for request in goto_requests {
        if let parser::BlockItem::Statement(parser::Statement::Goto(identifier)) = request {
            if label_map.contains_key(identifier) {
                *request = parser::BlockItem::Statement(parser::Statement::Goto(
                    label_map[identifier].clone(),
                ));
            } else {
                return Err(anyhow!("undeclared label {:?}", identifier));
            }
        }
    }

    Ok(function_body)
}
