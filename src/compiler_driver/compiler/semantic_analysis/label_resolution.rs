use std::collections::HashMap;
use std::collections::HashSet;

use super::generator;
use super::parser;
use anyhow::anyhow;

pub fn analyse(
    mut function_body: Vec<parser::BlockItem>,
) -> anyhow::Result<Vec<parser::BlockItem>> {
    let mut label_map: HashMap<String, String> = HashMap::new();
    let mut goto_requests: Vec<&mut parser::BlockItem> = Vec::new();
    let mut visited: HashSet<String> = HashSet::new();

    let mut is_last_item_label_statement = false;
    for block_item in &mut function_body {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                if let parser::Statement::Label(identifier) = statement {
                    if label_map.contains_key(identifier) {
                        return Err(anyhow!("Duplicate label statement!"));
                    }
                    let unique_name = generator::make_temporary_from_identifier(&identifier);
                    label_map.insert(identifier.clone(), unique_name.clone());
                    *block_item = parser::BlockItem::Statement(parser::Statement::Label(
                        label_map[identifier].clone(),
                    ));
                    is_last_item_label_statement = true;
                } else if let parser::Statement::Goto(identifier) = statement {
                    visited.insert(identifier.clone());
                    if label_map.contains_key(identifier) {
                        *block_item = parser::BlockItem::Statement(parser::Statement::Goto(
                            label_map[identifier].clone(),
                        ));
                    } else {
                        goto_requests.push(block_item);
                    }
                } else {
                    is_last_item_label_statement = false;
                }
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

    for request in goto_requests {
        if let parser::BlockItem::Statement(parser::Statement::Goto(identifier)) = request {
            if label_map.contains_key(identifier) {
                visited.insert(identifier.clone());
                *request = parser::BlockItem::Statement(parser::Statement::Goto(
                    label_map[identifier].clone(),
                ));
            } else {
                return Err(anyhow!("undeclared label {:?}", identifier));
            }
        }
    }

    if visited.len() != label_map.len() {
        return Err(anyhow!("found a label that was never visited"));
    }

    Ok(function_body)
}
