use super::generator;
use super::parser;
use anyhow::{Context, anyhow};
use std::collections::HashMap;

fn resolve_statement(
    statement: parser::Statement,
    break_parent_label: Option<String>,
    continue_parent_label: Option<String>,
    cases: Option<&mut HashMap<String, parser::LabelAndMatchValue>>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Break { .. } => {
            if break_parent_label.is_none() {
                return Err(anyhow!("missing label for break statement"));
            }
            Ok(parser::Statement::Break {
                label: break_parent_label,
            })
        }
        parser::Statement::Continue { .. } => {
            if continue_parent_label.is_none() {
                return Err(anyhow!("missing label for continue statement"));
            }

            Ok(parser::Statement::Continue {
                label: continue_parent_label,
            })
        }
        parser::Statement::Case {
            match_value,
            follow_statement,
            label,
            ..
        } => match cases {
            Some(unpacked_cases) => {
                if unpacked_cases.contains_key(&label) {
                    return Err(anyhow!("Duplicate case statement: {}!", &label));
                }

                if continue_parent_label.eq(&break_parent_label) {}
                let unique_label = generator::make_label(&label);
                unpacked_cases.insert(
                    label.clone(),
                    parser::LabelAndMatchValue {
                        unique_label: unique_label.clone(),
                        match_value: Some(match_value),
                    },
                );
                Ok(parser::Statement::Case {
                    match_value,
                    follow_statement: Box::new(
                        resolve_statement(
                            *follow_statement,
                            break_parent_label.clone(),
                            continue_parent_label.clone(),
                            Some(unpacked_cases),
                        )
                        .context("resolving a switch-case statement")?,
                    ),
                    break_label: break_parent_label,
                    label: unique_label,
                })
            }
            None => Err(anyhow!(
                "case statement outside of switch scope: {}!",
                &label
            )),
        },
        parser::Statement::Default {
            follow_statement,
            label,
            ..
        } => match cases {
            Some(unpacked_cases) => {
                if unpacked_cases.contains_key(&label) {
                    return Err(anyhow!("Duplicate default statement: {}!", &label));
                }
                let unique_label = generator::make_label(&label);
                unpacked_cases.insert(
                    label.clone(),
                    parser::LabelAndMatchValue {
                        unique_label: unique_label.clone(),
                        match_value: None,
                    },
                );
                Ok(parser::Statement::Default {
                    follow_statement: Box::new(
                        resolve_statement(
                            *follow_statement,
                            break_parent_label.clone(),
                            continue_parent_label.clone(),
                            Some(unpacked_cases),
                        )
                        .context("resolving a switch-default statement")?,
                    ),
                    break_label: break_parent_label,
                    label: unique_label.clone(),
                })
            }
            None => Err(anyhow!(
                "case statement outside of switch scope: {}!",
                &label
            )),
        },
        parser::Statement::While {
            condition, body, ..
        } => {
            let label = generator::make_label("while");
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()), cases)
                .context("resolving a while statement")?;
            Ok(parser::Statement::While {
                condition,
                body: Box::new(body),
                label: Some(label),
            })
        }
        parser::Statement::DoWhile {
            body, condition, ..
        } => {
            let label = generator::make_label("dowhile");
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()), cases)
                .context("resolving a do-while statement")?;
            Ok(parser::Statement::DoWhile {
                body: Box::new(body),
                condition,
                label: Some(label),
            })
        }
        parser::Statement::For {
            init,
            condition,
            post,
            body,
            ..
        } => {
            let label = generator::make_label("for");
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()), cases)
                .context("resolving a for statement")?;
            Ok(parser::Statement::For {
                init,
                body: Box::new(body),
                condition,
                post,
                label: Some(label),
            })
        }
        parser::Statement::Switch {
            condition, body, ..
        } => {
            let label = generator::make_label("switch");
            let mut cases_map: HashMap<String, parser::LabelAndMatchValue> = HashMap::new();
            let body = resolve_statement(
                *body,
                Some(label.clone()),
                continue_parent_label,
                Some(&mut cases_map),
            )
            .context("resolving a switch statement")?;

            let cases: Vec<parser::LabelAndMatchValue> = cases_map.into_values().collect();

            Ok(parser::Statement::Switch {
                condition,
                cases,
                body: Box::new(body),
                label: Some(label),
            })
        }
        parser::Statement::If {
            condition,
            then_statement,
            optional_else_statement,
        } => {
            let mut cases = cases;
            Ok(parser::Statement::If {
                condition,
                then_statement: Box::new(
                    resolve_statement(
                        *then_statement,
                        break_parent_label.clone(),
                        continue_parent_label.clone(),
                        cases.as_deref_mut(),
                    )
                    .context("resolving an if-statement")?,
                ),
                optional_else_statement: if let Some(else_statement) = optional_else_statement {
                    Some(Box::new(
                        resolve_statement(
                            *else_statement,
                            break_parent_label,
                            continue_parent_label,
                            cases,
                        )
                        .context("resolving an if-statement")?,
                    ))
                } else {
                    None
                },
            })
        }
        parser::Statement::Compound(block) => Ok(parser::Statement::Compound(
            resolve_block(block, break_parent_label, continue_parent_label, cases)
                .context("resolving a compound statement")?,
        )),
        parser::Statement::Label(goto_label, statement) => Ok(parser::Statement::Label(
            goto_label,
            Box::new(
                resolve_statement(*statement, break_parent_label, continue_parent_label, cases)
                    .context("resolving a compound statement")?,
            ),
        )),
        parser::Statement::Return(_)
        | parser::Statement::Expression(_)
        | parser::Statement::Goto(_)
        | parser::Statement::Null => Ok(statement),
    }
}

fn resolve_block(
    block: parser::Block,
    break_parent_label: Option<String>,
    continue_parent_label: Option<String>,
    cases: Option<&mut HashMap<String, parser::LabelAndMatchValue>>,
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;

    let mut cases = cases;
    for block_item in &mut block {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                let original = std::mem::take(statement);
                let resolved_statement = resolve_statement(
                    original,
                    break_parent_label.clone(),
                    continue_parent_label.clone(),
                    cases.as_deref_mut(),
                )
                .context("analysed statement block item")?;
                *block_item = parser::BlockItem::Statement(resolved_statement);
            }
            parser::BlockItem::Declaration(..) => {}
        }
    }

    Ok(parser::Block::Block(block))
}

pub fn analyse(function_name: &str, function_body: parser::Block) -> anyhow::Result<parser::Block> {
    resolve_block(function_body, None, None, None)
        .with_context(|| format!("analysing function body of: {}", function_name))
}
