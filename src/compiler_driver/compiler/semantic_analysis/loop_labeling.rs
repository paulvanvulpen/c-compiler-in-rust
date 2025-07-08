use super::generator;
use super::parser;
use anyhow::{Context, anyhow};

fn resolve_statement(
    statement: parser::Statement,
    break_parent_label: Option<String>,
    continue_parent_label: Option<String>,
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
            ..
        } => Ok(parser::Statement::Case {
            match_value,
            follow_statement: Box::new(
                resolve_statement(
                    *follow_statement,
                    break_parent_label.clone(),
                    continue_parent_label.clone(),
                )
                .context("resolving a switch-case statement")?,
            ),
            label: break_parent_label,
        }),
        parser::Statement::Default {
            follow_statement, ..
        } => Ok(parser::Statement::Default {
            follow_statement: Box::new(
                resolve_statement(
                    *follow_statement,
                    break_parent_label.clone(),
                    continue_parent_label.clone(),
                )
                .context("resolving a switch-default statement")?,
            ),
            label: break_parent_label,
        }),
        parser::Statement::While {
            condition, body, ..
        } => {
            let label = generator::make_label("while");
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()))
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
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()))
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
            let body = resolve_statement(*body, Some(label.clone()), Some(label.clone()))
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
            condition,
            cases,
            body,
            ..
        } => {
            let label = generator::make_label("switch");
            let body = resolve_statement(*body, Some(label.clone()), None)
                .context("resolving a switch statement")?;
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
        } => Ok(parser::Statement::If {
            condition,
            then_statement: Box::new(
                resolve_statement(
                    *then_statement,
                    break_parent_label.clone(),
                    continue_parent_label.clone(),
                )
                .context("resolving an if-statement")?,
            ),
            optional_else_statement: if let Some(else_statement) = optional_else_statement {
                Some(Box::new(
                    resolve_statement(*else_statement, break_parent_label, continue_parent_label)
                        .context("resolving an if-statement")?,
                ))
            } else {
                None
            },
        }),
        parser::Statement::Compound(block) => Ok(parser::Statement::Compound(
            resolve_block(block, break_parent_label, continue_parent_label)
                .context("resolving a compound statement")?,
        )),
        parser::Statement::Label(goto_label, statement) => Ok(parser::Statement::Label(
            goto_label,
            Box::new(
                resolve_statement(*statement, break_parent_label, continue_parent_label)
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
) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;

    for block_item in &mut block {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                let original = std::mem::take(statement);
                let resolved_statement = resolve_statement(
                    original,
                    break_parent_label.clone(),
                    continue_parent_label.clone(),
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
    resolve_block(function_body, None, None)
        .with_context(|| format!("analysing function body of: {}", function_name))
}
