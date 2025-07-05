use super::generator;
use super::parser;
use crate::compiler_driver::compiler::parser::Statement;
use anyhow::{Context, anyhow};

fn resolve_statement(
    statement: parser::Statement,
    label: Option<String>,
) -> anyhow::Result<parser::Statement> {
    match statement {
        parser::Statement::Break { .. } => {
            if label.is_none() {
                return Err(anyhow!("missing label for break statement"));
            }
            Ok(parser::Statement::Break { label })
        }
        parser::Statement::Continue { .. } => {
            if label.is_none() {
                return Err(anyhow!("missing label for continue statement"));
            }
            Ok(parser::Statement::Continue { label })
        }
        parser::Statement::While {
            condition, body, ..
        } => {
            let label = generator::make_label("while");
            let body = resolve_statement(*body, Some(label.clone()))
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
            let body = resolve_statement(*body, Some(label.clone()))
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
            let body = resolve_statement(*body, Some(label.clone()))
                .context("resolving a for statement")?;
            Ok(parser::Statement::For {
                init,
                body: Box::new(body),
                condition,
                post,
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
                resolve_statement(*then_statement, label.clone())
                    .context("resolving an if-statement")?,
            ),
            optional_else_statement: if let Some(else_statement) = optional_else_statement {
                Some(Box::new(
                    resolve_statement(*else_statement, label)
                        .context("resolving an if-statement")?,
                ))
            } else {
                None
            },
        }),
        parser::Statement::Compound(block) => Ok(parser::Statement::Compound(
            resolve_block(block, label).context("resolving a compound statement")?,
        )),
        Statement::Return(_)
        | Statement::Expression(_)
        | Statement::Goto(_)
        | Statement::Label(_)
        | Statement::Null => Ok(statement),
    }
}

fn resolve_block(block: parser::Block, label: Option<String>) -> anyhow::Result<parser::Block> {
    let parser::Block::Block(mut block) = block;

    for block_item in &mut block {
        match block_item {
            parser::BlockItem::Statement(statement) => {
                let original = std::mem::take(statement);
                let resolved_statement = resolve_statement(original, label.clone())
                    .context("analysed statement block item")?;
                *block_item = parser::BlockItem::Statement(resolved_statement);
            }
            parser::BlockItem::Declaration(..) => {}
        }
    }

    Ok(parser::Block::Block(block))
}

pub fn analyse(function_name: &str, function_body: parser::Block) -> anyhow::Result<parser::Block> {
    resolve_block(function_body, None)
        .with_context(|| format!("analysing function body of: {}", function_name))
}
