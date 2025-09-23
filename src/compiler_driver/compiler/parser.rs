use super::lexer::Token;
use anyhow::{Context, Error, Result, anyhow, bail};
use std::mem::discriminant;

mod visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(declaration*)
// declaration = FuncDecl(function_declaration) | VarDecl(variable_declaration)
// variable_declaration = (identifier name, exp? init, storage_class?)
// function_declaration = (identifier name, identifier* params, block? body, storage_class?)
// storage_class = Static | Extern
// type = Int
// block_item = S(statement) | D(declaration)
// block = Block(block_item*)
// for_init = InitDecl(variable_declaration), InitExp(exp?)
// statement = Return(exp)
//              | If(exp condition, statement then, statement? else)
//              | Expression(exp)
//              | Compound(block)
//              | Break(identifier name)
//              | Continue(identifier name)
//              | While(exp condition, statement body)
//              | DoWhile(statement body, exp condition)
//              | For(for_init init, exp? condition, exp? post, statement body)
//              | Goto(identifier name)
//              | Label(identifier name, statement following_statement)
//              | Switch(exp condition, statement body, identifier* cases)
//              | Case(exp match_value, identifier label, statement? follow_statement)
//              | Default(identifier label, statement? follow_statement)
//              | Null
// exp = Constant(int)
//        | Var(identifier)
//        | Unary(unary_operator, exp)
//        | Binary(binary_operator, exp, exp)
//        | Assignment(exp, exp)
//        | Conditional(exp condition, exp, exp)
//        | FunctionCall(identifier, exp* args)
// unary_operator = Complement | Negate | Not | PrefixDecrement | PostfixDecrement | PrefixIncrement | PostfixIncrement
// binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

pub enum AbstractSyntaxTree {
    Program(Program),
}

pub enum Program {
    Program(Vec<Declaration>),
}

pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
}

pub struct FunctionDeclaration {
    pub identifier: String,
    pub parameters: Vec<String>,
    pub body: Option<Block>,
    pub storage_class: Option<StorageClass>,
}

pub struct VariableDeclaration {
    pub identifier: String,
    pub init: Option<Expression>,
    pub storage_class: Option<StorageClass>,
}

pub enum Type {
    Int,
}

pub enum StorageClass {
    Static,
    Extern,
}

pub enum Block {
    Block(Vec<BlockItem>),
}

impl Default for Block {
    fn default() -> Self {
        Block::Block(vec![])
    }
}

pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl Default for BlockItem {
    fn default() -> Self {
        BlockItem::Statement(Default::default())
    }
}

pub struct LabelAndMatchValue {
    pub unique_label: String,
    pub match_value: Option<usize>,
}

#[derive(Default)]
pub enum Statement {
    Return(Expression),
    Expression(Expression),
    If {
        condition: Expression,
        then_statement: Box<Statement>,
        optional_else_statement: Option<Box<Statement>>,
    },
    Goto(String),
    Label(String, Box<Statement>),
    Compound(Block),
    Break {
        label: Option<String>,
    },
    Continue {
        label: Option<String>,
    },
    While {
        condition: Expression,
        body: Box<Statement>,
        label: Option<String>,
    },
    DoWhile {
        body: Box<Statement>,
        condition: Expression,
        label: Option<String>,
    },
    For {
        init: ForInit,
        condition: Option<Expression>,
        post: Option<Expression>,
        body: Box<Statement>,
        label: Option<String>,
    },
    Switch {
        condition: Expression,
        cases: Vec<LabelAndMatchValue>,
        body: Box<Statement>,
        label: Option<String>,
    },
    Case {
        match_value: usize, // only support positive integers for now
        follow_statement: Box<Statement>,
        break_label: Option<String>,
        label: String,
    },
    Default {
        break_label: Option<String>,
        follow_statement: Box<Statement>,
        label: String,
    },
    #[default]
    Null,
}

pub enum ForInit {
    InitialDeclaration(VariableDeclaration),
    InitialOptionalExpression(Option<Expression>),
}

pub enum Expression {
    Constant(usize),
    Var {
        identifier: String,
    },
    Unary(UnaryOperator, Box<Expression>),
    BinaryOperation {
        binary_operator: BinaryOperator,
        left_operand: Box<Expression>,
        right_operand: Box<Expression>,
    },
    Assignment(Box<Expression>, Box<Expression>),
    Conditional(Box<Expression>, Box<Expression>, Box<Expression>),
    FunctionCall {
        identifier: String,
        arguments: Vec<Expression>,
    },
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Constant(Default::default())
    }
}

pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    PrefixDecrement,
    PostfixDecrement,
    PrefixIncrement,
    PostfixIncrement,
}

pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Remainder,
    LeftShift,
    RightShift,
    BitwiseAnd,
    BitwiseXOr,
    BitwiseOr,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    LessOrEqual,
    GreaterOrEqual,
    GreaterThan,
    Conditional,
    Assign,
    SumAssign,
    DifferenceAssign,
    ProductAssign,
    QuotientAssign,
    RemainderAssign,
    BitwiseAndAssign,
    BitwiseOrAssign,
    BitwiseXOrAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

impl BinaryOperator {
    fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Remainder => 50,
            BinaryOperator::Add | BinaryOperator::Subtract => 45,
            BinaryOperator::LeftShift | BinaryOperator::RightShift => 40,
            BinaryOperator::LessThan
            | BinaryOperator::LessOrEqual
            | BinaryOperator::GreaterThan
            | BinaryOperator::GreaterOrEqual => 37,
            BinaryOperator::Equal | BinaryOperator::NotEqual => 36,
            BinaryOperator::BitwiseAnd => 35,
            BinaryOperator::BitwiseXOr => 34,
            BinaryOperator::BitwiseOr => 33,
            BinaryOperator::And => 10,
            BinaryOperator::Or => 5,
            BinaryOperator::Conditional => 3,
            BinaryOperator::Assign
            | BinaryOperator::SumAssign
            | BinaryOperator::DifferenceAssign
            | BinaryOperator::ProductAssign
            | BinaryOperator::QuotientAssign
            | BinaryOperator::RemainderAssign
            | BinaryOperator::BitwiseAndAssign
            | BinaryOperator::BitwiseOrAssign
            | BinaryOperator::BitwiseXOrAssign
            | BinaryOperator::LeftShiftAssign
            | BinaryOperator::RightShiftAssign => 1,
        }
    }
}

// <program> ::= { <declaration> }
fn parse_program(mut lexer_tokens: Vec<Token>) -> Result<Program> {
    if lexer_tokens.is_empty() {
        return Err(anyhow!("failed to parse program, no tokens found"));
    }

    let mut declarations: Vec<Declaration> = vec![];
    let mut lexer_tokens = lexer_tokens.as_mut_slice();
    while !lexer_tokens.is_empty() {
        let declaration;
        (declaration, lexer_tokens) =
            parse_declaration(lexer_tokens).context("parsing a program")?;
        declarations.push(declaration);
    }

    Ok(Program::Program(declarations))
}

// <specifier> ::= <type> | <storage_class>
fn parse_specifiers(
    lexer_tokens: &mut [Token],
) -> Result<(Type, Option<StorageClass>, &mut [Token])> {
    let mut types = vec![];
    let mut storage_classes = vec![];
    let mut lexer_tokens = lexer_tokens;
    loop {
        match &lexer_tokens[0] {
            Token::Static | Token::Extern => {
                let storage_class;
                (storage_class, lexer_tokens) =
                    parse_storage_class(lexer_tokens).context("parsing a specifier")?;
                storage_classes.push(storage_class)
            }
            Token::Int => {
                let type_specifier;
                (type_specifier, lexer_tokens) =
                    parse_type(lexer_tokens).context("parsing a specifier")?;
                types.push(type_specifier)
            }
            _ => break,
        }
    }

    if types.len() != 1 {
        return Err(anyhow!("invalid type specifier"));
    }
    if storage_classes.len() > 1 {
        return Err(anyhow!("invalid storage class specifier"));
    }

    Ok((
        types.into_iter().next().unwrap(),
        storage_classes.into_iter().next(),
        lexer_tokens,
    ))
}

// <declaration> ::= <variable-declaration> | <function-declaration>
// <variable-declaration> ::= { <specifier> }+ <identifier> [ "=" <exp> ] ";"
// <function_declaration> ::= { <specifier> }+ <identifier> "(" <param-list> ")" ( <block> | ";")
fn parse_declaration(lexer_tokens: &mut [Token]) -> Result<(Declaration, &mut [Token])> {
    let (.., storage_class, lexer_tokens) =
        parse_specifiers(lexer_tokens).context("parsing a variable declaration")?;

    let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
    match &lexer_tokens[0] {
        Token::Equal => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("parsing a variable declaration")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("parsing a variable declaration")?;
            Ok((
                Declaration::VariableDeclaration(VariableDeclaration {
                    identifier,
                    init: Some(expression),
                    storage_class,
                }),
                lexer_tokens,
            ))
        }
        Token::Semicolon => {
            let lexer_tokens = &mut lexer_tokens[1..];
            Ok((
                Declaration::VariableDeclaration(VariableDeclaration {
                    identifier,
                    init: None,
                    storage_class,
                }),
                lexer_tokens,
            ))
        }
        Token::OpenParenthesis => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (parameters, lexer_tokens) =
                parse_param_list(lexer_tokens).context("Parsing a function")?;
            let lexer_tokens =
                expect(Token::CloseParenthesis, lexer_tokens).context("Parsing a function")?;

            match &mut lexer_tokens[0] {
                Token::Semicolon => Ok((
                    Declaration::FunctionDeclaration(FunctionDeclaration {
                        identifier,
                        parameters,
                        body: None,
                        storage_class,
                    }),
                    &mut lexer_tokens[1..],
                )),
                _ => {
                    let (body, lexer_tokens) =
                        parse_block(lexer_tokens).context("Parsing a function")?;
                    Ok((
                        Declaration::FunctionDeclaration(FunctionDeclaration {
                            identifier,
                            parameters,
                            body: Some(body),
                            storage_class,
                        }),
                        lexer_tokens,
                    ))
                }
            }
        }
        _ => Err(anyhow!(
            "Syntax error, found token {:?} while parsing a variable declaration",
            &lexer_tokens[0]
        )),
    }
}

// <function_declaration> ::= { <specifier> }+ <identifier> "(" <param-list> ")" ( <block> | ";")
fn parse_function_declaration(
    lexer_tokens: &mut [Token],
) -> Result<(FunctionDeclaration, &mut [Token])> {
    let (.., storage_class, lexer_tokens) =
        parse_specifiers(lexer_tokens).context("parsing a function")?;

    let (identifier, lexer_tokens) =
        parse_identifier(lexer_tokens).context("Parsing a function")?;
    let lexer_tokens =
        expect(Token::OpenParenthesis, lexer_tokens).context("Parsing a function")?;
    let (parameters, lexer_tokens) =
        parse_param_list(lexer_tokens).context("Parsing a function")?;
    let lexer_tokens =
        expect(Token::CloseParenthesis, lexer_tokens).context("Parsing a function")?;

    match &mut lexer_tokens[0] {
        Token::Semicolon => Ok((
            FunctionDeclaration {
                identifier,
                parameters,
                body: None,
                storage_class,
            },
            &mut lexer_tokens[1..],
        )),
        _ => {
            let (body, lexer_tokens) = parse_block(lexer_tokens).context("Parsing a function")?;
            Ok((
                FunctionDeclaration {
                    identifier,
                    parameters,
                    body: Some(body),
                    storage_class,
                },
                lexer_tokens,
            ))
        }
    }
}

// <param-list> ::= "void" | "int" <identifier> { "," "int" <identifier> }
fn parse_param_list(lexer_tokens: &mut [Token]) -> Result<(Vec<String>, &mut [Token])> {
    let mut params: Vec<String> = vec![];
    let mut lexer_tokens = lexer_tokens;
    match &lexer_tokens[0] {
        Token::Void => {
            lexer_tokens = &mut lexer_tokens[1..];
        }
        Token::Int => loop {
            lexer_tokens = expect(Token::Int, lexer_tokens).context("Parsing a parameter list")?;
            let identifier;
            (identifier, lexer_tokens) =
                parse_identifier(lexer_tokens).context("Parsing a parameter list")?;
            params.push(identifier);
            if !matches!(lexer_tokens[0], Token::Comma) {
                break;
            }
            lexer_tokens = &mut lexer_tokens[1..];
        },
        _ => {
            return Err(anyhow!(
                "Syntax error, found token {:?} while parsing a parameter list",
                &lexer_tokens[0]
            ));
        }
    }

    Ok((params, lexer_tokens))
}

// <block> ::= "{" { <block_item> } "}"
fn parse_block(lexer_tokens: &mut [Token]) -> Result<(Block, &mut [Token])> {
    let mut lexer_tokens = expect(Token::OpenBrace, lexer_tokens)?;
    let mut block = vec![];

    while !matches!(&lexer_tokens[0], Token::CloseBrace) {
        let block_item;
        (block_item, lexer_tokens) =
            parse_block_item(lexer_tokens).context("Parsing a compound statement")?;
        block.push(block_item);
    }

    Ok((Block::Block(block), &mut lexer_tokens[1..]))
}

// <block-item> ::= <statement> | <declaration>
fn parse_block_item(lexer_tokens: &mut [Token]) -> Result<(BlockItem, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Int => {
            let (declaration, lexer_tokens) =
                parse_declaration(lexer_tokens).context("Parsing a block item declaration")?;
            Ok((BlockItem::Declaration(declaration), lexer_tokens))
        }
        _ => {
            let (statement, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a block item statement")?;
            Ok((BlockItem::Statement(statement), lexer_tokens))
        }
    }
}

// <for-init> ::= <variable-declaration> | [ <exp> ] ";"
fn parse_for_init(lexer_tokens: &mut [Token]) -> Result<(ForInit, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Int => {
            let (declaration, lexer_tokens) =
                parse_declaration(lexer_tokens).context("Parsing a for_init statement")?;
            if let Declaration::VariableDeclaration(variable_declaration) = declaration {
                Ok((
                    ForInit::InitialDeclaration(variable_declaration),
                    lexer_tokens,
                ))
            } else {
                bail!("Expected a variable declaration but found a function declaration")
            }
        }
        Token::Semicolon => Ok((
            ForInit::InitialOptionalExpression(None),
            &mut lexer_tokens[1..],
        )),
        _ => {
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a for_init statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a for_init statement")?;
            Ok((
                ForInit::InitialOptionalExpression(Some(expression)),
                lexer_tokens,
            ))
        }
    }
}

// <identifier> ::= ? An identifier token ?
fn parse_identifier(lexer_tokens: &mut [Token]) -> Result<(String, &mut [Token])> {
    match &mut lexer_tokens[0] {
        Token::Identifier(identifier) => {
            let identifier = std::mem::take(identifier);
            Ok((identifier, &mut lexer_tokens[1..]))
        }
        _ => Err(fail()),
    }
}

// <statement> ::= "return" <exp> ";"
//      | <exp> ";"
//      | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
//      | "goto" identifier ";"
//      | identifier ":" <statement>
//      | <block>
//      | "break" ";"
//      | "continue" ";"
//      | "while" "(" <exp> ")" <statement>
//      | "do <statement> "while" "(" <exp> ")" ";"
//      | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
//      | "switch" "(" <exp> ")" <statement>
//      | "case" <const_exp> ":" [ <statement> ]
//      | "default" ":" [ <statement> ]
//      | ";"
fn parse_statement(lexer_tokens: &mut [Token]) -> Result<(Statement, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Return => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a return statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a return statement")?;
            Ok((Statement::Return(expression), lexer_tokens))
        }
        Token::If => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::OpenParenthesis, lexer_tokens).context("Parsing an if statement")?;
            let (condition, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
            let lexer_tokens =
                expect(Token::CloseParenthesis, lexer_tokens).context("Parsing an if statement")?;
            let (then_statement, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing an if statement")?;
            match lexer_tokens[0] {
                Token::Else => {
                    let lexer_tokens = &mut lexer_tokens[1..];
                    let (else_statement, lexer_tokens) =
                        parse_statement(lexer_tokens).context("Parsing an else statement")?;
                    Ok((
                        Statement::If {
                            condition,
                            then_statement: Box::new(then_statement),
                            optional_else_statement: Some(Box::new(else_statement)),
                        },
                        lexer_tokens,
                    ))
                }
                _ => Ok((
                    Statement::If {
                        condition,
                        then_statement: Box::new(then_statement),
                        optional_else_statement: None,
                    },
                    lexer_tokens,
                )),
            }
        }
        Token::Goto => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (identifier, lexer_tokens) =
                parse_identifier(lexer_tokens).context("Parsing a goto statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a goto statement")?;
            Ok((Statement::Goto(identifier), lexer_tokens))
        }
        Token::Semicolon => Ok((Statement::Null, &mut lexer_tokens[1..])),
        Token::OpenBrace => {
            let (block, lexer_tokens) =
                parse_block(lexer_tokens).context("Parsing a compound statement")?;
            Ok((Statement::Compound(block), lexer_tokens))
        }
        Token::Break => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a break statement")?;
            Ok((Statement::Break { label: None }, lexer_tokens))
        }
        Token::Continue => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a continue statement")?;
            Ok((Statement::Continue { label: None }, lexer_tokens))
        }
        Token::While => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)
                .context("Parsing a while statement")?;
            let (condition, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a while statement")?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                .context("Parsing a while statement")?;
            let (body, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a while statement")?;
            Ok((
                Statement::While {
                    condition,
                    body: Box::new(body),
                    label: None,
                },
                lexer_tokens,
            ))
        }
        Token::Do => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (body, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a do-while statement")?;
            let lexer_tokens =
                expect(Token::While, lexer_tokens).context("Parsing a do-while statement")?;
            let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)
                .context("Parsing a do-while statement")?;
            let (condition, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a do-while statement")?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                .context("Parsing a do-while statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a do-while statement")?;
            Ok((
                Statement::DoWhile {
                    body: Box::new(body),
                    condition,
                    label: None,
                },
                lexer_tokens,
            ))
        }
        Token::For => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::OpenParenthesis, lexer_tokens).context("Parsing a for statement")?;
            let (init, lexer_tokens) =
                parse_for_init(lexer_tokens).context("Parsing a for statement")?;
            let (condition, lexer_tokens) = match &lexer_tokens[0] {
                Token::Semicolon => (None, &mut lexer_tokens[1..]),
                _ => {
                    let (expression, lexer_tokens) =
                        parse_expression(lexer_tokens, 0).context("Parsing a for statement")?;
                    let lexer_tokens = expect(Token::Semicolon, lexer_tokens)
                        .context("Parsing a for statement")?;
                    (Some(expression), lexer_tokens)
                }
            };
            let (post, lexer_tokens) = match &lexer_tokens[0] {
                Token::CloseParenthesis => (None, &mut lexer_tokens[1..]),
                _ => {
                    let (expression, lexer_tokens) =
                        parse_expression(lexer_tokens, 0).context("Parsing a for statement")?;
                    let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                        .context("Parsing a for statement")?;
                    (Some(expression), lexer_tokens)
                }
            };
            let (body, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a for statement")?;
            Ok((
                Statement::For {
                    init,
                    condition,
                    post,
                    body: Box::new(body),
                    label: None,
                },
                lexer_tokens,
            ))
        }
        Token::Switch => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens = expect(Token::OpenParenthesis, lexer_tokens)
                .context("Parsing a switch statement")?;
            let (condition, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a switch statement")?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                .context("Parsing a switch statement")?;
            let (body, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a switch statement")?;
            Ok((
                Statement::Switch {
                    condition,
                    body: Box::new(body),
                    cases: vec![],
                    label: None,
                },
                lexer_tokens,
            ))
        }
        Token::Case => {
            let lexer_tokens = &mut lexer_tokens[1..];
            if !matches!(&lexer_tokens[0], Token::Constant(..)) {
                return Err(anyhow!(
                    "Only supporting positive integer values for now. Missing support for constant-folding"
                ));
            }
            let match_value: usize = match &lexer_tokens[0] {
                Token::Constant(x) => *x,
                _ => unreachable!("earlier check already guarantees this is a constant"),
            };
            let lexer_tokens = expect(Token::Colon, &mut lexer_tokens[1..])
                .context("Parsing a switch-case statement")?;
            let (follow_statement, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a switch-case statement")?;
            Ok((
                Statement::Case {
                    match_value,
                    follow_statement: Box::new(follow_statement),
                    break_label: None,
                    label: format!("case_{}_", match_value),
                },
                lexer_tokens,
            ))
        }
        Token::Default => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::Colon, lexer_tokens).context("Parsing a switch-default statement")?;
            let (follow_statement, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing a switch-default statement")?;
            Ok((
                Statement::Default {
                    follow_statement: Box::new(follow_statement),
                    break_label: None,
                    label: String::from("default"),
                },
                lexer_tokens,
            ))
        }
        _ => match &mut lexer_tokens[0..2] {
            [Token::Identifier(identifier), Token::Colon] => {
                let identifier = std::mem::take(identifier);
                let (following_statement, lexer_tokens) =
                    parse_statement(&mut lexer_tokens[2..]).context("Parsing a label statement")?;
                Ok((
                    Statement::Label(identifier, Box::new(following_statement)),
                    lexer_tokens,
                ))
            }
            _ => {
                let (expression, lexer_tokens) =
                    parse_expression(lexer_tokens, 0).context("Parsing an expression statement")?;
                let lexer_tokens = expect(Token::Semicolon, lexer_tokens)
                    .context("Parsing an expression statement")?;
                Ok((Statement::Expression(expression), lexer_tokens))
            }
        },
    }
}

// <primary> ::= <int> | <identifier> [ <postfix_op> ] | "(" <exp> ")" [ <postfix_op> ] | <identifier> "(" [ <argument-list> ] ")"
fn parse_primary(lexer_tokens: &mut [Token]) -> Result<(Expression, &mut [Token])> {
    match &mut lexer_tokens[0] {
        Token::Constant(constant) => Ok((Expression::Constant(*constant), &mut lexer_tokens[1..])),
        Token::Identifier(identifier) => {
            let identifier = std::mem::take(identifier);
            let mut lexer_tokens = &mut lexer_tokens[1..];
            if matches!(lexer_tokens[0], Token::OpenParenthesis) {
                let (arguments, lexer_tokens) = match lexer_tokens[1] {
                    Token::CloseParenthesis => (vec![], &mut lexer_tokens[2..]),
                    _ => {
                        let (arguments, lexer_tokens) = parse_argument_list(&mut lexer_tokens[1..])
                            .context("Parsing a primary")?;
                        let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                            .context("Parsing a primary")?;
                        (arguments, lexer_tokens)
                    }
                };
                Ok((
                    Expression::FunctionCall {
                        identifier,
                        arguments,
                    },
                    lexer_tokens,
                ))
            } else {
                let mut left = Expression::Var { identifier };
                while let Some(unary_operator) = parse_postfix_operator(&lexer_tokens) {
                    left = Expression::Unary(unary_operator, Box::new(left));
                    lexer_tokens = &mut lexer_tokens[1..];
                }
                Ok((left, lexer_tokens))
            }
        }
        Token::OpenParenthesis => {
            let lexer_tokens = &mut lexer_tokens[1..];
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("parsing a primary expression")?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                .context("parsing a primary expression")?;
            let mut left = expression;
            let mut lexer_tokens = lexer_tokens;
            while let Some(unary_operator) = parse_postfix_operator(&lexer_tokens) {
                left = Expression::Unary(unary_operator, Box::new(left));
                lexer_tokens = &mut lexer_tokens[1..];
            }
            Ok((left, lexer_tokens))
        }
        _ => Err(anyhow!(
            "Syntax error, found token {:?} while parsing a primary expression",
            &lexer_tokens[0]
        )),
    }
}

// <argument-list> ::= <exp> { "," <exp> }
fn parse_argument_list(lexer_tokens: &mut [Token]) -> Result<(Vec<Expression>, &mut [Token])> {
    let mut arguments: Vec<Expression> = vec![];
    let mut lexer_tokens = lexer_tokens;
    loop {
        let expression;
        (expression, lexer_tokens) =
            parse_expression(lexer_tokens, 0).context("parsing an argument-list")?;
        arguments.push(expression);
        if !matches!(lexer_tokens[0], Token::Comma) {
            break;
        }
        lexer_tokens = &mut lexer_tokens[1..];
    }
    Ok((arguments, lexer_tokens))
}

// <unary_exp> ::= <prefix_op> <unary_exp> | <primary>
fn parse_unary_expression(lexer_tokens: &mut [Token]) -> Result<(Expression, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen
        | Token::Tilde
        | Token::Exclamation
        | Token::DoubleHyphen
        | Token::DoublePlus => {
            let (unary_op, lexer_tokens) =
                parse_prefix_operator(lexer_tokens).context("parsing a prefix unary expression")?;
            let (primary, lexer_tokens) = parse_unary_expression(lexer_tokens)
                .context("parsing a prefix unary expression")?;
            Ok((Expression::Unary(unary_op, Box::new(primary)), lexer_tokens))
        }
        _ => parse_primary(lexer_tokens),
    }
}

// <exp> ::= <unary_exp> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
fn parse_expression(
    lexer_tokens: &mut [Token],
    min_precedence: u8,
) -> Result<(Expression, &mut [Token])> {
    let (mut left, mut lexer_tokens) =
        parse_unary_expression(lexer_tokens).context("parsing an expression")?;
    let mut right;
    let mut binary_operator;
    while get_binary_operator_precedence(&lexer_tokens[0]) >= Some(min_precedence) {
        match &lexer_tokens[0] {
            Token::Equal => {
                lexer_tokens = &mut lexer_tokens[1..];
                (right, lexer_tokens) =
                    parse_expression(lexer_tokens, BinaryOperator::Assign.precedence())
                        .context("parsing an expression")?;
                left = Expression::Assignment(Box::new(left), Box::new(right));
            }
            Token::QuestionMark => {
                lexer_tokens = &mut lexer_tokens[1..];
                let middle: Expression;
                (middle, lexer_tokens) = parse_expression(lexer_tokens, 0)
                    .context("parsing a conditional expression")?;
                lexer_tokens = expect(Token::Colon, lexer_tokens)
                    .context("parsing a conditional expression")?;
                (right, lexer_tokens) =
                    parse_expression(lexer_tokens, BinaryOperator::Conditional.precedence())
                        .context("parsing a conditional expression")?;
                left = Expression::Conditional(Box::new(left), Box::new(middle), Box::new(right));
            }
            _ => {
                (binary_operator, lexer_tokens) = parse_binary_operator(lexer_tokens)
                    .context("parsing a conditional expression")?;
                let enforce_left_precedence: u8 = match binary_operator {
                    BinaryOperator::SumAssign
                    | BinaryOperator::DifferenceAssign
                    | BinaryOperator::ProductAssign
                    | BinaryOperator::QuotientAssign
                    | BinaryOperator::RemainderAssign
                    | BinaryOperator::BitwiseAndAssign
                    | BinaryOperator::BitwiseOrAssign
                    | BinaryOperator::BitwiseXOrAssign
                    | BinaryOperator::LeftShiftAssign
                    | BinaryOperator::RightShiftAssign => 0,
                    _ => 1,
                };
                (right, lexer_tokens) = parse_expression(
                    lexer_tokens,
                    binary_operator.precedence() + enforce_left_precedence,
                )
                .context("Parsing a binary expression")?;
                left = Expression::BinaryOperation {
                    binary_operator,
                    left_operand: Box::new(left),
                    right_operand: Box::new(right),
                };
            }
        }
    }
    Ok((left, lexer_tokens))
}

// <unop> ::= "-" | "~" | "!" | -- | ++
fn parse_prefix_operator(lexer_tokens: &mut [Token]) -> Result<(UnaryOperator, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen => Ok((UnaryOperator::Negate, &mut lexer_tokens[1..])),
        Token::Tilde => Ok((UnaryOperator::Complement, &mut lexer_tokens[1..])),
        Token::Exclamation => Ok((UnaryOperator::Not, &mut lexer_tokens[1..])),
        Token::DoubleHyphen => Ok((UnaryOperator::PrefixDecrement, &mut lexer_tokens[1..])),
        Token::DoublePlus => Ok((UnaryOperator::PrefixIncrement, &mut lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <type> ::= "int"
fn parse_type(lexer_tokens: &mut [Token]) -> Result<(Type, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Int => Ok((Type::Int, &mut lexer_tokens[1..])),
        _ => Err(anyhow!(
            "Syntax error, found token {:?} while parsing a type",
            &lexer_tokens[0]
        )),
    }
}

// <storage_class> ::= "static" | "extern"
fn parse_storage_class(lexer_tokens: &mut [Token]) -> Result<(StorageClass, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Static => Ok((StorageClass::Static, &mut lexer_tokens[1..])),
        Token::Extern => Ok((StorageClass::Extern, &mut lexer_tokens[1..])),
        _ => Err(anyhow!(
            "Syntax error, found token {:?} while parsing a storage class",
            &lexer_tokens[0]
        )),
    }
}

fn expect(expected: Token, lexer_tokens: &mut [Token]) -> Result<&mut [Token]> {
    match lexer_tokens {
        t if t.is_empty() => Err(anyhow!(
            "Syntax error, expected {:?} but found nothing",
            expected
        )),
        t if discriminant(&expected) != discriminant(&t[0]) => Err(anyhow!(
            "Syntax error, expected {:?} but found {:?}",
            expected,
            &t[0]
        )),
        _ => Ok(&mut lexer_tokens[1..]),
    }
}

fn fail() -> Error {
    anyhow!("Syntax error")
}

fn parse_binary_operator(lexer_tokens: &mut [Token]) -> Result<(BinaryOperator, &mut [Token])> {
    match &lexer_tokens[0] {
        Token::Plus => Ok((BinaryOperator::Add, &mut lexer_tokens[1..])),
        Token::Hyphen => Ok((BinaryOperator::Subtract, &mut lexer_tokens[1..])),
        Token::Asterisk => Ok((BinaryOperator::Multiply, &mut lexer_tokens[1..])),
        Token::ForwardSlash => Ok((BinaryOperator::Divide, &mut lexer_tokens[1..])),
        Token::PercentSign => Ok((BinaryOperator::Remainder, &mut lexer_tokens[1..])),
        Token::DoubleOpenAngleBracket => Ok((BinaryOperator::LeftShift, &mut lexer_tokens[1..])),
        Token::DoubleCloseAngleBracket => Ok((BinaryOperator::RightShift, &mut lexer_tokens[1..])),
        Token::Ampersand => Ok((BinaryOperator::BitwiseAnd, &mut lexer_tokens[1..])),
        Token::Caret => Ok((BinaryOperator::BitwiseXOr, &mut lexer_tokens[1..])),
        Token::Pipe => Ok((BinaryOperator::BitwiseOr, &mut lexer_tokens[1..])),
        Token::DoubleAmpersand => Ok((BinaryOperator::And, &mut lexer_tokens[1..])),
        Token::DoublePipe => Ok((BinaryOperator::Or, &mut lexer_tokens[1..])),
        Token::DoubleEqual => Ok((BinaryOperator::Equal, &mut lexer_tokens[1..])),
        Token::ExclamationEqual => Ok((BinaryOperator::NotEqual, &mut lexer_tokens[1..])),
        Token::OpenAngleBracket => Ok((BinaryOperator::LessThan, &mut lexer_tokens[1..])),
        Token::OpenAngleBracketEqual => Ok((BinaryOperator::LessOrEqual, &mut lexer_tokens[1..])),
        Token::CloseAngleBracket => Ok((BinaryOperator::GreaterThan, &mut lexer_tokens[1..])),
        Token::CloseAngleBracketEqual => {
            Ok((BinaryOperator::GreaterOrEqual, &mut lexer_tokens[1..]))
        }
        Token::PlusEqual => Ok((BinaryOperator::SumAssign, &mut lexer_tokens[1..])),
        Token::HyphenEqual => Ok((BinaryOperator::DifferenceAssign, &mut lexer_tokens[1..])),
        Token::AsteriskEqual => Ok((BinaryOperator::ProductAssign, &mut lexer_tokens[1..])),
        Token::ForwardSlashEqual => Ok((BinaryOperator::QuotientAssign, &mut lexer_tokens[1..])),
        Token::PercentSignEqual => Ok((BinaryOperator::RemainderAssign, &mut lexer_tokens[1..])),
        Token::AmpersandEqual => Ok((BinaryOperator::BitwiseAndAssign, &mut lexer_tokens[1..])),
        Token::PipeEqual => Ok((BinaryOperator::BitwiseOrAssign, &mut lexer_tokens[1..])),
        Token::CaretEqual => Ok((BinaryOperator::BitwiseXOrAssign, &mut lexer_tokens[1..])),
        Token::DoubleOpenAngleBracketEqual => {
            Ok((BinaryOperator::LeftShiftAssign, &mut lexer_tokens[1..]))
        }
        Token::DoubleCloseAngleBracketEqual => {
            Ok((BinaryOperator::RightShiftAssign, &mut lexer_tokens[1..]))
        }
        _ => Err(fail()),
    }
}

fn get_binary_operator_precedence(token: &Token) -> Option<u8> {
    match token {
        Token::Hyphen => Some(BinaryOperator::Subtract.precedence()),
        Token::Plus => Some(BinaryOperator::Add.precedence()),
        Token::Asterisk => Some(BinaryOperator::Multiply.precedence()),
        Token::ForwardSlash => Some(BinaryOperator::Divide.precedence()),
        Token::PercentSign => Some(BinaryOperator::Remainder.precedence()),
        Token::DoubleOpenAngleBracket => Some(BinaryOperator::LeftShift.precedence()),
        Token::DoubleCloseAngleBracket => Some(BinaryOperator::RightShift.precedence()),
        Token::Ampersand => Some(BinaryOperator::BitwiseAnd.precedence()),
        Token::Caret => Some(BinaryOperator::BitwiseXOr.precedence()),
        Token::Pipe => Some(BinaryOperator::BitwiseOr.precedence()),
        Token::DoubleAmpersand => Some(BinaryOperator::And.precedence()),
        Token::DoublePipe => Some(BinaryOperator::Or.precedence()),
        Token::DoubleEqual => Some(BinaryOperator::Equal.precedence()),
        Token::ExclamationEqual => Some(BinaryOperator::NotEqual.precedence()),
        Token::OpenAngleBracket => Some(BinaryOperator::LessThan.precedence()),
        Token::OpenAngleBracketEqual => Some(BinaryOperator::LessOrEqual.precedence()),
        Token::CloseAngleBracket => Some(BinaryOperator::GreaterThan.precedence()),
        Token::CloseAngleBracketEqual => Some(BinaryOperator::GreaterOrEqual.precedence()),
        Token::Equal => Some(BinaryOperator::Assign.precedence()),
        Token::PlusEqual => Some(BinaryOperator::SumAssign.precedence()),
        Token::HyphenEqual => Some(BinaryOperator::DifferenceAssign.precedence()),
        Token::AsteriskEqual => Some(BinaryOperator::ProductAssign.precedence()),
        Token::ForwardSlashEqual => Some(BinaryOperator::QuotientAssign.precedence()),
        Token::PercentSignEqual => Some(BinaryOperator::RemainderAssign.precedence()),
        Token::AmpersandEqual => Some(BinaryOperator::BitwiseAndAssign.precedence()),
        Token::PipeEqual => Some(BinaryOperator::BitwiseOrAssign.precedence()),
        Token::CaretEqual => Some(BinaryOperator::BitwiseXOrAssign.precedence()),
        Token::DoubleOpenAngleBracketEqual => Some(BinaryOperator::LeftShiftAssign.precedence()),
        Token::DoubleCloseAngleBracketEqual => Some(BinaryOperator::RightShiftAssign.precedence()),
        Token::QuestionMark => Some(BinaryOperator::Conditional.precedence()),
        Token::Identifier(_)
        | Token::Constant(_)
        | Token::Int
        | Token::Void
        | Token::If
        | Token::Else
        | Token::Return
        | Token::OpenParenthesis
        | Token::CloseParenthesis
        | Token::OpenBrace
        | Token::CloseBrace
        | Token::Semicolon
        | Token::Tilde
        | Token::Exclamation
        | Token::DoubleHyphen
        | Token::DoublePlus
        | Token::Colon
        | Token::Goto
        | Token::Break
        | Token::Continue
        | Token::Do
        | Token::While
        | Token::For
        | Token::Switch
        | Token::Case
        | Token::Comma
        | Token::Default
        | Token::Static
        | Token::Extern => None,
    }
}

fn parse_postfix_operator(lexer_tokens: &[Token]) -> Option<UnaryOperator> {
    match &lexer_tokens[0] {
        Token::DoubleHyphen => Some(UnaryOperator::PostfixDecrement),
        Token::DoublePlus => Some(UnaryOperator::PostfixIncrement),
        Token::Identifier(_)
        | Token::Constant(_)
        | Token::Int
        | Token::Void
        | Token::If
        | Token::Else
        | Token::Return
        | Token::OpenParenthesis
        | Token::CloseParenthesis
        | Token::OpenBrace
        | Token::CloseBrace
        | Token::Semicolon
        | Token::Tilde
        | Token::Hyphen
        | Token::Plus
        | Token::Asterisk
        | Token::ForwardSlash
        | Token::PercentSign
        | Token::Pipe
        | Token::DoublePipe
        | Token::Ampersand
        | Token::DoubleAmpersand
        | Token::Caret
        | Token::OpenAngleBracket
        | Token::DoubleOpenAngleBracket
        | Token::CloseAngleBracket
        | Token::DoubleCloseAngleBracket
        | Token::Exclamation
        | Token::Equal
        | Token::DoubleEqual
        | Token::ExclamationEqual
        | Token::OpenAngleBracketEqual
        | Token::CloseAngleBracketEqual
        | Token::PlusEqual
        | Token::HyphenEqual
        | Token::AsteriskEqual
        | Token::ForwardSlashEqual
        | Token::PercentSignEqual
        | Token::AmpersandEqual
        | Token::PipeEqual
        | Token::CaretEqual
        | Token::DoubleOpenAngleBracketEqual
        | Token::DoubleCloseAngleBracketEqual
        | Token::QuestionMark
        | Token::Colon
        | Token::Goto
        | Token::Break
        | Token::Continue
        | Token::Do
        | Token::While
        | Token::For
        | Token::Switch
        | Token::Case
        | Token::Default
        | Token::Comma
        | Token::Static
        | Token::Extern => None,
    }
}

pub fn run_parser(lexer_tokens: Vec<Token>) -> Result<AbstractSyntaxTree> {
    let program = parse_program(lexer_tokens).context("parse phase")?;
    Ok(AbstractSyntaxTree::Program(program))
}
