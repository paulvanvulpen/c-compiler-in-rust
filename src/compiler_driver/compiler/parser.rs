use super::lexer::Token;
use anyhow::{Context, Error, Result, anyhow};
use std::mem::discriminant;

mod visualize;

// Implementation AST Nodes in Zephyr Abstract Syntax Description Language (ASDL)
// program = Program(function_definition)
// function_definition = Function(identifier name, block_item* body)
// statement = Return(exp)
//              | If(exp condition, statement then, statement? else)
//              | Expression(exp)
//              | Compound(block)
//              | Break
//              | Continue
//              | While(exp condition, statement body)
//              | DoWhile(statement body, exp condition)
//              | For(for_init init, exp? condition, exp? post, statement body)
//              | Goto(identifier name)
//              | Label(identifier name)
//              | Null
// declaration = Declaration(identifier name, exp? init)
// for_init = InitDecl(declaration), InitExp(exp?)
// block_item = S(statement) | D(declaration)
// exp = Constant(int) | Var(identifier) | Unary(unary_operator, exp) | Binary(binary_operator, exp, exp) | Assignment(exp, exp)
// unary_operator = Complement | Negate | Not | PrefixDecrement | PostfixDecrement | PrefixIncrement | PostfixIncrement
// binary_operator = Add | Subtract | Multiply | Divide | Remainder | And | Or | Equal | NotEqual | LessThan | LessOrEqual | GreaterThan | GreaterOrEqual

pub enum AbstractSyntaxTree {
    Program(Program),
}

// Comments on the enums in Extended Backus-Naur form (EBNF)

// <program>
pub enum Program {
    Program(FunctionDefinition),
}

// <function>
pub enum FunctionDefinition {
    Function { identifier: String, body: Block },
}

pub enum Block {
    Block(Vec<BlockItem>),
}

// <block-item>
pub enum BlockItem {
    Statement(Statement),
    Declaration(Declaration),
}

impl Default for BlockItem {
    fn default() -> Self {
        BlockItem::Statement(Default::default())
    }
}

// <declaration>
pub enum Declaration {
    Declaration {
        identifier: String,
        init: Option<Expression>,
    },
}

// <statement>
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
    Label(String),
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
    #[default]
    Null,
}

// <for-init>
pub enum ForInit {
    InitialDeclaration(Declaration),
    InitialOptionalExpression(Option<Expression>),
}

// <exp>
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
}

impl Default for Expression {
    fn default() -> Self {
        Expression::Constant(Default::default())
    }
}

// <unop>
pub enum UnaryOperator {
    Complement,
    Negate,
    Not,
    PrefixDecrement,
    PostfixDecrement,
    PrefixIncrement,
    PostfixIncrement,
}

// <binop>
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

// <program> ::= <function>
fn parse_program(lexer_tokens: &[Token]) -> Result<Program> {
    match parse_function(lexer_tokens) {
        Ok(function_definition) => Ok(Program::Program(function_definition)),
        Err(e) => Err(e),
    }
}

// <function> ::= "int" <identifier> "(" "void" ")" <block>
fn parse_function(lexer_tokens: &[Token]) -> Result<FunctionDefinition> {
    let lexer_tokens = expect(Token::Int, lexer_tokens).context("Parsing a function")?;
    let (identifier, lexer_tokens) =
        parse_identifier(lexer_tokens).context("Parsing a function")?;
    let lexer_tokens =
        expect(Token::OpenParenthesis, lexer_tokens).context("Parsing a function")?;
    let lexer_tokens = expect(Token::Void, lexer_tokens).context("Parsing a function")?;
    let lexer_tokens =
        expect(Token::CloseParenthesis, lexer_tokens).context("Parsing a function")?;
    let (body, lexer_tokens) = parse_block(lexer_tokens).context("Parsing a function")?;

    match lexer_tokens {
        t if t.is_empty() => Ok(FunctionDefinition::Function { identifier, body }),
        _ => Err(fail()),
    }
}

// <identifier> ::= ? An identifier token ?
fn parse_identifier(lexer_tokens: &[Token]) -> Result<(String, &[Token])> {
    match &lexer_tokens[0] {
        Token::Identifier(identifier) => Ok((identifier.clone(), &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

// <block> ::= "{" { <block_item> } "}"
fn parse_block(lexer_tokens: &[Token]) -> Result<(Block, &[Token])> {
    let mut lexer_tokens = expect(Token::OpenBrace, lexer_tokens)?;
    let mut block = vec![];

    while !matches!(&lexer_tokens[0], Token::CloseBrace) {
        let block_item;
        (block_item, lexer_tokens) =
            parse_block_item(&lexer_tokens).context("Parsing a compound statement")?;
        block.push(block_item);
    }

    Ok((Block::Block(block), &lexer_tokens[1..]))
}

// <block-item> ::= <statement> | <declaration>
fn parse_block_item(lexer_tokens: &[Token]) -> Result<(BlockItem, &[Token])> {
    match &lexer_tokens[0] {
        Token::Int => {
            let (declaration, lexer_tokens) =
                parse_declaration(&lexer_tokens).context("Parsing a block item declaration")?;
            Ok((BlockItem::Declaration(declaration), &lexer_tokens))
        }
        _ => {
            let (statement, lexer_tokens) =
                parse_statement(&lexer_tokens).context("Parsing a block item statement")?;
            Ok((BlockItem::Statement(statement), &lexer_tokens))
        }
    }
}

// <declaration> ::= "int" <identifier> [ "=" <exp> ]
fn parse_declaration(lexer_tokens: &[Token]) -> Result<(Declaration, &[Token])> {
    let lexer_tokens = expect(Token::Int, lexer_tokens)?;
    let (identifier, lexer_tokens) = parse_identifier(lexer_tokens)?;
    if !matches!(lexer_tokens[0], Token::Semicolon) {
        let lexer_tokens = expect(Token::Equal, lexer_tokens)?;
        let (expression, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
        let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
        Ok((
            Declaration::Declaration {
                identifier,
                init: Some(expression),
            },
            lexer_tokens,
        ))
    } else {
        let lexer_tokens = expect(Token::Semicolon, lexer_tokens)?;
        Ok((
            Declaration::Declaration {
                identifier,
                init: None,
            },
            lexer_tokens,
        ))
    }
}

// <statement> ::= "return" <exp> ";"
//      | <exp> ";"
//      | "if" "(" <exp> ")" <statement> [ "else" <statement> ]
//      | "goto" identifier ";"
//      | identifier ":"
//      | <block>
//      | "break" ";"
//      | "continue" ";"
//      | "while" "(" <exp> ")" <statement>
//      | "do <statement> "while" "(" <exp> ")" ";"
//      | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
//      | ";"
fn parse_statement(lexer_tokens: &[Token]) -> Result<(Statement, &[Token])> {
    match &lexer_tokens[0] {
        Token::Return => {
            let lexer_tokens = &lexer_tokens[1..];
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("Parsing a return statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a return statement")?;
            Ok((Statement::Return(expression), lexer_tokens))
        }
        Token::If => {
            let lexer_tokens = &lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::OpenParenthesis, lexer_tokens).context("Parsing an if statement")?;
            let (condition, lexer_tokens) = parse_expression(lexer_tokens, 0)?;
            let lexer_tokens =
                expect(Token::CloseParenthesis, lexer_tokens).context("Parsing an if statement")?;
            let (then_statement, lexer_tokens) =
                parse_statement(lexer_tokens).context("Parsing an if statement")?;
            match lexer_tokens[0] {
                Token::Else => {
                    let lexer_tokens = &lexer_tokens[1..];
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
            let lexer_tokens = &lexer_tokens[1..];
            let (identifier, lexer_tokens) =
                parse_identifier(lexer_tokens).context("Parsing a goto statement")?;
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a goto statement")?;
            Ok((Statement::Goto(identifier), &lexer_tokens))
        }
        Token::Semicolon => Ok((Statement::Null, &lexer_tokens[1..])),
        Token::OpenBrace => {
            let (block, lexer_tokens) =
                parse_block(lexer_tokens).context("Parsing a compound statement")?;
            Ok((Statement::Compound(block), &lexer_tokens))
        }
        Token::Break => {
            let lexer_tokens = &lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a break statement")?;
            Ok((Statement::Break { label: None }, &lexer_tokens))
        }
        Token::Continue => {
            let lexer_tokens = &lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::Semicolon, lexer_tokens).context("Parsing a continue statement")?;
            Ok((Statement::Continue { label: None }, &lexer_tokens))
        }
        Token::While => {
            let lexer_tokens = &lexer_tokens[1..];
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
                &lexer_tokens,
            ))
        }
        Token::Do => {
            let lexer_tokens = &lexer_tokens[1..];
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
                &lexer_tokens,
            ))
        }
        // | "for" "(" <for-init> [ <exp> ] ";" [ <exp> ] ")" <statement>
        Token::For => {
            let lexer_tokens = &lexer_tokens[1..];
            let lexer_tokens =
                expect(Token::OpenParenthesis, lexer_tokens).context("Parsing a for statement")?;
            let (init, lexer_tokens) =
                parse_for_init(lexer_tokens).context("Parsing a for statement")?;
            let (condition, lexer_tokens) = match &lexer_tokens[0] {
                Token::Semicolon => (None, &lexer_tokens[1..]),
                _ => {
                    let (expression, lexer_tokens) =
                        parse_expression(lexer_tokens, 0).context("Parsing a for statement")?;
                    let lexer_tokens = expect(Token::Semicolon, lexer_tokens)
                        .context("Parsing a for statement")?;
                    (Some(expression), lexer_tokens)
                }
            };
            let (post, lexer_tokens) = match &lexer_tokens[0] {
                Token::CloseParenthesis => (None, &lexer_tokens[1..]),
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
                &lexer_tokens,
            ))
        }
        _ => match &lexer_tokens[0..2] {
            [Token::Identifier(identifier), Token::Colon] => {
                Ok((Statement::Label(identifier.clone()), &lexer_tokens[2..]))
            }
            _ => {
                let (expression, lexer_tokens) =
                    parse_expression(lexer_tokens, 0).context("Parsing an expression statement")?;
                let lexer_tokens = expect(Token::Semicolon, lexer_tokens)
                    .context("Parsing an expression statement")?;
                Ok((Statement::Expression(expression), &lexer_tokens))
            }
        },
    }
}

// <for-init> ::= <declaration> | [ <exp> ] ";"
fn parse_for_init(lexer_tokens: &[Token]) -> Result<(ForInit, &[Token])> {
    match &lexer_tokens[0] {
        Token::Int => {
            let (declaration, lexer_tokens) =
                parse_declaration(lexer_tokens).context("Parsing a for_init statement")?;
            Ok((ForInit::InitialDeclaration(declaration), lexer_tokens))
        }
        Token::Semicolon => Ok((ForInit::InitialOptionalExpression(None), &lexer_tokens[1..])),
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
        | Token::For => None,
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
        | Token::For => None,
    }
}

// <primary> ::= <int> | <identifier> [ <postfix_op> ] | "(" <exp> ")" [ <postfix_op> ]
fn parse_primary(lexer_tokens: &[Token]) -> Result<(Expression, &[Token])> {
    match &lexer_tokens[0] {
        Token::Constant(constant) => Ok((Expression::Constant(*constant), &lexer_tokens[1..])),
        Token::Identifier(identifier) => {
            let mut left = Expression::Var {
                identifier: identifier.clone(),
            };
            let mut lexer_tokens = &lexer_tokens[1..];
            while let Some(unary_operator) = parse_postfix_operator(&lexer_tokens) {
                left = Expression::Unary(unary_operator, Box::new(left));
                lexer_tokens = &lexer_tokens[1..];
            }
            Ok((left, lexer_tokens))
        }
        Token::OpenParenthesis => {
            let lexer_tokens = &lexer_tokens[1..];
            let (expression, lexer_tokens) =
                parse_expression(lexer_tokens, 0).context("parsing a primary expression")?;
            let lexer_tokens = expect(Token::CloseParenthesis, lexer_tokens)
                .context("parsing a primary expression")?;
            let mut left = expression;
            let mut lexer_tokens = &lexer_tokens[..];
            while let Some(unary_operator) = parse_postfix_operator(&lexer_tokens) {
                left = Expression::Unary(unary_operator, Box::new(left));
                lexer_tokens = &lexer_tokens[1..];
            }
            Ok((left, lexer_tokens))
        }
        _ => Err(anyhow!(
            "Syntax error, found token {:?} while parsing a primary expression",
            &lexer_tokens[0]
        )),
    }
}

// unary_exp> ::= <prefix_op> <unary_exp> | <primary>
fn parse_unary_expression(lexer_tokens: &[Token]) -> Result<(Expression, &[Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen
        | Token::Tilde
        | Token::Exclamation
        | Token::DoubleHyphen
        | Token::DoublePlus => {
            let (unary_op, lexer_tokens) = parse_prefix_operator(&lexer_tokens)
                .context("parsing a prefix unary expression")?;
            let (primary, lexer_tokens) = parse_unary_expression(lexer_tokens)
                .context("parsing a prefix unary expression")?;
            Ok((Expression::Unary(unary_op, Box::new(primary)), lexer_tokens))
        }
        _ => parse_primary(lexer_tokens),
    }
}

// <exp> ::= <unary_exp> | <exp> <binop> <exp> | <exp> "?" <exp> ":" <exp>
fn parse_expression(lexer_tokens: &[Token], min_precedence: u8) -> Result<(Expression, &[Token])> {
    let (mut left, mut lexer_tokens) =
        parse_unary_expression(lexer_tokens).context("parsing a unary expression")?;
    let mut right;
    let mut binary_operator;
    while get_binary_operator_precedence(&lexer_tokens[0]) >= Some(min_precedence) {
        match &lexer_tokens[0] {
            Token::Equal => {
                lexer_tokens = &lexer_tokens[1..];
                (right, lexer_tokens) =
                    parse_expression(lexer_tokens, BinaryOperator::Assign.precedence())
                        .context("parsing an assignment expression")?;
                left = Expression::Assignment(Box::new(left), Box::new(right));
            }
            Token::QuestionMark => {
                lexer_tokens = &lexer_tokens[1..];
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
                (binary_operator, lexer_tokens) = parse_binary_operator(&lexer_tokens)?;
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

fn parse_binary_operator(lexer_tokens: &[Token]) -> Result<(BinaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Plus => Ok((BinaryOperator::Add, &lexer_tokens[1..])),
        Token::Hyphen => Ok((BinaryOperator::Subtract, &lexer_tokens[1..])),
        Token::Asterisk => Ok((BinaryOperator::Multiply, &lexer_tokens[1..])),
        Token::ForwardSlash => Ok((BinaryOperator::Divide, &lexer_tokens[1..])),
        Token::PercentSign => Ok((BinaryOperator::Remainder, &lexer_tokens[1..])),
        Token::DoubleOpenAngleBracket => Ok((BinaryOperator::LeftShift, &lexer_tokens[1..])),
        Token::DoubleCloseAngleBracket => Ok((BinaryOperator::RightShift, &lexer_tokens[1..])),
        Token::Ampersand => Ok((BinaryOperator::BitwiseAnd, &lexer_tokens[1..])),
        Token::Caret => Ok((BinaryOperator::BitwiseXOr, &lexer_tokens[1..])),
        Token::Pipe => Ok((BinaryOperator::BitwiseOr, &lexer_tokens[1..])),
        Token::DoubleAmpersand => Ok((BinaryOperator::And, &lexer_tokens[1..])),
        Token::DoublePipe => Ok((BinaryOperator::Or, &lexer_tokens[1..])),
        Token::DoubleEqual => Ok((BinaryOperator::Equal, &lexer_tokens[1..])),
        Token::ExclamationEqual => Ok((BinaryOperator::NotEqual, &lexer_tokens[1..])),
        Token::OpenAngleBracket => Ok((BinaryOperator::LessThan, &lexer_tokens[1..])),
        Token::OpenAngleBracketEqual => Ok((BinaryOperator::LessOrEqual, &lexer_tokens[1..])),
        Token::CloseAngleBracket => Ok((BinaryOperator::GreaterThan, &lexer_tokens[1..])),
        Token::CloseAngleBracketEqual => Ok((BinaryOperator::GreaterOrEqual, &lexer_tokens[1..])),
        Token::PlusEqual => Ok((BinaryOperator::SumAssign, &lexer_tokens[1..])),
        Token::HyphenEqual => Ok((BinaryOperator::DifferenceAssign, &lexer_tokens[1..])),
        Token::AsteriskEqual => Ok((BinaryOperator::ProductAssign, &lexer_tokens[1..])),
        Token::ForwardSlashEqual => Ok((BinaryOperator::QuotientAssign, &lexer_tokens[1..])),
        Token::PercentSignEqual => Ok((BinaryOperator::RemainderAssign, &lexer_tokens[1..])),
        Token::AmpersandEqual => Ok((BinaryOperator::BitwiseAndAssign, &lexer_tokens[1..])),
        Token::PipeEqual => Ok((BinaryOperator::BitwiseOrAssign, &lexer_tokens[1..])),
        Token::CaretEqual => Ok((BinaryOperator::BitwiseXOrAssign, &lexer_tokens[1..])),
        Token::DoubleOpenAngleBracketEqual => {
            Ok((BinaryOperator::LeftShiftAssign, &lexer_tokens[1..]))
        }
        Token::DoubleCloseAngleBracketEqual => {
            Ok((BinaryOperator::RightShiftAssign, &lexer_tokens[1..]))
        }
        _ => Err(fail()),
    }
}

// <unop> ::= "-" | "~" | "!" | -- | ++
fn parse_prefix_operator(lexer_tokens: &[Token]) -> Result<(UnaryOperator, &[Token])> {
    match &lexer_tokens[0] {
        Token::Hyphen => Ok((UnaryOperator::Negate, &lexer_tokens[1..])),
        Token::Tilde => Ok((UnaryOperator::Complement, &lexer_tokens[1..])),
        Token::Exclamation => Ok((UnaryOperator::Not, &lexer_tokens[1..])),
        Token::DoubleHyphen => Ok((UnaryOperator::PrefixDecrement, &lexer_tokens[1..])),
        Token::DoublePlus => Ok((UnaryOperator::PrefixIncrement, &lexer_tokens[1..])),
        _ => Err(fail()),
    }
}

fn expect(expected: Token, lexer_tokens: &[Token]) -> Result<&[Token]> {
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
        _ => Ok(&lexer_tokens[1..]),
    }
}

fn fail() -> Error {
    anyhow!("Syntax error")
}

pub fn run_parser(lexer_tokens: &[Token]) -> Result<AbstractSyntaxTree> {
    let program = parse_program(lexer_tokens).context("parse phase")?;
    Ok(AbstractSyntaxTree::Program(program))
}
