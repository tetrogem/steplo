use std::{iter::Peekable, sync::Arc};

use anyhow::bail;

#[derive(Debug)]
pub enum Token {
    Op(Op),
    Value(Arc<str>),
    Comment(Arc<str>),
    Hashtag,
    Eol,
    Register(Arc<str>),
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
    // memory
    Lit,
    Move,
    MoveDerefDest,
    MoveDerefSrc,
    // control
    Jump,
    Branch,
    Exit,
    // io
    In,
    Out,
    // math
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    // inequality
    Eq,
    Neq,
    Gt,
    Lt,
    Gte,
    Lte,
    // boolean
    And,
    Or,
    Xor,
    Not,
    // string
    Join,
}

pub fn tokenize(input: String) -> anyhow::Result<Vec<Arc<Token>>> {
    let mut tokens = Vec::<Arc<Token>>::new();
    for line in input.lines() {
        let line = line.trim();
        if line.is_empty() {
            continue;
        }

        let mut chars = line.chars().peekable();

        while let Some(next) = chars.peek() {
            let token = match next {
                '"' => {
                    let Some('"') = chars.next() else { bail!("Expected opening quote") };

                    let mut word = String::new();
                    let mut closed = false;
                    for next in chars.by_ref() {
                        if next == '"' {
                            closed = true;
                            break;
                        }

                        word.push(next);
                    }

                    if !closed {
                        bail!("Expected closing quote");
                    }

                    Some(Token::Value(word.into()))
                },
                ';' => {
                    let Some(';') = chars.next() else { bail!("Expected semicolon") };
                    let mut comment = String::new();
                    for c in chars.by_ref() {
                        comment.push(c);
                    }
                    Some(Token::Comment(comment.into()))
                },
                '#' => {
                    let Some('#') = chars.next() else { bail!("Expected hashtag") };
                    Some(Token::Hashtag)
                },
                '$' => {
                    let Some('$') = chars.next() else { bail!("Expected dollar sign") };
                    let name = read_word(&mut chars).into();
                    Some(Token::Register(name))
                },
                c if c.is_whitespace() => {
                    chars.next(); // ignore whitespace
                    None
                },
                c if c.is_alphabetic() => {
                    let op = match read_word(&mut chars).to_lowercase().as_str() {
                        "lit" => Op::Lit,
                        "add" => Op::Add,
                        "sub" => Op::Sub,
                        "mv" => Op::Move,
                        "mvdd" => Op::MoveDerefDest,
                        "mvds" => Op::MoveDerefSrc,
                        "jmp" => Op::Jump,
                        "brc" => Op::Branch,
                        "out" => Op::Out,
                        "eq" => Op::Eq,
                        "not" => Op::Not,
                        "exit" => Op::Exit,
                        "in" => Op::In,
                        "mul" => Op::Mul,
                        "div" => Op::Div,
                        "mod" => Op::Mod,
                        "neq" => Op::Neq,
                        "gt" => Op::Gt,
                        "lt" => Op::Lt,
                        "gte" => Op::Gte,
                        "lte" => Op::Lte,
                        "and" => Op::And,
                        "or" => Op::Or,
                        "xor" => Op::Xor,
                        "join" => Op::Join,
                        word => bail!("Invalid operator: `{}`", word),
                    };

                    Some(Token::Op(op))
                },
                _ => bail!("Unexpected character: {next}"),
            };

            if let Some(token) = token {
                tokens.push(Arc::new(token));
            }
        }

        tokens.push(Arc::new(Token::Eol));
    }

    Ok(tokens)
}

fn read_word(chars: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut word = String::new();
    while let Some(next) = chars.next_if(|c| c.is_alphabetic()) {
        word.push(next);
    }

    word
}
