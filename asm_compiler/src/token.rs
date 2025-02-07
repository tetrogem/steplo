use std::sync::Arc;

use anyhow::bail;

#[derive(Debug)]
pub enum Token {
    Op(Op),
    Value(Arc<str>),
    Comment(Arc<str>),
    Hashtag,
    Eol,
}

#[derive(Clone, Copy, Debug)]
pub enum Op {
    Lit,
    Add,
    Sub,
    Move,
    MoveDerefDest,
    MoveDerefSrc,
    Jump,
    Branch,
    Out,
    Eq,
    Not,
    Exit,
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
                _ if next.is_whitespace() => {
                    chars.next(); // ignore whitespace
                    None
                },
                _ if next.is_alphabetic() => {
                    let mut word = String::new();
                    while let Some(next) = chars.next_if(|c| c.is_alphabetic()) {
                        word.push(next);
                    }

                    let op = match word.to_lowercase().as_str() {
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
                        _ => bail!("Invalid operator: {}", word),
                    };

                    Some(Token::Op(op))
                },
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
