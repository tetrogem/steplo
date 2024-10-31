use std::sync::Arc;

use anyhow::bail;

use crate::token;

#[derive(Debug)]
pub enum Command {
    Push { val: Value },
    Set { dest: Value, val: Value },
    Load { dest: Value, src: Value },
    Store { dest: Value, src: Value },
    Add { dest: Value, left: Value, right: Value },
    Sub { dest: Value, left: Value, right: Value },
}

#[derive(Debug)]
pub struct Value {
    pub str: Arc<str>,
}

pub fn parse<'a>(
    tokens: impl Iterator<Item = &'a token::Token>,
) -> anyhow::Result<Vec<Arc<Command>>> {
    let mut commands = Vec::<Arc<Command>>::new();

    let mut tokens = tokens.into_iter().peekable();
    while let Some(next) = tokens.peek() {
        dbg!(next);
        let command = match next {
            token::Token::Op(_) => {
                let Some(token::Token::Op(op_token)) = tokens.next() else { bail!("Expected op") };

                let command = match op_token {
                    token::Op::Push => {
                        let Some(token::Token::Value(val)) = tokens.next() else {
                            bail!("Expected value for [val]")
                        };
                        Command::Push { val: Value { str: Arc::clone(val) } }
                    },
                    token::Op::Set => {
                        let Some(token::Token::Value(dest)) = tokens.next() else {
                            bail!("Expected value for [dest]")
                        };
                        let Some(token::Token::Value(val)) = tokens.next() else {
                            bail!("Expected value for [val]")
                        };
                        Command::Set {
                            dest: Value { str: Arc::clone(dest) },
                            val: Value { str: Arc::clone(val) },
                        }
                    },
                    token::Op::Load => {
                        let Some(token::Token::Value(dest)) = tokens.next() else {
                            bail!("Expected value for [dest]")
                        };
                        let Some(token::Token::Value(src)) = tokens.next() else {
                            bail!("Expected value for [val]")
                        };
                        Command::Load {
                            dest: Value { str: Arc::clone(dest) },
                            src: Value { str: Arc::clone(src) },
                        }
                    },
                    token::Op::Store => {
                        let Some(token::Token::Value(dest)) = tokens.next() else {
                            bail!("Expected value for [dest]")
                        };
                        let Some(token::Token::Value(src)) = tokens.next() else {
                            bail!("Expected value for [val]")
                        };
                        Command::Store {
                            dest: Value { str: Arc::clone(dest) },
                            src: Value { str: Arc::clone(src) },
                        }
                    },
                    token::Op::Add => {
                        let Some(token::Token::Value(dest)) = tokens.next() else {
                            bail!("Expected value for [dest]")
                        };
                        let Some(token::Token::Value(left)) = tokens.next() else {
                            bail!("Expected value for [left]")
                        };
                        let Some(token::Token::Value(right)) = tokens.next() else {
                            bail!("Expected value for [right]")
                        };
                        Command::Add {
                            dest: Value { str: Arc::clone(dest) },
                            left: Value { str: Arc::clone(left) },
                            right: Value { str: Arc::clone(right) },
                        }
                    },
                    token::Op::Sub => {
                        let Some(token::Token::Value(dest)) = tokens.next() else {
                            bail!("Expected value for [dest]")
                        };
                        let Some(token::Token::Value(left)) = tokens.next() else {
                            bail!("Expected value for [left]")
                        };
                        let Some(token::Token::Value(right)) = tokens.next() else {
                            bail!("Expected value for [right]")
                        };
                        Command::Sub {
                            dest: Value { str: Arc::clone(dest) },
                            left: Value { str: Arc::clone(left) },
                            right: Value { str: Arc::clone(right) },
                        }
                    },
                };

                Some(command)
            },
            token::Token::Value(_) => bail!("Found unexpected value"),
            token::Token::Comment(_) => {
                // comments do not generate code
                tokens.next();
                None
            },
            token::Token::Eol => {
                // ignore extra EOLs
                tokens.next();
                None
            },
        };

        if let Some(command) = command {
            commands.push(Arc::new(command));
        }
    }

    Ok(commands)
}
