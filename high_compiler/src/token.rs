use std::{iter::Peekable, str::FromStr};

use anyhow::bail;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Comword(Comword),
    Eq,
    Semi,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Pipe,
    Comma,
    Literal(String),
    Deref,
    Main,
    Func,
    If,
    Else,
    While,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comword {
    Literal,
    Eq,
    Not,
    Add,
    Ref,
    CopyDeref,
    Copy,
    Sub,
}

impl FromStr for Comword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let comword = match s {
            "lit" => Self::Literal,
            "eq" => Self::Eq,
            "not" => Self::Not,
            "add" => Self::Add,
            "ref" => Self::Ref,
            "copy_deref" => Self::CopyDeref,
            "copy" => Self::Copy,
            "sub" => Self::Sub,
            _ => return Err(()),
        };

        Ok(comword)
    }
}

fn consume_char(chars: &mut impl Iterator<Item = char>, token: Option<Token>) -> Option<Token> {
    chars.next();
    token
}

fn consume_literal(chars: &mut impl Iterator<Item = char>) -> anyhow::Result<Token> {
    let Some('"') = chars.next() else { bail!("Expected literal opening quote") };

    let mut value = String::new();
    let mut closed = false;
    for c in chars {
        if c == '"' {
            closed = true;
            break;
        }

        value.push(c);
    }

    if !closed {
        bail!("Expected literal closing quote");
    }

    Ok(Token::Literal(value))
}

fn consume_word(chars: &mut Peekable<impl Iterator<Item = char>>) -> anyhow::Result<Token> {
    let mut word = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_alphanumeric() || c == '_' {
            word.push(c);
            chars.next();
        } else {
            break;
        }
    }

    let token = match word.as_str() {
        "main" => Token::Main,
        "func" => Token::Func,
        "deref" => Token::Deref,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        w => match Comword::from_str(w) {
            Ok(comword) => Token::Comword(comword),
            Err(()) => {
                if word.is_empty() {
                    bail!("Word is empty");
                }

                Token::Name(word)
            },
        },
    };

    Ok(token)
}

pub fn tokenize(code: &str) -> anyhow::Result<Vec<Token>> {
    let mut chars = code.chars().peekable();
    let mut tokens = Vec::new();

    while let Some(peek) = chars.peek() {
        let token = match *peek {
            '=' => consume_char(&mut chars, Some(Token::Eq)),
            ';' => consume_char(&mut chars, Some(Token::Semi)),
            '(' => consume_char(&mut chars, Some(Token::LeftParen)),
            ')' => consume_char(&mut chars, Some(Token::RightParen)),
            '{' => consume_char(&mut chars, Some(Token::LeftBrace)),
            '}' => consume_char(&mut chars, Some(Token::RightBrace)),
            '|' => consume_char(&mut chars, Some(Token::Pipe)),
            ',' => consume_char(&mut chars, Some(Token::Comma)),
            '"' => Some(consume_literal(&mut chars)?),
            c if c.is_whitespace() => consume_char(&mut chars, None),
            _ => Some(consume_word(&mut chars)?),
        };

        if let Some(token) = token {
            tokens.push(token);
        }
    }

    Ok(tokens)
}
