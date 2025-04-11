use std::{iter::Peekable, str::FromStr};

use anyhow::bail;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Comword(Opword),
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
    Ref,
    LeftBracket,
    RightBracket,
    Slice,
    Period,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Opword {
    // memory
    Deref,
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

impl FromStr for Opword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let comword = match s {
            "deref" => Opword::Deref,
            "add" => Opword::Add,
            "sub" => Opword::Sub,
            "mul" => Opword::Mul,
            "div" => Opword::Div,
            "mod" => Opword::Mod,
            "eq" => Opword::Eq,
            "neq" => Opword::Neq,
            "gt" => Opword::Gt,
            "lt" => Opword::Lt,
            "gte" => Opword::Gte,
            "lte" => Opword::Lte,
            "and" => Opword::And,
            "or" => Opword::Or,
            "xor" => Opword::Xor,
            "not" => Opword::Not,
            "join" => Opword::Join,
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
        "ref" => Token::Ref,
        "deref" => Token::Deref,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "slice" => Token::Slice,
        w => match Opword::from_str(w) {
            Ok(comword) => Token::Comword(comword),
            Err(()) => {
                if word.is_empty() {
                    bail!("Word is empty");
                }

                if word.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
                    Token::Literal(word)
                } else {
                    Token::Name(word)
                }
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
            '[' => consume_char(&mut chars, Some(Token::LeftBracket)),
            ']' => consume_char(&mut chars, Some(Token::RightBracket)),
            '|' => consume_char(&mut chars, Some(Token::Pipe)),
            ',' => consume_char(&mut chars, Some(Token::Comma)),
            '.' => consume_char(&mut chars, Some(Token::Period)),
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
