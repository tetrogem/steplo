use std::{iter::Peekable, str::FromStr};

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
    Ref,
    Main,
    Func,
    If,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Comword {
    Literal,
    Eq,
    Ref,
    Add,
}

impl FromStr for Comword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let comword = match s {
            "lit" => Self::Literal,
            "eq" => Self::Eq,
            "ref" => Self::Ref,
            "add" => Self::Add,
            _ => return Err(()),
        };

        Ok(comword)
    }
}

fn consume_char(chars: &mut impl Iterator<Item = char>, token: Option<Token>) -> Option<Token> {
    chars.next();
    token
}

fn consume_literal(chars: &mut impl Iterator<Item = char>) -> Result<Token, ()> {
    let Some('"') = chars.next() else { return Err(()) };

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
        return Err(());
    }

    Ok(Token::Literal(value))
}

fn consume_word(chars: &mut Peekable<impl Iterator<Item = char>>) -> Result<Token, ()> {
    let mut word = String::new();
    while let Some(&c) = chars.peek() {
        if c.is_ascii_alphanumeric() {
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
        "if" => Token::If,
        w => match Comword::from_str(w) {
            Ok(comword) => Token::Comword(comword),
            Err(()) => {
                if word.is_empty() {
                    return Err(());
                }

                Token::Name(word)
            },
        },
    };

    Ok(token)
}

pub fn tokenize(code: &str) -> Result<Vec<Token>, ()> {
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
