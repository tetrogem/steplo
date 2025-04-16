use std::str::FromStr;

use anyhow::bail;
use itertools::{Itertools, MultiPeek};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Eq,
    Semi,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Pipe,
    Comma,
    Literal(String),
    Main,
    Func,
    If,
    Else,
    While,
    LeftBracket,
    RightBracket,
    Slice,
    Period,
    Comment(String),
    Asterisk,
    Slash,
    Bang,
    Plus,
    Dash,
    Tilde,
    LeftAngle,
    RightAngle,
    Percent,
    Ampersand,
    Set,
    Call,
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

fn consume_word(chars: &mut MultiPeek<impl Iterator<Item = char>>) -> anyhow::Result<Token> {
    let mut word = String::new();
    while let Some(&c) = {
        chars.reset_peek();
        chars.peek()
    } {
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
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "slice" => Token::Slice,
        "set" => Token::Set,
        "call" => Token::Call,
        _ => {
            if word.is_empty() {
                bail!("Word is empty");
            }

            if word.chars().next().map(|c| c.is_numeric()).unwrap_or(false) {
                Token::Literal(word)
            } else {
                Token::Name(word)
            }
        },
    };

    Ok(token)
}

fn consume_comment(chars: &mut impl Iterator<Item = char>) -> anyhow::Result<Token> {
    let Some('/') = chars.next() else { bail!("Expected //") };
    let Some('/') = chars.next() else { bail!("Expected //") };

    let mut comment = String::new();
    for c in chars.by_ref() {
        if c == '\n' {
            break;
        }

        comment.push(c);
    }

    Ok(Token::Comment(comment))
}

pub fn tokenize(code: &str) -> anyhow::Result<Vec<Token>> {
    let mut chars = code.chars().multipeek();
    let mut tokens = Vec::new();

    while let Some(peek) = {
        chars.reset_peek();
        chars.peek()
    } {
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
            '*' => consume_char(&mut chars, Some(Token::Asterisk)),
            '!' => consume_char(&mut chars, Some(Token::Bang)),
            '+' => consume_char(&mut chars, Some(Token::Plus)),
            '-' => consume_char(&mut chars, Some(Token::Dash)),
            '~' => consume_char(&mut chars, Some(Token::Tilde)),
            '<' => consume_char(&mut chars, Some(Token::LeftAngle)),
            '>' => consume_char(&mut chars, Some(Token::RightAngle)),
            '%' => consume_char(&mut chars, Some(Token::Percent)),
            '&' => consume_char(&mut chars, Some(Token::Ampersand)),
            '"' => Some(consume_literal(&mut chars)?),
            '/' => match chars.peek() {
                Some('/') => Some(consume_comment(&mut chars)?),
                _ => consume_char(&mut chars, Some(Token::Slash)),
            },
            c if c.is_whitespace() => consume_char(&mut chars, None),
            _ => Some(consume_word(&mut chars)?),
        };

        if let Some(token) = token {
            tokens.push(token);
        }
    }

    Ok(tokens)
}
