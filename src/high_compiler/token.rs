use anyhow::bail;
use itertools::{Itertools, MultiPeek};

use crate::srced::{SrcPos, SrcRange, Srced};

#[derive(Debug, Clone, PartialEq, Eq, strum::EnumDiscriminants)]
#[strum_discriminants(name(TokenKind), derive(Hash))]
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
    String(String),
    Main,
    Func,
    If,
    Else,
    While,
    LeftBracket,
    RightBracket,
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
    Colon,
    Digits(String),
    True,
    False,
    Struct,
    Enum,
    Type,
}

fn consume_char(
    chars: &mut impl Iterator<Item = SrcChar>,
    token: Option<Token>,
) -> anyhow::Result<Option<Srced<Token>>> {
    let Some(char) = chars.next() else { bail!("Expected char") };
    Ok(token.map(|token| Srced { val: token, range: SrcRange::exact_pos(char.pos) }))
}

fn consume_string(chars: &mut impl Iterator<Item = SrcChar>) -> anyhow::Result<Srced<Token>> {
    let Some(opening_quote) = chars.next() else { bail!("Expected literal opening quote") };

    if opening_quote.char != '"' {
        bail!("Expected literal opening quote");
    }

    let mut value = String::new();
    let mut range = SrcRange::exact_pos(opening_quote.pos);

    let mut closed = false;
    for c in chars {
        range = range.extend_to(c.pos);

        if c.char == '"' {
            closed = true;
            break;
        }

        value.push(c.char);
    }

    if !closed {
        bail!("Expected literal closing quote");
    }

    Ok(Srced { val: Token::String(value), range })
}

fn consume_word(
    chars: &mut MultiPeek<impl Iterator<Item = SrcChar>>,
) -> anyhow::Result<Srced<Token>> {
    let mut word = String::new();
    let mut range: Option<SrcRange> = None;

    while let Some(&c) = {
        chars.reset_peek();
        chars.peek()
    } {
        if c.char.is_ascii_alphanumeric() || c.char == '_' {
            word.push(c.char);
            chars.next();

            range = Some(match range {
                None => SrcRange::exact_pos(c.pos),
                Some(range) => range.extend_to(c.pos),
            });
        } else {
            break;
        }
    }

    let Some(range) = range else { bail!("Word is empty") };

    let token = match word.as_str() {
        "main" => Token::Main,
        "func" => Token::Func,
        "if" => Token::If,
        "else" => Token::Else,
        "while" => Token::While,
        "true" => Token::True,
        "false" => Token::False,
        "struct" => Token::Struct,
        "enum" => Token::Enum,
        "type" => Token::Type,
        _ => {
            if word.is_empty() {
                bail!("Word is empty");
            }

            Token::Name(word)
        },
    };

    Ok(Srced { val: token, range })
}

fn consume_digits(
    chars: &mut MultiPeek<impl Iterator<Item = SrcChar>>,
) -> anyhow::Result<Srced<Token>> {
    let mut value = String::new();
    let mut range: Option<SrcRange> = None;

    while let Some(&c) = {
        chars.reset_peek();
        chars.peek()
    } {
        if c.char.is_ascii_alphanumeric() || c.char == '_' {
            value.push(c.char);
            chars.next();

            range = Some(match range {
                None => SrcRange::exact_pos(c.pos),
                Some(range) => range.extend_to(c.pos),
            });
        } else {
            break;
        }
    }

    let Some(range) = range else { bail!("Digits is empty") };

    Ok(Srced { val: Token::Digits(value), range })
}

fn consume_comment(chars: &mut impl Iterator<Item = SrcChar>) -> anyhow::Result<Srced<Token>> {
    let Some(slash) = chars.next() else { bail!("Expected //") };
    if slash.char != '/' {
        bail!("Expected //");
    }

    let mut range = SrcRange::exact_pos(slash.pos);

    let Some(slash) = chars.next() else { bail!("Expected //") };
    if slash.char != '/' {
        bail!("Expected //");
    }

    range = range.extend_to(slash.pos);
    let mut comment = String::new();

    for c in chars {
        if c.char == '\n' {
            break;
        }

        comment.push(c.char);
        range = range.extend_to(c.pos);
    }

    Ok(Srced { val: Token::Comment(comment), range })
}

#[derive(Clone, Copy, PartialEq, Eq)]
struct SrcChar {
    char: char,
    pos: SrcPos,
}

fn position_src_chars(code: &str) -> Vec<SrcChar> {
    let mut pos = SrcPos { line: 1, col: 1 };
    let mut src_chars = Vec::new();
    for c in code.chars() {
        src_chars.push(SrcChar { char: c, pos });

        if c == '\n' {
            pos.line += 1;
            pos.col = 1;
        } else {
            pos.col += 1;
        }
    }

    src_chars
}

pub fn tokenize(code: &str) -> anyhow::Result<Vec<Srced<Token>>> {
    let mut chars = position_src_chars(code).into_iter().multipeek();
    let mut tokens = Vec::new();

    while let Some(peek) = {
        chars.reset_peek();
        chars.peek()
    } {
        let token = match peek.char {
            '=' => consume_char(&mut chars, Some(Token::Eq))?,
            ';' => consume_char(&mut chars, Some(Token::Semi))?,
            '(' => consume_char(&mut chars, Some(Token::LeftParen))?,
            ')' => consume_char(&mut chars, Some(Token::RightParen))?,
            '{' => consume_char(&mut chars, Some(Token::LeftBrace))?,
            '}' => consume_char(&mut chars, Some(Token::RightBrace))?,
            '[' => consume_char(&mut chars, Some(Token::LeftBracket))?,
            ']' => consume_char(&mut chars, Some(Token::RightBracket))?,
            '|' => consume_char(&mut chars, Some(Token::Pipe))?,
            ',' => consume_char(&mut chars, Some(Token::Comma))?,
            '.' => consume_char(&mut chars, Some(Token::Period))?,
            '*' => consume_char(&mut chars, Some(Token::Asterisk))?,
            '!' => consume_char(&mut chars, Some(Token::Bang))?,
            '+' => consume_char(&mut chars, Some(Token::Plus))?,
            '-' => consume_char(&mut chars, Some(Token::Dash))?,
            '~' => consume_char(&mut chars, Some(Token::Tilde))?,
            '<' => consume_char(&mut chars, Some(Token::LeftAngle))?,
            '>' => consume_char(&mut chars, Some(Token::RightAngle))?,
            '%' => consume_char(&mut chars, Some(Token::Percent))?,
            '&' => consume_char(&mut chars, Some(Token::Ampersand))?,
            ':' => consume_char(&mut chars, Some(Token::Colon))?,
            '"' => Some(consume_string(&mut chars)?),
            '/' => match chars.peek().map(|c| c.char) {
                Some('/') => Some(consume_comment(&mut chars)?),
                _ => consume_char(&mut chars, Some(Token::Slash))?,
            },
            c if c.is_whitespace() => consume_char(&mut chars, None)?,
            c if c.is_numeric() => Some(consume_digits(&mut chars)?),
            _ => Some(consume_word(&mut chars)?),
        };

        if let Some(token) = token {
            tokens.push(token);
        }
    }

    Ok(tokens)
}
