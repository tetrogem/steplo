use std::ops::{ControlFlow, Not};

use crate::token::Token;

pub struct TokenFeed {
    tokens: Vec<Token>,
    cursor: usize,
}

impl<T: IntoIterator<Item = Token>> From<T> for TokenFeed {
    fn from(value: T) -> Self {
        TokenFeed {
            tokens: value.into_iter().filter(|t| matches!(t, Token::Comment(_)).not()).collect(),
            cursor: 0,
        }
    }
}

impl TokenFeed {
    fn next(&mut self) -> Option<&Token> {
        let token = self.tokens.get(self.cursor);
        self.cursor += 1;
        token
    }

    pub fn try_match<T, E>(
        &mut self,
        parser: impl FnOnce(&mut Self) -> Result<T, E>,
    ) -> Result<T, E> {
        let prev_cursor = self.cursor;
        let res = parser(self);

        if res.is_err() {
            self.cursor = prev_cursor;
        }

        res
    }

    pub fn is_finished(&self) -> bool {
        self.tokens.len() == self.cursor
    }

    pub fn try_next<T, E>(
        &mut self,
        consumer: impl FnOnce(Option<&Token>) -> Result<T, E>,
    ) -> Result<T, E> {
        self.try_match(|tokens| consumer(tokens.next()))
    }

    pub fn parse<T: Parse>(&mut self) -> Result<T, T::Error> {
        self.try_match(T::parse)
    }
}

pub trait Parse: Sized {
    type Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error>;
}
