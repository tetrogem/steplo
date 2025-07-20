use std::ops::Not;

use crate::{
    src_pos::SrcPos,
    token::{SrcToken, Token},
    token_feed::cursor::Cursor,
};

pub struct TokenFeed {
    tokens: Vec<SrcToken>,
    cursor: Cursor,
}

mod cursor {
    use crate::{src_pos::SrcPos, token::SrcToken};

    #[derive(Clone, Copy, Default)]
    pub struct Cursor {
        index: usize,
        pos: SrcPos,
    }

    impl Cursor {
        pub fn index(&self) -> usize {
            self.index
        }

        pub fn pos(&self) -> SrcPos {
            self.pos
        }

        pub fn increment(self, last_token: Option<&SrcToken>) -> Self {
            let pos = match last_token {
                Some(t) => t.range.end,
                None => SrcPos { col: self.pos.col + 1, ..self.pos },
            };

            Self { index: self.index + 1, pos }
        }
    }
}

impl<T: IntoIterator<Item = SrcToken>> From<T> for TokenFeed {
    fn from(value: T) -> Self {
        TokenFeed {
            tokens: value
                .into_iter()
                .filter(|t| matches!(t.token, Token::Comment(_)).not())
                .collect(),
            cursor: Cursor::default(),
        }
    }
}

impl TokenFeed {
    fn next(&mut self) -> FeedCell<Option<&SrcToken>> {
        let token = self.tokens.get(self.cursor.index());
        self.cursor = self.cursor.increment(token);
        FeedCell { res: token, pos: self.cursor.pos() }
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
        self.tokens.len() == self.cursor.index()
    }

    pub fn try_next<T, E>(
        &mut self,
        consumer: impl FnOnce(FeedCell<Option<&SrcToken>>) -> Result<T, E>,
    ) -> Result<T, E> {
        self.try_match(|tokens| consumer(tokens.next()))
    }

    pub fn parse<T: Parse>(&mut self) -> FeedCell<Result<T, T::Error>> {
        let res = self.try_match(T::parse);
        FeedCell { res, pos: self.cursor.pos() }
    }
}

pub trait Parse: Sized {
    type Error;

    fn parse(tokens: &mut TokenFeed) -> Result<Self, Self::Error>;
}

#[derive(Clone, Copy, Debug)]
pub struct FeedCell<T> {
    pub res: T,
    pub pos: SrcPos,
}
