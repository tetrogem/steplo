use std::ops::Not;

use super::{
    srced::{SrcRange, Srced},
    token::Token,
    token_feed::cursor::Cursor,
};

pub struct TokenFeed {
    tokens: Vec<Srced<Token>>,
    cursor: Cursor,
}

mod cursor {
    use crate::{
        srced::{SrcPos, SrcRange, Srced},
        token::Token,
    };

    #[derive(Clone, Copy, Default)]
    pub struct Cursor {
        index: usize,
        range: SrcRange,
    }

    impl Cursor {
        pub fn index(&self) -> usize {
            self.index
        }

        pub fn range(&self) -> SrcRange {
            self.range
        }

        pub fn increment(self, last_token: Option<&Srced<Token>>) -> Self {
            let range = match last_token {
                Some(t) => t.range,
                None => SrcRange::exact_pos(SrcPos {
                    col: self.range.end().col + 1,
                    ..self.range.end()
                }),
            };

            Self { index: self.index + 1, range }
        }
    }
}

impl<T: IntoIterator<Item = Srced<Token>>> From<T> for TokenFeed {
    fn from(value: T) -> Self {
        TokenFeed {
            tokens: value
                .into_iter()
                .filter(|t| matches!(t.val, Token::Comment(_)).not())
                .collect(),
            cursor: Cursor::default(),
        }
    }
}

impl TokenFeed {
    fn next(&mut self) -> FeedCell<Option<&Srced<Token>>> {
        let token = self.tokens.get(self.cursor.index());
        self.cursor = self.cursor.increment(token);
        FeedCell { res: token, range: self.cursor.range() }
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
        consumer: impl FnOnce(FeedCell<Option<&Srced<Token>>>) -> Result<T, E>,
    ) -> Result<T, E> {
        self.try_match(|tokens| consumer(tokens.next()))
    }

    pub fn cur_range(&self) -> SrcRange {
        self.cursor.range()
    }
}

#[derive(Clone, Copy, Debug)]
pub struct FeedCell<T> {
    pub res: T,
    pub range: SrcRange,
}
