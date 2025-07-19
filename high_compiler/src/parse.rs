use std::ops::Try;

use crate::token_feed::TokenFeed;

pub trait Parse2 {
    fn parse(tokens: &mut TokenFeed) -> ParseResult<Self>;
}

pub enum ParseResult<T> {
    Ok(T),
    Continue(ParseError),
    Break(ParseError),
}

pub struct ParseError {
    message: Arc<str>,
    context: Vec<Arc<str>>,
}

impl ParseError {
    pub fn new(message: &str) -> Self {
        Self { message: message.into(), context: Vec::new() }
    }
}
