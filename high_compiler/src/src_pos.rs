use std::fmt::Display;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SrcPos {
    pub line: usize,
    pub col: usize,
}

impl Default for SrcPos {
    fn default() -> Self {
        Self { line: 1, col: 1 }
    }
}

impl Ord for SrcPos {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.line.cmp(&other.line).then(self.col.cmp(&other.col))
    }
}

impl PartialOrd for SrcPos {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Display for SrcPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

// #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// pub struct SrcPos {
//     pub char: usize,
// }

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct SrcRange {
    pub start: SrcPos,
    pub end: SrcPos,
}

impl SrcRange {
    pub fn new_zero_len(pos: SrcPos) -> Self {
        Self { start: pos, end: pos }
    }

    pub fn extend_to(self, pos: SrcPos) -> Self {
        Self { start: self.start.min(pos), end: self.end.max(pos) }
    }
}

impl Ord for SrcRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start).then(self.end.cmp(&other.end))
    }
}

impl PartialOrd for SrcRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
