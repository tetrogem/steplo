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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum SrcRange {
    Exact { start: SrcPos, end: SrcPos },
    Guess { start: SrcPos, end: SrcPos },
}

impl Default for SrcRange {
    fn default() -> Self {
        Self::guess_pos(SrcPos::default())
    }
}

impl SrcRange {
    pub fn exact_pos(pos: SrcPos) -> Self {
        Self::Exact { start: pos, end: pos }
    }

    pub fn guess_pos(pos: SrcPos) -> Self {
        Self::Guess { start: pos, end: pos }
    }

    pub fn extend_to(self, pos: SrcPos) -> Self {
        match self {
            Self::Exact { start, end } => Self::Exact { start: start.min(pos), end: end.max(pos) },
            Self::Guess { .. } => Self::exact_pos(pos),
        }
    }

    pub fn merge(self, other: SrcRange) -> Self {
        match (self, other) {
            (Self::Exact { start, end }, Self::Exact { start: other_start, end: other_end }) => {
                Self::Exact { start: start.min(other_start), end: end.max(other_end) }
            },
            (Self::Exact { start, end }, Self::Guess { .. })
            | (Self::Guess { .. }, Self::Exact { start, end }) => Self::Exact { start, end },
            (Self::Guess { start, end }, Self::Guess { start: other_start, end: other_end }) => {
                Self::Guess { start: start.min(other_start), end: end.max(other_end) }
            },
        }
    }

    pub fn start(&self) -> SrcPos {
        match self {
            Self::Exact { start, .. } => *start,
            Self::Guess { start, .. } => *start,
        }
    }

    pub fn end(&self) -> SrcPos {
        match self {
            Self::Exact { end, .. } => *end,
            Self::Guess { end, .. } => *end,
        }
    }

    pub fn is_guess(&self) -> bool {
        match self {
            Self::Exact { .. } => false,
            Self::Guess { .. } => true,
        }
    }
}

impl Ord for SrcRange {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start().cmp(&other.start()).then(self.end().cmp(&other.end()))
    }
}

impl PartialOrd for SrcRange {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Srced<T> {
    pub range: SrcRange,
    pub val: T,
}
