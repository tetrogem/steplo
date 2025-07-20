use itertools::Itertools;

use crate::src_pos::SrcRange;

pub struct AstErrorSet {
    errors: Vec<AstError>,
}

pub struct AstError {
    msg: String,
    range: SrcRange,
}

impl AstErrorSet {
    pub fn new() -> Self {
        AstErrorSet { errors: Vec::new() }
    }

    pub fn new_error(range: SrcRange, msg: impl ToString) -> Self {
        let err = AstError { range, msg: msg.to_string() };
        AstErrorSet { errors: Vec::from([err]) }
    }

    pub fn merge(self, other: AstErrorSet) -> Self {
        Self { errors: self.errors.into_iter().chain(other.errors).collect() }
    }
}

pub fn report_ast_errors(code: &str, set: AstErrorSet) {
    let errors = set.errors.into_iter().max_set_by_key(|e| e.range);
    let Some(range) = errors.first().map(|e| e.range) else { return };

    for err in &errors {
        println!("@{}: {}", range.start, err.msg);
    }

    let nearby_lines = code
        .lines()
        .skip(range.start.line.saturating_sub(2).saturating_sub(1))
        .take(range.start.line.min(3))
        .collect_vec();

    for line in nearby_lines {
        println!("{}", line);
    }

    println!(
        "{}{}",
        " ".repeat(range.start.col.saturating_sub(1)),
        "^".repeat(1 + range.end.col.saturating_sub(range.start.col))
    );
}
