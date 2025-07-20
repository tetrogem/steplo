use std::path::Path;

use colored::Colorize;
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

pub fn report_ast_errors(code: &str, code_path: &Path, set: AstErrorSet) {
    let errors = set.errors.into_iter().max_set_by_key(|e| e.range);
    let Some(range) = errors.first().map(|e| e.range) else { return };

    println!("{}", "Error!".red().bold());

    for err in &errors {
        let src_info =
            format!("{}:{}:{}", code_path.to_string_lossy(), range.start.line, range.start.col)
                .normal();
        println!("{} {} {} {}", "@".blue(), src_info, "-->".blue(), err.msg);
    }

    let nearby_lines = code
        .lines()
        .enumerate()
        .map(|(i, line)| (i + 1, line))
        .skip(range.start.line.saturating_sub(2).saturating_sub(1))
        .take(range.start.line.min(3))
        .collect_vec();

    let Some(max_line_number) = nearby_lines.iter().map(|(i, _)| i.to_string()).max() else {
        return;
    };

    let line_numbers_width = max_line_number.len();

    let empty_sidebar = format!("{:>width$} |", "", width = line_numbers_width).blue().bold();
    println!("{}", empty_sidebar);

    for (i, line) in nearby_lines {
        let sidebar = format!("{:>width$} |", i, width = line_numbers_width);
        println!("{}  {}", sidebar.blue().bold(), line);
    }

    println!(
        "{}  {}{}",
        empty_sidebar,
        " ".repeat(range.start.col.saturating_sub(1)),
        "^".repeat(1 + range.end.col.saturating_sub(range.start.col)).red(),
    );
}
