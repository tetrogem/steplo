use std::time::Instant;

pub fn time<R>(message: &str, runner: impl FnOnce() -> R) -> R {
    print!("{} ", message);
    let start = Instant::now();
    let res = runner();
    let end = Instant::now();
    let duration = end - start;
    println!("took {:.5} ms", duration.as_micros() as f64 / 1000.);
    res
}
