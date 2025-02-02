use macro_string_eval::eval;

const _: &str = eval!(concat!("ru"i32, "st"));

fn main() {}
