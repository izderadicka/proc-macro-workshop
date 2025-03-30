#![allow(dead_code)]
use derive_builder::Builder;

#[derive(Builder)]
pub struct Command {
    executable: String,
    #[builder(each = "arg")]
    args: Vec<String>,
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {
    let command = Command::builder()
        .executable("cargo".to_owned())
        .args(vec!["build".to_owned(), "--release".to_owned()])
        .arg("--verbose".to_owned())
        .env(vec![])
        .current_dir("..".to_owned())
        .build()
        .unwrap();
    println!("Hello, {}", command.executable);
}
