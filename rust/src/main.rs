mod node;
mod parser;
mod tests;
use std::io::{self, Read};

use crate::parser::JsonParser;

fn main() -> io::Result<()> {
    let mut buf = String::new();
    io::stdin().read_to_string(&mut buf)?;
    let parsed = JsonParser::new().parse(&buf);
    println!("{:#?}", parsed);
    Ok(())
}
