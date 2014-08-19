// this should be being tested in the main docs in src/lib.rs above,
// but it's not, due the circular macro dependency.
#![feature(phase)]

#[phase(plugin)] extern crate unicode_names_macros;

fn main() {
    let x: char = named_char!("snowman");
    assert_eq!(x, '☃');

    let y: &str = named!("foo bar \\N{BLACK STAR} baz qux");
    assert_eq!(y, "foo bar ★ baz qux");
}
