#![feature(plugin)]
#![allow(unstable)]

#[plugin] #[no_link] extern crate unicode_names_macros;

#[test]
fn named_char() {
    assert_eq!(named_char!("LATIN SMALL LETTER A"), 'a');
    assert_eq!(named_char!("snowman"), '☃');
}

#[test]
fn named() {
    assert_eq!(named!("123\\N{latin small letter a}456"), "123a456");
    assert_eq!(named!("123\\N{SNOWMAN}\\N{BLACK STAR}456"), "123☃★456");
}
