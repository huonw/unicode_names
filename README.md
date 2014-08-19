# unicode_names

[![Build Status](https://travis-ci.org/huonw/unicode_names.png)](https://travis-ci.org/huonw/unicode_names)

Mapping characters to and from their Unicode names, at runtime and
compile-time.

```rust
extern crate unicode_names;

fn main() {
    println!("☃ is called {}", unicode_names::name('☃')); // SNOWMAN
    println!("{} is happy", unicode_names::character("white smiling face")); // ☺
    // (NB. case insensitivity)
}
```

[**Documentation**](http://www.rust-ci.org/huonw/unicode_names/doc/unicode_names/)
