# unicode_names

[![Build Status](https://travis-ci.org/huonw/unicode_names.png)](https://travis-ci.org/huonw/unicode_names)

Time and memory efficiently mapping characters to and from their
Unicode 7.0 names, at runtime and compile-time.

```rust
extern crate unicode_names;

fn main() {
    println!("☃ is called {}", unicode_names::name('☃')); // SNOWMAN
    println!("{} is happy", unicode_names::character("white smiling face")); // ☺
    // (NB. case insensitivity)
}
```

The maps are compressed using similar tricks to Python's `unicodedata`
module, although those here are about 70KB (12%) smaller.

[**Documentation**](http://huonw.github.io/unicode_names/unicode_names)
