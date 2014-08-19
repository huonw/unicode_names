#![no_std]
#![feature(phase)]

//! Convert between characters and their standard names.
//!
//! This crate provides two functions for mapping from a `char` to the
//! name given by the Unicode standard (7.0). There are no runtime
//! requirements and this is usable with only `core`, but the
//! associated tables are large (8MB).
//!
//! ```rust
//! extern crate unicode_names;
//!
//! fn main() {
//!     println!("☃ is called {}", unicode_names::name('☃')); // SNOWMAN
//!     println!("{} is happy", unicode_names::character("white smiling face")); // ☺
//!     // (NB. case insensitivity)
//! }
//! ```
//!
//! [**Source**](https://github.com/huonw/unicode_names).
//!
//! # Macros
//!
//! The associated `unicode_names_macros` crate provides two macros
//! for converting at compile-time, giving named literals similar to
//! Python's `"\N{...}"`.
//!
//! - `named_char!(name)` takes a single string `name` and creates a
//!   `char` literal.
//! - `named!(string)` takes a string and replaces any `\\N{name}`
//!   sequences with the character with that name. NB. String escape
//!   sequences cannot be customised, so the extra backslash (or a raw
//!   string) is required.
//!
//! ```rust,ignore
//! #![feature(phase)]
//!
//! #[phase(plugin)] extern crate unicode_names_macros;
//!
//! fn main() {
//!     let x: char = named_char!("snowman");
//!     assert_eq!(x, '☃');
//!
//!     let y: &str = named!("foo bar \\N{BLACK STAR} baz qux");
//!     assert_eq!(y, "foo bar ★ baz qux");
//! }
//! ```
//!
//! # Cargo-enabled
//!
//! Add either (or both!) of the following to your `Cargo.toml`.
//!
//! ```toml
//! [dependencies.unicode_names]
//! git = "https://github.com/huonw/unicode_names"
//!
//! [dependencies.unicode_names_macros]
//! git = "https://github.com/huonw/unicode_names"
//! ```


#[phase(link, plugin)] extern crate core;

#[cfg(test)] #[phase(link, plugin)] extern crate std;
#[cfg(test)] extern crate native;
#[cfg(test)] extern crate test;

use core::cmp::{Equal, Less, Greater};
use core::collections::Collection;
use core::option::{Option, None, Some};
use core::iter::{order, Iterator, DoubleEndedIterator};
use core::slice::{ImmutableSlice, ImmutablePartialEqSlice, MutableSlice};
use core::str::StrSlice;
use core::char;

#[allow(dead_code)] mod generated;
#[allow(dead_code)] mod jamo;

/// Find the name of `c`, or `None` if `c` has no name.
///
/// # Example
///
/// ```rust
/// assert_eq!(unicode_names::name('a'), Some("LATIN SMALL LETTER A"));
/// assert_eq!(unicode_names::name('\u2605'), Some("BLACK STAR"));
/// assert_eq!(unicode_names::name('☃'), Some("SNOWMAN"));
///
/// // control code
/// assert_eq!(unicode_names::name('\x00'), None);
/// // unassigned
/// assert_eq!(unicode_names::name('\U0010FFFF'), None);
/// ```
pub fn name(c: char) -> Option<&'static str> {
    generated::CHARACTER_TO_NAME.binary_search(|&(lo,hi, _)| {
        if c < lo {
            Greater
        } else if hi < c {
            Less
        } else {
            Equal
        }
    }).found().map(|idx| {
        let (lo, _, names) = generated::CHARACTER_TO_NAME[idx];
        names[c as uint - lo as uint]
    })
}

/// Find the character called `name`, or `None` if no such character
/// exists.
///
/// This searches case-insensitively.
///
/// # Example
///
/// ```rust
/// assert_eq!(unicode_names::character("LATIN SMALL LETTER A"), Some('a'));
/// assert_eq!(unicode_names::character("latin SMALL letter A"), Some('a'));
/// assert_eq!(unicode_names::character("latin small letter a"), Some('a'));
/// assert_eq!(unicode_names::character("BLACK STAR"), Some('★'));
/// assert_eq!(unicode_names::character("SNOWMAN"), Some('☃'));
///
/// assert_eq!(unicode_names::character("nonsense"), None);
/// ```
pub fn character(name: &str) -> Option<char> {
    let mut buf = [0u8, .. generated::MAX_NAME_LENGTH + 1];
    for (place, byte) in buf.mut_iter().zip(name.bytes()) {
        *place = ASCII_UPPER_MAP[byte as uint]
    }
    let search_name = buf.slice_to(name.len());

    // try `HANGUL SYLLABLE <choseong><jungseong><jongseong>`
    static HANGUL_SYLLABLE_PREFIX: &'static [u8] = b"HANGUL SYLLABLE ";
    if search_name.starts_with(HANGUL_SYLLABLE_PREFIX) {
        let remaining = search_name.slice_from(HANGUL_SYLLABLE_PREFIX.len());
        let (choseong, remaining) = jamo::slice_shift_choseong(remaining);
        let (jungseong, remaining) = jamo::slice_shift_jungseong(remaining);
        let (jongseong, remaining) = jamo::slice_shift_jongseong(remaining);
        match (choseong, jungseong, jongseong, remaining) {
            (Some(choseong), Some(jungseong), Some(jongseong), b"") => {
                let c = 0xac00 + (choseong * 21 + jungseong) * 28 + jongseong;
                return char::from_u32(c);
            }
            (_, _, _, _) => {
                // there are no other names starting with `HANGUL SYLLABLE `
                // (verified by `src/generate.py`).
                return None;
            }
        }
    }

    // try `CJK UNIFIED IDEOGRAPH-<digits>`
    static CJK_UNIFIED_IDEOGRAPH_PREFIX: &'static [u8] = b"CJK UNIFIED IDEOGRAPH-";
    if search_name.starts_with(CJK_UNIFIED_IDEOGRAPH_PREFIX) {
        let remaining = search_name.slice_from(CJK_UNIFIED_IDEOGRAPH_PREFIX.len());
        if remaining.len() > 5 { return None; } // avoid overflow

        let mut v = 0u32;
        for &c in remaining.iter() {
            match c {
                b'0'..b'9' => v = (v << 4) | (c - b'0') as u32,
                b'A'..b'F' => v = (v << 4) | (c - b'A' + 10) as u32,
                _ => return None,
            }
        }
        let ch = match char::from_u32(v) {
            Some(ch) => ch,
            None => return None,
        };

        // check if the resulting code is indeed in the known ranges
        if generated::CJK_IDEOGRAPH_RANGES.iter().any(|&(lo, hi)| lo <= ch && ch <= hi) {
            return Some(ch);
        } else {
            // there are no other names starting with `CJK UNIFIED IDEOGRAPH-`
            // (verified by `src/generate.py`).
            return None;
        }
    }

    generated::NAME_TO_CHARACTER.binary_search(|&(this_name, _)| {
        // reverse order because this is more unique, and thus more
        // efficient (less time spend iterating over common LATIN
        // CAPITAL LETTER ... etc. prefixes)
        order::cmp(this_name.bytes().rev(),
                   search_name.iter().map(|&b| b).rev())
    }).found().map(|idx| {
        let (_, c) = generated::NAME_TO_CHARACTER[idx];
        c
    })
}

// FIXME: use the stdlib one if/when std::ascii moves into `core` (or
// some such).
static ASCII_UPPER_MAP: [u8, ..256] = [
    0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07,
    0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d, 0x0e, 0x0f,
    0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17,
    0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f,
    b' ', b'!', b'"', b'#', b'$', b'%', b'&', b'\'', // " // syntax highlighting :/
    b'(', b')', b'*', b'+', b',', b'-', b'.', b'/',
    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7',
    b'8', b'9', b':', b';', b'<', b'=', b'>', b'?',
    b'@', b'A', b'B', b'C', b'D', b'E', b'F', b'G',
    b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O',
    b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W',
    b'X', b'Y', b'Z', b'[', b'\\', b']', b'^', b'_',
    b'`',

          b'A', b'B', b'C', b'D', b'E', b'F', b'G',
    b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O',
    b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W',
    b'X', b'Y', b'Z',

                      b'{', b'|', b'}', b'~', 0x7f,
    0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87,
    0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f,
    0x90, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97,
    0x98, 0x99, 0x9a, 0x9b, 0x9c, 0x9d, 0x9e, 0x9f,
    0xa0, 0xa1, 0xa2, 0xa3, 0xa4, 0xa5, 0xa6, 0xa7,
    0xa8, 0xa9, 0xaa, 0xab, 0xac, 0xad, 0xae, 0xaf,
    0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7,
    0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf,
    0xc0, 0xc1, 0xc2, 0xc3, 0xc4, 0xc5, 0xc6, 0xc7,
    0xc8, 0xc9, 0xca, 0xcb, 0xcc, 0xcd, 0xce, 0xcf,
    0xd0, 0xd1, 0xd2, 0xd3, 0xd4, 0xd5, 0xd6, 0xd7,
    0xd8, 0xd9, 0xda, 0xdb, 0xdc, 0xdd, 0xde, 0xdf,
    0xe0, 0xe1, 0xe2, 0xe3, 0xe4, 0xe5, 0xe6, 0xe7,
    0xe8, 0xe9, 0xea, 0xeb, 0xec, 0xed, 0xee, 0xef,
    0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7,
    0xf8, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
];

#[cfg(test)]
mod tests {
    use core::collections::Collection;
    use core::slice::ImmutableSlice;
    use core::option::{None, Some};
    use core::iter::{Iterator, range};
    use core::char;

    use std::ascii::AsciiExt;
    use std::rand::{Rng, XorShiftRng, SeedableRng};
    use std::str::Str;
    use std::vec::Vec;

    use test::{mod, Bencher};

    use super::{generated, name, character};

    #[test]
    fn exhaustive_positive() {
        for &(n, c) in generated::NAME_TO_CHARACTER.iter() {
            assert_eq!(name(c), Some(n));
            assert_eq!(character(n), Some(c));
            assert_eq!(character(n.to_ascii_lower().as_slice()), Some(c))
        }
    }

    #[test]
    fn exhaustive_name_negative() {
        // check that gaps in the CHARACTER_TO_NAME table have no
        // names (these are unassigned/control codes).
        fn test_range(from: u32, to: u32) {
            for x in range(from, to) {
                match char::from_u32(x) {
                    None => {}
                    Some(c) => {
                        assert_eq!(name(c), None);
                    }
                }
            }
        }

        let mut last = 0;
        for &(lo, hi, _) in generated::CHARACTER_TO_NAME.iter() {
            test_range(last, lo as u32);
            last = hi as u32 + 1;
        }
        test_range(last, 0x10FFFF + 1)
    }

    #[test]
    fn character_negative() {
        let names = [
            "",
            "x",
            "öäå",
            "SPAACE"
            ];
        for &n in names.iter() {
            assert_eq!(character(n), None);
        }
    }

    #[test]
    #[ignore]
    fn name_hangul_syllable() {
        assert_eq!(name('\uac00'), Some("HANGUL SYLLABLE GA")); // first
        assert_eq!(name('\ubdc1'), Some("HANGUL SYLLABLE BWELG"));
        assert_eq!(name('\ud7a3'), Some("HANGUL SYLLABLE HIH")); // last
    }

    #[test]
    fn character_hangul_syllable() {
        assert_eq!(character("HANGUL SYLLABLE GA"), Some('\uac00'));
        assert_eq!(character("HANGUL SYLLABLE BWELG"), Some('\ubdc1'));
        assert_eq!(character("HANGUL SYLLABLE HIH"), Some('\ud7a3'));
        assert_eq!(character("HANGUL SYLLABLE BLAH"), None);
    }

    #[test]
    #[ignore]
    fn name_cjk_unified_ideograph() {
        assert_eq!(name('\u4e00'), Some("CJK UNIFIED IDEOGRAPH-4E00")); // first in BMP
        assert_eq!(name('\u9fcc'), Some("CJK UNIFIED IDEOGRAPH-9FCC")); // last in BMP (as of 6.1)
        assert_eq!(name('\U00020000'), Some("CJK UNIFIED IDEOGRAPH-20000")); // first in SIP
        assert_eq!(name('\U0002a6d7'), Some("CJK UNIFIED IDEOGRAPH-2A6D6"));
        assert_eq!(name('\U0002a700'), Some("CJK UNIFIED IDEOGRAPH-2A700"));
        assert_eq!(name('\U0002b81f'), Some("CJK UNIFIED IDEOGRAPH-2B81F")); // last in SIP (as of 6.0)
    }

    #[test]
    fn character_cjk_unified_ideograph() {
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-4E00"), Some('\u4e00'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-9FCC"), Some('\u9fcc'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-20000"), Some('\U00020000'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6D6"), Some('\U0002a6d6'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A700"), Some('\U0002a700'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2B81D"), Some('\U0002b81d'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-!@#$"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-1234"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-EFGH"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-12345"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6D7"), None); // between Ext B and Ext C
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6FF"), None);
    }


    #[bench]
    fn name_one_hundreth(b: &mut Bencher) {
        let mut chars: Vec<char> = range(0u32, 0x10FFFF + 1).filter_map(char::from_u32).collect();

        // be consistent across runs, but avoid sequential/caching.
        let mut rng: XorShiftRng = SeedableRng::from_seed([0xFF00FF00, 0xF0F0F0F0,
                                                           0x00FF00FF, 0x0F0F0F0F]);
        rng.shuffle(chars.as_mut_slice());
        let new_len = chars.len() / 100;
        chars.truncate(new_len);

        b.iter(|| {
            for c in chars.iter() {
                test::black_box(name(*c));
            }
        })
    }

    #[bench]
    fn character_one_hundreth(b: &mut Bencher) {
        let mut names: Vec<&str> = generated::NAME_TO_CHARACTER.iter().map(|&(n,_)| n).collect();
        // be consistent across runs, but avoid sequential/caching.
        let mut rng: XorShiftRng = SeedableRng::from_seed([0xFF00FF00, 0xF0F0F0F0,
                                                           0x00FF00FF, 0x0F0F0F0F]);
        rng.shuffle(names.as_mut_slice());

        let new_len = names.len() / 100;
        names.truncate(new_len);
        b.iter(|| {
            for n in names.iter() {
                test::black_box(character(*n))
            }
        })
    }
}
