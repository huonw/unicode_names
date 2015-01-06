#![no_std]
#![feature(phase, globs, associated_types)]
#![deny(missing_docs, warnings, unsafe_blocks)]

//! Convert between characters and their standard names.
//!
//! This crate provides two functions for mapping from a `char` to the
//! name given by the Unicode standard (7.0). There are no runtime
//! requirements so this is usable with only `core`. The tables are
//! heavily compressed, but still large (500KB), and still offer
//! efficient `O(1)` look-ups in both directions (more precisely,
//! `O(length of name)`).
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
//! ```rust
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
#[cfg(test)] extern crate test;

use core::prelude::*;
use core::char;
use core::fmt::{self, Show};

use generated::{PHRASEBOOK_OFFSET_SHIFT, PHRASEBOOK_OFFSETS1, PHRASEBOOK_OFFSETS2, MAX_NAME_LENGTH};
use generated_phf as phf;

#[allow(dead_code)] mod generated;
#[allow(dead_code)] mod generated_phf;
#[allow(dead_code)] mod jamo;

mod iter_str;

static HANGUL_SYLLABLE_PREFIX: &'static str = "HANGUL SYLLABLE ";
static CJK_UNIFIED_IDEOGRAPH_PREFIX: &'static str = "CJK UNIFIED IDEOGRAPH-";

fn is_cjk_unified_ideograph(ch: char) -> bool {
    generated::CJK_IDEOGRAPH_RANGES.iter().any(|&(lo, hi)| lo <= ch && ch <= hi)
}

/// An iterator over the components of a code point's name, it also
/// implements `Show`.
///
/// The size hint is exact for the number of pieces, but iterates
/// (although iteration is cheap and all names are short).
#[derive(Clone)]
pub struct Name {
    data: Name_
}
#[derive(Clone)]
enum Name_ {
    Plain(iter_str::IterStr),
    CJK(CJK),
    Hangul(Hangul),
}

#[derive(Copy)]
struct CJK {
    emit_prefix: bool,
    idx: u8,
    // the longest character is 0x10FFFF
    data: [u8; 6]
}
#[derive(Copy)]
struct Hangul {
    emit_prefix: bool,
    idx: u8,
    // stores the choseong, jungseong, jongseong syllable numbers (in
    // that order)
    data: [u8; 3]
}
impl Clone for CJK { fn clone(&self) -> CJK { *self } }
impl Clone for Hangul { fn clone(&self) -> Hangul { *self } }

impl Name {
    /// The number of bytes in the name.
    ///
    /// All names are plain ASCII, so this is also the number of
    /// Unicode codepoints and the number of graphemes.
    pub fn len(&self) -> uint {
        let counted = self.clone();
        counted.fold(0, |a, s| a + s.len())
    }
}

impl Iterator for Name {
    type Item = &'static str;
    fn next(&mut self) -> Option<&'static str> {
        match self.data {
            Name_::Plain(ref mut s) => s.next(),
            Name_::CJK(ref mut state) => {
                // we're a CJK unified ideograph
                if state.emit_prefix {
                    state.emit_prefix = false;
                    return Some(CJK_UNIFIED_IDEOGRAPH_PREFIX)
                }
                // run until we've run out of array: the construction
                // of the data means this is exactly when we have
                // finished emitting the number.
                state.data.get(state.idx as uint)
                    // (avoid conflicting mutable borrow problems)
                    .map(|digit| *digit as uint)
                    .map(|d| {
                        state.idx += 1;
                        static DIGITS: &'static str = "0123456789ABCDEF";
                        DIGITS.slice(d, d + 1)
                    })
            }
            Name_::Hangul(ref mut state) => {
                if state.emit_prefix {
                    state.emit_prefix = false;
                    return Some(HANGUL_SYLLABLE_PREFIX)
                }

                let idx = state.idx as uint;
                state.data.get(idx)
                    .map(|x| *x as uint)
                    .map(|x| {
                        // progressively walk through the syllables
                        state.idx += 1;
                        [jamo::CHOSEONG,
                         jamo::JUNGSEONG,
                         jamo::JONGSEONG][idx][x]
                    })
            }
        }
    }

    fn size_hint(&self) -> (uint, Option<uint>) {
        // we can estimate exactly by just iterating and summing up.
        let counted = self.clone();
        let n = counted.count();
        (n, Some(n))
    }
}

impl Show for Name {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        let mut printed = self.clone();
        for s in printed {
            try!(write!(fmtr, "{}", s))
        }
        Ok(())
    }
}

/// Find the name of `c`, or `None` if `c` has no name.
///
/// The return value is an iterator that yields `&str` components of
/// the name successively (including spaces and hyphens). It
/// implements `Show`, and thus can be used naturally to build
/// `String`s, or be printed, etc.
///
/// # Example
///
/// ```rust
/// assert_eq!(unicode_names::name('a').map(|n| n.to_string()),
///            Some("LATIN SMALL LETTER A".to_string()));
/// assert_eq!(unicode_names::name('\u{2605}').map(|n| n.to_string()),
///            Some("BLACK STAR".to_string()));
/// assert_eq!(unicode_names::name('☃').map(|n| n.to_string()),
///            Some("SNOWMAN".to_string()));
///
/// // control code
/// assert!(unicode_names::name('\x00').is_none());
/// // unassigned
/// assert!(unicode_names::name('\u{10FFFF}').is_none());
/// ```
pub fn name(c: char) -> Option<Name> {
    let cc = c as uint;
    let offset = (PHRASEBOOK_OFFSETS1[cc >> PHRASEBOOK_OFFSET_SHIFT] as uint)
        << PHRASEBOOK_OFFSET_SHIFT;

    let mask = (1 << PHRASEBOOK_OFFSET_SHIFT) - 1;
    let offset = PHRASEBOOK_OFFSETS2[offset + (cc & mask) as uint];
    if offset == 0 {
        if is_cjk_unified_ideograph(c) {
            // write the hex number out right aligned in this array.
            let mut data = [b'0'; 6];
            let mut number = c as u32;
            let mut data_start = 6;
            for place in data.iter_mut().rev() {
                // this would be incorrect if U+0000 was CJK unified
                // ideograph, but it's not, so it's fine.
                if number == 0 { break }
                *place = (number % 16) as u8;
                number /= 16;
                data_start -= 1;
            }
            Some(Name {
                data: Name_::CJK(CJK {
                    emit_prefix: true,
                    idx: data_start,
                    data: data
                })
            })
        } else {
            // maybe it is a hangul syllable?
            jamo::syllable_decomposition(c).map(|(ch, ju, jo)| {
                Name {
                    data: Name_::Hangul(Hangul {
                        emit_prefix: true,
                        idx: 0,
                        data: [ch, ju, jo]
                    })
                }
            })
        }
    } else {
        Some(Name {
            data:  Name_::Plain(iter_str::IterStr::new(offset as uint))
        })
    }
}

fn fnv_hash<I: Iterator<Item=u8>>(mut x: I) -> u64 {
    let mut g = 0xcbf29ce484222325 ^ phf::NAME2CODE_N;
    for b in x { g ^= b as u64; g *= 0x100000001b3; }
    g
}
fn displace(f1: u32, f2: u32, d1: u32, d2: u32) -> u32 {
    d2 + f1 * d1 + f2
}
fn split(hash: u64) -> (u32, u32, u32) {
    let bits = 21;
    let mask = (1 << bits) - 1;
    ((hash & mask) as u32,
     ((hash >> bits) & mask) as u32,
     ((hash >> (2 * bits)) & mask) as u32)
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
    // + 1 so that we properly handle the case when `name` has a
    // prefix of the longest name, but isn't exactly equal.
    let mut buf = [0u8; MAX_NAME_LENGTH + 1];
    for (place, byte) in buf.iter_mut().zip(name.bytes()) {
        *place = ASCII_UPPER_MAP[byte as uint]
    }
    let search_name = buf.slice_to(name.len());

    // try `HANGUL SYLLABLE <choseong><jungseong><jongseong>`
    if search_name.starts_with(HANGUL_SYLLABLE_PREFIX.as_bytes()) {
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
                // (verified by `generator/...`).
                return None;
            }
        }
    }

    // try `CJK UNIFIED IDEOGRAPH-<digits>`
    if search_name.starts_with(CJK_UNIFIED_IDEOGRAPH_PREFIX.as_bytes()) {
        let remaining = search_name.slice_from(CJK_UNIFIED_IDEOGRAPH_PREFIX.len());
        if remaining.len() > 5 { return None; } // avoid overflow

        let mut v = 0u32;
        for &c in remaining.iter() {
            match c {
                b'0'...b'9' => v = (v << 4) | (c - b'0') as u32,
                b'A'...b'F' => v = (v << 4) | (c - b'A' + 10) as u32,
                _ => return None,
            }
        }
        let ch = match char::from_u32(v) {
            Some(ch) => ch,
            None => return None,
        };

        // check if the resulting code is indeed in the known ranges
        if is_cjk_unified_ideograph(ch) {
            return Some(ch);
        } else {
            // there are no other names starting with `CJK UNIFIED IDEOGRAPH-`
            // (verified by `src/generate.py`).
            return None;
        }
    }

    // get the parts of the hash...
    let (g, f1, f2) = split(fnv_hash(search_name.iter().map(|x| *x)));
    // ...and the appropriate displacements...
    let (d1, d2) = phf::NAME2CODE_DISP[g as uint % phf::NAME2CODE_DISP.len()];

    // ...to find the right index...
    let idx = displace(f1, f2, d1 as u32, d2 as u32) as uint;
    // ...for looking up the codepoint.
    let codepoint = phf::NAME2CODE_CODE[idx % phf::NAME2CODE_CODE.len()];

    // Now check that this is actually correct. Since this is a
    // perfect hash table, valid names map precisely to their code
    // point (and invalid names map to anything), so we only need to
    // check the name for this codepoint matches the input and we know
    // everything. (i.e. no need for probing)
    let mut maybe_name = match ::name(codepoint) {
        None => {
            if true { debug_assert!(false) }
            return None
        }
        Some(name) => name
    };

    // run through the parts of the name, matching them against the
    // parts of the input.
    let mut passed_name = search_name;
    for part in maybe_name {
        let part = part.as_bytes();
        let part_l = part.len();
        if passed_name.len() < part_l || passed_name.slice_to(part_l) != part {
            return None
        }
        passed_name = passed_name.slice_from(part_l)
    }

    Some(codepoint)
}

// FIXME: use the stdlib one if/when std::ascii moves into `core` (or
// some such).
static ASCII_UPPER_MAP: [u8; 256] = [
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
    b'`', b'A', b'B', b'C', b'D', b'E', b'F', b'G',
    b'H', b'I', b'J', b'K', b'L', b'M', b'N', b'O',
    b'P', b'Q', b'R', b'S', b'T', b'U', b'V', b'W',
    b'X', b'Y', b'Z', b'{', b'|', b'}', b'~', 0x7f,
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
    use std::prelude::v1::*;
    use std::ascii::AsciiExt;
    use std::char;
    use std::iter::{range_inclusive};
    use std::rand::{self, XorShiftRng, SeedableRng};

    use test::{self, Bencher};
    use super::{generated, name, character, is_cjk_unified_ideograph, jamo, Name};

    static DATA: &'static str = include_str!(concat!(env!("CARGO_MANIFEST_DIR"),
                                                     "/data/codepoint_name.csv"));

    #[test]
    fn exhaustive() {
        // check that gaps have no names (these are unassigned/control
        // codes).
        fn negative_range(from: u32, to: u32) {
            for c in range(from, to).filter_map(char::from_u32) {
                if !is_cjk_unified_ideograph(c) && !jamo::is_hangul_syllable(c) {
                    let n = name(c);
                    assert!(n.is_none(),
                            "{} ({}) shouldn't have a name but is called {}",
                            c, c as u32, n);
                }
            }
        }

        let mut last = 0;
        for line in DATA.lines() {
            let mut it = line.split(';');

            let raw_c = it.next();
            let c = match char::from_u32(raw_c.and_then(StrExt::parse).unwrap()) {
                Some(c) => c,
                None => continue
            };

            let n = it.next().unwrap();
            if n.starts_with("<") {
                continue
            }

            let computed_n = name(c).unwrap();
            let n_str = computed_n.to_string();
            assert_eq!(n_str, n.to_string());
            assert_eq!(computed_n.len(), n_str.len());

            let (hint_low, hint_high) = computed_n.size_hint();
            let number_of_parts = computed_n.count();
            assert_eq!(hint_low, number_of_parts);
            assert_eq!(hint_high, Some(number_of_parts));


            assert_eq!(character(n), Some(c));
            assert_eq!(character(n.to_ascii_lowercase().as_slice()), Some(c));

            negative_range(last, c as u32);
            last = c as u32 + 1;
        }
        negative_range(last, 0x10FFFF + 1)
    }

    #[test]
    fn name_to_string() {
        let n = name('a').unwrap();
        assert_eq!(n.to_string(), "LATIN SMALL LETTER A".to_string());
        let n = name('🁣').unwrap();
        assert_eq!(n.to_string(), "DOMINO TILE VERTICAL-00-00".to_string());
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
    fn name_hangul_syllable() {
        assert_eq!(name('\u{ac00}').map(|s| s.to_string()),
                   Some("HANGUL SYLLABLE GA".to_string())); // first
        assert_eq!(name('\u{bdc1}').map(|s| s.to_string()),
                   Some("HANGUL SYLLABLE BWELG".to_string()));
        assert_eq!(name('\u{d7a3}').map(|s| s.to_string()),
                   Some("HANGUL SYLLABLE HIH".to_string())); // last
    }

    #[test]
    fn character_hangul_syllable() {
        assert_eq!(character("HANGUL SYLLABLE GA"), Some('\u{ac00}'));
        assert_eq!(character("HANGUL SYLLABLE BWELG"), Some('\u{bdc1}'));
        assert_eq!(character("HANGUL SYLLABLE HIH"), Some('\u{d7a3}'));
        assert_eq!(character("HANGUL SYLLABLE BLAH"), None);
    }

    #[test]
    fn cjk_unified_ideograph_exhaustive() {
        for &(lo, hi) in generated::CJK_IDEOGRAPH_RANGES.iter() {
            for x in range_inclusive(lo as u32, hi as u32) {
                let c = char::from_u32(x).unwrap();

                let real_name = format!("CJK UNIFIED IDEOGRAPH-{:X}", x);
                let lower_real_name = format!("CJK UNIFIED IDEOGRAPH-{:x}", x);
                assert_eq!(character(real_name.as_slice()), Some(c));
                assert_eq!(character(lower_real_name.as_slice()), Some(c));

                assert_eq!(name(c).map(|s| s.to_string()),
                           Some(real_name));
            }
        }
    }
    #[test]
    fn name_cjk_unified_ideograph() {
        assert_eq!(name('\u{4e00}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-4E00".to_string())); // first in BMP
        assert_eq!(name('\u{9fcc}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-9FCC".to_string())); // last in BMP (as of 6.1)
        assert_eq!(name('\u{20000}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-20000".to_string())); // first in SIP
        assert_eq!(name('\u{2a6d6}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-2A6D6".to_string()));
        assert_eq!(name('\u{2a700}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-2A700".to_string()));
        assert_eq!(name('\u{2b81d}').map(|s| s.to_string()),
                   Some("CJK UNIFIED IDEOGRAPH-2B81D".to_string())); // last in SIP (as of 6.0)
    }

    #[test]
    fn character_cjk_unified_ideograph() {
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-4E00"), Some('\u{4e00}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-9FCC"), Some('\u{9fcc}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-20000"), Some('\u{20000}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6D6"), Some('\u{2a6d6}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A700"), Some('\u{2a700}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2B81D"), Some('\u{2b81d}'));
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-!@#$"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-1234"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-EFGH"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-12345"), None);
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6D7"), None); // between Ext B and Ext C
        assert_eq!(character("CJK UNIFIED IDEOGRAPH-2A6FF"), None);
    }


    #[bench]
    fn name_basic(b: &mut Bencher) {
        b.iter(|| {
            for s in name('ö').unwrap() {
                test::black_box(s);
            }
        })
    }

    #[bench]
    fn character_basic(b: &mut Bencher) {
        b.iter(|| character("LATIN SMALL LETTER O WITH DIAERESIS"));
    }

    #[bench]
    fn name_10000_invalid(b: &mut Bencher) {
        // be consistent across runs, but avoid sequential/caching.
        let mut rng: XorShiftRng = SeedableRng::from_seed([0xFF00FF00, 0xF0F0F0F0,
                                                           0x00FF00FF, 0x0F0F0F0F]);
        let chars = rand::sample(&mut rng,
                                 range(0u32, 0x10FFFFF)
                                 .filter_map(|x| {
                                     match char::from_u32(x) {
                                         Some(c) if name(c).is_none() => Some(c),
                                         _ => None
                                     }
                                 }),
                                 10000);

        b.iter(|| {
            for &c in chars.iter() {
                assert!(name(c).is_none());
            }
        })
    }

    #[bench]
    fn name_all_valid(b: &mut Bencher) {
        let chars = range(0u32, 0x10FFFFF)
            .filter_map(|x| {
                match char::from_u32(x) {
                    Some(c) if name(c).is_some() => Some(c),
                    _ => None
                }
            }).collect::<Vec<char>>();

        b.iter(|| {
            for c in chars.iter() {
                for s in name(*c).unwrap() {
                    test::black_box(s);
                }
            }
        });
    }

    #[bench]
    fn character_10000(b: &mut Bencher) {
        // be consistent across runs, but avoid sequential/caching.
        let mut rng: XorShiftRng = SeedableRng::from_seed([0xFF00FF00, 0xF0F0F0F0,
                                                           0x00FF00FF, 0x0F0F0F0F]);

        let names = rand::sample(&mut rng,
                                 range(0u32, 0x10FFFFF).filter_map(|x| char::from_u32(x).and_then(name)),
                                 10000)
            .iter()
            .map(|n: &Name| n.to_string())
            .collect::<Vec<_>>();

        b.iter(|| {
            for n in names.iter() {
                test::black_box(character(n.as_slice()))
            }
        })
    }
}

#[cfg(not(test))]
mod std {
    pub use core::{clone, fmt, kinds};
}
