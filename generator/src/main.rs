#[macro_use] extern crate log;
extern crate getopts;
extern crate rand;

use std::{cmp, char};
use std::collections::{HashMap, hash_map};
use std::fs::{File, self};
use std::io::{BufReader, BufWriter, self};
use std::io::prelude::*;
use std::iter::repeat;
use std::path::Path;

use formatting::Context;

macro_rules! w {
    ($ctxt: expr, $($tt: tt)*) => {
        (write!($ctxt.out, $($tt)*)).unwrap()
    }
}

mod formatting;
mod phf;
mod trie;
mod util;

static OUT_FILE: &'static str = "../src/generated.rs";
static PHF_OUT_FILE: &'static str = "../src/generated_phf.rs";
static IN_FILE: &'static str = "../data/UnicodeData.txt";

static SPLITTERS: &'static [u8] = b"-";

fn get_table_data() -> (Vec<(char, String)>, Vec<(char, char)>) {
    macro_rules! extract {
        ($line: expr) => {{
            let line = $line;
            let mut splits = line.split(';');
            let cp = splits.next().and_then(|s| u32::from_str_radix(s, 16).ok())
                .unwrap_or_else(|| panic!("invalid {}", line));
            let c = match char::from_u32(cp) {
                None => continue,
                Some(c) => c,
            };
            let name = splits.next().unwrap_or_else(|| panic!("missing name {}", line));
            (c, name)
        }}
    }

    let r = BufReader::new(File::open(Path::new(IN_FILE)).unwrap());
    let mut iter = r.lines().map(|x| x.unwrap());

    let mut codepoint_names = vec![];
    let mut cjk_ideograph_ranges = vec![];

    loop {
        let l = match iter.next() {
            Some(l) => l,
            None => break
        };

        let (cp, name) = extract!(l.trim());
        if name.starts_with("<") {
            assert!(name.ends_with(">"), "should >: {}", name);
            let name = &name[1..name.len() - 1];
            if name.starts_with("CJK Ideograph") {
                assert!(name.ends_with("First"));
                // should be CJK Ideograph ..., Last
                let line2 = iter.next().expect("unclosed ideograph range");
                let (cp2, name2) = extract!(line2.trim());
                assert_eq!(&*name.replace("First", "Last"),
                           &name2[1..name2.len() - 1]);

                cjk_ideograph_ranges.push((cp, cp2));
            } else if name.starts_with("Hangul Syllable") {
                // the main lib only knows this range, so lets make
                // sure we're not going out of date wrt the unicode
                // standard.
                if name.ends_with("First") {
                    assert_eq!(cp, '\u{AC00}');
                } else if name.ends_with("Last") {
                    assert_eq!(cp, '\u{D7A3}');
                } else {
                    panic!("unknown hangul syllable {}", name)
                }
            }
        } else {
            codepoint_names.push((cp, name.to_string()))
        }
    }
    (codepoint_names, cjk_ideograph_ranges)
}

fn write_cjk_ideograph_ranges(ctxt: &mut Context, ranges: &[(char, char)]) {
    ctxt.write_debugs("CJK_IDEOGRAPH_RANGES", "(char, char)", ranges)
}

/// Construct a huge string storing the text data, and return it,
/// along with information about the position and frequency of the
/// constituent words of the input.
fn create_lexicon_and_offsets(mut codepoint_names: Vec<(char, String)>) -> (String,
                                                                            Vec<(usize, Vec<u8>,
                                                                                 usize)>) {
    codepoint_names.sort_by(|a, b| a.1.len().cmp(&b.1.len()).reverse());

    // a trie of all the suffixes of the data,
    let mut t = trie::Trie::new();
    let mut output = String::new();

    let mut substring_overlaps = 0;
    let mut substring_o_bytes = 0;

    for &(_, ref name) in codepoint_names.iter() {
        for n in util::split(&**name, SPLITTERS) {
            if n.len() == 1 && SPLITTERS.contains(&n.as_bytes()[0]) {
                continue
            }

            let (already, previous_was_exact) = t.insert(n.bytes(), None, false);
            if already {
                if !previous_was_exact {
                    substring_overlaps += 1;
                    substring_o_bytes += n.len();
                }
            } else {
                // completely new element, i.e. not a substring of
                // anything, so record its position & add it.
                let offset = output.len();
                t.set_offset(n.bytes(), offset);
                output.push_str(n);

                // insert the suffixes of this word which saves about
                // 10KB (we could theoretically insert all substrings,
                // upto a certain length, but this only saves ~300
                // bytes or so and is noticably slower).
                for i in 1..n.len() {
                    if t.insert(n[i..].bytes(), Some(offset + i), true).0 {
                        // once we've found a string that's already
                        // been inserted, we know all suffixes will've
                        // been inserted too.
                        break
                    }
                }
            }
        }
    }
    let words: Vec<_> = t.iter().map(|(a, b, c)| (a, b, c.expect("unset offset?"))).collect();
    println!("Lexicon: # words {}, byte size {}, with {} ({} bytes) non-exact matches",
             words.len(), output.len(), substring_overlaps, substring_o_bytes);
    (output, words)
}

// creates arrays t1, t2 and a shift such that `dat[i] == t2[t1[i >>
// shift] << shift + i & mask]`; this allows us to share blocks of
// length `1 << shift`, and so compress an array with a lot of repeats
// (like the 0's of the phrasebook_offsets below).
fn bin_data(dat: &[u32]) -> (Vec<u32>, Vec<u32>, usize) {
    let mut smallest = 0xFFFFFFFF;
    let mut data = (vec![], vec![], 0);
    let mut cache = HashMap::new();

    // brute force search for the shift that words best.
    for shift in 0..14 {
        cache.clear();

        let mut t1 = vec![];
        let mut t2 = vec![];
        for chunk in dat.chunks(1 << shift) {
            // have we stored this chunk already?
            let index = *match cache.entry(chunk) {
                hash_map::Entry::Occupied(o) => o.into_mut(),
                hash_map::Entry::Vacant(v) => {
                    // no :(, better put it in.
                    let index = t2.len();
                    t2.extend(chunk.iter().cloned());
                    v.insert(index)
                }
            };
            t1.push((index >> shift) as u32)
        }

        let my_size = t1.len() * util::smallest_type(t1.iter().map(|&x| x)) +
            t2.len() * util::smallest_type(t2.iter().map(|&x| x));
        println!("binning: shift {}, size {}", shift, my_size);
        if my_size < smallest {
            data = (t1, t2, shift);
            smallest = my_size
        }
    }

    // verify.
    {
        let (ref t1, ref t2, shift) = data;
        let mask = (1 << shift) - 1;
        for (i, &elem) in dat.iter().enumerate() {
            assert_eq!(elem, t2[((t1[i >> shift] << shift) + (i as u32 & mask)) as usize])
        }
    }

    data
}

fn write_codepoint_maps(ctxt: &mut Context, codepoint_names: Vec<(char, String)>) {
    let (lexicon_string, mut lexicon_words) = create_lexicon_and_offsets(codepoint_names.clone());

    let num_escapes = (lexicon_words.len() + 255) / 256;

    // we reserve the high bit (end of word) and 127,126... for
    // non-space splits.  The high bit saves about 10KB, and doing the
    // extra splits reduces the space required even more (e.g. - is a
    // reduction of 14KB).
    let short = 128 - SPLITTERS.len() - num_escapes;

    // find the `short` most common elements
    lexicon_words.sort_by(|a, b| a.cmp(b).reverse());

    // and then sort the rest into groups of equal length, to allow us
    // to avoid storing the full length table; just the indices. The
    // ordering is irrelevant here; just that they are in groups.
    lexicon_words[short..].sort_by(|&(_, ref a, _), &(_, ref b, _)| a.len().cmp(&b.len()));

    // the encoding for each word, to avoid having to recompute it
    // each time, we can just blit it out of here.
    let mut word_encodings = HashMap::new();
    for (i, x) in SPLITTERS.iter().enumerate() {
        // precomputed
        word_encodings.insert(vec![*x], vec![128 - 1 - i as u32]);
    }

    // the indices into the main string
    let mut lexicon_offsets = vec![];
    // and their lengths, for the most common strings, since these
    // have no information about their length (they were chosen by
    // frequency).
    let mut lexicon_short_lengths = vec![];
    let mut iter = lexicon_words.into_iter().enumerate();

    for (i, (_, word, offset)) in iter.by_ref().take(short) {
        lexicon_offsets.push(offset);
        lexicon_short_lengths.push(word.len());
        // encoded as a single byte.
        assert!(word_encodings.insert(word, vec![i as u32]).is_none())
    }

    // this stores (end point, length) for each block of words of a
    // given length, where `end point` is one-past-the-end.
    let mut lexicon_ordered_lengths = vec![];
    let mut previous_len = 0xFFFF;
    for (i, (_, word, offset)) in iter {
        let (hi, lo) = (short + i / 256, i % 256);
        assert!(short <= hi && hi < 128 - SPLITTERS.len());
        lexicon_offsets.push(offset);
        let len = word.len();
        if len != previous_len {
            if previous_len != 0xFFFF {
                lexicon_ordered_lengths.push((i, previous_len));
            }
            previous_len = len;
        }

        assert!(word_encodings.insert(word, vec![hi as u32, lo as u32]).is_none());
    }
    // don't forget the last one.
    lexicon_ordered_lengths.push((lexicon_offsets.len(), previous_len));

    // phrasebook encodes the words out of the lexicon that make each
    // codepoint name.
    let mut phrasebook = vec![0u32];
    // this is a map from `char` -> the index in phrasebook. it is
    // currently huge, but it has a lot of 0's, so we compress it
    // using the binning, below.
    let mut phrasebook_offsets = repeat(0).take(0x10FFFF + 1).collect::<Vec<_>>();
    let mut longest_name = 0;
    for &(cp, ref name) in codepoint_names.iter() {
        longest_name = cmp::max(name.len(), longest_name);

        let start = phrasebook.len() as u32;
        phrasebook_offsets[cp as usize] = start;

        let mut last_len = 0;
        for w in util::split(&**name, SPLITTERS) {
            let data = word_encodings.get(w.as_bytes()).expect(concat!("option on ", line!()));
            last_len = data.len();
            // info!("{}: '{}' {}", name, w, data);

            // blit the data.
            phrasebook.extend(data.iter().cloned())
        }

        // add the high bit to the first byte of the last encoded
        // phrase, to indicate the end.
        let idx = phrasebook.len() - last_len;
        phrasebook[idx] |= 0b1000_0000;
    }

    // compress the offsets, hopefully collapsing all the 0's.
    let (t1, t2, shift) = bin_data(&*phrasebook_offsets);

    w!(ctxt, "pub const MAX_NAME_LENGTH: usize = {};\n", longest_name);
    ctxt.write_plain_string("LEXICON", &*lexicon_string);
    ctxt.write_debugs("LEXICON_OFFSETS", "u16", &*lexicon_offsets);
    ctxt.write_debugs("LEXICON_SHORT_LENGTHS", "u8",
                        &*lexicon_short_lengths);
    ctxt.write_debugs("LEXICON_ORDERED_LENGTHS", "(usize, u8)",
                     &*lexicon_ordered_lengths);
    w!(ctxt, "pub static PHRASEBOOK_SHORT: u8 = {};\n", short);
    ctxt.write_debugs("PHRASEBOOK", "u8",
                        &*phrasebook);
    w!(ctxt, "pub static PHRASEBOOK_OFFSET_SHIFT: usize = {};\n", shift);
    ctxt.write_debugs("PHRASEBOOK_OFFSETS1",
                        &*util::smallest_u(t1.iter().map(|x| *x)),
                        &*t1);
    ctxt.write_debugs("PHRASEBOOK_OFFSETS2",
                        &*util::smallest_u(t2.iter().map(|x| *x)),
                        &*t2);
}

fn main() {
    let mut opts = getopts::Options::new();
    opts.optflag("p", "phf", "compute the name -> codepoint PHF");
    opts.optopt("l", "phf-lambda", "the lambda to use for PHF", "N");
    opts.optopt("t", "phf-tries", "the number of attempts when computing PHF", "N");
    opts.optflag("s", "silent", "don't write anything to files");
    opts.optopt("", "truncate", "only handle the first N", "N");
    opts.optflag("h", "help", "print this message");
    let matches = match opts.parse(std::env::args().skip(1)) {
        Ok(m) => m, Err(f) => panic!(f.to_string()),
    };

    if matches.opt_present("h") {
        println!("{}", opts.usage("generate compressed codepoint <-> name tables"));
        return
    }
    let do_phf = matches.opt_present("phf");
    let file = if matches.opt_present("s") {
        None
    } else if do_phf {
        Some(Path::new(PHF_OUT_FILE))
    } else {
        Some(Path::new(OUT_FILE))
    };

    let mut ctxt = Context {
        out: match file {
            Some(ref p) => Box::new(BufWriter::new(File::create(&p.with_extension("tmp")).unwrap()))
                as Box<Write>,
            None => Box::new(io::sink()) as Box<Write>
        }
    };
    ctxt.out.write(b"// autogenerated by generator.rs\n").unwrap();

    let lambda = matches.opt_str("phf-lambda");
    let tries = matches.opt_str("phf-tries");

    let (mut codepoint_names, cjk) = get_table_data();
    match matches.opt_str("truncate").map(
            |s| s.parse().ok().expect("truncate should be an integer")) {
        Some(n) => codepoint_names.truncate(n),
        None => {}
    }

    if do_phf {
        let (n, disps, data) =
            phf::create_phf(&*codepoint_names,
                            lambda.map(|s| s.parse().ok().expect("invalid -l")).unwrap_or(3),
                            tries.map(|s| s.parse().ok().expect("invalid -t")).unwrap_or(2));


        w!(ctxt, "pub static NAME2CODE_N: u64 = {};\n", n);
        ctxt.write_debugs("NAME2CODE_DISP",
                         "(u16, u16)",
                         &*disps);

        ctxt.write_debugs("NAME2CODE_CODE", "char", &*data);
    } else {
        if lambda.is_some() { println!("-l/--phf-lambda only applies with --phf") }
        if tries.is_some() { println!("-t/--phf-tries only applies with --phf") }

        write_cjk_ideograph_ranges(&mut ctxt, &*cjk);
        ctxt.out.write(b"\n").unwrap();
        write_codepoint_maps(&mut ctxt, codepoint_names);
    }

    match file {
        Some(f) => fs::rename(&f.with_extension("tmp"), &f).unwrap(),
        None => {}
    }
}
