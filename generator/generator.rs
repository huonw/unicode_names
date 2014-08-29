#![feature(macro_rules)]

extern crate getopts;

use std::io::{File, BufferedReader, BufferedWriter, mod};
use std::collections::HashMap;

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
static IN_FILE: &'static str = "../data/codepoint_name.csv";

static SPLITTERS: &'static [u8] = b"-";

fn get_table_data() -> (Vec<(u32, String)>, Vec<(u32, u32)>) {
    fn extract(line: &str) -> (u32, &str) {
        let mut splits = line.split(';');
        let cp = splits.next().and_then(from_str).unwrap_or_else(|| fail!("invalid {}", line));
        let name = splits.next().unwrap_or_else(|| fail!("missing name {}", line));
        (cp, name)
    }

    let mut r = BufferedReader::new(File::open(&Path::new(IN_FILE)));
    let mut iter = r.lines().map(|x| x.unwrap());

    let mut codepoint_names = vec![];
    let mut cjk_ideograph_ranges = vec![];

    for l in iter {
        let (cp, name) = extract(l.as_slice().trim());
        if name.starts_with("<") {
            assert!(name.ends_with(">"), "should >: {}", name);
            let name = name.slice(1, name.len() - 1);
            if name.starts_with("CJK Ideograph") {
                assert!(name.ends_with("First"));
                // should be CJK Ideograph ..., Last
                let line2 = iter.next().expect("unclosed ideograph range");
                let (cp2, name2) = extract(line2.as_slice().trim());
                assert_eq!(name.replace("First", "Last").as_slice(),
                           name2.slice(1, name2.len() - 1));

                cjk_ideograph_ranges.push((cp, cp2));
            } else if name.starts_with("Hangul Syllable") {
                // the main lib only knows this range, so lets make
                // sure we're not going out of date wrt the unicode
                // standard.
                if name.ends_with("First") {
                    assert_eq!(cp, 0xAC00);
                } else if name.ends_with("Last") {
                    assert_eq!(cp, 0xD7A3);
                } else {
                    fail!("unknown hangul syllable {}", name)
                }
            }
        } else {
            codepoint_names.push((cp, name.to_string()))
        }
    }
    (codepoint_names, cjk_ideograph_ranges)
}

fn write_cjk_ideograph_ranges(ctxt: &mut Context, ranges: &[(u32, u32)]) {
    ctxt.write_array("CJK_IDEOGRAPH_RANGES", "(char, char)", ranges,
                     |&(a, b)| format!("({}, {})", formatting::chr(a), formatting::chr(b)))
}

fn create_lexicon_and_offsets(mut codepoint_names: Vec<(u32, String)>) -> (String,
                                                                           Vec<(uint, Vec<u8>,
                                                                                uint)>) {
    codepoint_names.sort_by(|a, b| a.ref1().len().cmp(&b.ref1().len()).reverse());

    let mut t = trie::Trie::new();
    let mut output = String::new();

    for &(_, ref name) in codepoint_names.iter() {
        for n in util::split(name.as_slice(), SPLITTERS) {
            if n.len() == 1 && SPLITTERS.contains(&n.as_bytes()[0]) {
                continue
            }

            if t.insert(n.bytes(), None, false).is_none() {
                // new element
                let offset = output.len();
                t.set_offset(n.bytes(), offset);
                output.push_str(n);

                for i in range(1, n.len()) {
                    if !t.insert(n.slice_from(i).bytes(), Some(offset + i), true).is_none() {
                        break
                    }
                }
            }
        }
    }
    let words: Vec<_> = t.iter().map(|(a, b, c)| (a, b, c.expect("unset offset?"))).collect();
    println!("Lexicon: # words {}, byte size {}", words.len(), output.len());
    (output, words)
}

fn bin_data(dat: &[u32]) -> (Vec<u32>, Vec<u32>, uint) {
    let mut smallest = 0xFFFFFFFF;
    let mut data = (vec![], vec![], 0);
    let mut cache = HashMap::new();
    for shift in range(0, 14) {
        cache.clear();

        let mut t1 = vec![];
        let mut t2 = vec![];
        for chunk in dat.chunks(1 << shift) {
            let index = *cache.find_or_insert_with(chunk, |_| {
                let index = t2.len();
                t2.push_all(chunk);
                index
            });
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

    {
        let (ref t1, ref t2, shift) = data;
        let mask = (1 << shift) - 1;
        for (i, &elem) in dat.iter().enumerate() {
            assert_eq!(elem, t2[((t1[i >> shift] << shift) + (i as u32 & mask)) as uint])
        }
    }

    data
}

fn write_codepoint_maps(ctxt: &mut Context, codepoint_names: Vec<(u32, String)>) {
    let (lexicon_string, mut lexicon_words) = create_lexicon_and_offsets(codepoint_names.clone());

    let num_escapes = (lexicon_words.len() + 255) / 256;

    // we reserve 255 (end of word) and 254... for non-space splits.
    // Doing these extra splits reduces the space required even more
    // (e.g. - is a reduction of 20KB)

    let short = 256 - 1 - SPLITTERS.len() - num_escapes;

    lexicon_words.sort_by(|a, b| a.cmp(b).reverse());

    let mut lexicon_offsets = vec![];
    let mut lexicon_lengths = vec![];
    let mut word_encodings = HashMap::new();
    for (i, x) in SPLITTERS.iter().enumerate() {
        word_encodings.insert(vec![*x], vec![254 - i as u32]);
    }

    let mut iter = lexicon_words.move_iter().enumerate();
    for (i, (_, word, offset)) in iter.by_ref().take(short) {
        lexicon_offsets.push(offset);
        lexicon_lengths.push(word.len());
        assert!(word_encodings.insert(word, vec![i as u32]))
    }
    for (i, (_, word, offset)) in iter {
        let (hi, lo) = (short + i / 256, i % 256);
        assert!(short <= hi && hi < 256 - 1 - SPLITTERS.len());
        lexicon_offsets.push(offset);
        lexicon_lengths.push(word.len());
        assert!(word_encodings.insert(word, vec![hi as u32, lo as u32]));
    }

    let mut phrasebook = vec![0u32];
    let mut phrasebook_offsets = Vec::from_elem(0x10FFFF + 1, 0);
    for &(cp, ref name) in codepoint_names.iter() {
        let start = phrasebook.len() as u32;
        *phrasebook_offsets.get_mut(cp as uint) = start;

        for w in util::split(name.as_slice(), SPLITTERS) {
            phrasebook.push_all(word_encodings.find_equiv(&w.as_bytes()).unwrap().as_slice())
        }
        phrasebook.push(255);
    }

    let (t1, t2, shift) = bin_data(phrasebook_offsets.as_slice());

    ctxt.write_plain_string("LEXICON", lexicon_string.as_slice());
    ctxt.write_shows("LEXICON_OFFSETS", "u16", lexicon_offsets.as_slice());
    ctxt.write_shows("LEXICON_LENGTHS", "u8", lexicon_lengths.as_slice());
    w!(ctxt, "pub static PHRASEBOOK_SHORT: u8 = {};\n", short);
    ctxt.write_shows("PHRASEBOOK", "u8", phrasebook.as_slice());
    w!(ctxt, "pub static PHRASEBOOK_OFFSET_SHIFT: uint = {};\n", shift);
    ctxt.write_shows("PHRASEBOOK_OFFSETS1", util::smallest_u(t1.iter().map(|x| *x)).as_slice(),
                     t1.as_slice());
    ctxt.write_shows("PHRASEBOOK_OFFSETS2", util::smallest_u(t2.iter().map(|x| *x)).as_slice(),
                     t2.as_slice());

    // TODO hash table
}

fn main() {
    let opts = [
        getopts::optflag("p", "phf", "compute the name -> codepoint PHF"),
        getopts::optopt("l", "phf-lambda", "the lambda to use for PHF", "N"),
        getopts::optopt("t", "phf-tries", "the number of attempts when computing PHF", "N"),
        getopts::optflag("s", "silent", "don't write anything to files"),
        getopts::optopt("", "truncate", "only handle the first N", "N"),
        getopts::optflag("h", "help", "print this message"),
        ];
    let matches = match getopts::getopts(std::os::args().tail(), opts) {
        Ok(m) => m, Err(f) => fail!(f.to_string()),
    };

    if matches.opt_present("h") {
        println!("{}", getopts::usage("generate compressed codepoint <-> name tables", opts));
        return
    }
    let do_phf = matches.opt_present("phf");
    let mut ctxt = Context {
        out: if matches.opt_present("s") {
            box io::util::NullWriter
        } else {
            let name = if do_phf {PHF_OUT_FILE} else {OUT_FILE};
            box BufferedWriter::new(File::create(&Path::new(name)))
        }
    };
    ctxt.out.write_str("// autogenerated by generator.rs\n").unwrap();

    let lambda = matches.opt_str("phf-lambda");
    let tries = matches.opt_str("phf-tries");

    let (mut codepoint_names, cjk) = get_table_data();
    match matches.opt_str("truncate").map(|s| from_str(s.as_slice()).unwrap()) {
        Some(n) => codepoint_names.truncate(n),
        None => {}
    }

    if do_phf {
        let (n, disps, data) =
            phf::create_phf(codepoint_names.as_slice(),
                            lambda.map(|s| from_str(s.as_slice()).unwrap()).unwrap_or(3),
                            tries.map(|s| from_str(s.as_slice()).unwrap()).unwrap_or(2));


        w!(ctxt, "pub static NAME2CODE_N: u64 = {};\n", n);
        ctxt.write_shows("NAME2CODE_DISP", "(u16, u16)", disps.as_slice());

        ctxt.write_shows("NAME2CODE_CODE", "u32", data.as_slice());
    } else {
        if lambda.is_some() { println!("-l/--phf-lambda only applies with --phf") }
        if tries.is_some() { println!("-t/--phf-tries only applies with --phf") }

        write_cjk_ideograph_ranges(&mut ctxt, cjk.as_slice());
        ctxt.out.write_str("\n").unwrap();
        write_codepoint_maps(&mut ctxt, codepoint_names);
    }
}
