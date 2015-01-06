use core::prelude::*;
use core::{slice, fmt};

use generated::{PHRASEBOOK_SHORT, PHRASEBOOK, LEXICON_SHORT_LENGTHS, LEXICON_ORDERED_LENGTHS,
                LEXICON_OFFSETS, LEXICON};

#[derive(Clone)]
pub struct IterStr {
    phrasebook: slice::Iter<'static u8>,
    last_was_word: bool
}

impl IterStr {
    pub fn new(start_index: uint) -> IterStr {
        IterStr {
            phrasebook: PHRASEBOOK.slice_from(start_index).iter(),
            last_was_word: false
        }
    }
}

static HYPHEN: u8 = 127;

impl Iterator for IterStr {
    type Item = &'static str;
    fn next(&mut self) -> Option<&'static str> {
        let mut tmp = self.phrasebook;
        tmp.next().map(|&raw_b| {
            // the first byte includes if it is the last in this name
            // in the high bit.
            let (is_end, b) = (raw_b & 0b1000_0000 != 0,
                               raw_b & 0b0111_1111);

            let ret = if b == HYPHEN {
                // have to handle this before the case below, because a -
                // replaces the space entirely.
                self.last_was_word = false;
                "-"
            } else if self.last_was_word {
                self.last_was_word = false;
                // early return, we don't want to update the
                // phrasebook (i.e. we're pretending we didn't touch
                // this byte).
                return " "
            } else {
                self.last_was_word = true;

                let idx;
                let length = if b < PHRASEBOOK_SHORT {
                    idx = b as uint;
                    // these lengths are hard-coded
                    LEXICON_SHORT_LENGTHS[idx] as uint
                } else {
                    idx = (b - PHRASEBOOK_SHORT) as uint * 256 +
                        (*tmp.next().unwrap()) as uint;

                    // search for the right place: the first one where
                    // the end-point is after our current index.
                    match LEXICON_ORDERED_LENGTHS.iter().find(|&&(end, _)| idx < end) {
                        Some(&(_, len)) => len as uint,
                        None => unreachable!()
                    }
                };
                let offset = LEXICON_OFFSETS[idx] as uint;
                LEXICON.slice(offset, offset + length)
            };
            self.phrasebook = if is_end {
                (&[]).iter()
            } else {
                tmp
            };
            ret
        })
    }
}

impl fmt::Show for IterStr {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        let mut printed = self.clone();
        for s in printed {
            try!(write!(fmtr, "{}", s))
        }
        Ok(())
    }
}
