use core::{slice, fmt};
use core::slice::ImmutableSlice;
use core::str::StrSlice;
use core::iter::Iterator;
use core::option::{Some, None, Option};
use core::result::{Err, Ok};

use generated2::{PHRASEBOOK_SHORT, PHRASEBOOK, LEXICON_LENGTHS, LEXICON_OFFSETS, LEXICON};

pub struct IterStr {
    phrasebook: slice::Items<'static u8>,
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

impl Iterator<&'static str> for IterStr {
    fn next(&mut self) -> Option<&'static str> {
        let mut tmp = self.phrasebook;
        match tmp.next() {
            None => None,
            Some(&0) => {
                self.phrasebook = (&[]).iter();
                None
            }
            Some(_) if self.last_was_word => {
                self.last_was_word = false;
                Some(" ")
            }
            Some(&b) => {
                self.phrasebook = tmp;
                self.last_was_word = true;

                let idx = if b <= PHRASEBOOK_SHORT {
                    b as uint
                } else {
                    PHRASEBOOK_SHORT as uint +
                        (b - PHRASEBOOK_SHORT - 1) as uint * 255 +
                        (*self.phrasebook.next().unwrap()) as uint
                };

                let offset = LEXICON_OFFSETS[idx] as uint;
                let length = LEXICON_LENGTHS[idx] as uint;
                Some(LEXICON.slice(offset, offset + length))
            }
        }
    }
}

impl fmt::Show for IterStr {
    fn fmt(&self, fmtr: &mut fmt::Formatter) -> fmt::Result {
        let mut printed = *self;
        for s in printed {
            try!(write!(fmtr, "{}", s))
        }
        Ok(())
    }
}
