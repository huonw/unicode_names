use std::mem;

pub fn smallest_index(n: uint) -> uint {
    for &x in [8, 16, 32, 64].iter() {
        if n < (1 << x) { return x / 8 }
    }
    fail!("{} too large", n)
}
pub fn smallest_type<I: Iterator<u32>>(mut x: I) -> uint {
    smallest_index(x.max().unwrap_or(0) as uint)
}
pub fn smallest_u<I: Iterator<u32>>(x: I) -> String {
    format!("u{}", 8 * smallest_type(x))
}
pub fn split<'a, 'b>(s: &'a str, splitters: &'b [u8]) -> Split<'a, 'b> {
    Split {
        s: s,
        splitters: splitters,
        pending: "",
        done: false,
    }
}

pub struct Split<'a, 'b> {
    s: &'a str,
    splitters: &'b [u8],
    pending: &'a str,
    done: bool,
}
impl<'a, 'b> Iterator<&'a str> for Split<'a, 'b> {
    fn next(&mut self) -> Option<&'a str> {
        if self.done {
            return None
        } else if self.s.is_empty() {
            self.done = true;
            return Some("")
        } else if !self.pending.is_empty() {
            return Some(mem::replace(&mut self.pending, ""))
        }

        for (i, b) in self.s.bytes().enumerate() {
            if b == b' ' || self.splitters.contains(&b) {
                let ret = self.s.slice_to(i);
                // dont include the space, but include everything else on the next step
                if b != b' ' {
                    self.pending = self.s.slice(i, i + 1)
                }
                self.s = self.s.slice_from(i + 1);
                return Some(ret)
            }
        }
        // trailing data
        self.done = true;
        return Some(self.s)
    }
}
