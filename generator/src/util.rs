use std::mem;

pub fn smallest_index(n: usize) -> usize {
    for &x in [8, 16, 32].iter() {
        if n < (1 << x) { return x / 8 }
    }
    64
}
pub fn smallest_type<I: Iterator<Item = u32>>(x: I) -> usize {
    smallest_index(x.max().unwrap_or(0) as usize)
}
pub fn smallest_u<I: Iterator<Item = u32>>(x: I) -> String {
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
impl<'a, 'b> Iterator for Split<'a, 'b> {
    type Item = &'a str;
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
                let ret = &self.s[..i];
                // dont include the space, but include everything else on the next step
                if b != b' ' {
                    self.pending = &self.s[i..i+1]
                }
                self.s = &self.s[i+1..];
                return Some(ret)
            }
        }
        // trailing data
        self.done = true;
        return Some(self.s)
    }
}

#[test]
fn test_split() {
    let tests: &[(&str, &[&str])] =
        &[("a", &["a"]),
          ("a b", &["a", "b"]),
          (" a b ", &["", "a", "b", ""]),
          ("a-b", &["a", "-", "b"]),
          ("a- b", &["a", "-", "", "b"]),
          ("a -b", &["a", "", "-", "b"])];

    for &(s, v) in tests.iter() {
        assert_eq!(split(s, b"-").collect::<Vec<&str>>().as_slice(), v)
    }
}
