use std::collections::HashMap;
use std::collections::hash_map::{self, Entry};

pub struct Trie {
    children: HashMap<u8, Trie>,
    count: uint,
    offset: Option<uint>,
}

impl Trie {
    pub fn new() -> Trie {
        Trie {
            children: HashMap::new(),
            count: 0,
            offset: None
        }
    }

    pub fn get_child(&mut self, b: u8) -> &mut Trie {
        // this shouldn't be necessary, but the .entry API is broken
        // at the moment.
        let b_ref = unsafe {&*(&b as *const _)};
        match self.children.entry(b_ref) {
            Entry::Occupied(o) => o.into_mut(),
            Entry::Vacant(v) => v.insert(Trie::new())
        }
    }

    pub fn set_offset<I: Iterator<Item = u8>>(&mut self, mut it: I, offset: uint) {
        if self.offset.is_none() {
            self.offset = Some(offset)
        }
        match it.next() {
            None => {}
            Some(b) => self.get_child(b).set_offset(it, offset),
        }
    }
    /// insert the value given by the sequence `it`, returning a tuple
    /// (is this a substring already in the tree, was this exact
    /// sequence previously inserted).
    pub fn insert<I: Iterator<Item = u8>>(&mut self, mut it: I, offset: Option<uint>,
                                          weak: bool) -> (bool, bool) {
        let ret = match it.next() {
            None => {
                let old_count = self.count;
                if !weak { self.count += 1 }

                (self.offset.is_some(), old_count > 0)
            }
            Some(b) => self.get_child(b).insert(it, offset, weak)
        };
        if self.offset.is_none() { self.offset = offset }
        ret
    }

    pub fn iter(&self) -> Items {
        Items {
            parents: vec![],
            current: Some(self),
            stack: vec![],
        }
    }
}

pub struct Items<'a> {
    parents: Vec<u8>,
    current: Option<&'a Trie>,
    stack: Vec<hash_map::Iter<'a, u8, Trie>>
}

impl<'a> Iterator for Items<'a> {
    type Item = (uint, Vec<u8>, Option<uint>);
    fn next(&mut self) -> Option<(uint, Vec<u8>, Option<uint>)> {
        'outer: loop {
            match self.current {
                Some(t) => {
                    self.current = None;
                    self.stack.push(t.children.iter());
                    if t.count > 0 {
                        return Some((t.count, self.parents.clone(), t.offset))
                    }
                }
                None => {}
            }


            loop {
                match self.stack.pop() {
                    None => return None,
                    Some(mut it) => {
                        match it.next() {
                            Some((&b, t)) => {
                                self.parents.push(b);
                                self.current = Some(t);
                                self.stack.push(it);
                                continue 'outer
                            }
                            None => { self.parents.pop(); }
                        }
                    }
                }
            }
        }
    }
}
