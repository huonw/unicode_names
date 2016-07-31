use std::fmt::Debug;
use std::io::prelude::*;

static LINE_LIMIT: usize = 95;

pub struct Context {
    pub out: Box<Write+'static>
}

impl Context {
    pub fn write_array<T, F>(&mut self, name: &str, ty: &str, elements: &[T], format: F)
            where F: Fn(&T) -> String{
        w!(self, "pub static {}: &'static [{}] = &[", name, ty);

        let mut width = LINE_LIMIT;
        for e in elements.iter() {
            let mut text = format(e);
            text.push_str(",");
            if 1 + width + text.len() >= LINE_LIMIT {
                w!(self, "\n    ");
                width = 4;
            } else {
                w!(self, " ");
                width += 1
            }
            w!(self, "{}", text);
            width += text.len()
        }
        w!(self, "];\n");
    }

    pub fn write_debugs<T: Debug>(&mut self, name: &str, ty: &str, elements: &[T]) {
        self.write_array(name, ty, elements, |x| format!("{:?}", x))
    }

    pub fn write_plain_string(&mut self, name: &str, data: &str) {
        assert!(!data.contains('\\'));

        w!(self, "pub static {}: &'static str = \"", name);

        for chunk in data.as_bytes().chunks(LINE_LIMIT - 6) {
            w!(self, "\\\n    ");
            self.out.write_all(chunk).unwrap();
        }
        w!(self, "\";\n");
    }
}
