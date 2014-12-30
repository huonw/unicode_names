use std::char;
use std::fmt::Show;

static LINE_LIMIT: uint = 95;

pub struct Context {
    pub out: Box<Writer+'static>
}

pub fn chr(c: u32) -> String { char::from_u32(c).unwrap().escape_unicode().collect() }

impl Context {
    pub fn write_array<T>(&mut self, name: &str, ty: &str, elements: &[T], format: |&T| -> String) {
        w!(self, "#[inline(never)] pub static {}: &'static [{}] = &[", name, ty);

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

    pub fn write_shows<T: Show>(&mut self, name: &str, ty: &str, elements: &[T]) {
        self.write_array(name, ty, elements, |x| x.to_string())
    }

    pub fn write_plain_string(&mut self, name: &str, data: &str) {
        assert!(!data.contains_char('\\'));

        w!(self, "#[inline(never)] pub static {}: &'static str = \"", name);

        for chunk in data.as_bytes().chunks(LINE_LIMIT - 6) {
            w!(self, "\\\n    ");
            self.out.write(chunk).unwrap();
        }
        w!(self, "\";\n");
    }
}
