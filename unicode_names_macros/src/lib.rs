//! A macro that maps unicode names to chars and strings.

#![crate_type="dylib"]

#![feature(quote, plugin_registrar, macro_rules, phase)]

extern crate syntax;
extern crate rustc;

extern crate regex;
#[phase(plugin)] extern crate regex_macros;

extern crate unicode_names;

use syntax::ast;
use syntax::codemap;
use syntax::parse::token;
use syntax::ext::base::{mod, ExtCtxt, MacResult, MacExpr, DummyResult};
use syntax::ext::build::AstBuilder;
use rustc::plugin::Registry;

#[plugin_registrar]
#[doc(hidden)]
pub fn plugin_registrar(registrar: &mut Registry) {
    registrar.register_macro("named_char", named_char);
    registrar.register_macro("named", named);
}
fn named_char(cx: &mut ExtCtxt, sp: codemap::Span,
              tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
    match base::get_single_str_from_tts(cx, sp, tts, "named_char") {
        None => {}
        Some(name) => match unicode_names::character(name.as_slice()) {
            None => cx.span_err(sp, format!("`{}` does not name a character", name).as_slice()),

            // everything worked!
            Some(c) => return MacExpr::new(cx.expr_lit(sp, ast::LitChar(c))),
        }
    }
    // failed :(
    DummyResult::expr(sp)
}
fn named(cx: &mut ExtCtxt, sp: codemap::Span, tts: &[ast::TokenTree]) -> Box<MacResult+'static> {
    let string = match base::get_single_str_from_tts(cx, sp, tts, "named") {
        None => return DummyResult::expr(sp),
        Some(s) => s
    };

     // make sure unclosed braces don't escape.
    static NAMES: regex::Regex = regex!(r"\\N\{(.*?)(?:\}|$)");

    let new = NAMES.replace_all(string.as_slice(), |c: &regex::Captures| {
        if !c.at(0).ends_with("}") {
            cx.span_err(sp, format!("unclosed escaped in `named!`: {}", c.at(0)).as_slice());
        } else {
            match unicode_names::character(c.at(1)) {
                Some(c) => return String::from_char(1, c),
                None => {
                    cx.span_err(sp, format!("`{}` does not name a character", c.at(1)).as_slice());
                }
            }
        }
        // failed :(
        String::new()
    });

    MacExpr::new(cx.expr_str(sp, token::intern_and_get_ident(new.as_slice())))
}
