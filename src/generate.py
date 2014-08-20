#!/usr/bin/env python

# invoke like `python3 generate.py` (or via stdin), this assumes you
# have a copy of UnicodeData.txt in the current directory, and
# downloads it if not.
#
# (This should work with python 2.x too.)

import sys, fileinput, os, stat, contextlib
try:
    from urllib.request import urlretrieve # Python 3
except ImportError:
    from urllib import urlretrieve # Python 2

MOD_FILE = 'generated.rs'

UNICODE_DATA_URL = 'http://unicode.org/Public/7.0.0/ucd/UnicodeData.txt'
LINE_LIMIT = 95

def construct_tables(lines):
    all_names = []
    codepoint_to_name = []
    name_to_codepoint = []
    cjk_ideograph_ranges = []

    this_start = 1
    for line in lines:
        splits = line.split(';')
        codepoint = int(splits[0], 16)
        name = splits[1]
        if name.startswith('<'):
            assert name.endswith('>')
            name = name[1:-1]
            if name.startswith('CJK Ideograph'):
                if name.endswith('First'):
                    assert len(cjk_ideograph_ranges) % 2 == 0
                    cjk_ideograph_ranges.append(codepoint)
                elif name.endswith('Last'):
                    assert len(cjk_ideograph_ranges) % 2 == 1
                    cjk_ideograph_ranges.append(codepoint)
                else:
                    assert False, 'corrupted UnicodeData.txt'
            elif name.startswith('Hangul Syllable'):
                # verify that this is the only range we know of
                assert (name.endswith('First') and codepoint == 0xac00) or \
                       (name.endswith('Last') and codepoint == 0xd7a3), 'corrupted UnicodeData.txt'
        else:
            assert len(cjk_ideograph_ranges) % 2 == 0
            assert not name.startswith('CJK UNIFIED IDEOGRAPH-')
            assert not name.startswith('HANGUL SYLLABLE ')

            this_end = this_start + len(name) - 1
            name_range = (this_start, this_end)
            this_start = this_end + 2 # 0 byte

            if len(codepoint_to_name) and codepoint == codepoint_to_name[-1][1] + 1:
                codepoint_to_name[-1][1] += 1
                codepoint_to_name[-1][2].append(name_range)
            else:
                codepoint_to_name.append([codepoint, codepoint, [name_range]])

            name_to_codepoint.append((name, name_range, codepoint))
            all_names.append(name)

    # [a, b, c, d, ...] -> [(a, b), (c, d), ...]
    cjk_ideograph_ranges = zip(cjk_ideograph_ranges[0::2], cjk_ideograph_ranges[1::2])
    return {
        'c2n': codepoint_to_name,
        'n2c': name_to_codepoint,
        'ideoranges': cjk_ideograph_ranges,
        'all_names': '\\x00%s\\x00' % '\\x00'.join(all_names),
    }

def char(c):
    return "'\\U%08x'" % c
def string(s):
    return '"%s"' %s
def name_start(range):
    return 'S{idx:%d}' % range[0]
def name_end(range):
    return 'E{idx:%d}' % range[1]

def write_all_names(f, all_names):
    f.write("pub static ALL_NAMES: &'static str = concat!(\n    ");
    step = LINE_LIMIT - 10
    splits = []
    last = 0
    for end in range(step, len(all_names), step):
        i = all_names[end - 4:end].find('\\')
        if i >= 0:
            end += i - 4
        splits.append(string(all_names[last: end]))
        last = end
    splits.append(string(all_names[last:]))
    f.write(',\n    '.join(splits))
    f.write(');\n')

def write_codepoint_to_name(f, codepoint_to_name):
    f.write("pub static CHARACTER_TO_NAME: &'static [(char, char, &'static [S])] = &[")

    for i, (low, high, names) in enumerate(codepoint_to_name):
        f.write('''\n    (%s, %s, &[''' % (char(low), char(high)))
        width = LINE_LIMIT
        for r in names:
            this = name_start(r) + ','
            if 1 + width + len(this) >= LINE_LIMIT:
                f.write('\n         ')
                width = 8
            else:
                f.write(' ')
                width += 1

            f.write(this)
            width += len(this)
        f.write(']),')

    f.write('];\n')

def write_name_to_codepoint(f, name_to_codepoint):
    f.write(
        "pub static MAX_NAME_LENGTH: uint = %s;\n\n" % max(len(s) for s, _, _ in name_to_codepoint))
    # sort by the reverse of the strings: they are far, *far* more
    # unique and thus the binary search will be faster.
    name_to_codepoint.sort(key=lambda s: s[0][::-1])

    f.write("pub static NAME_TO_CHARACTER: &'static [(E, char)] = &[");

    width = LINE_LIMIT

    for _name, r, cp in name_to_codepoint:
        this = '(%s, %s),' % (name_end(r), char(cp))
        if 1 + width + len(this) >= LINE_LIMIT:
            f.write('\n    ')
            width = 4
        else:
            f.write(' ')
            width += 1

        f.write(this)
        width += len(this)
    f.write('];\n')

def write_cjk_ideograph_ranges(f, cjk_ideograph_ranges):
    f.write("pub static CJK_IDEOGRAPH_RANGES: &'static [(char, char)] = &[\n");
    for first, last in cjk_ideograph_ranges:
        f.write('    (%s, %s),\n' % (char(first), char(last)))
    f.write('];\n')

@contextlib.contextmanager
def modify_read_only(name):
    if os.path.exists(name):
        os.chmod(name, stat.S_IWUSR)

    f = open(name, 'wt')
    try:
        yield f
    finally:
        f.close()

    # mark file read-only
    os.chmod(name, stat.S_IRUSR|stat.S_IRGRP|stat.S_IROTH)

if __name__ == '__main__':
    if not os.path.exists('UnicodeData.txt'):
        print('UnicodeData.txt not found; downloading...')
        urlretrieve(UNICODE_DATA_URL, filename='UnicodeData.txt')

    d = construct_tables(open('UnicodeData.txt'))

    with modify_read_only(MOD_FILE) as f:
        f.write('''// autogenerated by generate.py
use core::iter::{mod, Iterator, DoubleEndedIterator, ExactSize};
use core::str::{mod, StrSlice};

/// An index pointing to the start of a word in ALL_NAMES
pub struct S { idx: u32 }
/// An index pointing to the end of a word in ALL_NAMES
pub struct E { idx: u32 }

impl S {
    pub fn as_str(&self) -> &'static str {
        let idx = self.idx as uint;
        let len = ALL_NAMES.slice_from(idx).bytes().position(|b| b == 0).unwrap_or(0);

        ALL_NAMES.slice(idx, idx + len)
    }
}
impl E {
    pub fn as_str(&self) -> &'static str {
        let idx = self.idx as uint + 1;
        let start = ALL_NAMES.slice_to(idx).bytes().rposition(|b| b == 0).unwrap_or(0);

        ALL_NAMES.slice(start + 1, idx)
    }
    pub fn rev_bytes(&self) -> iter::TakeWhile<'static, u8, iter::Rev<str::Bytes<'static>>> {
        ALL_NAMES.slice_to(self.idx as uint + 1).bytes().rev().take_while(|&b| b != 0)
    }
}

''')
        write_all_names(f, d['all_names'])
        f.write('\n')
        write_codepoint_to_name(f, d['c2n'])
        f.write('\n')
        write_name_to_codepoint(f, d['n2c'])
        f.write('\n')
        write_cjk_ideograph_ranges(f, d['ideoranges'])
