#!/usr/bin/env python

# invoke like `python3 generate.py` (or via stdin), this assumes you
# have a copy of UnicodeData.txt in the current directory, and
# downloads it if not.
#
# (This should work with python 2.x too.)

import sys, fileinput, os, stat, urllib, contextlib

MOD_FILE = 'generated.rs'

UNICODE_DATA_URL = 'http://unicode.org/Public/7.0.0/ucd/UnicodeData.txt'
LINE_LIMIT = 95

def construct_tables(lines):
    codepoint_to_name = []
    name_to_codepoint = []

    for line in lines:
        splits = line.split(';')
        codepoint = int(splits[0], 16)
        name = splits[1]
        if name.startswith('<'):
            continue
        if len(codepoint_to_name) and codepoint == codepoint_to_name[-1][1] + 1:
            codepoint_to_name[-1][1] += 1
            codepoint_to_name[-1][2].append(name)
        else:
            codepoint_to_name.append([codepoint, codepoint, [name]])

        name_to_codepoint.append((name, codepoint))
    return codepoint_to_name, name_to_codepoint

def char(c):
    return "'\\U%08x'" % c
def string(s):
    return '"%s"' %s

def write_codepoint_to_name(f, codepoint_to_name):
    f.write("pub static CHARACTER_TO_NAME: &'static [(char, char, &'static [&'static str])] = &[")

    for i, (low, high, names) in enumerate(codepoint_to_name):
        f.write('''\n    (%s, %s, &[''' % (char(low), char(high)))
        width = LINE_LIMIT
        for n in names:
            this = string(n) + ','
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
    f.write("pub static MAX_NAME_LENGTH: uint = %s;\n\n" % max(len(s) for s, _ in name_to_codepoint))
    # sort by the reverse of the strings: they are far, *far* more
    # unique and thus the binary search will be faster.
    name_to_codepoint.sort(key=lambda s: s[0][::-1])

    f.write("pub static NAME_TO_CHARACTER: &'static [(&'static str, char)] = &[");

    width = LINE_LIMIT

    for name, cp in name_to_codepoint:
        this = '(%s, %s),' % (string(name), char(cp))
        if 1 + width + len(this) >= LINE_LIMIT:
            f.write('\n    ')
            width = 4
        else:
            f.write(' ')
            width += 1

        f.write(this)
        width += len(this)
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
        urllib.request.urlretrieve(UNICODE_DATA_URL, filename='UnicodeData.txt')

    c2n, n2c = construct_tables(open('UnicodeData.txt'))

    with modify_read_only(MOD_FILE) as f:
        write_codepoint_to_name(f, c2n)
        f.write('\n')
        write_name_to_codepoint(f, n2c)
