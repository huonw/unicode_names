#!/usr/bin/env python

# invoke like `python3 generate.py` (or via stdin), this assumes you
# have a copy of UnicodeData.txt in the current directory, and
# downloads it if not.
#
# (This should work with python 2.x too.)

import sys, fileinput, os, stat, contextlib
from collections import defaultdict, Counter
try:
    from urllib.request import urlretrieve # Python 3
except ImportError:
    from urllib import urlretrieve # Python 2

MOD_FILE = 'generated2.rs'

UNICODE_DATA_URL = 'http://unicode.org/Public/7.0.0/ucd/UnicodeData.txt'
LINE_LIMIT = 95

def construct_tables(lines):
    codepoint_names = []
    cjk_ideograph_ranges = []

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

            codepoint_names.append((codepoint, name))

    # [a, b, c, d, ...] -> [(a, b), (c, d), ...]
    cjk_ideograph_ranges = zip(cjk_ideograph_ranges[0::2], cjk_ideograph_ranges[1::2])
    return {
        'codepoint_names': codepoint_names,
        'ideoranges': cjk_ideograph_ranges,
    }


def char(c):
    return "'\\U%08x'" % c
def string(s):
    return '"%s"' %s

def write_array(f, name, type, elements, format=str):
    f.write("pub static %s: &'static [%s] = &[" % (name, type))
    width = LINE_LIMIT
    for e in elements:
        text = format(e) + ','
        if 1 + width + len(text) >= LINE_LIMIT:
            f.write('\n    ')
            width = 4
        else:
            f.write(' ')
            width += 1
        f.write(text)
        width += len(text)
    f.write('];\n')

def write_string(f, name, s):
    f.write("pub static %s: &'static str = concat!(\n    " % name);
    step = LINE_LIMIT - 10
    splits = []
    last = 0
    for end in range(step, len(s), step):
        i = s[end - 4:end].find('\\')
        if i >= 0:
            end += i - 4
        splits.append(string(s[last: end]))
        last = end
    splits.append(string(s[last:]))
    f.write(',\n    '.join(splits))
    f.write(');\n')


def smallest_index(n):
    for x in [8, 16, 32, 64]:
        if n < (1 << x):
            return x / 8
    raise ValueError('%s too large' % n)
def smallest_type(array):
    return smallest_index(max(array))
def smallest_u(array):
    return 'u%d' % (8*smallest_type(array))

class Trie:
    def __init__(self):
        self.children = defaultdict(Trie)
        self.element = 0
        self.offset = None
        self.strong_chain = False

    def set_offset(self, sequence, offset):
        if not self.offset:
            self.offset = offset
        if sequence:
            head = sequence[0]
            rest = sequence[1:]
            assert head in self.children
            self.children[head].set_offset(rest, offset)

    def insert(self, sequence, offset=None, weak=False):
        if not sequence:
            ret = self.offset, self.element, self.strong_chain
            if not weak:
                self.element += 1
        else:
            head = sequence[0]
            rest = sequence[1:]
            ret = self.children[head].insert(rest, offset, weak)

        if self.offset is None:
            # every prefix of this sequence has the same offset
            self.offset = offset
        if not weak:
            self.strong_chain = True

        return ret

    def __len__(self):
        return self.element + sum(len(c) for c in self.children.values())
    def count_subtries(self):
        return 1 + sum(c.count_subtries() for c in self.children.values())
    def count_leaves(self):
        if len(self.children) == 0:
            return 1
        else:
            return sum(c.count_leaves() for c in self.children.values())

    def compress(self, separator = ' '):
        for k, child in self.children.items():
            child.compress()
            if len(child.children) == 1:
                if child.element:
                    pass
                else:
                    sub_k, sub_child = child.children.items()[0]
                    new_key = k + separator + sub_k
                    del self.children[k]
                    assert new_key not in self.children
                    self.children[new_key] = sub_child

    def traverse(self, parents):
        for k, child in self.children.items():
            for x in child.traverse(parents + [k]):
                yield x
        if self.element:
            yield parents, self.element, self.offset

    def __iter__(self):
        return self.traverse([])
    def __str__(self):
        return repr(self)
    def __repr__(self):
        return '%s' % dict(self.children)



def hash(s, h=5380):
    for c in s:
        h = ((h << 5) + h) + ord(c)
    return h & ((1<<64) - 1)
def hash_(s, h=5380):
    return hash(s, h)
def hash2(s, magic=601): # 40714 collisions
    h = 0
    for c in s:
        c = ord(c.upper())
        h = (h * magic) + c
        ix = h & 0xFF000000
        if ix:
            h = (h ^ ((ix>>24)&0xFF)) & 0x00FFFFFF
    return h

def make_hash_table(codepoint_names, hasher):
    import math
    size = 1 << (int(math.ceil(math.log(len(codepoint_names), 2))))
    assert size == (1 << 15)

    mask = size - 1
    table = [0] * size
    collisions = 0
    out_of_place = 0
    for codepoint, name in codepoint_names:
        h = hasher(name) & mask
        if table[h] != 0:
            out_of_place += 1
            incr =(h ^ (h >> 3)) & mask
            if not incr:
                incr = mask
            while table[h] != 0:
                collisions += 1
                h = (h + incr) & mask
                incr *= 2
                if incr > mask:
                    incr ^= 32768 + 3
        table[h] = codepoint
    return table, collisions, out_of_place
def search_hash_tables(codepoint_names):
    import random

    best = 1e100
    data = None
    for i in range(0, 100):
        x = random.randint(0, 1000)
        t, col, oop = make_hash_table(codepoint_names, lambda s: hash2(s, x))
        if col < best:
            best = col
            data = x, t, col, oop
    print data[0], data[2]

def write_cjk_ideograph_ranges(f, cjk_ideograph_ranges):
    write_array(f, 'CJK_IDEOGRAPH_RANGES', '(char, char)', cjk_ideograph_ranges,
                lambda c: '(%s, %s)' % (char(c[0]), char(c[1])))



def encode_length_offset(length, offset):
    assert offset < (1 << 17) and length < (1 << (32 - 17))
    return (length << 17) | offset
def create_lexicon_and_offsets(codepoint_names):
    # returns (string, [(int,string,int)]) where string is the raw
    # data, and the list of tuples contains -frequency, word, and offset

    codepoint_names.sort(key=lambda s: -len(s[1]))

    t = Trie()
    original_length = 0
    seen = set()
    output = []
    length = 0
    offsets = {}
    map = []
    for _, name in codepoint_names:
        for n in name.split():
            if n not in seen:
                original_length += len(n)
                seen.add(n)
            #t.insert(n[::-1])
            # not a substring
            offset = length
            maybe_offset, is_main, strong_chain = t.insert(n)
            if maybe_offset is None:
                length += len(n)
                t.set_offset(n, offset)
                output.append(n)

                for i in range(1, len(n)):
                    if t.insert(n[i:], offset = offset + i, weak=True)[0] is not None:
                        break
            else:
                offset = maybe_offset

    map = [(-freq, ''.join(elem), offset)
           for elem, freq, offset in t]
    raw_output = ''.join(output)
    print('lexicon from %d -> %d' % (original_length, len(raw_output)))
    return raw_output, map

def bin_data(array):
    smallest_size = 1e100
    data = None
    array = tuple(array)
    for shift in range(0, 14):
        slice_size = 1 << shift
        cache = {}
        t1 = []
        t2 = []
        for i in range(0, len(array), slice_size):
            slice = array[i:i + slice_size]
            try:
                index = cache[slice]
            except KeyError:
                index = len(t2)
                cache[slice] = index
                t2.extend(slice)
            t1.append(index >> shift)
        my_size = len(t1) * smallest_type(t1) + len(t2) * smallest_type(t2)
        print shift, my_size, len(t1), len(t2), smallest_type(t1), smallest_type(t2)
        if my_size < smallest_size:
            data = t1, t2, shift
            smallest_size = my_size
    t1, t2, shift = data
    mask = (1 << shift) - 1
    for i in range(len(array)):
        assert array[i] == t2[(t1[i >> shift] << shift) + (i & mask)]

    return data

def write_codepoint_maps(f, codepoint_names):
    raw_codepoint_names = codepoint_names[:]
    lexicon_string, lexicon_words = create_lexicon_and_offsets(codepoint_names)

    print len(lexicon_words)
    # we can't use 0
    # TODO, investigate using a single bit, rather than many bytes
    escapes = (len(lexicon_words) + 254) / 255
    short = 255 - escapes
    lexicon_words.sort()

    lexicon_offsets = [0]
    lexicon_lengths = [0]
    word_encodings = {}
    for i, (_, word, offset) in enumerate(lexicon_words[:short]):
        lexicon_offsets.append(offset)
        lexicon_lengths.append(len(word))
        word_encodings[word] = [i + 1]

    for i, (_, word, offset) in enumerate(lexicon_words[short:]):
        lo = 1 + (i % 255)
        hi = short + 1 + (i / 255)
        assert short + 1 <= hi < 256
        lexicon_offsets.append(offset)
        lexicon_lengths.append(len(word))
        word_encodings[word] = [hi, lo]
    # check we don't have any encoded 0s
    for w, enc in word_encodings.items():
        assert 0 not in enc

    phrasebook = [0]
    phrasebook_offsets = [0] * (0x10FFFF + 1)
    for cp, name in codepoint_names:
        phrasebook_offsets[cp] = len(phrasebook)
        for w in name.split():
            phrasebook.extend(word_encodings[w])
        phrasebook.append(0)

    t1, t2, shift = bin_data(phrasebook_offsets)

    c = Counter(lexicon_lengths)
    for i in range(0, max(c)):
        print i, c.get(i, 0)
    write_string(f, 'LEXICON', lexicon_string)
    write_array(f, 'LEXICON_OFFSETS', 'u16', lexicon_offsets)
    write_array(f, 'LEXICON_LENGTHS', 'u8', lexicon_lengths)
    f.write('pub static PHRASEBOOK_SHORT: u8 = %d;\n' % short)
    write_array(f, 'PHRASEBOOK', 'u8', phrasebook)
    f.write('pub static PHRASEBOOK_OFFSET_SHIFT: uint = %d;\n' % shift)
    write_array(f, 'PHRASEBOOK_OFFSETS1', smallest_u(t1), t1)
    write_array(f, 'PHRASEBOOK_OFFSETS2', smallest_u(t2), t2)

    import phf_computed as pc
    n, disp, table = pc.data
    print len(disp)

    f.write('pub static NAME2CODE_N: u64 = %d;\n' % n);
    t_first = smallest_u(x for x,_ in disp)
    t_second = smallest_u(y for _,y in disp)

    write_array(f, 'NAME2CODE_DISP', '(%s, %s)' % (t_first, t_second), disp)
    write_array(f, 'NAME2CODE_CODE', 'u32', table, lambda n: str(raw_codepoint_names[n][0]))


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

    import phf
    print len(d['codepoint_names'])
    #print phf.phf_table(hash_, [n for _, n in d['codepoint_names']], lambd=1)
    with modify_read_only(MOD_FILE) as f:
        f.write('''// autogenerated by generate.py\n''')
        write_cjk_ideograph_ranges(f, d['ideoranges'])
        f.write('\n')
        write_codepoint_maps(f, d['codepoint_names'])
