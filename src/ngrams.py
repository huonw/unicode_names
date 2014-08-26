from collections import Counter, defaultdict
names = list(x.split(';')[1] for x in open('UnicodeData.txt') if ';<' not in x)

class Trie:
    def __init__(self):
        self.children = defaultdict(Trie)
        self.element = False

    def insert(self, sequence):
        if not sequence:
            self.element = True
        else:
            head = sequence[0]
            rest = sequence[1:]
            self.children[head].insert(rest)
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
        if self.element:
            yield parents
        for k, child in self.children.items():
            for x in child.traverse(parents + [k]):
                yield x

    def __iter__(self):
        return self.traverse([])
    def __str__(self):
        return repr(self)
    def __repr__(self):
        return '%s' % dict(self.children)

def split_ngrams(s, step):
    return [s[i:i+step] for i in range(0, len(s), step)]

def estimate_ngram():
    def fixed_stepper(step):
        return ('%d-grams' % step, lambda s: split_ngrams(s, step))


    splitting_schemes = [
        ('words', lambda s: s.split()),
        fixed_stepper(2),
        fixed_stepper(3),
        fixed_stepper(4),
        fixed_stepper(5),
        fixed_stepper(6),
        fixed_stepper(7),
        fixed_stepper(8),
        fixed_stepper(9),
        fixed_stepper(10),
        fixed_stepper(11),
        fixed_stepper(12),
        fixed_stepper(13),
        fixed_stepper(14),
        fixed_stepper(15),
        fixed_stepper(16),
    ]

    for label, scheme in splitting_schemes:
        c = Counter(x for n in names for x in scheme(n))
        total = sum(len(n) for n, x in c.items())
        m = max(len(n) for n, x in c.items())
        print(label, len(c), total, m * len(c))


def smallest_index(n):
    for x in [8, 16, 32, 64]:
        if n < (1 << x):
            return x
    raise ValueError('%s too large' % n)


def ngram_storage_fixed(ngrams):
    return 'fixed', max(len(g) for g in ngrams) * len(ngrams), smallest_index(len(ngrams))
def ngram_storage_0sep(ngrams):
    storage = sum(len(g) + 1 for g in ngrams)
    return '0-sep', storage, smallest_index(storage)
def ngram_storage_0sep_suffix(ngrams):
    def sum_leaf_lengths(t, n):
        if len(t.children) == 0:
            return n
        else:
            return sum(sum_leaf_lengths(c, n + 1) for c in t.children.values())
    t = Trie()
    for n in ngrams:
        t.insert(n[::-1])

    storage = sum_leaf_lengths(t, 0)
    return '0sep-suffix', storage, smallest_index(storage)
def ngram_storage_0sep_suffix_offset(ngrams):
    def sum_leaf_lengths(t, n):
        if len(t.children) == 0:
            return n
        else:
            return sum(sum_leaf_lengths(c, n + 1) for c in t.children.values())
    t = Trie()
    for n in ngrams:
        t.insert(n[::-1])

    storage = sum_leaf_lengths(t, 0)
    num_offsets = len(ngrams)
    offset_table = smallest_index(storage) * num_offsets
    return '0sep-suffix offset', storage + offset_table, smallest_index(num_offsets)


# a contiguous buffer where each chunk is the same length
def name_storage_slice(word_size, split_names, ngram_index_size):
    storage = sum(2 * word_size + ngram_index_size * len(s) for s in split_names)
    return 'slice', storage, smallest_index(len(names))
# Foo\0BarBaz\0A\0 etc.
def name_storage_sep(word_size, split_names, ngram_index_size):
    storage = sum(ngram_index_size * (len(s) + 1) for s in split_names)
    return 'sep',storage, smallest_index(storage)
def name_storage_bitsep(word_size, split_names, ngram_index_size):
    storage = sum(ngram_index_size * len(s) for s in split_names)
    return 'bitsep', storage, smallest_index(storage)
def name_storage_bitsep_offset_table(word_size, split_names, ngram_index_size):
    storage = sum(ngram_index_size * len(s) for s in split_names)
    num_offsets = sum(len(s) for s in split_names)
    offset_table = smallest_index(storage) * num_offsets
    return 'bitsep offset', storage + offset_table, smallest_index(num_offsets)

def c2n_storage(word_size, split_names, name_index_size):
    return name_index_size * len(names)

def n2c_storage_plain(word_size, split_names, ngram_index_size, name_index_size):
    return 'flat', (4 + name_index_size) * len(names)
def n2c_storage_trie(word_size, split_names, ngram_index_size, name_index_size):
    def calculate_size(t):
        return ngram_index_size + sum(ngram_index_size + word_size + calculate_size(t)
                                      for c in t.children.values() if len(c) > 0)

    t = Trie()
    for s in split_names:
        t.insert(s)

    return 'trie', calculate_size(t)

def estimate_memory(splitters, word_size = 16,
                    ngram_storages = [ngram_storage_fixed, ngram_storage_0sep, ngram_storage_0sep_suffix, ngram_storage_0sep_suffix_offset],
                    name_storages = [name_storage_slice, name_storage_sep, name_storage_bitsep,
                                     name_storage_bitsep_offset_table],
                    n2c_storages = [n2c_storage_plain]):

    possibilities = []

    for splitter_name, splitter in splitters:
        split_names = splitter(names)
        ngrams = {s for n in split_names for s in n}
        for ngram_f in ngram_storages:
            ngram_name, ngram_storage, ngram_idx_size = ngram_f(ngrams)
            print splitter_name, ngram_name, ngram_storage, len(ngrams)
            continue
            for name_f in name_storages:
                if not all(len(s) == 1 for s in split_names):
                    name_name, name_storage, name_idx_size = name_f(word_size, split_names,
                                                                    ngram_idx_size)
                else:
                    name_name = 'no name'
                    name_storage = 0
                    name_idx_size = ngram_idx_size # ngram == names

                for n2c_f in n2c_storages:
                    n2c_name, n2c_storage = n2c_f(word_size, split_names, ngram_idx_size,
                                                  name_idx_size)
                    #print splitter_name, ngram_name, ngram_storage, name_name, name_storage, n2c_storage
                    possibilities.append({
                        'split': splitter_name,
                        'ngram': ngram_name, 'name': name_name, 'n2c': n2c_name,
                        'size': (ngram_storage + name_storage
                                 + c2n_storage(word_size, split_names, name_idx_size)
                                 + n2c_storage)
                    })
        possibilities.sort(key=lambda d: d['size'])
    return possibilities

def plain(f):
    return lambda names: [f(n) for n in names]


def path_compress_words(names):
    t = Trie()
    for n in names:
        t.insert(n.split())
    #print(t.count_subtries())
    t.compress(' ')
    #print('%d %d' % (t.count_subtries(), len(t)))
    return list(t)

def path_compress_ngrams(n):
    def f(names, n=n):
        t = Trie()
        for s in names:
            t.insert(split_ngrams(s, n))
        #print(t.count_subtries())
        t.compress('')
        #print('%d %d' % (t.count_subtries(), len(t)))
        return list(t)
    return f

# estimate_ngram()
schemes = [
    ('words ptrie', path_compress_words),
    ('raw', plain(lambda s: [s])),
    ('words', plain(lambda s: s.split())),
]
schemes.extend(('%d-grams ptrie' % x, path_compress_ngrams(x))
               for x in range(4, 32+1, 4))

#schemes.extend(('%d-grams' % x, plain(lambda s, x=x: split_ngrams(s, x)))
#               for x in range(4, 32 + 1, 4))

for p in estimate_memory(schemes):
    print(p)
