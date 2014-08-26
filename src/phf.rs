extern crate time;
use std::rand::{XorShiftRng, Rng, mod};
use std::collections::HashMap;
use std::hash::sip;

static NOVAL: u32 = 0xFFFF_FFFF;

fn hash(s: &str, h: u64) -> u64 {
    //sip::hash_with_keys(h, !h, &s)
    //let mut h = h; for b in s.bytes() { h = (h * (1 + (1 << 5))) + b as u64 } h
    let mut g = 0xcbf29ce484222325 ^ h; for b in s.bytes() { g ^= b as u64; g *= 0x100000001b3; } g
}

pub fn displace(f1: u32, f2: u32, d1: u32, d2: u32) -> u32 {
    d2 + f1 * d1 + f2
}
fn split(hash: u64) -> (u32, u32, u32) {
    let bits = 21;
    let mask = (1 << bits) - 1;
    ((hash & mask) as u32,
     ((hash >> bits) & mask) as u32,
     ((hash >> (2 * bits)) & mask) as u32)
}

fn try_phf_table(values: &[String], lambda: uint, seed: u64) -> Option<(Vec<(u32, u32)>, Vec<u32>)> {
    let hashes: Vec<(u32, u32, u32)> = values.iter().map(|s| split(hash(s.as_slice(), seed))).collect();

    let table_len = hashes.len();
    let buckets_len = (table_len + lambda - 1) / lambda;

    let mut buckets = Vec::from_fn(buckets_len, |i| (i, vec![]));
    for (i, h) in hashes.iter().enumerate() {
        buckets.get_mut(h.val0() as uint % buckets_len).mut1().push(i as u32)
    }

    buckets.sort_by(|&(_, ref a), &(_, ref b)| b.len().cmp(&a.len()));
    let mut map = Vec::from_elem(table_len, NOVAL);
    let mut try_map = HashMap::new();
    let mut disps = Vec::from_elem(buckets_len, (0, 0));

    let mut d1s = Vec::from_fn(table_len, |i| i as u32);
    let mut d2s = d1s.clone();
    let mut rng: XorShiftRng = rand::random();

    'next_bucket: for &(bkt_idx, ref bkt_keys) in buckets.iter() {
        rng.shuffle(d1s.as_mut_slice());
        rng.shuffle(d2s.as_mut_slice());

        for &d1 in d1s.iter() {
            'next_disp: for &d2 in d2s.iter() {
                try_map.clear();

                for &k in bkt_keys.iter() {
                    let (_, f1, f2) = hashes[k as uint];
                    let idx = (displace(f1, f2, d1, d2) % table_len as u32) as uint;
                    if map[idx] != NOVAL || try_map.contains_key(&idx) {
                        continue 'next_disp
                    }
                    try_map.insert(idx, k);
                }

                *disps.get_mut(bkt_idx) = (d1, d2);
                for (idx, key) in try_map.iter() {
                    *map.get_mut(*idx) = *key
                }
                continue 'next_bucket
            }
        }
        return None
    }

    return Some((disps, map))

}

fn main() {
    use std::io;
    let mut data = io::BufferedReader::new(io::File::open(&Path::new("UnicodeData.txt")).unwrap())
        .lines().filter_map(|line| {
        let line = line.unwrap();
        let name = line.as_slice().split(';').nth(1).unwrap();
        if name.starts_with("<") {
            None
        } else {
            Some(name.to_string())
        }
    }).collect::<Vec<String>>();

    data.truncate(100);
    let start = time::precise_time_s();
    for _ in range(0u, 10) {
        let n = std::rand::random();
        match try_phf_table(data.as_slice(), 3, n) {
            Some((disp, map)) => {
                println!("took {}\n\n", time::precise_time_s() - start)
                write!(&mut io::File::create(&Path::new("phf_computed2.py")).unwrap(),
                       "data = ({}, {}, {})", n, disp, map);
                break
            }
            None => println!("retrying")
        }
    }
}
