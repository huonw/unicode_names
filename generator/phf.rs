extern crate time;
use std::rand::{XorShiftRng, Rng, mod};
use std::collections::HashMap;

static NOVAL: u32 = 0xFFFF_FFFF;

fn hash(s: &str, h: u64) -> u64 {
    //sip::hash_with_keys(h, !h, &s)
    //let mut h = h; for b in s.bytes() { h = (h * (1 + (1 << 5))) + b as u64 } h
    let mut g = 0xcbf29ce484222325 ^ h; for b in s.bytes() { g ^= b as u64; g *= 0x100000001b3; } g
}

pub fn displace(f1: u32, f2: u32, d1: u32, d2: u32) -> u32 {
    d2 + f1 * d1 + f2
}
fn split(hash: u64) -> Hash {
    let bits = 21;
    let mask = (1 << bits) - 1;
    Hash {
        g: (hash & mask) as u32,
        f1: ((hash >> bits) & mask) as u32,
        f2: ((hash >> (2 * bits)) & mask) as u32
    }
}

struct Hash { g: u32, f1: u32, f2: u32 }

fn try_phf_table(values: &[(u32, String)],
                 lambda: uint, seed: u64) -> Option<(Vec<(u32, u32)>, Vec<u32>)> {

    let hashes: Vec<(Hash, u32)> =
        values.iter().map(|&(n, ref s)| (split(hash(s.as_slice(), seed)), n)).collect();

    let table_len = hashes.len();
    let buckets_len = (table_len + lambda - 1) / lambda;

    let mut buckets = Vec::from_fn(buckets_len, |i| (i, vec![]));
    for (i, &(h, cp)) in hashes.iter().enumerate() {
        buckets.get_mut(h.g as uint % buckets_len).mut1().push((h, cp))
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

                for &(h, cp) in bkt_keys.iter() {
                    let idx = (displace(h.f1, h.f2, d1, d2) % table_len as u32) as uint;
                    if map[idx] != NOVAL || try_map.contains_key(&idx) {
                        continue 'next_disp
                    }
                    try_map.insert(idx, cp);
                }

                *disps.get_mut(bkt_idx) = (d1, d2);
                for (idx, cp) in try_map.iter() {
                    *map.get_mut(*idx) = *cp
                }
                continue 'next_bucket
            }
        }
        return None
    }

    return Some((disps, map))

}

pub fn create_phf(data: &[(u32, String)], lambda: uint,
                  max_tries: uint) -> (u64, Vec<(u32, u32)>, Vec<u32>) {
    let start = time::precise_time_s();
    for i in range(0, max_tries){
        let my_start = time::precise_time_s();
        println!("PHF #{}: starting {:.2}", i, my_start - start);

        let n = rand::random();
        match try_phf_table(data, lambda, n) {
            Some((disp, map)) => {
                let end = time::precise_time_s();
                println!("PHF took: total {:.2} s, successive {:.2} s",
                         end - start, end - my_start);
                return (n, disp, map)
            }
            None => {}
        }
    }
    fail!("could not create a length {} PHF with {}, {}",
          data.len(), lambda, max_tries);
}
