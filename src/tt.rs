use std;
use types::*;
pub struct TTEntry {
    key16: u16,
    move16: u16,
    value16: i16,
    eval16: i16,
    gen_bound8: u8,
    depth8: i8,
}
impl TTEntry {
    pub fn mov(&self) -> Move {
        Move(self.move16 as u32)
    }
    pub fn value(&self) -> Value {
        Value(self.value16 as i32)
    }
    pub fn eval(&self) -> Value {
        Value(self.eval16 as i32)
    }
    pub fn depth(&self) -> Depth {
        Depth(self.depth8 as i32)
    }
    pub fn bound(&self) -> Bound {
        Bound((self.gen_bound8 & 3) as u32)
    }
    pub fn save(&mut self, k: Key, v: Value, b: Bound, d: Depth, m: Move, ev: Value, g: u8) {
        debug_assert!(d / ONE_PLY * ONE_PLY == d);
        let k16 = (k.0 >> 48) as u16;
        if m != Move::NONE || k16 != self.key16 {
            self.move16 = m.0 as u16;
        }
        if k16 != self.key16 || (d / ONE_PLY) as i8 > self.depth8 - 4 || b == Bound::EXACT {
            self.key16 = k16;
            self.value16 = v.0 as i16;
            self.eval16 = ev.0 as i16;
            self.gen_bound8 = g | (b.0 as u8);
            self.depth8 = (d / ONE_PLY) as i8;
        }
    }
}
const CLUSTER_SIZE: usize = 3;
struct Cluster {
    entry: [TTEntry; CLUSTER_SIZE],
    _padding: [u8; 2],
}
static mut CLUSTER_COUNT: usize = 0;
static mut TABLE: *mut Cluster = 0 as *mut Cluster;
static mut TABLE_CAP: usize = 0;
static mut GENERATION8: u8 = 0;
pub fn new_search() {
    unsafe {
        GENERATION8 += 4;
    }
}
pub fn generation() -> u8 {
    unsafe { GENERATION8 }
}
fn cluster(key: Key) -> &'static mut Cluster {
    unsafe {
        let p: *mut Cluster =
            TABLE.offset((((key.0 as u32 as u64) * (CLUSTER_COUNT as u64)) >> 32) as isize);
        let c: &'static mut Cluster = &mut *p;
        c
    }
}
pub fn resize(mb_size: usize) {
    let new_cluster_count = mb_size * 1024 * 1024 / std::mem::size_of::<Cluster>();
    unsafe {
        if new_cluster_count == CLUSTER_COUNT {
            return;
        }
        free();
        CLUSTER_COUNT = new_cluster_count;
        let mut v: Vec<Cluster> = Vec::with_capacity(new_cluster_count);
        TABLE = v.as_mut_ptr();
        TABLE_CAP = v.capacity();
        std::mem::forget(v);
    }
}
pub fn free() {
    unsafe {
        if !TABLE.is_null() {
            let _ = Vec::from_raw_parts(TABLE, 0, TABLE_CAP);
        }
    }
}
pub fn clear() {
    let tt_slice = unsafe { std::slice::from_raw_parts_mut(TABLE, CLUSTER_COUNT) };
    for cluster in tt_slice.iter_mut() {
        for tte in cluster.entry.iter_mut() {
            tte.key16 = 0;
            tte.move16 = 0;
            tte.value16 = 0;
            tte.eval16 = 0;
            tte.gen_bound8 = 0;
            tte.depth8 = 0;
            tte.key16 = 0;
        }
    }
}
pub fn probe(key: Key) -> (&'static mut TTEntry, bool) {
    let cl = cluster(key);
    let key16 = (key.0 >> 48) as u16;
    for i in 0..CLUSTER_SIZE {
        if cl.entry[i].key16 == 0 || cl.entry[i].key16 == key16 {
            if cl.entry[i].gen_bound8 & 0xfc != generation() && cl.entry[i].key16 != 0 {
                cl.entry[i].gen_bound8 = generation() | (cl.entry[i].bound().0 as u8);
            }
            let found = cl.entry[i].key16 != 0;
            return (&mut (cl.entry[i]), found);
        }
    }
    let mut r = 0;
    for i in 1..CLUSTER_SIZE {
        if (cl.entry[r].depth8 as i32)
            - ((259 + (generation() as i32) - (cl.entry[r].gen_bound8 as i32)) & 0xfc) * 2
            > (cl.entry[i].depth8 as i32)
                - ((259 + (generation() as i32) - (cl.entry[i].gen_bound8 as i32)) & 0xfc) * 2
        {
            r = i;
        }
    }
    (&mut (cl.entry[r]), false)
}
pub fn hashfull() -> i32 {
    let tt_slice = unsafe { std::slice::from_raw_parts(TABLE, 1000 / CLUSTER_SIZE) };
    let mut cnt = 0;
    for cluster in tt_slice.iter() {
        for tte in cluster.entry.iter() {
            if tte.gen_bound8 & 0xfc == generation() {
                cnt += 1;
            }
        }
    }
    cnt
}
