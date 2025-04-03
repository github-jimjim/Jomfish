use bitboard::*;
use types::*;
const MAX_INDEX: usize = 2 * 24 * 64 * 64;
static mut KPK_BITBASE: [u32; MAX_INDEX / 32] = [0; MAX_INDEX / 32];
fn index(us: Color, bksq: Square, wksq: Square, psq: Square) -> usize {
    (wksq.0 | (bksq.0 << 6) | (us.0 << 12) | (psq.file() << 13) | ((RANK_7 - psq.rank()) << 15))
        as usize
}
const INVALID: u8 = 0;
const UNKNOWN: u8 = 1;
const DRAW: u8 = 2;
const WIN: u8 = 4;
struct KPKPosition {
    us: Color,
    ksq: [Square; 2],
    psq: Square,
    result: u8,
}
impl KPKPosition {
    fn new(idx: u32) -> KPKPosition {
        let ksq = [Square((idx >> 0) & 0x3f), Square((idx >> 6) & 0x3f)];
        let us = Color((idx >> 12) & 0x01);
        let psq = Square::make((idx >> 13) & 0x03, RANK_7 - ((idx >> 15) & 0x07));
        let result;
        if Square::distance(ksq[WHITE.0 as usize], ksq[BLACK.0 as usize]) <= 1
            || ksq[WHITE.0 as usize] == psq
            || ksq[BLACK.0 as usize] == psq
            || (us == WHITE && pawn_attacks(WHITE, psq) & ksq[BLACK.0 as usize] != 0)
        {
            result = INVALID;
        } else if us == WHITE
            && psq.rank() == RANK_7
            && ksq[us.0 as usize] != psq + NORTH
            && (Square::distance(ksq[(!us).0 as usize], psq + NORTH) > 1
                || pseudo_attacks(KING, ksq[us.0 as usize]) & (psq + NORTH) != 0)
        {
            result = WIN;
        } else if us == BLACK
            && ((pseudo_attacks(KING, ksq[us.0 as usize])
                & !(pseudo_attacks(KING, ksq[(!us).0 as usize]) | pawn_attacks(!us, psq)))
                == 0
                || pseudo_attacks(KING, ksq[us.0 as usize])
                    & psq
                    & !pseudo_attacks(KING, ksq[(!us).0 as usize])
                    != 0)
        {
            result = DRAW;
        } else {
            result = UNKNOWN;
        }
        KPKPosition {
            us,
            ksq,
            psq,
            result,
        }
    }
    fn classify(&self, db: &Vec<KPKPosition>) -> u8 {
        let us = self.us;
        let psq = self.psq;
        let them = if us == WHITE { BLACK } else { WHITE };
        let good = if us == WHITE { WIN } else { DRAW };
        let bad = if us == WHITE { DRAW } else { WIN };
        let mut r = INVALID;
        for s in pseudo_attacks(KING, self.ksq[us.0 as usize]) {
            r |= if us == WHITE {
                db[index(them, self.ksq[them.0 as usize], s, psq)].result
            } else {
                db[index(them, s, self.ksq[them.0 as usize], psq)].result
            };
        }
        if us == WHITE {
            if psq.rank() < RANK_7 {
                r |= db[index(
                    them,
                    self.ksq[them.0 as usize],
                    self.ksq[us.0 as usize],
                    psq + NORTH,
                )]
                .result;
            }
            if psq.rank() == RANK_2
                && psq + NORTH != self.ksq[us.0 as usize]
                && psq + NORTH != self.ksq[them.0 as usize]
            {
                r |= db[index(
                    them,
                    self.ksq[them.0 as usize],
                    self.ksq[us.0 as usize],
                    psq + 2 * NORTH,
                )]
                .result;
            }
        }
        if r & good != 0 {
            good
        } else if r & UNKNOWN != 0 {
            UNKNOWN
        } else {
            bad
        }
    }
}
pub fn init() {
    let mut db: Vec<KPKPosition> = Vec::with_capacity(MAX_INDEX);
    for idx in 0..MAX_INDEX {
        db.push(KPKPosition::new(idx as u32));
    }
    let mut repeat = true;
    while repeat {
        repeat = false;
        for idx in 0..MAX_INDEX {
            if db[idx].result == UNKNOWN {
                let result = db[idx].classify(&db);
                if result != UNKNOWN {
                    db[idx].result = result;
                    repeat = true;
                }
            }
        }
    }
    for idx in 0..MAX_INDEX {
        if db[idx].result == WIN {
            unsafe {
                KPK_BITBASE[idx / 32] |= 1u32 << (idx & 0x1f);
            }
        }
    }
}
pub fn probe(wksq: Square, wpsq: Square, bksq: Square, us: Color) -> bool {
    debug_assert!(wpsq.file() <= FILE_D);
    let idx = index(us, bksq, wksq, wpsq);
    unsafe { KPK_BITBASE[idx / 32] & (1 << (idx & 0x1f)) != 0 }
}
