use bitboard::*;
use position::Position;
use std;
use types::*;
macro_rules! V {
    ($x:expr) => {
        Value($x)
    };
}
macro_rules! S {
    ($x:expr, $y:expr) => {
        Score(($y << 16) + $x)
    };
}
const V0: Value = Value::ZERO;
const ISOLATED: Score = S!(13, 18);
const BACKWARD: Score = S!(24, 12);
static mut CONNECTED: [[[[Score; 8]; 3]; 2]; 2] = [[[[Score::ZERO; 8]; 3]; 2]; 2];
const DOUBLED: Score = S!(18, 38);
const SHELTER_WEAKNESS: [[[Value; 8]; 4]; 2] = [
    [
        [V!(98), V!(20), V!(11), V!(42), V!(83), V!(84), V!(101), V0],
        [V!(103), V!(8), V!(33), V!(86), V!(87), V!(105), V!(113), V0],
        [V!(100), V!(2), V!(65), V!(95), V!(59), V!(89), V!(115), V0],
        [V!(72), V!(6), V!(52), V!(74), V!(83), V!(84), V!(112), V0],
    ],
    [
        [V!(105), V!(19), V!(3), V!(27), V!(85), V!(93), V!(84), V0],
        [V!(121), V!(7), V!(33), V!(95), V!(112), V!(86), V!(72), V0],
        [V!(121), V!(26), V!(65), V!(90), V!(65), V!(76), V!(117), V0],
        [V!(79), V!(0), V!(45), V!(65), V!(94), V!(92), V!(105), V0],
    ],
];
const STORM_DANGER: [[[Value; 8]; 4]; 4] = [
    [
        [V!(0), V!(-290), V!(-274), V!(57), V!(41), V0, V0, V0],
        [V!(0), V!(60), V!(144), V!(39), V!(13), V0, V0, V0],
        [V!(0), V!(65), V!(141), V!(41), V!(34), V0, V0, V0],
        [V!(0), V!(53), V!(127), V!(56), V!(14), V0, V0, V0],
    ],
    [
        [V!(4), V!(73), V!(132), V!(46), V!(31), V0, V0, V0],
        [V!(1), V!(64), V!(143), V!(26), V!(13), V0, V0, V0],
        [V!(1), V!(47), V!(110), V!(44), V!(24), V0, V0, V0],
        [V!(0), V!(72), V!(127), V!(50), V!(31), V0, V0, V0],
    ],
    [
        [V!(0), V!(0), V!(79), V!(23), V!(1), V0, V0, V0],
        [V!(0), V!(0), V!(148), V!(27), V!(2), V0, V0, V0],
        [V!(0), V!(0), V!(161), V!(16), V!(1), V0, V0, V0],
        [V!(0), V!(0), V!(171), V!(22), V!(15), V0, V0, V0],
    ],
    [
        [V!(22), V!(45), V!(104), V!(62), V!(6), V0, V0, V0],
        [V!(31), V!(30), V!(99), V!(39), V!(19), V0, V0, V0],
        [V!(23), V!(29), V!(96), V!(41), V!(15), V0, V0, V0],
        [V!(21), V!(23), V!(116), V!(41), V!(15), V0, V0, V0],
    ],
];
const MAX_SAFETY_BONUS: Value = V!(258);
pub struct Entry {
    key: Key,
    score: Score,
    passed_pawns: [Bitboard; 2],
    pawn_attacks: [Bitboard; 2],
    pawn_attacks_span: [Bitboard; 2],
    king_squares: [Square; 2],
    king_safety: [Score; 2],
    weak_unopposed: [i32; 2],
    castling_rights: [CastlingRight; 2],
    semiopen_files: [i32; 2],
    pawns_on_squares: [[i32; 2]; 2],
    asymmetry: i32,
    open_files: i32,
}
impl Entry {
    pub fn new() -> Entry {
        Entry {
            key: Key(0),
            score: Score::ZERO,
            passed_pawns: [Bitboard(0); 2],
            pawn_attacks: [Bitboard(0); 2],
            pawn_attacks_span: [Bitboard(0); 2],
            king_squares: [Square(0); 2],
            king_safety: [Score::ZERO; 2],
            weak_unopposed: [0; 2],
            castling_rights: [CastlingRight(0); 2],
            semiopen_files: [0; 2],
            pawns_on_squares: [[0; 2]; 2],
            asymmetry: 0,
            open_files: 0,
        }
    }
    pub fn pawns_score(&self) -> Score {
        self.score
    }
    pub fn pawn_attacks(&self, c: Color) -> Bitboard {
        self.pawn_attacks[c.0 as usize]
    }
    pub fn passed_pawns(&self, c: Color) -> Bitboard {
        self.passed_pawns[c.0 as usize]
    }
    pub fn pawn_attacks_span(&self, c: Color) -> Bitboard {
        self.pawn_attacks_span[c.0 as usize]
    }
    pub fn weak_unopposed(&self, c: Color) -> i32 {
        self.weak_unopposed[c.0 as usize]
    }
    pub fn pawn_asymmetry(&self) -> i32 {
        self.asymmetry
    }
    pub fn open_files(&self) -> i32 {
        self.open_files
    }
    pub fn semiopen_file(&self, c: Color, f: File) -> i32 {
        self.semiopen_files[c.0 as usize] & (1 << f)
    }
    pub fn pawns_on_same_color_squares(&self, c: Color, s: Square) -> i32 {
        self.pawns_on_squares[c.0 as usize][((DARK_SQUARES & s) != 0) as usize]
    }
    pub fn king_safety<Us: ColorTrait>(&mut self, pos: &Position, ksq: Square) -> Score {
        let us = Us::COLOR;
        if self.king_squares[us.0 as usize] != ksq
            || self.castling_rights[us.0 as usize] != pos.castling_rights(us)
        {
            self.king_safety[us.0 as usize] = self.do_king_safety::<Us>(pos, ksq);
        }
        self.king_safety[us.0 as usize]
    }
    fn shelter_storm<Us: ColorTrait>(&self, pos: &Position, ksq: Square) -> Value {
        let us = Us::COLOR;
        let them = if us == WHITE { BLACK } else { WHITE };
        let shelter_mask = if us == WHITE {
            bitboard!(A2, B3, C2, F2, G3, H2)
        } else {
            bitboard!(A7, B6, C7, F7, G6, H7)
        };
        let storm_mask = if us == WHITE {
            bitboard!(A3, C3, F3, H3)
        } else {
            bitboard!(A6, C6, F6, H6)
        };
        const BLOCKED_BY_KING: usize = 0;
        const UNOPPOSED: usize = 1;
        const BLOCKED_BY_PAWN: usize = 2;
        const UNBLOCKED: usize = 3;
        let center = std::cmp::max(FILE_B, std::cmp::min(FILE_G, ksq.file()));
        let b = pos.pieces_p(PAWN)
            & (forward_ranks_bb(us, ksq) | ksq.rank_bb())
            & (adjacent_files_bb(center) | file_bb(center));
        let our_pawns = b & pos.pieces_c(us);
        let their_pawns = b & pos.pieces_c(them);
        let mut safety = MAX_SAFETY_BONUS;
        for f in (center - 1)..(center + 2) {
            let b = our_pawns & file_bb(f);
            let rk_us = if b != 0 {
                backmost_sq(us, b).relative_rank(us)
            } else {
                RANK_1
            };
            let b = their_pawns & file_bb(f);
            let rk_them = if b != 0 {
                frontmost_sq(them, b).relative_rank(us)
            } else {
                RANK_1
            };
            let d = std::cmp::min(f, FILE_H - f);
            safety -= SHELTER_WEAKNESS[(f == ksq.file()) as usize][d as usize][rk_us as usize]
                + STORM_DANGER[if f == ksq.file() && rk_them == ksq.relative_rank(us) + 1 {
                    BLOCKED_BY_KING
                } else if rk_us == RANK_1 {
                    UNOPPOSED
                } else if rk_them == rk_us + 1 {
                    BLOCKED_BY_PAWN
                } else {
                    UNBLOCKED
                }][d as usize][rk_them as usize];
        }
        if popcount((our_pawns & shelter_mask) | (their_pawns & storm_mask)) == 5 {
            safety += 300;
        }
        safety
    }
    fn do_king_safety<Us: ColorTrait>(&mut self, pos: &Position, ksq: Square) -> Score {
        let us = Us::COLOR;
        self.king_squares[us.0 as usize] = ksq;
        self.castling_rights[us.0 as usize] = pos.castling_rights(us);
        let mut min_king_pawn_distance = 0i32;
        let pawns = pos.pieces_cp(us, PAWN);
        if pawns != 0 {
            while distance_ring_bb(ksq, min_king_pawn_distance) & pawns == 0 {
                min_king_pawn_distance += 1;
            }
            min_king_pawn_distance += 1;
        }
        let mut bonus = self.shelter_storm::<Us>(pos, ksq);
        if pos.has_castling_right(us | CastlingSide::KING) {
            bonus = std::cmp::max(
                bonus,
                self.shelter_storm::<Us>(pos, Square::G1.relative(us)),
            );
        }
        if pos.has_castling_right(us | CastlingSide::QUEEN) {
            bonus = std::cmp::max(
                bonus,
                self.shelter_storm::<Us>(pos, Square::C1.relative(us)),
            );
        }
        Score::make(bonus.0, -16 * min_king_pawn_distance)
    }
}
pub fn init() {
    const SEED: [i32; 8] = [0, 13, 24, 18, 76, 100, 175, 330];
    for opposed in 0..2 {
        for phalanx in 0..2 {
            for support in 0..3 {
                for r in 1..7i32 {
                    let v = 17 * (support as i32)
                        + ((SEED[r as usize]
                            + (if phalanx != 0 {
                                (SEED[(r + 1) as usize] - SEED[r as usize]) / 2
                            } else {
                                0
                            }))
                            >> opposed);
                    unsafe {
                        CONNECTED[opposed as usize][phalanx as usize][support as usize]
                            [r as usize] = Score::make(v, v * (r - 2) / 4);
                    }
                }
            }
        }
    }
}
pub fn probe(pos: &Position) -> &mut Entry {
    let key = pos.pawn_key();
    let e = pos.pawns_table[(key.0 & 16383) as usize].get();
    let e: &'static mut Entry = unsafe { &mut *e };
    if e.key == key {
        return e;
    }
    e.key = key;
    e.score = evaluate::<White>(pos, e) - evaluate::<Black>(pos, e);
    e.open_files = (e.semiopen_files[WHITE.0 as usize] & e.semiopen_files[BLACK.0 as usize])
        .count_ones() as i32;
    e.asymmetry = (e.passed_pawns[WHITE.0 as usize].0
        | e.passed_pawns[BLACK.0 as usize].0
        | (e.semiopen_files[WHITE.0 as usize] ^ e.semiopen_files[BLACK.0 as usize]) as u64)
        .count_ones() as i32;
    e
}
fn evaluate<Us: ColorTrait>(pos: &Position, e: &mut Entry) -> Score {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let up = if us == WHITE { NORTH } else { SOUTH };
    let right = if us == WHITE { NORTH_EAST } else { SOUTH_WEST };
    let left = if us == WHITE { NORTH_WEST } else { SOUTH_EAST };
    let mut score = Score::ZERO;
    let our_pawns = pos.pieces_cp(us, PAWN);
    let their_pawns = pos.pieces_cp(them, PAWN);
    e.passed_pawns[us.0 as usize] = Bitboard(0);
    e.pawn_attacks_span[us.0 as usize] = Bitboard(0);
    e.weak_unopposed[us.0 as usize] = 0;
    e.semiopen_files[us.0 as usize] = 0xff;
    e.king_squares[us.0 as usize] = Square::NONE;
    e.pawn_attacks[us.0 as usize] = our_pawns.shift(right) | our_pawns.shift(left);
    e.pawns_on_squares[us.0 as usize][BLACK.0 as usize] = popcount(our_pawns & DARK_SQUARES) as i32;
    e.pawns_on_squares[us.0 as usize][WHITE.0 as usize] =
        popcount(our_pawns & !DARK_SQUARES) as i32;
    for s in pos.square_list(us, PAWN) {
        debug_assert!(pos.piece_on(s) == Piece::make(us, PAWN));
        let f = s.file();
        e.semiopen_files[us.0 as usize] &= !(1 << f);
        e.pawn_attacks_span[us.0 as usize] |= pawn_attack_span(us, s);
        let opposed = their_pawns & forward_file_bb(us, s);
        let stoppers = their_pawns & passed_pawn_mask(us, s);
        let lever = their_pawns & pawn_attacks(us, s);
        let lever_push = their_pawns & pawn_attacks(us, s + up);
        let doubled = our_pawns & (s - up);
        let neighbours = our_pawns & adjacent_files_bb(f);
        let phalanx = neighbours & s.rank_bb();
        let supported = neighbours & (s - up).rank_bb();
        let backward;
        if neighbours == 0 || lever != 0 || s.relative_rank(us) >= RANK_5 {
            backward = false;
        } else {
            let b = backmost_sq(us, neighbours | stoppers).rank_bb();
            backward = (b | (b & adjacent_files_bb(f)).shift(up)) & stoppers != 0;
            debug_assert!(!(backward && forward_ranks_bb(them, s + up) & neighbours != 0));
        }
        if stoppers ^ lever ^ lever_push == 0
            && our_pawns & forward_file_bb(us, s) == 0
            && popcount(supported) >= popcount(lever)
            && popcount(phalanx) >= popcount(lever_push)
        {
            e.passed_pawns[us.0 as usize] |= s;
        } else if stoppers ^ (s + up) == 0 && s.relative_rank(us) >= RANK_5 {
            for sq in supported.shift(up) & !their_pawns {
                if !more_than_one(their_pawns & pawn_attacks(us, sq)) {
                    e.passed_pawns[us.0 as usize] |= s;
                }
            }
        }
        if supported | phalanx != 0 {
            score += unsafe {
                CONNECTED[(opposed != 0) as usize][(phalanx != 0) as usize]
                    [popcount(supported) as usize][s.relative_rank(us) as usize]
            };
        } else if neighbours == 0 {
            score -= ISOLATED;
            e.weak_unopposed[us.0 as usize] += (opposed == 0) as i32;
        } else if backward {
            score -= BACKWARD;
            e.weak_unopposed[us.0 as usize] += (opposed == 0) as i32;
        }
        if doubled != 0 && supported == 0 {
            score -= DOUBLED;
        }
    }
    score
}
