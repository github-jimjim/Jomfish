use bitbases;
use bitboard::*;
use movegen::*;
use position::zobrist;
use position::Position;
use std;
use types::*;
pub type EvalFn = fn(&Position, Color) -> Value;
pub type ScaleFn = fn(&Position, Color) -> ScaleFactor;
struct EvalInit {
    func: EvalFn,
    code: &'static str,
}
const EVAL_INITS: [EvalInit; 8] = [
    EvalInit {
        func: evaluate_kpk,
        code: "KPk",
    },
    EvalInit {
        func: evaluate_knnk,
        code: "KNNk",
    },
    EvalInit {
        func: evaluate_kbnk,
        code: "KBNk",
    },
    EvalInit {
        func: evaluate_krkp,
        code: "KRkp",
    },
    EvalInit {
        func: evaluate_krkb,
        code: "KRkb",
    },
    EvalInit {
        func: evaluate_krkn,
        code: "KRkn",
    },
    EvalInit {
        func: evaluate_kqkp,
        code: "KQkp",
    },
    EvalInit {
        func: evaluate_kqkr,
        code: "KQkr",
    },
];
struct ScaleInit {
    func: ScaleFn,
    code: &'static str,
}
const SCALE_INITS: [ScaleInit; 8] = [
    ScaleInit {
        func: scale_knpk,
        code: "KNPk",
    },
    ScaleInit {
        func: scale_knpkb,
        code: "KNPkb",
    },
    ScaleInit {
        func: scale_krpkr,
        code: "KRPkr",
    },
    ScaleInit {
        func: scale_krpkb,
        code: "KRPkb",
    },
    ScaleInit {
        func: scale_kbpkb,
        code: "KBPkb",
    },
    ScaleInit {
        func: scale_kbpkn,
        code: "KBPkn",
    },
    ScaleInit {
        func: scale_kbppkb,
        code: "KBPPkb",
    },
    ScaleInit {
        func: scale_krppkrp,
        code: "KRPPkrp",
    },
];
#[derive(Clone, Copy)]
pub struct EvalEntry {
    pub func: EvalFn,
    pub key: [Key; 2],
}
#[derive(Clone, Copy)]
pub struct ScaleEntry {
    pub func: ScaleFn,
    pub key: [Key; 2],
}
pub static mut EVAL_FNS: [EvalEntry; 8] = [EvalEntry {
    func: evaluate_kpk,
    key: [Key(0); 2],
}; 8];
pub static mut SCALE_FNS: [ScaleEntry; 8] = [ScaleEntry {
    func: scale_knpk,
    key: [Key(0); 2],
}; 8];
const PUSH_TO_EDGES: [i32; 64] = [
    100, 90, 80, 70, 70, 80, 90, 100, 90, 70, 60, 50, 50, 60, 70, 90, 80, 60, 40, 30, 30, 40, 60,
    80, 70, 50, 30, 20, 20, 30, 50, 70, 70, 50, 30, 20, 20, 30, 50, 70, 80, 60, 40, 30, 30, 40, 60,
    80, 90, 70, 60, 50, 50, 60, 70, 90, 100, 90, 80, 70, 70, 80, 90, 100,
];
const PUSH_TO_CORNERS: [i32; 64] = [
    200, 190, 180, 170, 160, 150, 140, 130, 190, 180, 170, 160, 150, 140, 130, 140, 180, 170, 155,
    140, 140, 125, 140, 150, 170, 160, 140, 120, 110, 140, 150, 160, 160, 150, 140, 110, 120, 140,
    160, 170, 150, 140, 125, 140, 140, 155, 170, 180, 140, 130, 140, 150, 160, 170, 180, 190, 130,
    140, 150, 160, 170, 180, 190, 200,
];
const PUSH_CLOSE: [i32; 8] = [0, 0, 100, 80, 60, 40, 20, 10];
const PUSH_AWAY: [i32; 8] = [0, 5, 20, 40, 60, 80, 90, 100];
const KRPPKRP_SCALE_FACTORS: [i32; 8] = [0, 9, 10, 14, 21, 44, 0, 0];
fn calc_key(code: &str, c: Color) -> Key {
    let mut cnt: [i32; 16] = [0; 16];
    let mut key = Key(0);
    for ch in code.chars() {
        let mut pc = Piece(Position::PIECE_TO_CHAR.find(ch).unwrap() as u32);
        if c == BLACK {
            pc = !pc;
        }
        key ^= zobrist::material(pc, cnt[pc.0 as usize]);
        cnt[pc.0 as usize] += 1;
    }
    key
}
pub fn init() {
    for i in 0..8 {
        let ei = &EVAL_INITS[i];
        unsafe {
            EVAL_FNS[i].func = ei.func;
            EVAL_FNS[i].key[WHITE.0 as usize] = calc_key(ei.code, WHITE);
            EVAL_FNS[i].key[BLACK.0 as usize] = calc_key(ei.code, BLACK);
        }
    }
    for i in 0..8 {
        let si = &SCALE_INITS[i];
        unsafe {
            SCALE_FNS[i].func = si.func;
            SCALE_FNS[i].key[WHITE.0 as usize] = calc_key(si.code, WHITE);
            SCALE_FNS[i].key[BLACK.0 as usize] = calc_key(si.code, BLACK);
        }
    }
}
fn verify_material(pos: &Position, c: Color, npm: Value, pawns_cnt: i32) -> bool {
    pos.non_pawn_material_c(c) == npm && pos.count(c, PAWN) == pawns_cnt
}
fn normalize(pos: &Position, strong_side: Color, mut sq: Square) -> Square {
    debug_assert!(pos.count(strong_side, PAWN) == 1);
    if pos.square(strong_side, PAWN).file() >= FILE_E {
        sq = Square(sq.0 ^ 7);
    }
    if strong_side == BLACK {
        sq = !sq;
    }
    sq
}
pub fn evaluate_kxk(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 0));
    debug_assert!(pos.checkers() == 0);
    if pos.side_to_move() == weak_side {
        if MoveList::new::<Legal>(pos).len() == 0 {
            return Value::DRAW;
        }
    }
    let winner_ksq = pos.square(strong_side, KING);
    let loser_ksq = pos.square(weak_side, KING);
    let mut result = pos.non_pawn_material_c(strong_side)
        + pos.count(strong_side, PAWN) * PawnValueEg
        + PUSH_TO_EDGES[loser_ksq.0 as usize]
        + PUSH_CLOSE[Square::distance(winner_ksq, loser_ksq) as usize];
    if pos.pieces_pp(QUEEN, ROOK) != 0
        || (pos.pieces_p(BISHOP) != 0 && pos.pieces_p(KNIGHT) != 0)
        || (pos.pieces_p(BISHOP) & !DARK_SQUARES != 0 && pos.pieces_p(BISHOP) & DARK_SQUARES != 0)
    {
        result = std::cmp::min(result + Value::KNOWN_WIN, Value::MATE_IN_MAX_PLY - 1);
    }
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_kbnk(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(
        pos,
        strong_side,
        KnightValueMg + BishopValueMg,
        0
    ));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 0));
    let mut winner_ksq = pos.square(strong_side, KING);
    let mut loser_ksq = pos.square(weak_side, KING);
    let bishop_sq = pos.square(strong_side, BISHOP);
    if opposite_colors(bishop_sq, Square::A1) {
        winner_ksq = !winner_ksq;
        loser_ksq = !loser_ksq;
    }
    let result = Value::KNOWN_WIN
        + PUSH_CLOSE[Square::distance(winner_ksq, loser_ksq) as usize]
        + PUSH_TO_CORNERS[loser_ksq.0 as usize];
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_kpk(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, Value::ZERO, 1));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 0));
    let wksq = normalize(pos, strong_side, pos.square(strong_side, KING));
    let bksq = normalize(pos, strong_side, pos.square(weak_side, KING));
    let psq = normalize(pos, strong_side, pos.square(strong_side, PAWN));
    let us = if strong_side == pos.side_to_move() {
        WHITE
    } else {
        BLACK
    };
    if !bitbases::probe(wksq, psq, bksq, us) {
        return Value::DRAW;
    }
    let result = Value::KNOWN_WIN + PawnValueEg + Value(psq.rank() as i32);
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_krkp(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 0));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 1));
    let wksq = pos.square(strong_side, KING).relative(strong_side);
    let bksq = pos.square(weak_side, KING).relative(strong_side);
    let rsq = pos.square(strong_side, ROOK).relative(strong_side);
    let psq = pos.square(weak_side, PAWN).relative(strong_side);
    let queening_sq = Square::make(psq.file(), RANK_1);
    let result;
    if wksq.0 < psq.0 && wksq.file() == psq.file() {
        result = RookValueEg - Square::distance(wksq, psq) as i32;
    } else if Square::distance(bksq, psq) >= 3 + (pos.side_to_move() == weak_side) as u32
        && Square::distance(bksq, rsq) >= 3
    {
        result = RookValueEg - Square::distance(wksq, psq) as i32;
    } else if bksq.rank() <= RANK_3
        && Square::distance(bksq, psq) == 1
        && wksq.rank() >= RANK_4
        && Square::distance(wksq, psq) > 2 + (pos.side_to_move() == strong_side) as u32
    {
        result = Value(80) - 8 * Square::distance(wksq, psq) as i32;
    } else {
        result = Value(200)
            - 8 * (Square::distance(wksq, psq + SOUTH) as i32
                - Square::distance(bksq, psq + SOUTH) as i32
                - Square::distance(psq, queening_sq) as i32);
    }
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_krkb(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 0));
    debug_assert!(verify_material(pos, weak_side, BishopValueMg, 0));
    let result = Value(PUSH_TO_EDGES[pos.square(weak_side, KING).0 as usize]);
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_krkn(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 0));
    debug_assert!(verify_material(pos, weak_side, KnightValueMg, 0));
    let bksq = pos.square(weak_side, KING);
    let bnsq = pos.square(weak_side, KNIGHT);
    let result =
        Value(PUSH_TO_EDGES[bksq.0 as usize] + PUSH_AWAY[Square::distance(bksq, bnsq) as usize]);
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_kqkp(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, QueenValueMg, 0));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 1));
    let winner_ksq = pos.square(strong_side, KING);
    let loser_ksq = pos.square(weak_side, KING);
    let pawn_sq = pos.square(weak_side, PAWN);
    let mut result = Value(PUSH_CLOSE[Square::distance(winner_ksq, loser_ksq) as usize] as i32);
    if pawn_sq.relative_rank(weak_side) != RANK_7
        || Square::distance(loser_ksq, pawn_sq) != 1
        || (FILEA_BB | FILEC_BB | FILEF_BB | FILEH_BB) & pawn_sq == 0
    {
        result += QueenValueEg - PawnValueEg;
    }
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_kqkr(pos: &Position, strong_side: Color) -> Value {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, QueenValueMg, 0));
    debug_assert!(verify_material(pos, weak_side, RookValueMg, 0));
    let winner_ksq = pos.square(strong_side, KING);
    let loser_ksq = pos.square(weak_side, KING);
    let result = QueenValueEg - RookValueEg
        + PUSH_TO_EDGES[loser_ksq.0 as usize]
        + PUSH_CLOSE[Square::distance(winner_ksq, loser_ksq) as usize];
    if strong_side == pos.side_to_move() {
        result
    } else {
        -result
    }
}
fn evaluate_knnk(_pos: &Position, _strong_side: Color) -> Value {
    Value::DRAW
}
pub fn scale_kbpsk(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(pos.non_pawn_material_c(strong_side) == BishopValueMg);
    debug_assert!(pos.count(strong_side, PAWN) >= 1);
    let pawns = pos.pieces_cp(strong_side, PAWN);
    let pawns_file = lsb(pawns).file();
    if (pawns_file == FILE_A || pawns_file == FILE_H) && pawns & !file_bb(pawns_file) == 0 {
        let bishop_sq = pos.square(strong_side, BISHOP);
        let queening_sq = Square::make(pawns_file, RANK_8).relative(strong_side);
        let king_sq = pos.square(weak_side, KING);
        if opposite_colors(queening_sq, bishop_sq) && Square::distance(queening_sq, king_sq) <= 1 {
            return ScaleFactor::DRAW;
        }
    }
    if (pawns_file == FILE_B || pawns_file == FILE_G)
        && pos.pieces_p(PAWN) & !file_bb(pawns_file) == 0
        && pos.non_pawn_material_c(weak_side) == Value::ZERO
        && pos.count(weak_side, PAWN) >= 1
    {
        let weak_pawn_sq = backmost_sq(weak_side, pos.pieces_cp(weak_side, PAWN));
        let strong_king_sq = pos.square(strong_side, KING);
        let weak_king_sq = pos.square(weak_side, KING);
        let bishop_sq = pos.square(strong_side, BISHOP);
        if weak_pawn_sq.relative_rank(strong_side) == RANK_7
            && pos.pieces_cp(strong_side, PAWN) & (weak_pawn_sq + pawn_push(weak_side)) != 0
            && (opposite_colors(bishop_sq, weak_pawn_sq) || pos.count(strong_side, PAWN) == 1)
        {
            let strong_king_dist = Square::distance(weak_pawn_sq, strong_king_sq);
            let weak_king_dist = Square::distance(weak_pawn_sq, weak_king_sq);
            if weak_king_sq.relative_rank(strong_side) >= RANK_7
                && weak_king_dist <= 2
                && weak_king_dist <= strong_king_dist
            {
                return ScaleFactor::DRAW;
            }
        }
    }
    ScaleFactor::NONE
}
pub fn scale_kqkrps(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, QueenValueMg, 0));
    debug_assert!(pos.count(weak_side, ROOK) == 1);
    debug_assert!(pos.count(weak_side, PAWN) >= 1);
    let king_sq = pos.square(weak_side, KING);
    let rsq = pos.square(weak_side, ROOK);
    if king_sq.relative_rank(weak_side) <= RANK_2
        && pos.square(strong_side, KING).relative_rank(weak_side) >= RANK_4
        && rsq.relative_rank(weak_side) == RANK_3
        && pos.pieces_cp(weak_side, PAWN)
            & pos.attacks_from(KING, king_sq)
            & pos.attacks_from_pawn(rsq, strong_side)
            != 0
    {
        return ScaleFactor::DRAW;
    }
    ScaleFactor::NONE
}
fn scale_krpkr(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 1));
    debug_assert!(verify_material(pos, weak_side, RookValueMg, 0));
    let wksq = normalize(pos, strong_side, pos.square(strong_side, KING));
    let bksq = normalize(pos, strong_side, pos.square(weak_side, KING));
    let wrsq = normalize(pos, strong_side, pos.square(strong_side, ROOK));
    let wpsq = normalize(pos, strong_side, pos.square(strong_side, PAWN));
    let brsq = normalize(pos, strong_side, pos.square(weak_side, ROOK));
    let f = wpsq.file();
    let r = wpsq.rank();
    let queening_sq = Square::make(f, RANK_8);
    let tempo = (pos.side_to_move() == strong_side) as u32;
    if r <= RANK_5
        && Square::distance(bksq, queening_sq) <= 1
        && wksq.0 <= Square::H5.0
        && (brsq.rank() == RANK_6 || (r <= RANK_3 && wrsq.rank() != RANK_6))
    {
        return ScaleFactor::DRAW;
    }
    if r == RANK_6
        && Square::distance(bksq, queening_sq) <= 1
        && wksq.rank() + tempo <= RANK_6
        && (brsq.rank() == RANK_1 || (tempo == 0 && u32::distance(brsq.file(), wpsq.file()) >= 3))
    {
        return ScaleFactor::DRAW;
    }
    if r >= RANK_6
        && bksq == queening_sq
        && brsq.rank() == RANK_1
        && (tempo == 0 || Square::distance(wksq, wpsq) >= 2)
    {
        return ScaleFactor::DRAW;
    }
    if wpsq == Square::A7
        && wrsq == Square::A8
        && (bksq == Square::H7 || bksq == Square::G7)
        && brsq.file() == FILE_A
        && (brsq.rank() <= RANK_3 || wksq.file() >= FILE_D || wksq.rank() <= RANK_5)
    {
        return ScaleFactor::DRAW;
    }
    if r <= RANK_5
        && bksq == wpsq + NORTH
        && Square::distance(wksq, wpsq) >= 2 + tempo
        && Square::distance(wksq, brsq) >= 2 + tempo
    {
        return ScaleFactor::DRAW;
    }
    if r == RANK_7
        && f != FILE_A
        && wrsq.file() == f
        && wrsq != queening_sq
        && Square::distance(wksq, queening_sq) + 2 < Square::distance(bksq, queening_sq) + tempo
        && Square::distance(wksq, queening_sq) < Square::distance(bksq, wrsq) + tempo
    {
        return ScaleFactor(ScaleFactor::MAX.0 - 2 * Square::distance(wksq, queening_sq) as i32);
    }
    if f != FILE_A
        && wrsq.file() == f
        && wrsq.0 < wpsq.0
        && Square::distance(wksq, queening_sq) + 2 < Square::distance(bksq, queening_sq) + tempo
        && Square::distance(wksq, wpsq + NORTH) + 2 < Square::distance(bksq, wpsq + NORTH) + tempo
        && (Square::distance(bksq, wrsq) + tempo >= 3
            || (Square::distance(wksq, queening_sq) < Square::distance(bksq, wrsq) + tempo
                && Square::distance(wksq, wpsq + NORTH) < Square::distance(bksq, wrsq) + tempo))
    {
        return ScaleFactor(
            ScaleFactor::MAX.0
                - 8 * Square::distance(wpsq, queening_sq) as i32
                - 2 * Square::distance(wksq, queening_sq) as i32,
        );
    }
    if r <= RANK_4 && bksq > wpsq {
        if bksq.file() == wpsq.file() {
            return ScaleFactor(10);
        }
        if u32::distance(bksq.file(), wpsq.file()) == 1 && Square::distance(wksq, bksq) > 2 {
            return ScaleFactor(24 - 2 * Square::distance(wksq, bksq) as i32);
        }
    }
    ScaleFactor::NONE
}
fn scale_krpkb(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 1));
    debug_assert!(verify_material(pos, weak_side, BishopValueMg, 0));
    if pos.pieces_p(PAWN) & (FILEA_BB | FILEH_BB) != 0 {
        let ksq = pos.square(weak_side, KING);
        let bsq = pos.square(weak_side, BISHOP);
        let psq = pos.square(strong_side, PAWN);
        let rk = psq.relative_rank(strong_side);
        let push = pawn_push(strong_side);
        if rk == RANK_5 && !opposite_colors(bsq, psq) {
            let d = Square::distance(psq + 3 * push, ksq);
            if d <= 2 && !(d == 0 && ksq == pos.square(strong_side, KING) + 2 * push) {
                return ScaleFactor(24);
            } else {
                return ScaleFactor(48);
            }
        }
        if rk == RANK_6
            && Square::distance(psq + 2 * push, ksq) <= 1
            && pseudo_attacks(BISHOP, bsq) & (psq + push) != 0
            && u32::distance(bsq.file(), psq.file()) >= 2
        {
            return ScaleFactor(8);
        }
    }
    ScaleFactor::NONE
}
fn scale_krppkrp(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, RookValueMg, 2));
    debug_assert!(verify_material(pos, weak_side, RookValueMg, 1));
    let wpsq1 = pos.squares(strong_side, PAWN)[0];
    let wpsq2 = pos.squares(strong_side, PAWN)[1];
    let bksq = pos.square(weak_side, KING);
    if pos.pawn_passed(strong_side, wpsq1) || pos.pawn_passed(strong_side, wpsq2) {
        return ScaleFactor::NONE;
    }
    let r = std::cmp::max(
        wpsq1.relative_rank(strong_side),
        wpsq2.relative_rank(strong_side),
    );
    if u32::distance(bksq.file(), wpsq1.file()) <= 1
        && u32::distance(bksq.file(), wpsq2.file()) <= 1
        && bksq.relative_rank(strong_side) > r
    {
        debug_assert!(r > RANK_1 && r < RANK_7);
        return ScaleFactor(KRPPKRP_SCALE_FACTORS[r as usize]);
    }
    ScaleFactor::NONE
}
pub fn scale_kpsk(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(pos.non_pawn_material_c(strong_side) == Value::ZERO);
    debug_assert!(pos.count(strong_side, PAWN) >= 2);
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 0));
    let ksq = pos.square(weak_side, KING);
    let pawns = pos.pieces_cp(strong_side, PAWN);
    if pawns & !forward_ranks_bb(weak_side, ksq) == 0
        && !(pawns & !FILEA_BB != 0 && pawns & !FILEH_BB != 0)
        && u32::distance(ksq.file(), lsb(pawns).file()) <= 1
    {
        return ScaleFactor::DRAW;
    }
    ScaleFactor::NONE
}
fn scale_kbpkb(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, BishopValueMg, 1));
    debug_assert!(verify_material(pos, weak_side, BishopValueMg, 0));
    let psq = pos.square(strong_side, PAWN);
    let sbsq = pos.square(strong_side, BISHOP);
    let wbsq = pos.square(weak_side, BISHOP);
    let wksq = pos.square(weak_side, KING);
    if wksq.file() == psq.file()
        && psq.relative_rank(strong_side) < wksq.relative_rank(strong_side)
        && (opposite_colors(wksq, sbsq) || wksq.relative_rank(strong_side) <= RANK_6)
    {
        return ScaleFactor::DRAW;
    }
    if opposite_colors(sbsq, wbsq) {
        if psq.relative_rank(strong_side) <= RANK_5 {
            return ScaleFactor::DRAW;
        }
        let path = forward_file_bb(strong_side, psq);
        if path & pos.pieces_cp(weak_side, KING) != 0 {
            return ScaleFactor::DRAW;
        }
        if pos.attacks_from(BISHOP, wbsq) & path != 0 && Square::distance(wbsq, psq) >= 3 {
            return ScaleFactor::DRAW;
        }
    }
    ScaleFactor::NONE
}
fn scale_kbppkb(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, BishopValueMg, 2));
    debug_assert!(verify_material(pos, weak_side, BishopValueMg, 0));
    let wbsq = pos.square(strong_side, BISHOP);
    let bbsq = pos.square(weak_side, BISHOP);
    if !opposite_colors(wbsq, bbsq) {
        return ScaleFactor::NONE;
    }
    let ksq = pos.square(weak_side, KING);
    let psq1 = pos.squares(strong_side, PAWN)[0];
    let psq2 = pos.squares(strong_side, PAWN)[1];
    let r1 = psq1.rank();
    let r2 = psq2.rank();
    let (block_sq1, block_sq2) =
        if psq1.relative_rank(strong_side) > psq2.relative_rank(strong_side) {
            (
                psq1 + pawn_push(strong_side),
                Square::make(psq2.file(), psq1.rank()),
            )
        } else {
            (
                psq2 + pawn_push(strong_side),
                Square::make(psq1.file(), psq2.rank()),
            )
        };
    match u32::distance(psq1.file(), psq2.file()) {
        0 => {
            if ksq.file() == block_sq1.file()
                && ksq.relative_rank(strong_side) >= block_sq1.relative_rank(strong_side)
                && opposite_colors(ksq, wbsq)
            {
                return ScaleFactor::DRAW;
            } else {
                return ScaleFactor::NONE;
            }
        }
        1 => {
            if ksq == block_sq1
                && opposite_colors(ksq, wbsq)
                && (bbsq == block_sq2
                    || pos.attacks_from(BISHOP, block_sq2) & pos.pieces_cp(weak_side, BISHOP) != 0
                    || u32::distance(r1, r2) >= 2)
            {
                return ScaleFactor::DRAW;
            } else if ksq == block_sq2
                && opposite_colors(ksq, wbsq)
                && (bbsq == block_sq1
                    || pos.attacks_from(BISHOP, block_sq1) & pos.pieces_cp(weak_side, BISHOP) != 0)
            {
                return ScaleFactor::DRAW;
            } else {
                return ScaleFactor::NONE;
            }
        }
        _ => ScaleFactor::NONE,
    }
}
fn scale_kbpkn(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, BishopValueMg, 1));
    debug_assert!(verify_material(pos, weak_side, KnightValueMg, 0));
    let psq = pos.square(strong_side, PAWN);
    let sbsq = pos.square(strong_side, BISHOP);
    let wksq = pos.square(weak_side, KING);
    if wksq.file() == psq.file()
        && psq.relative_rank(strong_side) < wksq.relative_rank(strong_side)
        && (opposite_colors(wksq, sbsq) || wksq.relative_rank(strong_side) <= RANK_6)
    {
        return ScaleFactor::DRAW;
    }
    ScaleFactor::NONE
}
fn scale_knpk(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, KnightValueMg, 1));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 0));
    let psq = normalize(pos, strong_side, pos.square(strong_side, PAWN));
    let wksq = normalize(pos, strong_side, pos.square(weak_side, KING));
    if psq == Square::A7 && Square::distance(Square::A8, wksq) <= 1 {
        return ScaleFactor::DRAW;
    }
    ScaleFactor::NONE
}
fn scale_knpkb(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    let psq = pos.square(strong_side, PAWN);
    let bsq = pos.square(weak_side, BISHOP);
    let wksq = pos.square(weak_side, KING);
    if forward_file_bb(strong_side, psq) & pos.attacks_from(BISHOP, bsq) != 0 {
        return ScaleFactor(Square::distance(wksq, psq) as i32);
    }
    ScaleFactor::NONE
}
pub fn scale_kpkp(pos: &Position, strong_side: Color) -> ScaleFactor {
    let weak_side = !strong_side;
    debug_assert!(verify_material(pos, strong_side, Value::ZERO, 1));
    debug_assert!(verify_material(pos, weak_side, Value::ZERO, 1));
    let wksq = normalize(pos, strong_side, pos.square(strong_side, KING));
    let bksq = normalize(pos, strong_side, pos.square(weak_side, KING));
    let psq = normalize(pos, strong_side, pos.square(strong_side, PAWN));
    let us = if strong_side == pos.side_to_move() {
        WHITE
    } else {
        BLACK
    };
    if psq.rank() >= RANK_5 && psq.file() != FILE_A {
        return ScaleFactor::NONE;
    }
    if bitbases::probe(wksq, psq, bksq, us) {
        ScaleFactor::NONE
    } else {
        ScaleFactor::DRAW
    }
}
