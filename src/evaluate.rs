use bitboard::*;
use material;
use pawns;
use position::Position;
use std;
use types::*;
pub const TEMPO: Value = Value(20);
pub static mut CONTEMPT: Score = Score::ZERO;
fn contempt() -> Score {
    unsafe { CONTEMPT }
}
const CENTER: Bitboard = Bitboard(0x0000001818000000);
const QUEEN_SIDE: Bitboard = Bitboard(0x0f0f0f0f0f0f0f0f);
const CENTER_FILES: Bitboard = Bitboard(0x3c3c3c3c3c3c3c3c);
const KING_SIDE: Bitboard = Bitboard(0xf0f0f0f0f0f0f0f0);
const KING_FLANK: [Bitboard; 8] = [
    QUEEN_SIDE,
    QUEEN_SIDE,
    QUEEN_SIDE,
    CENTER_FILES,
    CENTER_FILES,
    KING_SIDE,
    KING_SIDE,
    KING_SIDE,
];
struct EvalInfo<'a> {
    me: &'a material::Entry,
    pe: &'a mut pawns::Entry,
    mobility_area: [Bitboard; 2],
    mobility: [Score; 2],
    attacked_by: [[Bitboard; 8]; 2],
    attacked_by2: [Bitboard; 2],
    king_ring: [Bitboard; 2],
    king_attackers_count: [i32; 2],
    king_attackers_weight: [i32; 2],
    king_adjacent_zone_attacks_count: [i32; 2],
}
impl<'a> EvalInfo<'a> {
    fn new(me: &'a material::Entry, pe: &'a mut pawns::Entry) -> EvalInfo<'a> {
        EvalInfo {
            me: me,
            pe: pe,
            mobility_area: [Bitboard(0); 2],
            mobility: [Score::ZERO; 2],
            attacked_by: [[Bitboard(0); 8]; 2],
            attacked_by2: [Bitboard(0); 2],
            king_ring: [Bitboard(0); 2],
            king_attackers_count: [0; 2],
            king_attackers_weight: [0; 2],
            king_adjacent_zone_attacks_count: [0; 2],
        }
    }
}
macro_rules! S {
    ($x:expr, $y:expr) => {
        Score(($y << 16) + $x)
    };
}
const S0: Score = Score::ZERO;
const MOBILITY_BONUS: [[Score; 32]; 4] = [
    [
        S!(-75, -76),
        S!(-57, -54),
        S!(-9, -28),
        S!(-2, -10),
        S!(6, 5),
        S!(14, 12),
        S!(22, 26),
        S!(29, 29),
        S!(36, 29),
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
    ],
    [
        S!(-48, -59),
        S!(-20, -23),
        S!(16, -3),
        S!(26, 13),
        S!(38, 24),
        S!(51, 42),
        S!(55, 54),
        S!(63, 57),
        S!(63, 65),
        S!(68, 73),
        S!(81, 78),
        S!(81, 86),
        S!(91, 88),
        S!(98, 97),
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
    ],
    [
        S!(-58, -76),
        S!(-27, -18),
        S!(-15, 28),
        S!(-10, 55),
        S!(-5, 69),
        S!(-2, 82),
        S!(9, 112),
        S!(16, 118),
        S!(30, 132),
        S!(29, 142),
        S!(32, 155),
        S!(38, 165),
        S!(46, 166),
        S!(48, 169),
        S!(58, 171),
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
        S0,
    ],
    [
        S!(-39, -36),
        S!(-21, -15),
        S!(3, 8),
        S!(3, 18),
        S!(14, 34),
        S!(22, 54),
        S!(28, 61),
        S!(41, 73),
        S!(43, 79),
        S!(48, 92),
        S!(56, 94),
        S!(60, 104),
        S!(60, 113),
        S!(66, 120),
        S!(67, 123),
        S!(70, 126),
        S!(71, 133),
        S!(73, 136),
        S!(79, 140),
        S!(88, 143),
        S!(88, 148),
        S!(99, 166),
        S!(102, 170),
        S!(102, 175),
        S!(106, 184),
        S!(109, 191),
        S!(113, 206),
        S!(116, 212),
        S0,
        S0,
        S0,
        S0,
    ],
];
const OUTPOST: [[Score; 2]; 2] = [[S!(22, 6), S!(36, 12)], [S!(9, 2), S!(15, 5)]];
const ROOK_ON_FILE: [Score; 2] = [S!(20, 7), S!(45, 20)];
const THREAT_BY_MINOR: [Score; 8] = [
    S!(0, 0),
    S!(0, 31),
    S!(39, 42),
    S!(57, 44),
    S!(68, 112),
    S!(47, 120),
    S0,
    S0,
];
const THREAT_BY_ROOK: [Score; 8] = [
    S!(0, 0),
    S!(0, 24),
    S!(38, 71),
    S!(38, 61),
    S!(0, 38),
    S!(36, 38),
    S0,
    S0,
];
const THREAT_BY_KING: [Score; 2] = [S!(3, 65), S!(9, 145)];
const PASSED: [[i32; 8]; 2] = [
    [0, 5, 5, 32, 70, 172, 217, 0],
    [0, 7, 13, 42, 70, 170, 269, 0],
];
const PASSED_FILE: [Score; 8] = [
    S!(9, 10),
    S!(2, 10),
    S!(1, -8),
    S!(-20, -12),
    S!(-20, -12),
    S!(1, -8),
    S!(2, 10),
    S!(9, 10),
];
const RANK_FACTOR: [i32; 8] = [0, 0, 0, 2, 7, 12, 19, 0];
const KING_PROTECTOR: [Score; 4] = [S!(-3, -5), S!(-4, -3), S!(-3, 0), S!(-1, 1)];
const MINOR_BEHIND_PAWN: Score = S!(16, 0);
const BISHOP_PAWNS: Score = S!(8, 12);
const LONG_RANGED_BISHOP: Score = S!(22, 0);
const ROOK_ON_PAWN: Score = S!(8, 24);
const TRAPPED_ROOK: Score = S!(92, 0);
const WEAK_QUEEN: Score = S!(50, 10);
const CLOSE_ENEMIES: Score = S!(7, 0);
const PAWNLESS_FLANK: Score = S!(20, 80);
const THREAT_BY_SAFE_PAWN: Score = S!(175, 168);
const THREAT_BY_RANK: Score = S!(16, 3);
const HANGING: Score = S!(52, 30);
const WEAK_UNOPPOSED_PAWN: Score = S!(5, 25);
const THREAT_BY_PAWN_PUSH: Score = S!(47, 26);
const THREAT_BY_ATTACK_ON_QUEEN: Score = S!(42, 21);
const HINDER_PASSED_PAWN: Score = S!(8, 1);
const TRAPPED_BISHOP_A1H1: Score = S!(50, 50);
const KING_ATTACK_WEIGHTS: [i32; 8] = [0, 0, 78, 56, 45, 11, 0, 0];
const QUEEN_SAFE_CHECK: i32 = 780;
const ROOK_SAFE_CHECK: i32 = 880;
const BISHOP_SAFE_CHECK: i32 = 435;
const KNIGHT_SAFE_CHECK: i32 = 790;
const LAZY_THRESHOLD: Value = Value(1500);
const SPACE_THRESHOLD: Value = Value(12222);
fn initialize<Us: ColorTrait>(pos: &Position, ei: &mut EvalInfo) {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let up = if us == WHITE { NORTH } else { SOUTH };
    let down = if us == WHITE { SOUTH } else { NORTH };
    let low_ranks = if us == WHITE {
        RANK2_BB | RANK3_BB
    } else {
        RANK7_BB | RANK6_BB
    };
    let b = pos.pieces_cp(us, PAWN) & (pos.pieces().shift(down) | low_ranks);
    ei.mobility_area[us.0 as usize] = !(b | pos.square(us, KING) | ei.pe.pawn_attacks(them));
    let b = pos.attacks_from(KING, pos.square(us, KING));
    ei.attacked_by[us.0 as usize][KING.0 as usize] = b;
    ei.attacked_by[us.0 as usize][PAWN.0 as usize] = ei.pe.pawn_attacks(us);
    ei.attacked_by2[us.0 as usize] = b & ei.attacked_by[us.0 as usize][PAWN.0 as usize];
    ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize] =
        b | ei.attacked_by[us.0 as usize][PAWN.0 as usize];
    if pos.non_pawn_material_c(them) >= RookValueMg + KnightValueMg {
        ei.king_ring[us.0 as usize] = b;
        if pos.square(us, KING).relative_rank(us) == RANK_1 {
            ei.king_ring[us.0 as usize] |= b.shift(up);
        }
        ei.king_attackers_count[them.0 as usize] = popcount(b & ei.pe.pawn_attacks(them)) as i32;
        ei.king_adjacent_zone_attacks_count[them.0 as usize] = 0;
        ei.king_attackers_weight[them.0 as usize] = 0;
    } else {
        ei.king_ring[us.0 as usize] = Bitboard(0);
        ei.king_attackers_count[them.0 as usize] = 0;
    }
}
fn evaluate_pieces<Us: ColorTrait, Pt: PieceTypeTrait>(pos: &Position, ei: &mut EvalInfo) -> Score {
    let us = Us::COLOR;
    let pt = Pt::TYPE;
    let them = if us == WHITE { BLACK } else { WHITE };
    let outpost_ranks = if us == WHITE {
        RANK4_BB | RANK5_BB | RANK6_BB
    } else {
        RANK5_BB | RANK4_BB | RANK3_BB
    };
    let mut score = Score::ZERO;
    ei.attacked_by[us.0 as usize][pt.0 as usize] = Bitboard(0);
    if pt == QUEEN {
        ei.attacked_by[us.0 as usize][QUEEN_DIAGONAL.0 as usize] = Bitboard(0);
    }
    for s in pos.square_list(us, pt) {
        let mut b = match pt {
            BISHOP => attacks_bb(BISHOP, s, pos.pieces() ^ pos.pieces_p(QUEEN)),
            ROOK => attacks_bb(
                ROOK,
                s,
                pos.pieces() ^ pos.pieces_p(QUEEN) ^ pos.pieces_cp(us, ROOK),
            ),
            _ => pos.attacks_from(pt, s),
        };
        if pos.blockers_for_king(us) & s != 0 {
            b &= line_bb(pos.square(us, KING), s);
        }
        ei.attacked_by2[us.0 as usize] |= ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize] & b;
        ei.attacked_by[us.0 as usize][pt.0 as usize] |= b;
        ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize] |=
            ei.attacked_by[us.0 as usize][pt.0 as usize];
        if pt == QUEEN {
            ei.attacked_by[us.0 as usize][QUEEN_DIAGONAL.0 as usize] |=
                b & pseudo_attacks(BISHOP, s);
        }
        if b & ei.king_ring[them.0 as usize] != 0 {
            ei.king_attackers_count[us.0 as usize] += 1;
            ei.king_attackers_weight[us.0 as usize] += KING_ATTACK_WEIGHTS[pt.0 as usize];
            ei.king_adjacent_zone_attacks_count[us.0 as usize] +=
                popcount(b & ei.attacked_by[them.0 as usize][KING.0 as usize]) as i32;
        }
        let mob = popcount(b & ei.mobility_area[us.0 as usize]);
        ei.mobility[us.0 as usize] += MOBILITY_BONUS[(pt.0 - 2) as usize][mob as usize];
        score +=
            KING_PROTECTOR[(pt.0 - 2) as usize] * Square::distance(s, pos.square(us, KING)) as i32;
        if pt == BISHOP || pt == KNIGHT {
            let mut bb = outpost_ranks & !ei.pe.pawn_attacks_span(them);
            if bb & s != 0 {
                score += OUTPOST[(pt == BISHOP) as usize]
                    [(ei.attacked_by[us.0 as usize][PAWN.0 as usize] & s != 0) as usize]
                    * 2;
            } else {
                bb &= b & !pos.pieces_c(us);
                if bb != 0 {
                    score += OUTPOST[(pt == BISHOP) as usize]
                        [((ei.attacked_by[us.0 as usize][PAWN.0 as usize] & bb) != 0) as usize];
                }
            }
            if s.relative_rank(us) < RANK_5 && pos.pieces_p(PAWN) & (s + pawn_push(us)) != 0 {
                score += MINOR_BEHIND_PAWN;
            }
            if pt == BISHOP {
                score -= BISHOP_PAWNS * ei.pe.pawns_on_same_color_squares(us, s);
                if more_than_one(CENTER & (attacks_bb(BISHOP, s, pos.pieces_p(PAWN)) | s)) {
                    score += LONG_RANGED_BISHOP;
                }
            }
            if pt == BISHOP
                && pos.is_chess960()
                && (s == Square::A1.relative(us) || s == Square::H1.relative(us))
            {
                let d = pawn_push(us) + (if s.file() == FILE_A { EAST } else { WEST });
                if pos.piece_on(s + d) == Piece::make(us, PAWN) {
                    score -= if !pos.empty(s + d + pawn_push(us)) {
                        TRAPPED_BISHOP_A1H1 * 4
                    } else if pos.piece_on(s + 2 * d) == Piece::make(us, PAWN) {
                        TRAPPED_BISHOP_A1H1 * 2
                    } else {
                        TRAPPED_BISHOP_A1H1
                    }
                }
            }
        }
        if pt == ROOK {
            if s.relative_rank(us) >= RANK_5 {
                score += ROOK_ON_PAWN
                    * (popcount(pos.pieces_cp(them, PAWN) & pseudo_attacks(ROOK, s)) as i32);
            }
            if ei.pe.semiopen_file(us, s.file()) != 0 {
                score += ROOK_ON_FILE[(ei.pe.semiopen_file(them, s.file()) != 0) as usize];
            } else if mob <= 3 {
                let kf = pos.square(us, KING).file();
                if (kf < FILE_E) == (s.file() < kf) {
                    score -= (TRAPPED_ROOK - Score::make((mob as i32) * 22, 0))
                        * (1 + ((!pos.can_castle(us)) as i32));
                }
            }
        }
        if pt == QUEEN {
            let mut pinners = Bitboard(0);
            if pos.slider_blockers(pos.pieces_cpp(them, ROOK, BISHOP), s, &mut pinners) != 0 {
                score -= WEAK_QUEEN;
            }
        }
    }
    score
}
fn evaluate_king<Us: ColorTrait>(pos: &Position, ei: &mut EvalInfo) -> Score {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let camp = if us == WHITE {
        ALL_SQUARES ^ RANK6_BB ^ RANK7_BB ^ RANK8_BB
    } else {
        ALL_SQUARES ^ RANK1_BB ^ RANK2_BB ^ RANK3_BB
    };
    let ksq = pos.square(us, KING);
    let mut score = ei.pe.king_safety::<Us>(pos, ksq);
    if ei.king_attackers_count[them.0 as usize] > (1 - popcount(pos.pieces_cp(them, QUEEN)) as i32)
    {
        let weak = ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]
            & !ei.attacked_by2[us.0 as usize]
            & (ei.attacked_by[us.0 as usize][KING.0 as usize]
                | ei.attacked_by[us.0 as usize][QUEEN.0 as usize]
                | !ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize]);
        let mut king_danger = 0;
        let mut unsafe_checks = Bitboard(0);
        let safe = !pos.pieces_c(them)
            & (!ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize]
                | (weak & ei.attacked_by2[them.0 as usize]));
        let mut b1 = attacks_bb(ROOK, ksq, pos.pieces() ^ pos.pieces_cp(us, QUEEN));
        let mut b2 = attacks_bb(BISHOP, ksq, pos.pieces() ^ pos.pieces_cp(us, QUEEN));
        if (b1 | b2)
            & ei.attacked_by[them.0 as usize][QUEEN.0 as usize]
            & safe
            & !ei.attacked_by[us.0 as usize][QUEEN.0 as usize]
            != 0
        {
            king_danger += QUEEN_SAFE_CHECK;
        }
        b1 &= ei.attacked_by[them.0 as usize][ROOK.0 as usize];
        b2 &= ei.attacked_by[them.0 as usize][BISHOP.0 as usize];
        if b1 & safe != 0 {
            king_danger += ROOK_SAFE_CHECK;
        } else {
            unsafe_checks |= b1;
        }
        if b2 & safe != 0 {
            king_danger += BISHOP_SAFE_CHECK;
        } else {
            unsafe_checks |= b2;
        }
        let b = pos.attacks_from(KNIGHT, ksq) & ei.attacked_by[them.0 as usize][KNIGHT.0 as usize];
        if b & safe != 0 {
            king_danger += KNIGHT_SAFE_CHECK;
        } else {
            unsafe_checks |= b;
        }
        unsafe_checks &= ei.mobility_area[them.0 as usize];
        let pinned = pos.blockers_for_king(us) & pos.pieces_c(us);
        king_danger += ei.king_attackers_count[them.0 as usize]
            * ei.king_attackers_weight[them.0 as usize]
            + 102 * ei.king_adjacent_zone_attacks_count[them.0 as usize]
            + 191 * popcount(ei.king_ring[us.0 as usize] & weak) as i32
            + 143 * popcount(pinned | unsafe_checks) as i32
            - 848 * (pos.count(them, QUEEN) == 0) as i32
            - 9 * score.mg().0 / 8
            + 40;
        if king_danger > 0 {
            let mobility_danger = (ei.mobility[them.0 as usize] - ei.mobility[us.0 as usize])
                .mg()
                .0;
            king_danger = std::cmp::max(0, king_danger + mobility_danger);
            score -= Score::make(king_danger * king_danger / 4096, king_danger / 16);
        }
    }
    let kf = ksq.file();
    let mut b =
        ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize] & KING_FLANK[kf as usize] & camp;
    debug_assert!(((if us == WHITE { b << 4 } else { b >> 4 }) & b) == 0);
    debug_assert!(popcount(if us == WHITE { b << 4 } else { b >> 4 }) == popcount(b));
    b = (if us == WHITE { b << 4 } else { b >> 4 })
        | (b & ei.attacked_by2[them.0 as usize] & !ei.attacked_by[us.0 as usize][PAWN.0 as usize]);
    score -= CLOSE_ENEMIES * (popcount(b) as i32);
    if pos.pieces_p(PAWN) & KING_FLANK[kf as usize] == 0 {
        score -= PAWNLESS_FLANK;
    }
    score
}
fn evaluate_threats<Us: ColorTrait>(pos: &Position, ei: &EvalInfo) -> Score {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let up = if us == WHITE { NORTH } else { SOUTH };
    let left = if us == WHITE { NORTH_WEST } else { SOUTH_EAST };
    let right = if us == WHITE { NORTH_EAST } else { SOUTH_WEST };
    let trank3bb = if us == WHITE { RANK3_BB } else { RANK6_BB };
    let mut score = Score::ZERO;
    let weak = (pos.pieces_c(them) ^ pos.pieces_cp(them, PAWN))
        & ei.attacked_by[us.0 as usize][PAWN.0 as usize];
    if weak != 0 {
        let b = pos.pieces_cp(us, PAWN)
            & (!ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]
                | ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize]);
        let safe_threats = (b.shift(right) | b.shift(left)) & weak;
        score += THREAT_BY_SAFE_PAWN * (popcount(safe_threats) as i32);
    }
    let strongly_protected = ei.attacked_by[them.0 as usize][PAWN.0 as usize]
        | (ei.attacked_by2[them.0 as usize] & !ei.attacked_by2[us.0 as usize]);
    let defended = (pos.pieces_c(them) ^ pos.pieces_cp(them, PAWN)) & strongly_protected;
    let weak = pos.pieces_c(them)
        & !strongly_protected
        & ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize];
    if defended | weak != 0 {
        let b = (defended | weak)
            & (ei.attacked_by[us.0 as usize][KNIGHT.0 as usize]
                | ei.attacked_by[us.0 as usize][BISHOP.0 as usize]);
        for s in b {
            score += THREAT_BY_MINOR[pos.piece_on(s).piece_type().0 as usize];
            if pos.piece_on(s).piece_type() != PAWN {
                score += THREAT_BY_RANK * (s.relative_rank(them) as i32);
            }
        }
        let b =
            (pos.pieces_cp(them, QUEEN) | weak) & ei.attacked_by[us.0 as usize][ROOK.0 as usize];
        for s in b {
            score += THREAT_BY_ROOK[pos.piece_on(s).piece_type().0 as usize];
            if pos.piece_on(s).piece_type() != PAWN {
                score += THREAT_BY_RANK * (s.relative_rank(them) as i32);
            }
        }
        score += HANGING
            * (popcount(weak & !ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]) as i32);
        let b = weak & ei.attacked_by[us.0 as usize][KING.0 as usize];
        if b != 0 {
            score += THREAT_BY_KING[more_than_one(b) as usize];
        }
    }
    if pos.pieces_cpp(us, ROOK, QUEEN) != 0 {
        score += WEAK_UNOPPOSED_PAWN * ei.pe.weak_unopposed(them);
    }
    let mut b = pos.pieces_cp(us, PAWN).shift(up) & !pos.pieces();
    b |= (b & trank3bb).shift(up) & !pos.pieces();
    b &= !ei.attacked_by[them.0 as usize][PAWN.0 as usize]
        & (ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize]
            | !ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]);
    b = (b.shift(left) | b.shift(right))
        & pos.pieces_c(them)
        & !ei.attacked_by[us.0 as usize][PAWN.0 as usize];
    score += THREAT_BY_PAWN_PUSH * (popcount(b) as i32);
    let safe_threats =
        !pos.pieces_c(us) & !ei.attacked_by2[them.0 as usize] & ei.attacked_by2[us.0 as usize];
    b = (ei.attacked_by[us.0 as usize][BISHOP.0 as usize]
        & ei.attacked_by[them.0 as usize][QUEEN_DIAGONAL.0 as usize])
        | (ei.attacked_by[us.0 as usize][ROOK.0 as usize]
            & ei.attacked_by[them.0 as usize][QUEEN.0 as usize]
            & !ei.attacked_by[them.0 as usize][QUEEN_DIAGONAL.0 as usize]);
    score += THREAT_BY_ATTACK_ON_QUEEN * popcount(b & safe_threats) as i32;
    score
}
fn capped_distance(s1: Square, s2: Square) -> i32 {
    std::cmp::min(Square::distance(s1, s2), 5) as i32
}
fn evaluate_passed_pawns<Us: ColorTrait>(pos: &Position, ei: &EvalInfo) -> Score {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let up = if us == WHITE { NORTH } else { SOUTH };
    let mut score = Score::ZERO;
    for s in ei.pe.passed_pawns(us) {
        debug_assert!(pos.pieces_cp(them, PAWN) & forward_file_bb(us, s + up) == 0);
        let bb = forward_file_bb(us, s)
            & (ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize] | pos.pieces_c(them));
        score -= HINDER_PASSED_PAWN * popcount(bb) as i32;
        let r = s.relative_rank(us);
        let rr = RANK_FACTOR[r as usize];
        let mut mbonus = PASSED[MG][r as usize];
        let mut ebonus = PASSED[EG][r as usize];
        if rr != 0 {
            let block_sq = s + up;
            ebonus += capped_distance(pos.square(them, KING), block_sq) * 5 * rr
                - capped_distance(pos.square(us, KING), block_sq) * 2 * rr;
            if r != RANK_7 {
                ebonus -= capped_distance(pos.square(us, KING), block_sq + up) * rr;
            }
            if pos.empty(block_sq) {
                let mut defended_squares = forward_file_bb(us, s);
                let mut unsafe_squares = defended_squares;
                let squares_to_queen = defended_squares;
                let bb = forward_file_bb(them, s)
                    & pos.pieces_pp(ROOK, QUEEN)
                    & pos.attacks_from(ROOK, s);
                if pos.pieces_c(us) & bb == 0 {
                    defended_squares &= ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize];
                }
                if pos.pieces_c(them) & bb == 0 {
                    unsafe_squares &=
                        ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize] | pos.pieces_c(them);
                }
                let mut k = if unsafe_squares == 0 {
                    20
                } else if unsafe_squares & block_sq == 0 {
                    9
                } else {
                    0
                };
                if defended_squares == squares_to_queen {
                    k += 6;
                } else if defended_squares & block_sq != 0 {
                    k += 4;
                }
                mbonus += k * rr;
                ebonus += k * rr;
            } else if pos.pieces_c(us) & block_sq != 0 {
                mbonus += rr + r as i32 * 2;
                ebonus += rr + r as i32 * 2;
            }
        }
        if !pos.pawn_passed(us, s + up) || pos.pieces_p(PAWN) & forward_file_bb(us, s) != 0 {
            mbonus /= 2;
            ebonus /= 2;
        }
        score += Score::make(mbonus, ebonus) + PASSED_FILE[s.file() as usize];
    }
    score
}
fn evaluate_space<Us: ColorTrait>(pos: &Position, ei: &EvalInfo) -> Score {
    let us = Us::COLOR;
    let them = if us == WHITE { BLACK } else { WHITE };
    let space_mask = if us == WHITE {
        CENTER_FILES & (RANK2_BB | RANK3_BB | RANK4_BB)
    } else {
        CENTER_FILES & (RANK7_BB | RANK6_BB | RANK5_BB)
    };
    let safe = space_mask
        & !pos.pieces_cp(us, PAWN)
        & !ei.attacked_by[them.0 as usize][PAWN.0 as usize]
        & (ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize]
            | !ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]);
    let mut behind = pos.pieces_cp(us, PAWN);
    behind |= if us == WHITE {
        behind >> 8
    } else {
        behind << 8
    };
    behind |= if us == WHITE {
        behind >> 16
    } else {
        behind << 16
    };
    debug_assert!((safe >> (if us == WHITE { 32 } else { 0 })).0 as u32 == 0);
    let bonus =
        popcount((if us == WHITE { safe << 32 } else { safe >> 32 }) | (behind & safe)) as i32;
    let weight = pos.count(us, ALL_PIECES) - 2 * ei.pe.open_files();
    Score::make(bonus * weight * weight / 16, 0)
}
fn evaluate_initiative(pos: &Position, ei: &EvalInfo, eg: Value) -> Score {
    let king_distance = u32::distance(
        pos.square(WHITE, KING).file(),
        pos.square(BLACK, KING).file(),
    ) as i32
        - u32::distance(
            pos.square(WHITE, KING).rank(),
            pos.square(BLACK, KING).rank(),
        ) as i32;
    let both_flanks = pos.pieces_p(PAWN) & QUEEN_SIDE != 0 && pos.pieces_p(PAWN) & KING_SIDE != 0;
    let initiative = 8 * (ei.pe.pawn_asymmetry() + king_distance - 17)
        + 12 * (pos.count(WHITE, PAWN) + pos.count(BLACK, PAWN))
        + 16 * (both_flanks as i32);
    let v = ((eg.0 > 0) as i32 - (eg.0 < 0) as i32) * std::cmp::max(initiative, -eg.0.abs());
    Score::make(0, v)
}
fn evaluate_scale_factor(pos: &Position, ei: &EvalInfo, eg: Value) -> ScaleFactor {
    let strong_side = if eg > Value::DRAW { WHITE } else { BLACK };
    let sf = ei.me.scale_factor(pos, strong_side);
    if sf == ScaleFactor::NORMAL || sf == ScaleFactor::ONEPAWN {
        if pos.opposite_bishops() {
            if pos.non_pawn_material_c(WHITE) == BishopValueMg
                && pos.non_pawn_material_c(BLACK) == BishopValueMg
            {
                return if more_than_one(pos.pieces_p(PAWN)) {
                    ScaleFactor(31)
                } else {
                    ScaleFactor(9)
                };
            }
            return ScaleFactor(46);
        } else if eg.abs() <= BishopValueEg
            && pos.count(strong_side, PAWN) <= 2
            && !pos.pawn_passed(!strong_side, pos.square(!strong_side, KING))
        {
            return ScaleFactor(37 + 7 * pos.count(strong_side, PAWN));
        }
    }
    sf
}
pub fn evaluate(pos: &Position) -> Value {
    debug_assert!(pos.checkers() == 0);
    let me = material::probe(pos);
    if me.specialized_eval_exists() {
        return me.evaluate(pos);
    }
    let mut score = pos.psq_score() + me.imbalance() + contempt();
    let pe = pawns::probe(pos);
    score += pe.pawns_score();
    let v = (score.mg() + score.eg()) / 2;
    if v.abs() > LAZY_THRESHOLD {
        return if pos.side_to_move() == WHITE { v } else { -v };
    }
    let mut ei = EvalInfo::new(me, pe);
    initialize::<White>(pos, &mut ei);
    initialize::<Black>(pos, &mut ei);
    score += evaluate_pieces::<White, Knight>(pos, &mut ei)
        - evaluate_pieces::<Black, Knight>(pos, &mut ei);
    score += evaluate_pieces::<White, Bishop>(pos, &mut ei)
        - evaluate_pieces::<Black, Bishop>(pos, &mut ei);
    score +=
        evaluate_pieces::<White, Rook>(pos, &mut ei) - evaluate_pieces::<Black, Rook>(pos, &mut ei);
    score += evaluate_pieces::<White, Queen>(pos, &mut ei)
        - evaluate_pieces::<Black, Queen>(pos, &mut ei);
    score += ei.mobility[WHITE.0 as usize] - ei.mobility[BLACK.0 as usize];
    score += evaluate_king::<White>(pos, &mut ei) - evaluate_king::<Black>(pos, &mut ei);
    score += evaluate_threats::<White>(pos, &ei) - evaluate_threats::<Black>(pos, &ei);
    score += evaluate_passed_pawns::<White>(pos, &ei) - evaluate_passed_pawns::<Black>(pos, &ei);
    if pos.non_pawn_material() >= SPACE_THRESHOLD {
        score += evaluate_space::<White>(pos, &ei) - evaluate_space::<Black>(pos, &ei);
    }
    score += evaluate_initiative(pos, &ei, score.eg());
    let sf = evaluate_scale_factor(pos, &ei, score.eg());
    let mut v = score.mg() * ei.me.game_phase()
        + score.eg() * (PHASE_MIDGAME - ei.me.game_phase()) * sf.0 / ScaleFactor::NORMAL.0;
    v /= PHASE_MIDGAME;
    TEMPO + if pos.side_to_move() == WHITE { v } else { -v }
}
