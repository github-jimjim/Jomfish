use bitboard::*;
use endgame::*;
use position::Position;
use std;
use types::*;
pub struct Entry {
    key: Key,
    scaling_function: [Option<ScaleFn>; 2],
    evaluation_function: Option<EvalFn>,
    eval_side: Color,
    value: i16,
    factor: [u8; 2],
    game_phase: Phase,
}
impl Entry {
    pub fn new() -> Entry {
        Entry {
            key: Key(0),
            scaling_function: [None; 2],
            evaluation_function: None,
            eval_side: WHITE,
            value: 0,
            factor: [0; 2],
            game_phase: 0,
        }
    }
    pub fn imbalance(&self) -> Score {
        Score::make(self.value as i32, self.value as i32)
    }
    pub fn game_phase(&self) -> Phase {
        self.game_phase
    }
    pub fn specialized_eval_exists(&self) -> bool {
        match self.evaluation_function {
            Some(_) => true,
            None => false,
        }
    }
    pub fn evaluate(&self, pos: &Position) -> Value {
        self.evaluation_function.unwrap()(pos, self.eval_side)
    }
    pub fn scale_factor(&self, pos: &Position, c: Color) -> ScaleFactor {
        let sf = match self.scaling_function[c.0 as usize] {
            Some(f) => f(pos, c),
            None => ScaleFactor::NONE,
        };
        if sf != ScaleFactor::NONE {
            sf
        } else {
            ScaleFactor(self.factor[c.0 as usize] as i32)
        }
    }
}
const QUADRATIC_OURS: [[i32; 8]; 6] = [
    [1667, 0, 0, 0, 0, 0, 0, 0],
    [40, 0, 0, 0, 0, 0, 0, 0],
    [32, 255, -3, 0, 0, 0, 0, 0],
    [0, 104, 4, 0, 0, 0, 0, 0],
    [-26, -2, 47, 105, -149, 0, 0, 0],
    [-189, 24, 117, 133, -134, -10, 0, 0],
];
const QUADRATIC_THEIRS: [[i32; 8]; 6] = [
    [0, 0, 0, 0, 0, 0, 0, 0],
    [36, 0, 0, 0, 0, 0, 0, 0],
    [9, 63, 0, 0, 0, 0, 0, 0],
    [59, 65, 42, 0, 0, 0, 0, 0],
    [46, 39, 24, -24, 0, 0, 0, 0],
    [97, 100, -42, 137, 268, 0, 0, 0],
];
fn is_kxk(pos: &Position, us: Color) -> bool {
    !more_than_one(pos.pieces_c(!us)) && pos.non_pawn_material_c(us) >= RookValueMg
}
fn is_kbpsks(pos: &Position, us: Color) -> bool {
    pos.non_pawn_material_c(us) == BishopValueMg
        && pos.count(us, BISHOP) == 1
        && pos.count(us, PAWN) >= 1
}
fn is_kqkrps(pos: &Position, us: Color) -> bool {
    pos.count(us, PAWN) == 0
        && pos.non_pawn_material_c(us) == QueenValueMg
        && pos.count(us, QUEEN) == 1
        && pos.count(!us, ROOK) == 1
        && pos.count(!us, PAWN) >= 1
}
fn imbalance(pc: &[[i32; 6]; 2], us: Color) -> i32 {
    let them = if us == WHITE { BLACK } else { WHITE };
    let mut bonus = 0;
    for pt1 in 0..6 {
        if pc[us.0 as usize][pt1] == 0 {
            continue;
        }
        let mut v = 0;
        for pt2 in 0..(pt1 + 1) {
            v += QUADRATIC_OURS[pt1][pt2] * pc[us.0 as usize][pt2]
                + QUADRATIC_THEIRS[pt1][pt2] * pc[them.0 as usize][pt2];
        }
        bonus += pc[us.0 as usize][pt1] * v;
    }
    bonus
}
pub fn probe(pos: &Position) -> &'static mut Entry {
    let key = pos.material_key();
    let e = pos.material_table[(key.0 & 8191) as usize].get();
    let e: &'static mut Entry = unsafe { &mut *e };
    if e.key == key {
        return e;
    }
    e.key = key;
    e.evaluation_function = None;
    e.scaling_function = [None; 2];
    e.factor[WHITE.0 as usize] = ScaleFactor::NORMAL.0 as u8;
    e.factor[BLACK.0 as usize] = ScaleFactor::NORMAL.0 as u8;
    e.value = 0;
    let npm_w = pos.non_pawn_material_c(WHITE);
    let npm_b = pos.non_pawn_material_c(BLACK);
    let npm = std::cmp::max(ENDGAME_LIMIT, std::cmp::min(npm_w + npm_b, MIDGAME_LIMIT));
    e.game_phase =
        (((npm - ENDGAME_LIMIT) * PHASE_MIDGAME) / (MIDGAME_LIMIT - ENDGAME_LIMIT)) as i32;
    for entry in unsafe { EVAL_FNS.iter() } {
        for c in 0..2 {
            if entry.key[c] == key {
                e.evaluation_function = Some(entry.func);
                e.eval_side = Color(c as u32);
                return e;
            }
        }
    }
    for &c in [WHITE, BLACK].iter() {
        if is_kxk(pos, c) {
            e.evaluation_function = Some(evaluate_kxk);
            e.eval_side = c;
            return e;
        }
    }
    for entry in unsafe { SCALE_FNS.iter() } {
        for c in 0..2 {
            if entry.key[c] == key {
                e.scaling_function[c] = Some(entry.func);
                return e;
            }
        }
    }
    for &c in [WHITE, BLACK].iter() {
        if is_kbpsks(pos, c) {
            e.scaling_function[c.0 as usize] = Some(scale_kbpsk);
        } else if is_kqkrps(pos, c) {
            e.scaling_function[c.0 as usize] = Some(scale_kqkrps);
        }
    }
    if npm_w + npm_b == Value::ZERO && pos.pieces_p(PAWN) != 0 {
        if pos.count(BLACK, PAWN) == 0 {
            debug_assert!(pos.count(WHITE, PAWN) >= 2);
            e.scaling_function[WHITE.0 as usize] = Some(scale_kpsk);
        } else if pos.count(WHITE, PAWN) == 0 {
            debug_assert!(pos.count(BLACK, PAWN) >= 2);
            e.scaling_function[BLACK.0 as usize] = Some(scale_kpsk);
        } else if pos.count(WHITE, PAWN) == 1 && pos.count(BLACK, PAWN) == 1 {
            e.scaling_function[WHITE.0 as usize] = Some(scale_kpkp);
            e.scaling_function[BLACK.0 as usize] = Some(scale_kpkp);
        }
    }
    if pos.count(WHITE, PAWN) == 0 && npm_w - npm_b <= BishopValueMg {
        e.factor[WHITE.0 as usize] = if npm_w < RookValueMg {
            ScaleFactor::DRAW.0 as u8
        } else if npm_b <= BishopValueMg {
            4
        } else {
            14
        };
    }
    if pos.count(BLACK, PAWN) == 0 && npm_b - npm_w <= BishopValueMg {
        e.factor[BLACK.0 as usize] = if npm_b < RookValueMg {
            ScaleFactor::DRAW.0 as u8
        } else if npm_w <= BishopValueMg {
            4
        } else {
            14
        };
    }
    if pos.count(WHITE, PAWN) == 1 && npm_w - npm_b <= BishopValueMg {
        e.factor[WHITE.0 as usize] = ScaleFactor::ONEPAWN.0 as u8;
    }
    if pos.count(BLACK, PAWN) == 1 && npm_b - npm_w <= BishopValueMg {
        e.factor[BLACK.0 as usize] = ScaleFactor::ONEPAWN.0 as u8;
    }
    let pc = [
        [
            (pos.count(WHITE, BISHOP) > 1) as i32,
            pos.count(WHITE, PAWN),
            pos.count(WHITE, KNIGHT),
            pos.count(WHITE, BISHOP),
            pos.count(WHITE, ROOK),
            pos.count(WHITE, QUEEN),
        ],
        [
            (pos.count(BLACK, BISHOP) > 1) as i32,
            pos.count(BLACK, PAWN),
            pos.count(BLACK, KNIGHT),
            pos.count(BLACK, BISHOP),
            pos.count(BLACK, ROOK),
            pos.count(BLACK, QUEEN),
        ],
    ];
    e.value = ((imbalance(&pc, WHITE) - imbalance(&pc, BLACK)) / 16) as i16;
    e
}
