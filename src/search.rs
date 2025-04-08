use bitboard::*;
use evaluate;
use evaluate::evaluate;
use movegen::*;
use movepick::*;
use position::*;
use std;
use std::io::stdout;
use std::io::Write;
use std::time::Instant;
use tb;
use threads;
use timeman;
use tt;
use types::*;
use uci;
use ucioption;
use nnue;
pub const CM_THRESHOLD: i32 = 0;
pub struct Stack {
    pv: Vec<Move>,
    pub cont_history: &'static PieceToHistory,
    ply: i32,
    pub current_move: Move,
    excluded_move: Move,
    pub killers: [Move; 2],
    static_eval: Value,
    stat_score: i32,
    move_count: i32,
}
#[derive(Clone, Eq)]
pub struct RootMove {
    pub score: Value,
    pub previous_score: Value,
    pub tb_score: Value,
    pub tb_rank: i32,
    pub sel_depth: i32,
    pub pv: Vec<Move>,
}
impl RootMove {
    pub fn new(m: Move) -> RootMove {
        RootMove {
            score: -Value::INFINITE,
            previous_score: -Value::INFINITE,
            tb_score: Value::ZERO,
            tb_rank: 0,
            sel_depth: 0,
            pv: vec![m],
        }
    }
}
impl Ord for RootMove {
    fn cmp(&self, other: &RootMove) -> std::cmp::Ordering {
        match self.tb_rank.cmp(&other.tb_rank) {
            std::cmp::Ordering::Equal => match self.score.cmp(&other.score) {
                std::cmp::Ordering::Equal => self.previous_score.cmp(&other.previous_score),
                ord => ord,
            },
            ord => ord,
        }
    }
}
impl PartialOrd for RootMove {
    fn partial_cmp(&self, other: &RootMove) -> Option<std::cmp::Ordering> {
        Some(other.cmp(self))
    }
}
impl PartialEq for RootMove {
    fn eq(&self, other: &RootMove) -> bool {
        self.score == other.score && self.previous_score == other.previous_score
    }
}
pub type RootMoves = Vec<RootMove>;
#[derive(Clone)]
pub struct LimitsType {
    pub time: [i64; 2],
    pub inc: [i64; 2],
    pub movestogo: i32,
    pub depth: u32,
    pub movetime: i64,
    pub mate: u32,
    pub perft: u32,
    pub infinite: bool,
    pub nodes: u64,
    pub start_time: Option<Instant>,
}
impl LimitsType {
    pub fn new() -> LimitsType {
        LimitsType {
            time: [0; 2],
            inc: [0; 2],
            movestogo: 0,
            depth: 0,
            movetime: 0,
            mate: 0,
            perft: 0,
            infinite: false,
            nodes: 0,
            start_time: Some(Instant::now()),
        }
    }
    pub fn use_time_management(&self) -> bool {
        self.mate == 0
            && self.movetime == 0
            && self.depth == 0
            && self.nodes == 0
            && self.perft == 0
            && !self.infinite
    }
}
pub static mut LIMITS: LimitsType = LimitsType {
    time: [0; 2],
    inc: [0; 2],
    movestogo: 0,
    depth: 0,
    movetime: 0,
    mate: 0,
    perft: 0,
    infinite: false,
    nodes: 0,
    start_time: None,
};
pub fn limits() -> &'static mut LimitsType {
    unsafe { &mut LIMITS }
}
#[derive(Clone, Copy, PartialEq, Eq)]
struct NonPv;
struct Pv;
trait NodeType {
    const NT: usize;
}
impl NodeType for NonPv {
    const NT: usize = 0;
}
impl NodeType for Pv {
    const NT: usize = 1;
}
const SKIP_SIZE: [i32; 20] = [1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4];
const SKIP_PHASE: [i32; 20] = [0, 1, 0, 1, 2, 3, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 5, 6, 7];
fn futility_margin(d: Depth) -> Value {
    Value(150 * d / ONE_PLY)
}
const RAZOR_MARGIN1: i32 = 590;
const RAZOR_MARGIN2: i32 = 604;
static mut FUTILITY_MOVE_COUNTS: [[i32; 16]; 2] = [[0; 16]; 2];
static mut REDUCTIONS: [[[[i32; 64]; 64]; 2]; 2] = [[[[0; 64]; 64]; 2]; 2];
#[inline(always)]
fn reduction<PvNode: NodeType>(i: bool, d: Depth, mn: i32) -> Depth {
    unsafe {
        let d_index = (d / ONE_PLY).min(63) as usize;
        let mn_index = mn.min(63) as usize;
        REDUCTIONS[PvNode::NT][i as usize][d_index][mn_index] * ONE_PLY
    }
}

fn nnue_evaluate(pos: &Position) -> Value {
    match nnue::eval_nnue(&pos.fen()) {
        Ok(score) => Value((score * 1.0) as i32),
        Err(_) => evaluate(pos),
    }
}
fn get_nnue_threshold(max_depth: Depth) -> Depth {
    let seldepth = max_depth / ONE_PLY; 
    let threshold_plies = if seldepth < 18 {
        18
    } else if seldepth <= 24 { 
        18 - ((seldepth - 18) * (18 - 10)) / (24 - 18)
    } else if seldepth <= 30 { 
        10 - ((seldepth - 24) * (10 - 6)) / (26 - 24)
    } else if seldepth <= 127 { 
        5 - ((seldepth - 26) * (5 - 2)) / (127 - 26)
    } else {
        2
    };
    threshold_plies * ONE_PLY
}
fn dynamic_evaluate(pos: &Position, depth: Depth, max_depth: Depth) -> Value {
    let adaptive_threshold = ((max_depth.0 as f64 * 0.3).round() as i32) * ONE_PLY;
    let nnue_threshold = get_nnue_threshold(max_depth);
    let activation_threshold = if adaptive_threshold > nnue_threshold {
        adaptive_threshold
    } else {
        nnue_threshold
    };

    if depth.0 >= activation_threshold.0 {
        nnue_evaluate(pos)
    } else {
        evaluate(pos)
    }
}
fn futility_move_counts(i: bool, d: Depth) -> i32 {
    unsafe { FUTILITY_MOVE_COUNTS[i as usize][(d / ONE_PLY) as usize] }
}
fn stat_bonus(depth: Depth) -> i32 {
    let d = depth / ONE_PLY;
    if d > 17 {
        0
    } else {
        d * d + 2 * d - 2
    }
}
fn perft<Root: Bool>(pos: &mut Position, depth: Depth) -> u64 {
    let mut nodes = 0u64;
    let leaf = depth == 2 * ONE_PLY;
    for m in MoveList::new::<Legal>(pos) {
        let cnt;
        if Root::BOOL && depth <= ONE_PLY {
            cnt = 1;
            nodes += 1;
        } else {
            let checks = pos.gives_check(m);
            pos.do_move(m, checks);
            cnt = if leaf {
                MoveList::new::<Legal>(pos).len() as u64
            } else {
                perft::<False>(pos, depth - ONE_PLY)
            };
            nodes += cnt;
            pos.undo_move(m);
        }
        if Root::BOOL {
            println!("{}: {}", uci::move_str(m, pos.is_chess960()), cnt);
            stdout().flush().unwrap();
        }
    }
    nodes
}
pub fn init() {
    unsafe {
        for imp in 0..2 {
            for d in 1..64 {
                for mc in 1..64 {
                    let r = (d as f64).ln() * (mc as f64).ln() / 1.95;
                    REDUCTIONS[NonPv::NT][imp][d][mc] = r.round() as i32;
                    REDUCTIONS[Pv::NT][imp][d][mc] =
                        std::cmp::max(REDUCTIONS[NonPv::NT][imp][d][mc] - 1, 0);
                    if imp == 0 && REDUCTIONS[NonPv::NT][imp][d][mc] >= 2 {
                        REDUCTIONS[NonPv::NT][imp][d][mc] += 1;
                    }
                }
            }
        }
        for d in 0..16 {
            FUTILITY_MOVE_COUNTS[0][d] = (2.4 + 0.74 * (d as f64).powf(1.78)) as i32;
            FUTILITY_MOVE_COUNTS[1][d] = (5.0 + 1.00 * (d as f64).powf(2.00)) as i32;
        }
    }
}
pub fn clear() {
    threads::wait_for_all();
    tt::clear();
    threads::clear_search();
    threads::wait_for_all();
}
pub fn mainthread_search(pos: &mut Position, th: &threads::ThreadCtrl) {
    if limits().perft != 0 {
        let nodes = perft::<True>(pos, (limits().perft as i32) * ONE_PLY);
        println!("\nNodes searched: {}", nodes);
        return;
    }
    let us = pos.side_to_move();
    timeman::init(limits(), us, pos.game_ply());
    tt::new_search();
    if pos.root_moves.is_empty() {
        pos.root_moves.push(RootMove::new(Move::NONE));
        println!(
            "info depth 0 score {}",
            uci::value(if pos.checkers() != 0 {
                -Value::MATE
            } else {
                Value::DRAW
            })
        );
        stdout().flush().unwrap();
    } else {
        threads::wake_up_slaves();
        thread_search(pos, th);
    }
    threads::set_stop_on_ponderhit(true);
    while !threads::stop() && (threads::ponder() || limits().infinite) {}
    threads::set_stop(true);
    threads::wait_for_slaves();
    if ucioption::get_i32("MultiPV") == 1
        && limits().depth == 0
        && pos.root_moves[0].pv[0] != Move::NONE
    {
        let common = th.common.lock().unwrap();
        let result = &mut common.result.lock().unwrap();
        if result.score > pos.root_moves[0].score
            && (result.depth >= pos.completed_depth || result.score >= Value::MATE_IN_MAX_PLY)
        {
            pos.root_moves[0].score = result.score;
            pos.root_moves[0].pv = result.pv.clone();
        }
    }
    pos.previous_score = pos.root_moves[0].score;
    print!(
        "bestmove {}",
        uci::move_str(pos.root_moves[0].pv[0], pos.is_chess960())
    );
    if pos.root_moves[0].pv.len() > 1 || extract_ponder_from_tt(pos) {
        print!(
            " ponder {}",
            uci::move_str(pos.root_moves[0].pv[1], pos.is_chess960())
        );
    }
    print!("\n");
    stdout().flush().unwrap();
}
pub fn thread_search(pos: &mut Position, _th: &threads::ThreadCtrl) {
    let mut stack: Vec<Stack> = Vec::with_capacity((MAX_PLY + 7) as usize);
    let mut last_best_move = Move::NONE;
    let mut last_best_move_depth = Depth::ZERO;
    let mut time_reduction = 1.0f64;
    for _ in 0..(MAX_PLY + 7) as usize {
        stack.push(Stack {
            pv: Vec::new(),
            cont_history: pos.cont_history.get(NO_PIECE, Square(0)),
            ply: 0,
            current_move: Move::NONE,
            excluded_move: Move::NONE,
            killers: [Move::NONE; 2],
            static_eval: Value::ZERO,
            stat_score: 0,
            move_count: 0,
        });
    }
    pos.calls_cnt = 0;
    pos.nmp_ply = 0;
    pos.nmp_odd = 0;
    let mut alpha = -Value::INFINITE;
    let mut delta = -Value::INFINITE;
    let mut best_value = -Value::INFINITE;
    let mut beta = Value::INFINITE;
    if pos.is_main {
        pos.failed_low = false;
        pos.best_move_changes = 0.0;
    }
    let us = pos.side_to_move();
    let mut multi_pv = ucioption::get_i32("MultiPV") as usize;
    multi_pv = std::cmp::min(multi_pv, pos.root_moves.len());
    let mut base_ct = ucioption::get_i32("Contempt") * PawnValueEg.0 / 100;
    if limits().infinite || ucioption::get_bool("UCI_AnalyseMode") {
        base_ct = match ucioption::get_string("Analysis Contempt").as_ref() {
            "off" => 0,
            "white" => {
                if us == WHITE {
                    base_ct
                } else {
                    -base_ct
                }
            }
            "black" => {
                if us == BLACK {
                    base_ct
                } else {
                    -base_ct
                }
            }
            _ => base_ct,
        }
    }
    unsafe {
        let contempt = Score::make(base_ct, base_ct / 2);
        evaluate::CONTEMPT = if us == WHITE { contempt } else { -contempt };
    }
    let mut root_depth = Depth::ZERO;
    while !threads::stop() {
        root_depth += ONE_PLY;
        if root_depth >= Depth::MAX
            || (limits().depth != 0 && pos.is_main && root_depth / ONE_PLY > limits().depth as i32)
        {
            break;
        }
        if !pos.is_main {
            let i = ((pos.thread_idx - 1) & 20) as usize;
            if ((root_depth / ONE_PLY + pos.game_ply() + SKIP_PHASE[i]) / SKIP_SIZE[i]) % 2 != 0 {
                continue;
            }
        }
        if pos.is_main {
            pos.best_move_changes *= 0.517;
            pos.failed_low = false;
        }
        for ref mut rm in pos.root_moves.iter_mut() {
            rm.previous_score = rm.score;
        }
        let mut pv_first = 0;
        pos.pv_last = 0;
        pos.pv_idx = 0;
        while pos.pv_idx < multi_pv && !threads::stop() {
            if pos.pv_idx == pos.pv_last {
                pv_first = pos.pv_last;
                pos.pv_last += 1;
                while pos.pv_last < pos.root_moves.len() {
                    if pos.root_moves[pos.pv_last].tb_rank != pos.root_moves[pv_first].tb_rank {
                        break;
                    }
                    pos.pv_last += 1;
                }
            }
            pos.sel_depth = 0;
            if pos.root_moves[pos.pv_idx].tb_rank.abs() > 1000 {
                best_value = pos.root_moves[pos.pv_idx].tb_score;
                pos.root_moves[pos.pv_idx].score = best_value;
                if pos.is_main
                    && (threads::stop() || pos.pv_idx + 1 == multi_pv || timeman::elapsed() > 3000)
                {
                    print_pv(pos, root_depth, -Value::INFINITE, Value::INFINITE);
                }
                pos.pv_idx += 1;
                continue;
            }
            if root_depth >= 5 * ONE_PLY {
                delta = Value(18);
                alpha = std::cmp::max(
                    pos.root_moves[pos.pv_idx].previous_score - delta,
                    -Value::INFINITE,
                );
                beta = std::cmp::min(
                    pos.root_moves[pos.pv_idx].previous_score + delta,
                    Value::INFINITE,
                );
                let ct = base_ct
                    + (if best_value > Value(500) {
                        50
                    } else if best_value < Value(-500) {
                        -50
                    } else {
                        best_value.0 / 10
                    });
                let ct = Score::make(ct, ct / 2);
                unsafe { evaluate::CONTEMPT = if us == WHITE { ct } else { -ct } }
            }
            loop {
                best_value = search::<Pv>(pos, &mut stack, alpha, beta, root_depth, false, false);
                update_counters(pos);
                pos.root_moves[pos.pv_idx..].sort();
                if threads::stop() {
                    break;
                }
                if pos.is_main
                    && multi_pv == 1
                    && (best_value <= alpha || best_value >= beta)
                    && timeman::elapsed() > 3000
                {
                    print_pv(pos, root_depth, alpha, beta);
                }
                if best_value <= alpha {
                    beta = (alpha + beta) / 2;
                    alpha = std::cmp::max(best_value - delta, -Value::INFINITE);
                    if pos.is_main {
                        pos.failed_low = true;
                        threads::set_stop_on_ponderhit(false);
                    }
                } else if best_value >= beta {
                    beta = std::cmp::min(best_value + delta, Value::INFINITE);
                } else {
                    break;
                }
                delta += delta / 4 + 5;
                debug_assert!(alpha >= -Value::INFINITE && beta <= Value::INFINITE);
            }
            pos.root_moves[pv_first..pos.pv_idx + 1].sort();
            if pos.is_main
                && (threads::stop() || pos.pv_idx + 1 == multi_pv || timeman::elapsed() > 3000)
            {
                print_pv(pos, root_depth, alpha, beta);
            }
            pos.pv_idx += 1;
        }
        if !threads::stop() {
            pos.completed_depth = root_depth;
        }
        if pos.root_moves[0].pv[0] != last_best_move {
            last_best_move = pos.root_moves[0].pv[0];
            last_best_move_depth = root_depth;
        }
        if limits().mate != 0
            && best_value >= Value::MATE_IN_MAX_PLY
            && (Value::MATE - best_value).0 <= 2 * (limits().mate as i32)
        {
            threads::set_stop(true);
        }
        if !pos.is_main {
            continue;
        }
        if limits().use_time_management() {
            if !threads::stop() && !threads::stop_on_ponderhit() {
                let f = [pos.failed_low as i32, (best_value - pos.previous_score).0];
                let improving_factor =
                    std::cmp::max(246, std::cmp::min(832, 306 + 119 * f[0] - 6 * f[1]));
                let mut unstable_pv_factor = 1. + pos.best_move_changes;
                time_reduction = 1.;
                for i in 3..6 {
                    if last_best_move_depth * i < pos.completed_depth {
                        time_reduction *= 1.25;
                    }
                    unstable_pv_factor *= pos.previous_time_reduction.powf(0.528) / time_reduction;
                    if pos.root_moves.len() == 1
                        || (timeman::elapsed() as f64)
                            > (timeman::optimum() as f64)
                                * unstable_pv_factor
                                * (improving_factor as f64)
                                / 581.0
                    {
                        if threads::ponder() {
                            threads::set_stop_on_ponderhit(true);
                        } else {
                            threads::set_stop(true);
                        }
                    }
                }
            }
        }
    }
    if !pos.is_main {
        return;
    }
    pos.previous_time_reduction = time_reduction;
}
fn search<NT: NodeType>(
    pos: &mut Position,
    ss: &mut [Stack],
    mut alpha: Value,
    mut beta: Value,
    depth: Depth,
    cut_node: bool,
    skip_early_pruning: bool,
) -> Value {
    let pv_node = NT::NT == Pv::NT;
    let root_node = pv_node && ss[5].ply == 0;
    debug_assert!(-Value::INFINITE <= alpha && alpha < beta && beta <= Value::INFINITE);
    debug_assert!(pv_node || alpha == beta - 1);
    debug_assert!(Depth::ZERO < depth && depth < Depth::MAX);
    debug_assert!(!(pv_node && cut_node));
    debug_assert!(depth / ONE_PLY * ONE_PLY == depth);
    let mut captures_searched: [Move; 32] = [Move::NONE; 32];
    let mut quiets_searched: [Move; 64] = [Move::NONE; 64];
    let in_check = pos.checkers() != 0;
    let mut move_count = 0;
    let mut capture_count = 0;
    let mut quiet_count = 0;
    ss[5].move_count = 0;
    let mut best_value = -Value::INFINITE;
    let mut max_value = Value::INFINITE;
    pos.calls_cnt -= 1;
    if pos.calls_cnt < 0 {
        pos.calls_cnt = 4095;
        update_counters(pos);
        check_time();
    }
    if pv_node && pos.sel_depth < ss[5].ply {
        pos.sel_depth = ss[5].ply;
    }
    if !root_node {
        if threads::stop() || pos.is_draw(ss[5].ply) || ss[5].ply >= MAX_PLY {
            return if ss[5].ply >= MAX_PLY && !pos.checkers() {
                dynamic_evaluate(pos, depth, depth)
            } else {
                Value::DRAW
            };
        }
        alpha = std::cmp::max(mated_in(ss[5].ply), alpha);
        beta = std::cmp::min(mate_in(ss[5].ply + 1), beta);
        if alpha >= beta {
            return alpha;
        }
    }
    debug_assert!(0 <= ss[5].ply && ss[5].ply < MAX_PLY);
    ss[6].ply = ss[5].ply + 1;
    ss[5].current_move = Move::NONE;
    ss[6].excluded_move = Move::NONE;
    let mut best_move = Move::NONE;
    ss[5].cont_history = pos.cont_history.get(NO_PIECE, Square(0));
    ss[7].killers = [Move::NONE; 2];
    let prev_sq = ss[4].current_move.to();
    ss[7].stat_score = 0;
    let excluded_move = ss[5].excluded_move;
    let pos_key = pos.key() ^ Key((excluded_move.0 << 16) as u64);
    let (mut tte, mut tt_hit) = tt::probe(pos_key);
    let tt_value = if tt_hit {
        value_from_tt(tte.value(), ss[5].ply)
    } else {
        Value::NONE
    };
    let mut tt_move = if root_node {
        pos.root_moves[pos.pv_idx].pv[0]
    } else if tt_hit {
        tte.mov()
    } else {
        Move::NONE
    };
    if !pv_node
        && tt_hit
        && tte.depth() >= depth
        && tt_value != Value::NONE
        && (if tt_value >= beta {
            tte.bound() & Bound::LOWER != 0
        } else {
            tte.bound() & Bound::UPPER != 0
        })
    {
        if tt_move != Move::NONE {
            if tt_value >= beta {
                if !pos.capture_or_promotion(tt_move) {
                    update_stats(pos, ss, tt_move, &quiets_searched, 0, stat_bonus(depth));
                }
                if ss[4].move_count == 1 && pos.captured_piece() == NO_PIECE {
                    update_continuation_histories(
                        ss,
                        pos.piece_on(prev_sq),
                        prev_sq,
                        -stat_bonus(depth + ONE_PLY),
                    );
                }
            } else if !pos.capture_or_promotion(tt_move) {
                let penalty = -stat_bonus(depth);
                pos.main_history
                    .update(pos.side_to_move(), tt_move, penalty);
                update_continuation_histories(
                    &ss[1..],
                    pos.moved_piece(tt_move),
                    tt_move.to(),
                    penalty,
                );
            }
        }
        return tt_value;
    }
    if !root_node && tb::cardinality() != 0 {
        let pieces_cnt = popcount(pos.pieces());
        if pieces_cnt <= tb::cardinality()
            && (pieces_cnt < tb::cardinality() || depth >= tb::probe_depth())
            && pos.rule50_count() == 0
            && !pos.has_castling_right(ANY_CASTLING)
        {
            let mut found = 1;
            let wdl = tb::probe_wdl(pos, &mut found);
            if found != 0 {
                pos.tb_hits += 1;
                let draw_score = if tb::use_rule_50() { 1 } else { 0 };
                let value = if wdl < -draw_score {
                    -Value::MATE + MAX_MATE_PLY + 1 + ss[5].ply
                } else if wdl > draw_score {
                    Value::MATE - MAX_MATE_PLY - 1 - ss[5].ply
                } else {
                    Value::DRAW + 2 * wdl * draw_score
                };
                let b = if wdl < -draw_score {
                    Bound::UPPER
                } else if wdl > draw_score {
                    Bound::LOWER
                } else {
                    Bound::EXACT
                };
                if b == Bound::EXACT
                    || (if b == Bound::LOWER {
                        value >= beta
                    } else {
                        value <= alpha
                    })
                {
                    tte.save(
                        pos_key,
                        value_to_tt(value, ss[5].ply),
                        b,
                        std::cmp::min(Depth::MAX - ONE_PLY, depth + 6 * ONE_PLY),
                        Move::NONE,
                        Value::NONE,
                        tt::generation(),
                    );
                    return value;
                }
                if pieces_cnt <= tb::cardinality_dtm() {
                    let mut mate = tb::probe_dtm(pos, wdl, &mut found);
                    if found != 0 {
                        mate += if wdl > 0 { -ss[5].ply } else { ss[5].ply };
                        tte.save(
                            pos_key,
                            value_to_tt(mate, ss[5].ply),
                            Bound::EXACT,
                            std::cmp::min(Depth::MAX - ONE_PLY, depth + 6 * ONE_PLY),
                            Move::NONE,
                            Value::NONE,
                            tt::generation(),
                        );
                        return mate;
                    }
                }
                if pv_node {
                    if b == Bound::LOWER {
                        best_value = value;
                        if best_value > alpha {
                            alpha = best_value;
                        }
                    } else {
                        max_value = value;
                    }
                }
            }
        }
    }
    loop {
        let eval: Value;
        if in_check {
            ss[5].static_eval = Value::NONE;
            break;
		} else if tt_hit {
			let mut tmp = tte.eval();
			if tmp == Value::NONE {
				tmp = dynamic_evaluate(pos, depth, depth);
			}
			ss[5].static_eval = tmp;
			if tt_value != Value::NONE
				&& tte.bound() & (if tt_value > tmp { Bound::LOWER } else { Bound::UPPER }) != 0
			{
				tmp = tt_value;
			}
			eval = tmp;
		} else {
			eval = dynamic_evaluate(pos, depth, depth);
			
			ss[5].static_eval = eval;
			tte.save(
				pos_key,
				Value::NONE,
				Bound::NONE,
				Depth::NONE,
				Move::NONE,
				eval,
				tt::generation(),
			);
		}
        if skip_early_pruning || pos.non_pawn_material_c(pos.side_to_move()) == Value::ZERO {
            break;
        }
        if !pv_node && depth <= ONE_PLY {
            if eval + RAZOR_MARGIN1 <= alpha {
                return qsearch::<NonPv, False>(pos, ss, alpha, alpha + 1, Depth::ZERO);
            }
        } else if !pv_node && depth <= 2 * ONE_PLY && eval + RAZOR_MARGIN2 <= alpha {
            let ralpha = alpha - RAZOR_MARGIN2;
            let v = qsearch::<NonPv, False>(pos, ss, ralpha, ralpha + 1, Depth::ZERO);
            if v <= ralpha {
                return v;
            }
        }
        if !root_node
            && depth < 7 * ONE_PLY
            && eval - futility_margin(depth) >= beta
            && eval < Value::KNOWN_WIN
        {
            return eval;
        }
        if !pv_node
            && eval >= beta
            && ss[5].static_eval >= beta - 36 * depth / ONE_PLY + 225
            && (ss[5].ply >= pos.nmp_ply || ss[5].ply & 1 != pos.nmp_odd)
        {
            debug_assert!(eval - beta >= Value::ZERO);
            let r = ((823 + 67 * depth / ONE_PLY) / 256
                + std::cmp::min((eval - beta) / PawnValueMg, 3))
                * ONE_PLY;
            ss[5].current_move = Move::NULL;
            ss[5].cont_history = pos.cont_history.get(NO_PIECE, Square(0));
            pos.do_null_move();
            let mut null_value = if depth - r < ONE_PLY {
                -qsearch::<NonPv, False>(pos, &mut ss[1..], -beta, -beta + 1, Depth::ZERO)
            } else {
                -search::<NonPv>(
                    pos,
                    &mut ss[1..],
                    -beta,
                    -beta + 1,
                    depth - r,
                    !cut_node,
                    true,
                )
            };
            pos.undo_null_move();
            if null_value >= beta {
                if null_value >= Value::MATE_IN_MAX_PLY {
                    null_value = beta;
                }
                if (depth < 12 * ONE_PLY || pos.nmp_ply != 0) && beta.abs() < Value::KNOWN_WIN {
                    return null_value;
                }
                pos.nmp_ply = ss[5].ply + 3 * (depth - r) / (4 * ONE_PLY);
                pos.nmp_odd = ss[5].ply & 1;
                let v = if depth - r < ONE_PLY {
                    qsearch::<NonPv, False>(pos, ss, beta - 1, beta, Depth::ZERO)
                } else {
                    search::<NonPv>(pos, ss, beta - 1, beta, depth - r, false, true)
                };
                pos.nmp_odd = 0;
                pos.nmp_ply = 0;
                if v >= beta {
                    return null_value;
                }
            }
        }
        if !pv_node && depth >= 5 * ONE_PLY && beta.abs() < Value::MATE_IN_MAX_PLY {
            let rbeta = std::cmp::min(beta + 200, Value::INFINITE);
            debug_assert!(ss[4].current_move.is_ok());
            let mut mp = MovePickerPC::new(pos, tt_move, rbeta - ss[5].static_eval);
            let mut prob_cut_count = depth / ONE_PLY - 3;
            loop {
                let m = mp.next_move(pos);
                if m == Move::NONE {
                    break;
                }
                if pos.legal(m) {
                    ss[5].current_move = m;
                    ss[5].cont_history = pos.cont_history.get(pos.moved_piece(m), m.to());
                    debug_assert!(depth >= 5 * ONE_PLY);
                    let gives_check = pos.gives_check(m);
                    pos.do_move(m, gives_check);
                    let mut value = Value::ZERO;
                    if depth != 5 * ONE_PLY {
                        value = -search::<NonPv>(
                            pos,
                            &mut ss[1..],
                            -rbeta,
                            -rbeta + 1,
                            ONE_PLY,
                            !cut_node,
                            true,
                        );
                    }
                    if depth == 5 * ONE_PLY || value >= rbeta {
                        value = -search::<NonPv>(
                            pos,
                            &mut ss[1..],
                            -rbeta,
                            -rbeta + 1,
                            depth - 4 * ONE_PLY,
                            !cut_node,
                            false,
                        );
                    }
                    pos.undo_move(m);
                    if value >= rbeta {
                        return value;
                    }
                    prob_cut_count -= 1;
                    if prob_cut_count == 0 {
                        break;
                    }
                }
            }
        }
        if depth >= 6 * ONE_PLY
            && tt_move == Move::NONE
            && (pv_node || ss[5].static_eval + 256 >= beta)
        {
            let d = (3 * depth / (4 * ONE_PLY) - 2) * ONE_PLY;
            search::<NT>(pos, ss, alpha, beta, d, cut_node, true);
            let (tmp_tte, tmp_tt_hit) = tt::probe(pos_key);
            tte = tmp_tte;
            tt_hit = tmp_tt_hit;
            tt_move = if tt_hit { tte.mov() } else { Move::NONE };
        }
        break;
    }
    let cont_hist = (ss[4].cont_history, ss[3].cont_history, ss[1].cont_history);
    let mut mp = MovePicker::new(pos, tt_move, depth, ss);
    let mut value = best_value;
    let improving = ss[5].static_eval >= ss[3].static_eval || ss[3].static_eval == Value::NONE;
    let singular_extension_node = !root_node
        && depth >= 8 * ONE_PLY
        && tt_move != Move::NONE
        && tt_value != Value::NONE
        && excluded_move == Move::NONE
        && tte.bound() & Bound::LOWER != 0
        && tte.depth() >= depth - 3 * ONE_PLY;
    let mut skip_quiets = false;
    let mut tt_capture = false;
    let pv_exact = pv_node && tt_hit && tte.bound() == Bound::EXACT;
    loop {
        let m = mp.next_move(pos, skip_quiets);
        if m == Move::NONE {
            break;
        }
        debug_assert!(m.is_ok());
        if m == excluded_move {
            continue;
        }
        if root_node
            && !pos.root_moves[pos.pv_idx..]
                .iter()
                .any(|ref rm| rm.pv[0] == m)
        {
            continue;
        }
        move_count += 1;
        ss[5].move_count = move_count;
        if root_node && pos.is_main && timeman::elapsed() > 3000 {
            println!(
                "info depth {} currmove {} currmovenumber {}",
                depth / ONE_PLY,
                uci::move_str(m, pos.is_chess960()),
                move_count + pos.pv_idx as i32
            );
            stdout().flush().unwrap();
        }
        if pv_node {
            ss[6].pv.truncate(0);
        }
        let mut extension = Depth::ZERO;
        let capture_or_promotion = pos.capture_or_promotion(m);
        let moved_piece = pos.moved_piece(m);
        let gives_check = if m.move_type() == NORMAL
            && pos.blockers_for_king(!pos.side_to_move()) & pos.pieces_c(pos.side_to_move()) == 0
        {
            pos.check_squares(moved_piece.piece_type()) & m.to() != 0
        } else {
            pos.gives_check(m)
        };
        let move_count_pruning =
            depth < 16 * ONE_PLY && move_count >= futility_move_counts(improving, depth);
        if singular_extension_node && m == tt_move && pos.legal(m) {
            let rbeta = std::cmp::max(tt_value - 2 * depth / ONE_PLY, -Value::MATE);
            let d = (depth / (2 * ONE_PLY)) * ONE_PLY;
            ss[5].excluded_move = m;
            let value = search::<NonPv>(pos, ss, rbeta - 1, rbeta, d, cut_node, true);
            ss[5].excluded_move = Move::NONE;
            if value < rbeta {
                extension = ONE_PLY;
            }
        } else if gives_check && !move_count_pruning && pos.see_ge(m, Value::ZERO) {
            extension = ONE_PLY;
        }
        let new_depth = depth - ONE_PLY + extension;
        if !root_node
            && pos.non_pawn_material_c(pos.side_to_move()) != Value::ZERO
            && best_value > Value::MATED_IN_MAX_PLY
        {
            if !capture_or_promotion
                && !gives_check
                && (!pos.advanced_pawn_push(m) || pos.non_pawn_material() >= Value(5000))
            {
                if move_count_pruning {
                    skip_quiets = true;
                    continue;
                }
                let lmr_depth = std::cmp::max(
                    new_depth - reduction::<NT>(improving, depth, move_count),
                    Depth::ZERO,
                ) / ONE_PLY;
                if lmr_depth < 3
                    && cont_hist.0.get(moved_piece, m.to()) < CM_THRESHOLD
                    && cont_hist.1.get(moved_piece, m.to()) < CM_THRESHOLD
                {
                    continue;
                }
                if lmr_depth < 7 && !in_check && ss[5].static_eval + 256 + 200 * lmr_depth <= alpha
                {
                    continue;
                }
                if lmr_depth < 8 && !pos.see_ge(m, Value(-35 * lmr_depth * lmr_depth)) {
                    continue;
                }
            } else if depth < 7 * ONE_PLY
                && extension == Depth::ZERO
                && !pos.see_ge(m, -PawnValueEg * (depth / ONE_PLY))
            {
                continue;
            }
        }
        if !root_node && !pos.legal(m) {
            move_count -= 1;
            ss[5].move_count = move_count;
            continue;
        }
        if m == tt_move && capture_or_promotion {
            tt_capture = true;
        }
        ss[5].current_move = m;
        ss[5].cont_history = pos.cont_history.get(moved_piece, m.to());
        pos.do_move(m, gives_check);
        let do_full_depth_search;
        if depth >= 3 * ONE_PLY && move_count > 1 && (!capture_or_promotion || move_count_pruning) {
            let mut r = reduction::<NT>(improving, depth, move_count);
            if capture_or_promotion {
                r -= if r != Depth::ZERO {
                    ONE_PLY
                } else {
                    Depth::ZERO
                };
            } else {
                if ss[4].move_count > 15 {
                    r -= ONE_PLY;
                }
                if pv_exact {
                    r -= ONE_PLY;
                }
                if tt_capture {
                    r += ONE_PLY;
                }
                if cut_node {
                    r += 2 * ONE_PLY;
                } else if m.move_type() == NORMAL
                    && !pos.see_ge(Move::make(m.to(), m.from()), Value::ZERO)
                {
                    r -= 2 * ONE_PLY;
                }
                ss[5].stat_score = pos.main_history.get(!pos.side_to_move(), m)
                    + cont_hist.0.get(moved_piece, m.to())
                    + cont_hist.1.get(moved_piece, m.to())
                    + cont_hist.2.get(moved_piece, m.to())
                    - 4000;
                if ss[5].stat_score >= 0 && ss[4].stat_score < 0 {
                    r -= ONE_PLY;
                } else if ss[4].stat_score >= 0 && ss[5].stat_score < 0 {
                    r += ONE_PLY;
                }
                r = std::cmp::max(
                    Depth::ZERO,
                    (r / ONE_PLY - ss[5].stat_score / 20000) * ONE_PLY,
                );
            }
            let d = std::cmp::max(new_depth - r, ONE_PLY);
            value = -search::<NonPv>(pos, &mut ss[1..], -(alpha + 1), -alpha, d, true, false);
            do_full_depth_search = value > alpha && d != new_depth;
        } else {
            do_full_depth_search = !pv_node || move_count > 1;
        }
        if do_full_depth_search {
            value = if new_depth < ONE_PLY {
                if gives_check {
                    -qsearch::<NonPv, True>(pos, &mut ss[1..], -(alpha + 1), -alpha, Depth::ZERO)
                } else {
                    -qsearch::<NonPv, False>(pos, &mut ss[1..], -(alpha + 1), -alpha, Depth::ZERO)
                }
            } else {
                -search::<NonPv>(
                    pos,
                    &mut ss[1..],
                    -(alpha + 1),
                    -alpha,
                    new_depth,
                    !cut_node,
                    false,
                )
            }
        }
        if pv_node && (move_count == 1 || (value > alpha && (root_node || value < beta))) {
            ss[6].pv.truncate(0);
            value = if new_depth < ONE_PLY {
                if gives_check {
                    -qsearch::<Pv, True>(pos, &mut ss[1..], -beta, -alpha, Depth::ZERO)
                } else {
                    -qsearch::<Pv, False>(pos, &mut ss[1..], -beta, -alpha, Depth::ZERO)
                }
            } else {
                -search::<Pv>(pos, &mut ss[1..], -beta, -alpha, new_depth, false, false)
            }
        }
        pos.undo_move(m);
        debug_assert!(value > -Value::INFINITE && value < Value::INFINITE);
        if threads::stop() {
            return Value::ZERO;
        }
        if root_node {
            let rm = pos
                .root_moves
                .iter_mut()
                .find(|ref rm| rm.pv[0] == m)
                .unwrap();
            if move_count == 1 || value > alpha {
                rm.score = value;
                rm.sel_depth = pos.sel_depth;
                rm.pv.truncate(1);
                for &m in ss[6].pv.iter() {
                    rm.pv.push(m);
                }
                if move_count > 1 && pos.is_main {
                    pos.best_move_changes += 1.0;
                }
            } else {
                rm.score = -Value::INFINITE;
            }
        }
        if value > best_value {
            best_value = value;
            if value > alpha {
                best_move = m;
                if pv_node && !root_node {
                    update_pv(ss, m);
                }
                if pv_node && value < beta {
                    alpha = value;
                } else {
                    debug_assert!(value >= beta);
                    break;
                }
            }
        }
        if !capture_or_promotion && m != best_move && quiet_count < 64 {
            quiets_searched[quiet_count] = m;
            quiet_count += 1;
        } else if capture_or_promotion && m != best_move && capture_count < 32 {
            captures_searched[capture_count] = m;
            capture_count += 1;
        }
    }
    if move_count == 0 {
        best_value = if excluded_move != Move::NONE {
            alpha
        } else if in_check {
            mated_in(ss[5].ply)
        } else {
            Value::DRAW
        }
    } else if best_move != Move::NONE {
        if !pos.capture_or_promotion(best_move) {
            update_stats(
                pos,
                ss,
                best_move,
                &quiets_searched,
                quiet_count,
                stat_bonus(depth),
            );
        } else {
            update_capture_stats(
                pos,
                best_move,
                &captures_searched,
                capture_count,
                stat_bonus(depth),
            );
        }
        if ss[4].move_count == 1 && pos.captured_piece() == NO_PIECE {
            update_continuation_histories(
                ss,
                pos.piece_on(prev_sq),
                prev_sq,
                -stat_bonus(depth + ONE_PLY),
            );
        }
    } else if depth >= 3 * ONE_PLY && pos.captured_piece() == NO_PIECE && ss[4].current_move.is_ok()
    {
        update_continuation_histories(ss, pos.piece_on(prev_sq), prev_sq, stat_bonus(depth));
    }
    if pv_node && best_value > max_value {
        best_value = max_value;
    }
    if excluded_move == Move::NONE {
        tte.save(
            pos_key,
            value_to_tt(best_value, ss[5].ply),
            if best_value >= beta {
                Bound::LOWER
            } else if pv_node && best_move != Move::NONE {
                Bound::EXACT
            } else {
                Bound::UPPER
            },
            depth,
            best_move,
            ss[5].static_eval,
            tt::generation(),
        );
    }
    debug_assert!(best_value > -Value::INFINITE && best_value < Value::INFINITE);
    return best_value;
}
fn qsearch<NT: NodeType, InCheck: Bool>(
    pos: &mut Position,
    ss: &mut [Stack],
    mut alpha: Value,
    beta: Value,
    depth: Depth,
) -> Value {
    let in_check = InCheck::BOOL;
    let pv_node = NT::NT == Pv::NT;
    debug_assert!(in_check == (pos.checkers() != 0));
    debug_assert!(alpha >= -Value::INFINITE && alpha < beta && beta <= Value::INFINITE);
    debug_assert!(pv_node || (alpha == beta - 1));
    debug_assert!(depth <= Depth::ZERO);
    debug_assert!(depth / ONE_PLY * ONE_PLY == depth);
    let old_alpha = alpha;
    if pv_node {
        ss[5].pv.truncate(0);
    }
    ss[5].current_move = Move::NONE;
    let mut best_move = Move::NONE;
    ss[6].ply = ss[5].ply + 1;
    let mut move_count = 0;
    if pos.is_draw(ss[5].ply) || ss[5].ply >= MAX_PLY {
        return if ss[5].ply >= MAX_PLY && !in_check {
            evaluate(pos)
        } else {
            Value::DRAW
        };
    }
    debug_assert!(0 <= ss[5].ply && ss[5].ply < MAX_PLY);
    let tt_depth = if in_check || depth >= Depth::QS_CHECKS {
        Depth::QS_CHECKS
    } else {
        Depth::QS_NO_CHECKS
    };
    let pos_key = pos.key();
    let (tte, tt_hit) = tt::probe(pos_key);
    let tt_move = if tt_hit { tte.mov() } else { Move::NONE };
    let tt_value = if tt_hit {
        value_from_tt(tte.value(), ss[5].ply)
    } else {
        Value::NONE
    };
    if !pv_node
        && tt_hit
        && tte.depth() >= tt_depth
        && tt_value != Value::NONE
        && (if tt_value >= beta {
            tte.bound() & Bound::LOWER != 0
        } else {
            tte.bound() & Bound::UPPER != 0
        })
    {
        return tt_value;
    }
    let mut best_value;
    let futility_base;
	let eval = if in_check {
		ss[5].static_eval = Value::NONE;
		best_value = -Value::INFINITE;
		dynamic_evaluate(pos, depth, depth)
	} else if tt_hit {
		let mut tmp = tte.eval();
		if tmp == Value::NONE {
			tmp = dynamic_evaluate(pos, depth, depth);
		}
		ss[5].static_eval = tmp;
		best_value = tmp;
		tmp  
		} else {
			let val = dynamic_evaluate(pos, depth, depth);
			ss[5].static_eval = val;
			best_value = val;
			val
		};


        if best_value >= beta {
            if !tt_hit {
                tte.save(
                    pos.key(),
                    value_to_tt(best_value, ss[5].ply),
                    Bound::LOWER,
                    Depth::NONE,
                    Move::NONE,
                    ss[5].static_eval,
                    tt::generation(),
                );
            }
            return best_value;
        }
        if pv_node && best_value > alpha {
            alpha = best_value;
        }
        futility_base = best_value + 128;
    let mut mp = MovePickerQ::new(pos, tt_move, depth, ss[4].current_move.to());
    loop {
        let m = mp.next_move(pos);
        if m == Move::NONE {
            break;
        }
        debug_assert!(m.is_ok());
        let gives_check = if m.move_type() == NORMAL
            && pos.blockers_for_king(!pos.side_to_move()) & pos.pieces_c(pos.side_to_move()) == 0
        {
            pos.check_squares(pos.moved_piece(m).piece_type()) & m.to() != 0
        } else {
            pos.gives_check(m)
        };
        move_count += 1;
        if !in_check
            && !gives_check
            && futility_base > -Value::KNOWN_WIN
            && !pos.advanced_pawn_push(m)
        {
            debug_assert!(m.move_type() != ENPASSANT);
            let futility_value = futility_base + piece_value(EG, pos.piece_on(m.to()));
            if futility_value <= alpha {
                best_value = std::cmp::max(best_value, futility_value);
                continue;
            }
            if futility_base <= alpha && !pos.see_ge(m, Value::ZERO + 1) {
                best_value = std::cmp::max(best_value, futility_base);
                continue;
            }
        }
        let evasion_prunable = in_check
            && (depth != Depth::ZERO || move_count > 2)
            && best_value > Value::MATED_IN_MAX_PLY
            && !pos.capture(m);
        if (!in_check || evasion_prunable) && !pos.see_ge(m, Value::ZERO) {
            continue;
        }
        if !pos.legal(m) {
            move_count -= 1;
            continue;
        }
        ss[5].current_move = m;
        pos.do_move(m, gives_check);
        let value = if gives_check {
            -qsearch::<NT, True>(pos, &mut ss[1..], -beta, -alpha, depth - ONE_PLY)
        } else {
            -qsearch::<NT, False>(pos, &mut ss[1..], -beta, -alpha, depth - ONE_PLY)
        };
        pos.undo_move(m);
        debug_assert!(value > -Value::INFINITE && value < Value::INFINITE);
        if value > best_value {
            best_value = value;
            if value > alpha {
                if pv_node {
                    update_pv(ss, m);
                }
                if pv_node && value < beta {
                    alpha = value;
                    best_move = m;
                } else {
                    tte.save(
                        pos_key,
                        value_to_tt(value, ss[5].ply),
                        Bound::LOWER,
                        tt_depth,
                        m,
                        ss[5].static_eval,
                        tt::generation(),
                    );
                    return value;
                }
            }
        }
    }
    if in_check && best_value == -Value::INFINITE {
        return mated_in(ss[5].ply);
    }
    tte.save(
        pos_key,
        value_to_tt(best_value, ss[5].ply),
        if pv_node && best_value > old_alpha {
            Bound::EXACT
        } else {
            Bound::UPPER
        },
        tt_depth,
        best_move,
        ss[5].static_eval,
        tt::generation(),
    );
    debug_assert!(best_value > -Value::INFINITE && best_value < Value::INFINITE);
    return best_value;
}
fn value_to_tt(v: Value, ply: i32) -> Value {
    debug_assert!(v != Value::NONE);
    if v >= Value::MATE_IN_MAX_PLY {
        v + ply
    } else if v <= Value::MATED_IN_MAX_PLY {
        v - ply
    } else {
        v
    }
}
fn value_from_tt(v: Value, ply: i32) -> Value {
    if v == Value::NONE {
        Value::NONE
    } else if v >= Value::MATE_IN_MAX_PLY {
        v - ply
    } else if v <= Value::MATED_IN_MAX_PLY {
        v + ply
    } else {
        v
    }
}
fn update_pv(ss: &mut [Stack], m: Move) {
    ss[5].pv.clear();
    ss[5].pv.push(m);
    let pv6 = ss[6].pv.clone(); 
    ss[5].pv.extend_from_slice(&pv6);
}
fn update_continuation_histories(ss: &[Stack], pc: Piece, to: Square, bonus: i32) {
    if ss[3].current_move.is_ok() {
        ss[3].cont_history.update(pc, to, bonus);
    }
    if ss[2].current_move.is_ok() {
        ss[2].cont_history.update(pc, to, bonus);
    }
    if ss[0].current_move.is_ok() {
        ss[0].cont_history.update(pc, to, bonus);
    }
}
fn update_capture_stats(
    pos: &Position,
    m: Move,
    captures: &[Move],
    capture_cnt: usize,
    bonus: i32,
) {
    let capture_history = &pos.capture_history;
    let moved_piece = pos.moved_piece(m);
    let captured = pos.piece_on(m.to()).piece_type();
    capture_history.update(moved_piece, m.to(), captured, bonus);
    for i in 0..capture_cnt {
        let moved_piece = pos.moved_piece(captures[i]);
        let captured = pos.piece_on(captures[i].to()).piece_type();
        capture_history.update(moved_piece, captures[i].to(), captured, -bonus);
    }
}
fn update_stats(
    pos: &Position,
    ss: &mut [Stack],
    m: Move,
    quiets: &[Move],
    quiets_cnt: usize,
    bonus: i32,
) {
    if ss[5].killers[0] != m {
        ss[5].killers[1] = ss[5].killers[0];
        ss[5].killers[0] = m;
    }
    let c = pos.side_to_move();
    pos.main_history.update(c, m, bonus);
    update_continuation_histories(&ss[1..], pos.moved_piece(m), m.to(), bonus);
    if ss[4].current_move.is_ok() {
        let prev_sq = ss[4].current_move.to();
        pos.counter_moves.set(pos.piece_on(prev_sq), prev_sq, m);
    }
    for i in 0..quiets_cnt {
        pos.main_history.update(c, quiets[i], -bonus);
        update_continuation_histories(&ss[1..], pos.moved_piece(quiets[i]), quiets[i].to(), -bonus);
    }
}
fn update_counters(pos: &Position) {
    let th = pos.thread_ctrl.as_ref().unwrap();
    th.nodes.set(pos.nodes);
    th.tb_hits.set(pos.tb_hits);
}
fn check_time() {
    if threads::ponder() {
        return;
    }
    let elapsed = timeman::elapsed();
    if (limits().use_time_management() && elapsed > timeman::maximum() - 10)
        || (limits().movetime != 0 && elapsed >= limits().movetime)
        || (limits().nodes != 0 && threads::nodes_searched() >= limits().nodes)
    {
        threads::set_stop(true);
    }
}
fn print_pv(pos: &mut Position, depth: Depth, alpha: Value, beta: Value) {
    let elapsed = timeman::elapsed() + 1;
    let pv_idx = pos.pv_idx;
    let multi_pv = std::cmp::min(ucioption::get_i32("MultiPV") as usize, pos.root_moves.len());
    let nodes_searched = threads::nodes_searched();
    let tb_hits = threads::tb_hits();
    for i in 0..multi_pv {
        let updated = i <= pv_idx && pos.root_moves[i].score != -Value::INFINITE;
        if depth == ONE_PLY && !updated {
            continue;
        }
        let d = if updated { depth } else { depth - ONE_PLY };
        let mut v = if updated {
            pos.root_moves[i].score
        } else {
            pos.root_moves[i].previous_score
        };
        let tb = tb::root_in_tb() && v.abs() < Value::MATE - MAX_MATE_PLY;
        if tb {
            v = pos.root_moves[i].tb_score;
        }
        if v.abs() > Value::MATE - MAX_MATE_PLY
            && (pos.root_moves[i].pv.len() as i32) < (Value::MATE - v.abs()).0
            && tb::cardinality_dtm() > 0
        {
            tb::expand_mate(pos, i);
        }
        print!(
            "info depth {} seldepth {} multipv {} score {} ",
            d / ONE_PLY,
            pos.root_moves[i].sel_depth + 1,
            i + 1,
            uci::value(v)
        );
        if !tb && i == pv_idx {
            if v >= beta {
                print!("lowerbound ");
            } else if v <= alpha {
                print!("upperbound ");
            }
        }
        print!(
            "nodes {} nps {}",
            nodes_searched,
            nodes_searched * 1000 / (elapsed as u64)
        );
        if elapsed > 1000 {
            print!(" hashfull {}", tt::hashfull());
        }
        print!(" tbhits {} time {} pv", tb_hits, elapsed);
        for &m in pos.root_moves[i].pv.iter() {
            print!(" {}", uci::move_str(m, pos.is_chess960()));
        }
        println!("");
    }
    stdout().flush().unwrap();
}
fn extract_ponder_from_tt(pos: &mut Position) -> bool {
    debug_assert!(pos.root_moves[0].pv.len() == 1);
    let m1 = pos.root_moves[0].pv[0];
    if m1 == Move::NONE {
        return false;
    }
    let gives_check = pos.gives_check(m1);
    pos.do_move(m1, gives_check);
    let (tte, tt_hit) = tt::probe(pos.key());
    if tt_hit {
        let m2 = tte.mov();
        if MoveList::new::<Legal>(pos).contains(m2) {
            pos.root_moves[0].pv.push(m2);
        }
    }
    pos.undo_move(m1);
    return pos.root_moves[0].pv.len() > 1;
}
