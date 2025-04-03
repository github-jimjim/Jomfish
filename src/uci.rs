use benchmark::*;
use misc;
use movegen::*;
use position::*;
use search;
use std;
use std::env;
use std::sync::{Arc, RwLock};
use std::time::Instant;
use threads;
use threads::PosData;
use types::*;
use ucioption;
const START_FEN: &'static str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
fn position(pos: &mut Position, pos_data: &mut PosData, args: &str) {
    let fen: &str;
    let moves = match args.find("moves") {
        Some(idx) => idx,
        None => args.len(),
    };
    if &args[0..8] == "startpos" {
        fen = START_FEN;
    } else if &args[0..3] == "fen" {
        fen = (&args[3..moves]).trim();
    } else {
        return;
    }
    pos.init_states();
    pos.set(fen, ucioption::get_bool("UCI_Chess960"));
    pos_data.fen = String::from(fen);
    pos_data.moves = Vec::new();
    if moves == args.len() {
        return;
    }
    let moves = &args[moves + 5..].trim();
    let iter = moves.split_whitespace();
    for token in iter {
        let m = to_move(pos, token);
        if m == Move::NONE {
            break;
        }
        let gives_check = pos.gives_check(m);
        pos.do_move(m, gives_check);
        pos_data.moves.push(m);
    }
}
fn setoption(args: &str) {
    let idx = args.find("name").unwrap();
    let args = &args[idx + 4..];
    if let Some(idx) = args.find("value") {
        let name = &args[..idx].trim();
        let value = &args[idx + 5..].trim();
        ucioption::set(name, value);
    } else {
        let name = args.trim();
        ucioption::set(name, &"");
    }
}
fn go(pos: &mut Position, pos_data: &Arc<RwLock<PosData>>, args: &str) {
    let mut limits = search::LimitsType::new();
    let mut searchmoves: Vec<Move> = Vec::new();
    let mut ponder_mode = false;
    let mut iter = args.split_whitespace();
    while let Some(token) = iter.next() {
        match token {
            "searchmoves" => {
                while let Some(token) = iter.next() {
                    searchmoves.push(to_move(pos, token));
                }
            }
            "wtime" => limits.time[WHITE.0 as usize] = iter.next().unwrap().parse().unwrap(),
            "btime" => limits.time[BLACK.0 as usize] = iter.next().unwrap().parse().unwrap(),
            "winc" => limits.inc[WHITE.0 as usize] = iter.next().unwrap().parse().unwrap(),
            "binc" => limits.inc[BLACK.0 as usize] = iter.next().unwrap().parse().unwrap(),
            "movestogo" => limits.movestogo = iter.next().unwrap().parse().unwrap(),
            "depth" => limits.depth = iter.next().unwrap().parse().unwrap(),
            "nodes" => limits.nodes = iter.next().unwrap().parse().unwrap(),
            "movetime" => limits.movetime = iter.next().unwrap().parse().unwrap(),
            "mate" => limits.mate = iter.next().unwrap().parse().unwrap(),
            "perft" => limits.perft = iter.next().unwrap().parse().unwrap(),
            "infinite" => limits.infinite = true,
            "ponder" => ponder_mode = true,
            _ => {}
        }
    }
    threads::start_thinking(pos, pos_data, &limits, searchmoves, ponder_mode);
}
fn bench(pos: &mut Position, pos_data: &Arc<RwLock<PosData>>, args: &str) {
    let list = setup_bench(pos, args);
    let num = list.iter().filter(|&s| s.find("go ") != None).count();
    let now = Instant::now();
    let mut cnt = 1;
    let mut nodes = 0;
    for cmd in list.iter() {
        let cmd_slice: &str = &cmd;
        let (token, args) = if let Some(idx) = cmd_slice.find(char::is_whitespace) {
            cmd_slice.split_at(idx)
        } else {
            (cmd_slice, "")
        };
        let args = args.trim();
        if token == "go" {
            eprintln!("\nPosition: {}/{}", cnt, num);
            cnt += 1;
            go(pos, pos_data, args);
            threads::wait_for_main();
            nodes += threads::nodes_searched();
        } else if token == "setoption" {
            setoption(args);
        } else if token == "position" {
            position(pos, &mut pos_data.write().unwrap(), args);
        } else if token == "ucinewgame" {
            search::clear();
        }
    }
    let duration = now.elapsed();
    let elapsed =
        (duration.as_secs() as u64) * 1000 + (duration.subsec_nanos() as u64) / 10000000 + 1;
    eprintln!(
        "\n===========================\
\nTotal time (ms) : {}\
\nNode searched   : {}\
\nNodes/second    : {}",
        elapsed,
        nodes,
        1000 * nodes / elapsed
    );
}
pub fn cmd_loop() {
    let mut pos = Box::new(Position::new());
    pos.init_states();
    pos.set(START_FEN, false);
    let pos_data = Arc::new(RwLock::new(PosData {
        fen: String::from(START_FEN),
        moves: Vec::new(),
    }));
    let mut cmd = String::new();
    for arg in env::args().skip(1) {
        cmd.push_str(&arg);
        cmd.push(' ');
    }
    loop {
        if env::args().len() == 1 {
            cmd = String::new();
            if let Err(_) = std::io::stdin().read_line(&mut cmd) {
                cmd = String::from("quit");
            }
        }
        let cmd_slice = cmd.trim();
        let (token, args) = if let Some(idx) = cmd_slice.find(char::is_whitespace) {
            cmd_slice.split_at(idx)
        } else {
            (cmd_slice, "")
        };
        let args = args.trim();
        match token {
            "quit" | "stop" => threads::set_stop(true),
            "ponderhit" => {
                if threads::stop_on_ponderhit() {
                    threads::set_stop(true);
                } else {
                    threads::set_ponder(false);
                }
            }
            "uci" => {
                println!("id name {}", misc::engine_info(true));
                ucioption::print();
                println!("uciok");
            }
            "setoption" => setoption(args),
            "go" => go(&mut pos, &pos_data, args),
            "position" => position(&mut pos, &mut pos_data.write().unwrap(), args),
            "ucinewgame" => search::clear(),
            "isready" => println!("readyok"),
            "bench" => bench(&mut pos, &pos_data, args),
            "d" => pos.print(),
            _ => println!("Unknown command: {} {}", cmd, args),
        }
        if env::args().len() > 1 || token == "quit" {
            break;
        }
    }
}
pub fn value(v: Value) -> String {
    let mut s = String::new();
    let w = if v >= Value::ZERO { v } else { -v };
    if w < Value::MATE - Value(MAX_PLY) {
        s.push_str("cp ");
        s.push_str(&(v * 100 / PawnValueEg).to_string());
    } else {
        s.push_str("mate ");
        let mut dtm = if v > Value::ZERO {
            (Value::MATE - v).0 + 1
        } else {
            (-Value::MATE - v).0
        };
        dtm /= 2;
        s.push_str(&dtm.to_string());
    }
    return s;
}
pub fn square(s: Square) -> String {
    let mut sq = String::new();
    sq.push((97u8 + s.file() as u8) as char);
    sq.push((49u8 + s.rank() as u8) as char);
    sq
}
pub fn move_str(m: Move, chess960: bool) -> String {
    let from = m.from();
    let mut to = m.to();
    if m == Move::NONE {
        return String::from("(none)");
    }
    if m == Move::NULL {
        return String::from("0000");
    }
    if m.move_type() == CASTLING && !chess960 {
        to = Square::make(if to > from { FILE_G } else { FILE_C }, from.rank());
    }
    let mut move_str = square(from);
    move_str.push_str(&square(to));
    if m.move_type() == PROMOTION {
        move_str.push(
            " pnbrqk"
                .chars()
                .nth(m.promotion_type().0 as usize)
                .unwrap(),
        );
    }
    move_str
}
pub fn to_move(pos: &Position, s: &str) -> Move {
    if s.len() == 5 {}
    for m in MoveList::new::<Legal>(pos) {
        if s == move_str(m, pos.is_chess960()) {
            return m;
        }
    }
    Move::NONE
}
