extern crate memmap;
extern crate byteorder;
extern crate chess;
extern crate once_cell;

mod benchmark;
mod bitbases;
#[macro_use]
mod bitboard;
mod endgame;
mod evaluate;
mod material;
mod misc;
mod movegen;
mod movepick;
mod pawns;
mod position;
mod psqt;
mod search;
mod tb;
mod threads;
mod timeman;
mod tt;
mod types;
mod uci;
mod ucioption;
mod nnue;
use nnue::init_nnue;
use std::thread;

const NNUE_MODEL: &[u8] = include_bytes!("nnue.jnn");

fn main() {
    println!("{}", misc::engine_info(false));
    ucioption::init();
    psqt::init();
    bitboard::init();
    position::zobrist::init();
    bitbases::init();
    search::init();
    pawns::init();
    endgame::init();
    if let Err(e) = init_nnue(NNUE_MODEL) {
        eprintln!("Fehler beim Initialisieren des Modells: {}", e);
        return;
    }
    tt::resize(ucioption::get_i32("Hash") as usize);
    threads::init(ucioption::get_i32("Threads") as usize);
    tb::init(ucioption::get_string("SyzygyPath"));
    search::clear();
    let builder = thread::Builder::new().stack_size(16 * 1024 * 1024);
    let ui_thread = builder.spawn(|| uci::cmd_loop()).unwrap();
    let _ = ui_thread.join();
    threads::free();
    tb::free();
    tt::free();
    ucioption::free();
}
