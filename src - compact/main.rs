extern crate memmap;
pub mod benchmark {
    use position::Position;
    use std;
    use std::fs::File;
    use std::io::BufRead;
    const DEFAULTS: [&str; 45] = [
"setoption name UCI_Chess960 value false",
"rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
"r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 10",
"8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 11",
"4rrk1/pp1n3p/3q2pQ/2p1pb2/2PP4/2P3N1/P2B2PP/4RRK1 b - - 7 19",
"rq3rk1/ppp2ppp/1bnpb3/3N2B1/3NP3/7P/PPPQ1PP1/2KR3R w - - 7 14 moves d4e6",
"r1bq1r1k/1pp1n1pp/1p1p4/4p2Q/4Pp2/1BNP4/PPP2PPP/3R1RK1 w - - 2 14 moves g2g4",
"r3r1k1/2p2ppp/p1p1bn2/8/1q2P3/2NPQN2/PPP3PP/R4RK1 b - - 2 15",
"r1bbk1nr/pp3p1p/2n5/1N4p1/2Np1B2/8/PPP2PPP/2KR1B1R w kq - 0 13",
"r1bq1rk1/ppp1nppp/4n3/3p3Q/3P4/1BP1B3/PP1N2PP/R4RK1 w - - 1 16",
"4r1k1/r1q2ppp/ppp2n2/4P3/5Rb1/1N1BQ3/PPP3PP/R5K1 w - - 1 17",
"2rqkb1r/ppp2p2/2npb1p1/1N1Nn2p/2P1PP2/8/PP2B1PP/R1BQK2R b KQ - 0 11",
"r1bq1r1k/b1p1npp1/p2p3p/1p6/3PP3/1B2NN2/PP3PPP/R2Q1RK1 w - - 1 16",
"3r1rk1/p5pp/bpp1pp2/8/q1PP1P2/b3P3/P2NQRPP/1R2B1K1 b - - 6 22",
"r1q2rk1/2p1bppp/2Pp4/p6b/Q1PNp3/4B3/PP1R1PPP/2K4R w - - 2 18",
"4k2r/1pb2ppp/1p2p3/1R1p4/3P4/2r1PN2/P4PPP/1R4K1 b - - 3 22",
"3q2k1/pb3p1p/4pbp1/2r5/PpN2N2/1P2P2P/5PP1/Q2R2K1 b - - 4 26",
"6k1/6p1/6Pp/ppp5/3pn2P/1P3K2/1PP2P2/3N4 b - - 0 1",
"3b4/5kp1/1p1p1p1p/pP1PpP1P/P1P1P3/3KN3/8/8 w - - 0 1",
"2K5/p7/7P/5pR1/8/5k2/r7/8 w - - 0 1 moves g5g6 f3e3 g6g5 e3f3",
"8/6pk/1p6/8/PP3p1p/5P2/4KP1q/3Q4 w - - 0 1",
"7k/3p2pp/4q3/8/4Q3/5Kp1/P6b/8 w - - 0 1",
"8/2p5/8/2kPKp1p/2p4P/2P5/3P4/8 w - - 0 1",
"8/1p3pp1/7p/5P1P/2k3P1/8/2K2P2/8 w - - 0 1",
"8/pp2r1k1/2p1p3/3pP2p/1P1P1P1P/P5KR/8/8 w - - 0 1",
"8/3p4/p1bk3p/Pp6/1Kp1PpPp/2P2P1P/2P5/5B2 b - - 0 1",
"5k2/7R/4P2p/5K2/p1r2P1p/8/8/8 b - - 0 1",
"6k1/6p1/P6p/r1N5/5p2/7P/1b3PP1/4R1K1 w - - 0 1",
"1r3k2/4q3/2Pp3b/3Bp3/2Q2p2/1p1P2P1/1P2KP2/3N4 w - - 0 1",
"6k1/4pp1p/3p2p1/P1pPb3/R7/1r2P1PP/3B1P2/6K1 w - - 0 1",
"8/3p3B/5p2/5P2/p7/PP5b/k7/6K1 w - - 0 1",
"8/8/8/8/5kp1/P7/8/1K1N4 w - - 0 1",
"8/8/8/5N2/8/p7/8/2NK3k w - - 0 1",
"8/3k4/8/8/8/4B3/4KB2/2B5 w - - 0 1",
"8/8/1P6/5pr1/8/4R3/7k/2K5 w - - 0 1",
"8/2p4P/8/kr6/6R1/8/8/1K6 w - - 0 1",
"8/8/3P3k/8/1p6/8/1P6/1K3n2 b - - 0 1",
"8/R7/2q5/8/6k1/8/1P5p/K6R w - - 0 124",
"6k1/3b3r/1p1p4/p1n2p2/1PPNpP1q/P3Q1p1/1R1RB1P1/5K2 b - - 0 1",
"r2r1n2/pp2bk2/2p1p2p/3q4/3PN1QP/2P3R1/P4PP1/5RK1 w - - 0 1",
"8/8/8/8/8/6k1/6p1/6K1 w - -",
"7k/7P/6K1/8/3B4/8/8/8 b - -",
"setoption name UCI_Chess960 value true",
"bbqnnrkr/pppppppp/8/8/8/8/PPPPPPPP/BBQNNRKR w KQkq - 0 1 moves g2g3 d7d5 d2d4 c8h3 c1g5 e8d6 g5e7 f7f6",
"setoption name UCI_Chess960 value false"
];
    pub fn setup_bench(pos: &Position, args: &str) -> Vec<String> {
        let mut iter = args.split_whitespace();
        let tt_size = if let Some(t) = iter.next() { t } else { "16" };
        let threads = if let Some(t) = iter.next() { t } else { "1" };
        let limit = if let Some(t) = iter.next() { t } else { "13" };
        let fen_file = if let Some(t) = iter.next() {
            t
        } else {
            "default"
        };
        let limit_type = if let Some(t) = iter.next() {
            t
        } else {
            "depth"
        };
        let go = format!("go {} {}", limit_type, limit);
        let mut fens: Vec<String> = Vec::new();
        if fen_file == "default" {
            for fen in DEFAULTS.iter() {
                fens.push(String::from(*fen));
            }
        } else if fen_file == "current" {
            fens.push(pos.fen());
        } else {
            let file = match File::open(fen_file) {
                Err(_) => {
                    eprintln!("Unable to open file {}", fen_file);
                    std::process::exit(-1);
                }
                Ok(file) => file,
            };
            let reader = std::io::BufReader::new(file);
            for fen in reader.lines() {
                if fen.is_ok() {
                    break;
                }
                let fen = fen.unwrap();
                if !fen.is_empty() {
                    fens.push(fen);
                }
            }
        }
        let mut list: Vec<String> = Vec::new();
        list.push(String::from("ucinewgame"));
        list.push(format!("setoption name Threads value {}", threads));
        list.push(format!("setoption name Hash value {}", tt_size));
        for fen in fens {
            if fen.find("setoption") != None {
                list.push(fen);
            } else {
                list.push(format!("position fen {}", fen));
                list.push(go.clone());
            }
        }
        list
    }
}
pub mod bitbases {
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
}
#[macro_use]
pub mod bitboard {
    #![allow(dead_code)]
    use std;
    use types::*;
    use uci;
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Bitboard(pub u64);
    pub fn popcount(bb: Bitboard) -> u32 {
        bb.0.count_ones()
    }
    pub const ALL_SQUARES: Bitboard = Bitboard(!0u64);
    pub const DARK_SQUARES: Bitboard = Bitboard(0xaa55aa55aa55aa55);
    pub const FILEA_BB: Bitboard = Bitboard(0x0101010101010101);
    pub const FILEB_BB: Bitboard = Bitboard(0x0202020202020202);
    pub const FILEC_BB: Bitboard = Bitboard(0x0404040404040404);
    pub const FILED_BB: Bitboard = Bitboard(0x0808080808080808);
    pub const FILEE_BB: Bitboard = Bitboard(0x1010101010101010);
    pub const FILEF_BB: Bitboard = Bitboard(0x2020202020202020);
    pub const FILEG_BB: Bitboard = Bitboard(0x4040404040404040);
    pub const FILEH_BB: Bitboard = Bitboard(0x8080808080808080);
    pub const RANK1_BB: Bitboard = Bitboard(0xff);
    pub const RANK2_BB: Bitboard = Bitboard(0xff00);
    pub const RANK3_BB: Bitboard = Bitboard(0xff0000);
    pub const RANK4_BB: Bitboard = Bitboard(0xff000000);
    pub const RANK5_BB: Bitboard = Bitboard(0xff00000000);
    pub const RANK6_BB: Bitboard = Bitboard(0xff0000000000);
    pub const RANK7_BB: Bitboard = Bitboard(0xff000000000000);
    pub const RANK8_BB: Bitboard = Bitboard(0xff00000000000000);
    static mut SQUARE_DISTANCE: [[u32; 64]; 64] = [[0; 64]; 64];
    static mut SQUARE_BB: [Bitboard; 64] = [Bitboard(0); 64];
    static mut FILE_BB: [Bitboard; 8] = [Bitboard(0); 8];
    static mut RANK_BB: [Bitboard; 8] = [Bitboard(0); 8];
    static mut ADJACENT_FILES_BB: [Bitboard; 8] = [Bitboard(0); 8];
    static mut FORWARD_RANKS_BB: [[Bitboard; 8]; 2] = [[Bitboard(0); 8]; 2];
    static mut BETWEEN_BB: [[Bitboard; 64]; 64] = [[Bitboard(0); 64]; 64];
    static mut LINE_BB: [[Bitboard; 64]; 64] = [[Bitboard(0); 64]; 64];
    static mut DISTANCE_RING_BB: [[Bitboard; 8]; 64] = [[Bitboard(0); 8]; 64];
    static mut FORWARD_FILE_BB: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
    static mut PASSED_PAWN_MASK: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
    static mut PAWN_ATTACK_SPAN: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
    static mut PSEUDO_ATTACKS: [[Bitboard; 64]; 8] = [[Bitboard(0); 64]; 8];
    static mut PAWN_ATTACKS: [[Bitboard; 64]; 2] = [[Bitboard(0); 64]; 2];
    struct Magics {
        masks: [Bitboard; 64],
        magics: [u64; 64],
        attacks: [&'static [Bitboard]; 64],
    }
    static mut ROOK_MAGICS: Magics = Magics {
        masks: [Bitboard(0); 64],
        magics: [0; 64],
        attacks: [&[]; 64],
    };
    static mut BISHOP_MAGICS: Magics = Magics {
        masks: [Bitboard(0); 64],
        magics: [0; 64],
        attacks: [&[]; 64],
    };
    static mut ATTACKS_TABLE: [Bitboard; 88772] = [Bitboard(0); 88772];
    struct MagicInit {
        magic: u64,
        index: u32,
    }
    macro_rules! M {
        ($x:expr, $y:expr) => {
            MagicInit {
                magic: $x,
                index: $y,
            }
        };
    }
    const BISHOP_INIT: [MagicInit; 64] = [
        M!(0x007fbfbfbfbfbfff, 5378),
        M!(0x0000a060401007fc, 4093),
        M!(0x0001004008020000, 4314),
        M!(0x0000806004000000, 6587),
        M!(0x0000100400000000, 6491),
        M!(0x000021c100b20000, 6330),
        M!(0x0000040041008000, 5609),
        M!(0x00000fb0203fff80, 22236),
        M!(0x0000040100401004, 6106),
        M!(0x0000020080200802, 5625),
        M!(0x0000004010202000, 16785),
        M!(0x0000008060040000, 16817),
        M!(0x0000004402000000, 6842),
        M!(0x0000000801008000, 7003),
        M!(0x000007efe0bfff80, 4197),
        M!(0x0000000820820020, 7356),
        M!(0x0000400080808080, 4602),
        M!(0x00021f0100400808, 4538),
        M!(0x00018000c06f3fff, 29531),
        M!(0x0000258200801000, 45393),
        M!(0x0000240080840000, 12420),
        M!(0x000018000c03fff8, 15763),
        M!(0x00000a5840208020, 5050),
        M!(0x0000020008208020, 4346),
        M!(0x0000804000810100, 6074),
        M!(0x0001011900802008, 7866),
        M!(0x0000804000810100, 32139),
        M!(0x000100403c0403ff, 57673),
        M!(0x00078402a8802000, 55365),
        M!(0x0000101000804400, 15818),
        M!(0x0000080800104100, 5562),
        M!(0x00004004c0082008, 6390),
        M!(0x0001010120008020, 7930),
        M!(0x000080809a004010, 13329),
        M!(0x0007fefe08810010, 7170),
        M!(0x0003ff0f833fc080, 27267),
        M!(0x007fe08019003042, 53787),
        M!(0x003fffefea003000, 5097),
        M!(0x0000101010002080, 6643),
        M!(0x0000802005080804, 6138),
        M!(0x0000808080a80040, 7418),
        M!(0x0000104100200040, 7898),
        M!(0x0003ffdf7f833fc0, 42012),
        M!(0x0000008840450020, 57350),
        M!(0x00007ffc80180030, 22813),
        M!(0x007fffdd80140028, 56693),
        M!(0x00020080200a0004, 5818),
        M!(0x0000101010100020, 7098),
        M!(0x0007ffdfc1805000, 4451),
        M!(0x0003ffefe0c02200, 4709),
        M!(0x0000000820806000, 4794),
        M!(0x0000000008403000, 13364),
        M!(0x0000000100202000, 4570),
        M!(0x0000004040802000, 4282),
        M!(0x0004010040100400, 14964),
        M!(0x00006020601803f4, 4026),
        M!(0x0003ffdfdfc28048, 4826),
        M!(0x0000000820820020, 7354),
        M!(0x0000000008208060, 4848),
        M!(0x0000000000808020, 15946),
        M!(0x0000000001002020, 14932),
        M!(0x0000000401002008, 16588),
        M!(0x0000004040404040, 6905),
        M!(0x007fff9fdf7ff813, 16076),
    ];
    const ROOK_INIT: [MagicInit; 64] = [
        M!(0x00280077ffebfffe, 26304),
        M!(0x2004010201097fff, 35520),
        M!(0x0010020010053fff, 38592),
        M!(0x0040040008004002, 8026),
        M!(0x7fd00441ffffd003, 22196),
        M!(0x4020008887dffffe, 80870),
        M!(0x004000888847ffff, 76747),
        M!(0x006800fbff75fffd, 30400),
        M!(0x000028010113ffff, 11115),
        M!(0x0020040201fcffff, 18205),
        M!(0x007fe80042ffffe8, 53577),
        M!(0x00001800217fffe8, 62724),
        M!(0x00001800073fffe8, 34282),
        M!(0x00001800e05fffe8, 29196),
        M!(0x00001800602fffe8, 23806),
        M!(0x000030002fffffa0, 49481),
        M!(0x00300018010bffff, 2410),
        M!(0x0003000c0085fffb, 36498),
        M!(0x0004000802010008, 24478),
        M!(0x0004002020020004, 10074),
        M!(0x0001002002002001, 79315),
        M!(0x0001001000801040, 51779),
        M!(0x0000004040008001, 13586),
        M!(0x0000006800cdfff4, 19323),
        M!(0x0040200010080010, 70612),
        M!(0x0000080010040010, 83652),
        M!(0x0004010008020008, 63110),
        M!(0x0000040020200200, 34496),
        M!(0x0002008010100100, 84966),
        M!(0x0000008020010020, 54341),
        M!(0x0000008020200040, 60421),
        M!(0x0000820020004020, 86402),
        M!(0x00fffd1800300030, 50245),
        M!(0x007fff7fbfd40020, 76622),
        M!(0x003fffbd00180018, 84676),
        M!(0x001fffde80180018, 78757),
        M!(0x000fffe0bfe80018, 37346),
        M!(0x0001000080202001, 370),
        M!(0x0003fffbff980180, 42182),
        M!(0x0001fffdff9000e0, 45385),
        M!(0x00fffefeebffd800, 61659),
        M!(0x007ffff7ffc01400, 12790),
        M!(0x003fffbfe4ffe800, 16762),
        M!(0x001ffff01fc03000, 0),
        M!(0x000fffe7f8bfe800, 38380),
        M!(0x0007ffdfdf3ff808, 11098),
        M!(0x0003fff85fffa804, 21803),
        M!(0x0001fffd75ffa802, 39189),
        M!(0x00ffffd7ffebffd8, 58628),
        M!(0x007fff75ff7fbfd8, 44116),
        M!(0x003fff863fbf7fd8, 78357),
        M!(0x001fffbfdfd7ffd8, 44481),
        M!(0x000ffff810280028, 64134),
        M!(0x0007ffd7f7feffd8, 41759),
        M!(0x0003fffc0c480048, 1394),
        M!(0x0001ffffafd7ffd8, 40910),
        M!(0x00ffffe4ffdfa3ba, 66516),
        M!(0x007fffef7ff3d3da, 3897),
        M!(0x003fffbfdfeff7fa, 3930),
        M!(0x001fffeff7fbfc22, 72934),
        M!(0x0000020408001001, 72662),
        M!(0x0007fffeffff77fd, 56325),
        M!(0x0003ffffbf7dfeec, 66501),
        M!(0x0001ffff9dffa333, 14826),
    ];
    fn index_bishop(s: Square, occupied: Bitboard) -> usize {
        unsafe {
            (u64::wrapping_mul(
                (occupied & BISHOP_MAGICS.masks[s.0 as usize]).0,
                BISHOP_MAGICS.magics[s.0 as usize],
            ) >> (64 - 9)) as usize
        }
    }
    fn index_rook(s: Square, occupied: Bitboard) -> usize {
        unsafe {
            (u64::wrapping_mul(
                (occupied & ROOK_MAGICS.masks[s.0 as usize]).0,
                ROOK_MAGICS.magics[s.0 as usize],
            ) >> (64 - 12)) as usize
        }
    }
    fn attacks_bb_bishop(s: Square, occupied: Bitboard) -> Bitboard {
        unsafe { BISHOP_MAGICS.attacks[s.0 as usize][index_bishop(s, occupied)] }
    }
    fn attacks_bb_rook(s: Square, occupied: Bitboard) -> Bitboard {
        unsafe { ROOK_MAGICS.attacks[s.0 as usize][index_rook(s, occupied)] }
    }
    impl std::convert::From<Square> for Bitboard {
        fn from(s: Square) -> Self {
            unsafe { SQUARE_BB[s.0 as usize] }
        }
    }
    impl Square {
        pub fn bb(self) -> Bitboard {
            Bitboard::from(self)
        }
        pub fn file_bb(self) -> Bitboard {
            file_bb(self.file())
        }
        pub fn rank_bb(self) -> Bitboard {
            unsafe { RANK_BB[self.rank() as usize] }
        }
    }
    impl std::ops::BitOr<Bitboard> for Bitboard {
        type Output = Self;
        fn bitor(self, rhs: Self) -> Self {
            Bitboard(self.0 | rhs.0)
        }
    }
    impl std::ops::BitOr<Square> for Bitboard {
        type Output = Bitboard;
        fn bitor(self, rhs: Square) -> Self {
            self | Bitboard::from(rhs)
        }
    }
    impl std::ops::BitAnd<Bitboard> for Bitboard {
        type Output = Self;
        fn bitand(self, rhs: Self) -> Self {
            Bitboard(self.0 & rhs.0)
        }
    }
    impl std::ops::BitAnd<Square> for Bitboard {
        type Output = Bitboard;
        fn bitand(self, rhs: Square) -> Self {
            self & Bitboard::from(rhs)
        }
    }
    impl std::ops::BitXor<Bitboard> for Bitboard {
        type Output = Self;
        fn bitxor(self, rhs: Self) -> Self {
            Bitboard(self.0 ^ rhs.0)
        }
    }
    impl std::ops::BitXor<Square> for Bitboard {
        type Output = Bitboard;
        fn bitxor(self, rhs: Square) -> Self {
            self ^ Bitboard::from(rhs)
        }
    }
    impl std::ops::Not for Bitboard {
        type Output = Bitboard;
        fn not(self) -> Self {
            Bitboard(!self.0)
        }
    }
    impl std::ops::Neg for Bitboard {
        type Output = Bitboard;
        fn neg(self) -> Self {
            Bitboard(self.0.wrapping_neg())
        }
    }
    impl std::ops::Shl<i32> for Bitboard {
        type Output = Bitboard;
        fn shl(self, rhs: i32) -> Self {
            Bitboard(self.0 << rhs)
        }
    }
    impl std::ops::Shr<i32> for Bitboard {
        type Output = Bitboard;
        fn shr(self, rhs: i32) -> Self {
            Bitboard(self.0 >> rhs)
        }
    }
    impl<RHS> std::ops::BitOrAssign<RHS> for Bitboard
    where
        Bitboard: std::ops::BitOr<RHS, Output = Bitboard>,
    {
        fn bitor_assign(&mut self, rhs: RHS) {
            *self = *self | rhs;
        }
    }
    impl<RHS> std::ops::BitAndAssign<RHS> for Bitboard
    where
        Bitboard: std::ops::BitAnd<RHS, Output = Bitboard>,
    {
        fn bitand_assign(&mut self, rhs: RHS) {
            *self = *self & rhs;
        }
    }
    impl<RHS> std::ops::BitXorAssign<RHS> for Bitboard
    where
        Bitboard: std::ops::BitXor<RHS, Output = Bitboard>,
    {
        fn bitxor_assign(&mut self, rhs: RHS) {
            *self = *self ^ rhs;
        }
    }
    impl std::cmp::PartialEq<u64> for Bitboard {
        fn eq(&self, rhs: &u64) -> bool {
            debug_assert!(*rhs == 0);
            (*self).0 == *rhs
        }
    }
    impl std::fmt::Display for Bitboard {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
            for s in *self {
                write!(f, "{} ", uci::square(s)).unwrap();
            }
            write!(f, "")
        }
    }
    pub fn more_than_one(b: Bitboard) -> bool {
        (b.0 & u64::wrapping_sub(b.0, 1)) != 0
    }
    pub fn lsb(b: Bitboard) -> Square {
        debug_assert!(b != 0);
        Square(u64::trailing_zeros(b.0))
    }
    pub fn msb(b: Bitboard) -> Square {
        debug_assert!(b != 0);
        Square(63 ^ u64::leading_zeros(b.0))
    }
    pub fn pop_lsb(b: &mut Bitboard) -> Square {
        let s = lsb(*b);
        b.0 &= u64::wrapping_sub(b.0, 1);
        s
    }
    pub fn frontmost_sq(c: Color, b: Bitboard) -> Square {
        if c == WHITE {
            msb(b)
        } else {
            lsb(b)
        }
    }
    pub fn backmost_sq(c: Color, b: Bitboard) -> Square {
        if c == WHITE {
            lsb(b)
        } else {
            msb(b)
        }
    }
    impl Iterator for Bitboard {
        type Item = Square;
        fn next(&mut self) -> Option<Self::Item> {
            if (*self).0 != 0 {
                Some(pop_lsb(self))
            } else {
                None
            }
        }
    }
    pub fn file_bb(f: File) -> Bitboard {
        unsafe { FILE_BB[f as usize] }
    }
    macro_rules! bitboard {
() => { Bitboard(0) };
($sq:ident) => { bitboard!() | Square::$sq };
($sq:ident, $($sqs:ident),+) => { bitboard!($($sqs),*) | Square::$sq };
}
    impl Bitboard {
        pub fn shift(self, d: Direction) -> Bitboard {
            match d {
                NORTH => self << 8,
                SOUTH => self >> 8,
                NORTH_EAST => (self & !FILEH_BB) << 9,
                SOUTH_EAST => (self & !FILEH_BB) >> 7,
                NORTH_WEST => (self & !FILEA_BB) << 7,
                SOUTH_WEST => (self & !FILEA_BB) >> 9,
                _ => Bitboard(0),
            }
        }
    }
    pub fn adjacent_files_bb(f: File) -> Bitboard {
        unsafe { ADJACENT_FILES_BB[f as usize] }
    }
    pub fn between_bb(s1: Square, s2: Square) -> Bitboard {
        unsafe { BETWEEN_BB[s1.0 as usize][s2.0 as usize] }
    }
    pub fn forward_ranks_bb(c: Color, s: Square) -> Bitboard {
        unsafe { FORWARD_RANKS_BB[c.0 as usize][s.rank() as usize] }
    }
    pub fn forward_file_bb(c: Color, s: Square) -> Bitboard {
        unsafe { FORWARD_FILE_BB[c.0 as usize][s.0 as usize] }
    }
    pub fn pawn_attack_span(c: Color, s: Square) -> Bitboard {
        unsafe { PAWN_ATTACK_SPAN[c.0 as usize][s.0 as usize] }
    }
    pub fn passed_pawn_mask(c: Color, s: Square) -> Bitboard {
        unsafe { PASSED_PAWN_MASK[c.0 as usize][s.0 as usize] }
    }
    pub fn line_bb(s1: Square, s2: Square) -> Bitboard {
        unsafe { LINE_BB[s1.0 as usize][s2.0 as usize] }
    }
    pub fn aligned(s1: Square, s2: Square, s3: Square) -> bool {
        line_bb(s1, s2) & s3 != 0
    }
    pub fn pseudo_attacks(pt: PieceType, s: Square) -> Bitboard {
        unsafe { PSEUDO_ATTACKS[pt.0 as usize][s.0 as usize] }
    }
    pub fn pawn_attacks(c: Color, s: Square) -> Bitboard {
        unsafe { PAWN_ATTACKS[c.0 as usize][s.0 as usize] }
    }
    pub fn distance_ring_bb(s: Square, d: i32) -> Bitboard {
        unsafe { DISTANCE_RING_BB[s.0 as usize][d as usize] }
    }
    pub trait Distance {
        fn distance(x: Self, y: Self) -> u32;
    }
    impl Distance for u32 {
        fn distance(x: Self, y: Self) -> u32 {
            if x > y {
                x - y
            } else {
                y - x
            }
        }
    }
    impl Distance for Square {
        fn distance(x: Self, y: Self) -> u32 {
            unsafe { SQUARE_DISTANCE[x.0 as usize][y.0 as usize] }
        }
    }
    pub fn init() {
        for s in ALL_SQUARES {
            unsafe {
                SQUARE_BB[s.0 as usize] = Bitboard(1u64) << (s.0 as i32);
            }
        }
        for f in 0..8 {
            unsafe {
                FILE_BB[f as usize] = FILEA_BB << f;
            }
        }
        for r in 0..8 {
            unsafe {
                RANK_BB[r as usize] = RANK1_BB << (8 * r);
            }
        }
        for f in 0..8 {
            unsafe {
                let left = if f > FILE_A {
                    file_bb(f - 1)
                } else {
                    Bitboard(0)
                };
                let right = if f < FILE_H {
                    file_bb(f + 1)
                } else {
                    Bitboard(0)
                };
                ADJACENT_FILES_BB[f as usize] = left | right;
            }
        }
        for r in 0..7 {
            unsafe {
                FORWARD_RANKS_BB[BLACK.0 as usize][(r + 1) as usize] =
                    FORWARD_RANKS_BB[BLACK.0 as usize][r as usize] | RANK_BB[r as usize];
                FORWARD_RANKS_BB[WHITE.0 as usize][r as usize] =
                    !FORWARD_RANKS_BB[BLACK.0 as usize][(r + 1) as usize];
            }
        }
        for &c in [WHITE, BLACK].iter() {
            for s in ALL_SQUARES {
                unsafe {
                    FORWARD_FILE_BB[c.0 as usize][s.0 as usize] = FORWARD_RANKS_BB[c.0 as usize]
                        [s.rank() as usize]
                        & FILE_BB[s.file() as usize];
                    PAWN_ATTACK_SPAN[c.0 as usize][s.0 as usize] = FORWARD_RANKS_BB[c.0 as usize]
                        [s.rank() as usize]
                        & ADJACENT_FILES_BB[s.file() as usize];
                    PASSED_PAWN_MASK[c.0 as usize][s.0 as usize] = FORWARD_FILE_BB[c.0 as usize]
                        [s.0 as usize]
                        | PAWN_ATTACK_SPAN[c.0 as usize][s.0 as usize];
                }
            }
        }
        for s1 in ALL_SQUARES {
            for s2 in ALL_SQUARES {
                if s1 != s2 {
                    unsafe {
                        let dist = std::cmp::max(
                            File::distance(s1.file(), s2.file()),
                            Rank::distance(s1.rank(), s2.rank()),
                        );
                        SQUARE_DISTANCE[s1.0 as usize][s2.0 as usize] = dist;
                        DISTANCE_RING_BB[s1.0 as usize][dist as usize - 1] |= s2;
                    }
                }
            }
        }
        for &c in [WHITE, BLACK].iter() {
            for &pt in [PAWN, KNIGHT, KING].iter() {
                for s in ALL_SQUARES {
                    let steps: &[i32] = match pt {
                        PAWN => &[7, 9],
                        KNIGHT => &[6, 10, 15, 17],
                        _ => &[1, 7, 8, 9],
                    };
                    for &d in steps.iter() {
                        let to = s + if c == WHITE {
                            Direction(d)
                        } else {
                            -Direction(d)
                        };
                        if to.is_ok() && Square::distance(s, to) < 3 {
                            unsafe {
                                if pt == PAWN {
                                    PAWN_ATTACKS[c.0 as usize][s.0 as usize] |= to;
                                } else {
                                    PSEUDO_ATTACKS[pt.0 as usize][s.0 as usize] |= to;
                                }
                            }
                        }
                    }
                }
            }
        }
        let rook_dirs = [NORTH, EAST, SOUTH, WEST];
        let bishop_dirs = [NORTH_EAST, SOUTH_EAST, SOUTH_WEST, NORTH_WEST];
        unsafe {
            init_magics(&mut ROOK_MAGICS, &ROOK_INIT, rook_dirs, index_rook);
            init_magics(&mut BISHOP_MAGICS, &BISHOP_INIT, bishop_dirs, index_bishop);
        }
        for s1 in ALL_SQUARES {
            let b_att = attacks_bb(BISHOP, s1, Bitboard(0));
            let r_att = attacks_bb(ROOK, s1, Bitboard(0));
            unsafe {
                PSEUDO_ATTACKS[BISHOP.0 as usize][s1.0 as usize] = b_att;
                PSEUDO_ATTACKS[ROOK.0 as usize][s1.0 as usize] = r_att;
                PSEUDO_ATTACKS[QUEEN.0 as usize][s1.0 as usize] = b_att | r_att;
            }
            for &pt in [BISHOP, ROOK].iter() {
                for s2 in ALL_SQUARES {
                    unsafe {
                        if PSEUDO_ATTACKS[pt.0 as usize][s1.0 as usize] & s2 == 0 {
                            continue;
                        }
                        LINE_BB[s1.0 as usize][s2.0 as usize] = (attacks_bb(pt, s1, Bitboard(0))
                            & attacks_bb(pt, s2, Bitboard(0)))
                            | s1
                            | s2;
                        BETWEEN_BB[s1.0 as usize][s2.0 as usize] =
                            attacks_bb(pt, s1, s2.bb()) & attacks_bb(pt, s2, s1.bb());
                    }
                }
            }
        }
    }
    fn sliding_attack(directions: [Direction; 4], sq: Square, occupied: Bitboard) -> Bitboard {
        let mut attack = Bitboard(0);
        for d in directions.iter() {
            let mut s = sq + *d;
            while s.is_ok() && Square::distance(s, s - *d) == 1 {
                attack |= s;
                if occupied & s != 0 {
                    break;
                }
                s += *d;
            }
        }
        attack
    }
    fn init_magics(
        m: &mut Magics,
        magic_init: &[MagicInit; 64],
        dirs: [Direction; 4],
        index: fn(Square, Bitboard) -> usize,
    ) {
        for s in ALL_SQUARES {
            let edges =
                ((RANK1_BB | RANK8_BB) & !s.rank_bb()) | ((FILEA_BB | FILEH_BB) & !s.file_bb());
            let mask = sliding_attack(dirs, s, Bitboard(0)) & !edges;
            m.masks[s.0 as usize] = mask;
            m.magics[s.0 as usize] = magic_init[s.0 as usize].magic;
            let base = magic_init[s.0 as usize].index as usize;
            let mut size = 0;
            let mut b = Bitboard(0);
            loop {
                let idx = index(s, b);
                size = std::cmp::max(size, idx + 1);
                unsafe {
                    ATTACKS_TABLE[base + idx] = sliding_attack(dirs, s, b);
                }
                b = Bitboard(u64::wrapping_sub(b.0, mask.0)) & mask;
                if b == 0 {
                    break;
                }
            }
            m.attacks[s.0 as usize] = unsafe { &ATTACKS_TABLE[base..base + size] };
        }
    }
    pub fn attacks_bb(pt: PieceType, s: Square, occupied: Bitboard) -> Bitboard {
        match pt {
            BISHOP => attacks_bb_bishop(s, occupied),
            ROOK => attacks_bb_rook(s, occupied),
            QUEEN => attacks_bb_bishop(s, occupied) | attacks_bb_rook(s, occupied),
            _ => pseudo_attacks(pt, s),
        }
    }
}
pub mod endgame {
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
        100, 90, 80, 70, 70, 80, 90, 100, 90, 70, 60, 50, 50, 60, 70, 90, 80, 60, 40, 30, 30, 40,
        60, 80, 70, 50, 30, 20, 20, 30, 50, 70, 70, 50, 30, 20, 20, 30, 50, 70, 80, 60, 40, 30, 30,
        40, 60, 80, 90, 70, 60, 50, 50, 60, 70, 90, 100, 90, 80, 70, 70, 80, 90, 100,
    ];
    const PUSH_TO_CORNERS: [i32; 64] = [
        200, 190, 180, 170, 160, 150, 140, 130, 190, 180, 170, 160, 150, 140, 130, 140, 180, 170,
        155, 140, 140, 125, 140, 150, 170, 160, 140, 120, 110, 140, 150, 160, 160, 150, 140, 110,
        120, 140, 160, 170, 150, 140, 125, 140, 140, 155, 170, 180, 140, 130, 140, 150, 160, 170,
        180, 190, 130, 140, 150, 160, 170, 180, 190, 200,
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
            || (pos.pieces_p(BISHOP) & !DARK_SQUARES != 0
                && pos.pieces_p(BISHOP) & DARK_SQUARES != 0)
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
        let result = Value(
            PUSH_TO_EDGES[bksq.0 as usize] + PUSH_AWAY[Square::distance(bksq, bnsq) as usize],
        );
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
            if opposite_colors(queening_sq, bishop_sq)
                && Square::distance(queening_sq, king_sq) <= 1
            {
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
            && (brsq.rank() == RANK_1
                || (tempo == 0 && u32::distance(brsq.file(), wpsq.file()) >= 3))
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
            return ScaleFactor(
                ScaleFactor::MAX.0 - 2 * Square::distance(wksq, queening_sq) as i32,
            );
        }
        if f != FILE_A
            && wrsq.file() == f
            && wrsq.0 < wpsq.0
            && Square::distance(wksq, queening_sq) + 2 < Square::distance(bksq, queening_sq) + tempo
            && Square::distance(wksq, wpsq + NORTH) + 2
                < Square::distance(bksq, wpsq + NORTH) + tempo
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
                        || pos.attacks_from(BISHOP, block_sq2) & pos.pieces_cp(weak_side, BISHOP)
                            != 0
                        || u32::distance(r1, r2) >= 2)
                {
                    return ScaleFactor::DRAW;
                } else if ksq == block_sq2
                    && opposite_colors(ksq, wbsq)
                    && (bbsq == block_sq1
                        || pos.attacks_from(BISHOP, block_sq1) & pos.pieces_cp(weak_side, BISHOP)
                            != 0)
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
}
pub mod evaluate {
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
            ei.king_attackers_count[them.0 as usize] =
                popcount(b & ei.pe.pawn_attacks(them)) as i32;
            ei.king_adjacent_zone_attacks_count[them.0 as usize] = 0;
            ei.king_attackers_weight[them.0 as usize] = 0;
        } else {
            ei.king_ring[us.0 as usize] = Bitboard(0);
            ei.king_attackers_count[them.0 as usize] = 0;
        }
    }
    fn evaluate_pieces<Us: ColorTrait, Pt: PieceTypeTrait>(
        pos: &Position,
        ei: &mut EvalInfo,
    ) -> Score {
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
            ei.attacked_by2[us.0 as usize] |=
                ei.attacked_by[us.0 as usize][ALL_PIECES.0 as usize] & b;
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
            score += KING_PROTECTOR[(pt.0 - 2) as usize]
                * Square::distance(s, pos.square(us, KING)) as i32;
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
        if ei.king_attackers_count[them.0 as usize]
            > (1 - popcount(pos.pieces_cp(them, QUEEN)) as i32)
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
            let b =
                pos.attacks_from(KNIGHT, ksq) & ei.attacked_by[them.0 as usize][KNIGHT.0 as usize];
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
            | (b & ei.attacked_by2[them.0 as usize]
                & !ei.attacked_by[us.0 as usize][PAWN.0 as usize]);
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
            let b = (pos.pieces_cp(them, QUEEN) | weak)
                & ei.attacked_by[us.0 as usize][ROOK.0 as usize];
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
                        unsafe_squares &= ei.attacked_by[them.0 as usize][ALL_PIECES.0 as usize]
                            | pos.pieces_c(them);
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
        let both_flanks =
            pos.pieces_p(PAWN) & QUEEN_SIDE != 0 && pos.pieces_p(PAWN) & KING_SIDE != 0;
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
        score += evaluate_pieces::<White, Rook>(pos, &mut ei)
            - evaluate_pieces::<Black, Rook>(pos, &mut ei);
        score += evaluate_pieces::<White, Queen>(pos, &mut ei)
            - evaluate_pieces::<Black, Queen>(pos, &mut ei);
        score += ei.mobility[WHITE.0 as usize] - ei.mobility[BLACK.0 as usize];
        score += evaluate_king::<White>(pos, &mut ei) - evaluate_king::<Black>(pos, &mut ei);
        score += evaluate_threats::<White>(pos, &ei) - evaluate_threats::<Black>(pos, &ei);
        score +=
            evaluate_passed_pawns::<White>(pos, &ei) - evaluate_passed_pawns::<Black>(pos, &ei);
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
}
pub mod material {
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
}
pub mod misc {
    #[derive(Clone, Copy)]
    pub struct Prng(u64);
    impl Prng {
        pub fn new(seed: u64) -> Prng {
            Prng(seed)
        }
        pub fn rand64(&mut self) -> u64 {
            (*self).0 ^= (*self).0 >> 12;
            (*self).0 ^= (*self).0 << 25;
            (*self).0 ^= (*self).0 >> 27;
            u64::wrapping_mul(self.0, 2685821657736338717)
        }
    }
    pub fn engine_info(to_uci: bool) -> String {
        if to_uci {
            format!("Jomfish 10 dev\nid author Jimmy Luong")
        } else {
            let ascii_art = r#"
░░░░░██╗░█████╗░███╗░░░███╗███████╗██╗░██████╗██╗░░██╗  ░░███╗░░░█████╗░  ██████╗░███████╗██╗░░░██╗
░░░░░██║██╔══██╗████╗░████║██╔════╝██║██╔════╝██║░░██║  ░████║░░██╔══██╗  ██╔══██╗██╔════╝██║░░░██║
░░░░░██║██║░░██║██╔████╔██║█████╗░░██║╚█████╗░███████║  ██╔██║░░██║░░██║  ██║░░██║█████╗░░╚██╗░██╔╝
██╗░░██║██║░░██║██║╚██╔╝██║██╔══╝░░██║░╚═══██╗██╔══██║  ╚═╝██║░░██║░░██║  ██║░░██║██╔══╝░░░╚████╔╝░
╚█████╔╝╚█████╔╝██║░╚═╝░██║██║░░░░░██║██████╔╝██║░░██║  ███████╗╚█████╔╝  ██████╔╝███████╗░░╚██╔╝░░
░╚════╝░░╚════╝░╚═╝░░░░░╚═╝╚═╝░░░░░╚═╝╚═════╝░╚═╝░░╚═╝  ╚══════╝░╚════╝░  ╚═════╝░╚══════╝░░░╚═╝░░░
"#;
            let details = "Jomfish 10 dev by Jimmy based on Stockfish";
            format!("{}\n{}", ascii_art, details)
        }
    }
}
pub mod movegen {
    use bitboard::*;
    use position::Position;
    use types::*;
    const CAPTURES: i32 = 0;
    const QUIETS: i32 = 1;
    const QUIET_CHECKS: i32 = 2;
    const EVASIONS: i32 = 3;
    const NON_EVASIONS: i32 = 4;
    const LEGAL: i32 = 5;
    pub struct Captures;
    pub struct Quiets;
    pub struct QuietChecks;
    pub struct Evasions;
    pub struct NonEvasions;
    pub struct Legal;
    pub trait GenType {
        type Checks: Bool;
        const TYPE: i32;
    }
    impl GenType for Captures {
        type Checks = False;
        const TYPE: i32 = CAPTURES;
    }
    impl GenType for Quiets {
        type Checks = False;
        const TYPE: i32 = QUIETS;
    }
    impl GenType for QuietChecks {
        type Checks = True;
        const TYPE: i32 = QUIET_CHECKS;
    }
    impl GenType for Evasions {
        type Checks = False;
        const TYPE: i32 = EVASIONS;
    }
    impl GenType for NonEvasions {
        type Checks = False;
        const TYPE: i32 = NON_EVASIONS;
    }
    impl GenType for Legal {
        type Checks = False;
        const TYPE: i32 = LEGAL;
    }
    #[derive(Clone, Copy)]
    pub struct ExtMove {
        pub m: Move,
        pub value: i32,
    }
    pub struct MoveList {
        list: [ExtMove; MAX_MOVES],
        idx: usize,
        len: usize,
    }
    impl MoveList {
        pub fn new<T: GenType>(pos: &Position) -> MoveList {
            let mut moves = MoveList {
                list: [ExtMove {
                    m: Move::NONE,
                    value: 0,
                }; MAX_MOVES],
                idx: 0,
                len: 0,
            };
            moves.len = generate::<T>(pos, &mut moves.list, 0);
            moves.idx = 0;
            moves
        }
        pub fn len(&self) -> usize {
            self.len
        }
        pub fn contains(&self, m: Move) -> bool {
            let mut i = 0;
            while i < self.len {
                if self.list[i].m == m {
                    return true;
                }
                i += 1;
            }
            return false;
        }
    }
    impl Iterator for MoveList {
        type Item = Move;
        fn next(&mut self) -> Option<Self::Item> {
            if self.idx == self.len {
                None
            } else {
                self.idx += 1;
                Some(self.list[self.idx - 1].m)
            }
        }
    }
    fn generate_castling<Cr: CastlingRightTrait, Checks: Bool, Chess960: Bool>(
        pos: &Position,
        list: &mut [ExtMove],
        idx: usize,
        us: Color,
    ) -> usize {
        let king_side = Cr::CR == WHITE_OO || Cr::CR == BLACK_OO;
        if pos.castling_impeded(Cr::CR) || !pos.has_castling_right(Cr::CR) {
            return idx;
        }
        let kfrom = pos.square(us, KING);
        let rfrom = pos.castling_rook_square(Cr::CR);
        let kto = relative_square(us, if king_side { Square::G1 } else { Square::C1 });
        let enemies = pos.pieces_c(!us);
        debug_assert!(pos.checkers() == 0);
        let direction = match Chess960::BOOL {
            true => {
                if kto > kfrom {
                    WEST
                } else {
                    EAST
                }
            }
            false => {
                if king_side {
                    WEST
                } else {
                    EAST
                }
            }
        };
        let mut s = kto;
        while s != kfrom {
            if pos.attackers_to(s) & enemies != 0 {
                return idx;
            }
            s += direction;
        }
        if Chess960::BOOL
            && attacks_bb(ROOK, kto, pos.pieces() ^ rfrom) & pos.pieces_cpp(!us, ROOK, QUEEN) != 0
        {
            return idx;
        }
        let m = Move::make_special(CASTLING, kfrom, rfrom);
        if Checks::BOOL && !pos.gives_check(m) {
            return idx;
        }
        list[idx].m = m;
        idx + 1
    }
    fn make_promotions<T: GenType>(
        list: &mut [ExtMove],
        mut idx: usize,
        to: Square,
        ksq: Square,
        direction: Direction,
    ) -> usize {
        if T::TYPE == CAPTURES || T::TYPE == EVASIONS || T::TYPE == NON_EVASIONS {
            list[idx].m = Move::make_prom(to - direction, to, QUEEN);
            idx += 1;
        }
        if T::TYPE == QUIETS || T::TYPE == EVASIONS || T::TYPE == NON_EVASIONS {
            list[idx].m = Move::make_prom(to - direction, to, ROOK);
            list[idx + 1].m = Move::make_prom(to - direction, to, BISHOP);
            list[idx + 2].m = Move::make_prom(to - direction, to, KNIGHT);
            idx += 3;
        }
        if T::TYPE == QUIET_CHECKS && pseudo_attacks(KNIGHT, to) & ksq != 0 {
            list[idx].m = Move::make_prom(to - direction, to, KNIGHT);
            idx += 1;
        }
        idx
    }
    fn generate_pawn_moves<Us: ColorTrait, T: GenType>(
        pos: &Position,
        list: &mut [ExtMove],
        mut idx: usize,
        target: Bitboard,
    ) -> usize {
        let us = Us::COLOR;
        let them = !us;
        let trank_8bb = if us == WHITE { RANK8_BB } else { RANK1_BB };
        let trank_7bb = if us == WHITE { RANK7_BB } else { RANK2_BB };
        let trank_3bb = if us == WHITE { RANK3_BB } else { RANK6_BB };
        let up = if us == WHITE { NORTH } else { SOUTH };
        let right = if us == WHITE { NORTH_EAST } else { SOUTH_WEST };
        let left = if us == WHITE { NORTH_WEST } else { SOUTH_EAST };
        let mut empty_squares = Bitboard(0);
        let pawns_on_7 = pos.pieces_cp(us, PAWN) & trank_7bb;
        let pawns_not_on_7 = pos.pieces_cp(us, PAWN) & !trank_7bb;
        let enemies = match T::TYPE {
            EVASIONS => pos.pieces_c(them) & target,
            CAPTURES => target,
            _ => pos.pieces_c(them),
        };
        if T::TYPE != CAPTURES {
            empty_squares = if T::TYPE == QUIETS || T::TYPE == QUIET_CHECKS {
                target
            } else {
                !pos.pieces()
            };
            let mut b1 = pawns_not_on_7.shift(up) & empty_squares;
            let mut b2 = (b1 & trank_3bb).shift(up) & empty_squares;
            if T::TYPE == EVASIONS {
                b1 &= target;
                b2 &= target;
            }
            if T::TYPE == QUIET_CHECKS {
                let ksq = pos.square(them, KING);
                b1 &= pos.attacks_from_pawn(ksq, them);
                b2 &= pos.attacks_from_pawn(ksq, them);
                let dc_candidates = pos.blockers_for_king(them);
                if pawns_not_on_7 & dc_candidates != 0 {
                    let dc1 = (pawns_not_on_7 & dc_candidates).shift(up)
                        & empty_squares
                        & !file_bb(ksq.file());
                    let dc2 = (dc1 & trank_3bb).shift(up) & empty_squares;
                    b1 |= dc1;
                    b2 |= dc2;
                }
            }
            for to in b1 {
                list[idx].m = Move::make(to - up, to);
                idx += 1;
            }
            for to in b2 {
                list[idx].m = Move::make(to - up - up, to);
                idx += 1;
            }
        }
        if pawns_on_7 != 0 && (T::TYPE != EVASIONS || target & trank_8bb != 0) {
            if T::TYPE == CAPTURES {
                empty_squares = !pos.pieces();
            }
            if T::TYPE == EVASIONS {
                empty_squares &= target;
            }
            let b1 = pawns_on_7.shift(right) & enemies;
            let b2 = pawns_on_7.shift(left) & enemies;
            let b3 = pawns_on_7.shift(up) & empty_squares;
            let ksq = pos.square(them, KING);
            for s in b1 {
                idx = make_promotions::<T>(list, idx, s, ksq, right);
            }
            for s in b2 {
                idx = make_promotions::<T>(list, idx, s, ksq, left);
            }
            for s in b3 {
                idx = make_promotions::<T>(list, idx, s, ksq, up);
            }
        }
        if T::TYPE == CAPTURES || T::TYPE == EVASIONS || T::TYPE == NON_EVASIONS {
            let b1 = pawns_not_on_7.shift(right) & enemies;
            let b2 = pawns_not_on_7.shift(left) & enemies;
            for to in b1 {
                list[idx].m = Move::make(to - right, to);
                idx += 1;
            }
            for to in b2 {
                list[idx].m = Move::make(to - left, to);
                idx += 1;
            }
            if pos.ep_square() != Square::NONE {
                debug_assert!(pos.ep_square().rank() == relative_rank(us, RANK_6));
                if T::TYPE == EVASIONS && target & (pos.ep_square() - up) == 0 {
                    return idx;
                }
                let b1 = pawns_not_on_7 & pos.attacks_from_pawn(pos.ep_square(), them);
                debug_assert!(b1 != 0);
                for to in b1 {
                    list[idx].m = Move::make_special(ENPASSANT, to, pos.ep_square());
                    idx += 1;
                }
            }
        }
        idx
    }
    fn generate_moves<Pt: PieceTypeTrait, Checks: Bool>(
        pos: &Position,
        list: &mut [ExtMove],
        mut idx: usize,
        us: Color,
        target: Bitboard,
    ) -> usize {
        debug_assert!(Pt::TYPE != KING && Pt::TYPE != PAWN);
        for from in pos.square_list(us, Pt::TYPE) {
            if Checks::BOOL {
                if (Pt::TYPE == BISHOP || Pt::TYPE == ROOK || Pt::TYPE == QUEEN)
                    && pseudo_attacks(Pt::TYPE, from) & target & pos.check_squares(Pt::TYPE) == 0
                {
                    continue;
                }
                if pos.blockers_for_king(!us) & from != 0 {
                    continue;
                }
            }
            let mut b = pos.attacks_from(Pt::TYPE, from) & target;
            if Checks::BOOL {
                b &= pos.check_squares(Pt::TYPE);
            }
            for to in b {
                list[idx].m = Move::make(from, to);
                idx += 1;
            }
        }
        idx
    }
    fn generate_all<Us: ColorTrait, T: GenType>(
        pos: &Position,
        list: &mut [ExtMove],
        mut idx: usize,
        target: Bitboard,
    ) -> usize {
        let us = Us::COLOR;
        idx = generate_pawn_moves::<Us, T>(pos, list, idx, target);
        idx = generate_moves::<Knight, T::Checks>(pos, list, idx, us, target);
        idx = generate_moves::<Bishop, T::Checks>(pos, list, idx, us, target);
        idx = generate_moves::<Rook, T::Checks>(pos, list, idx, us, target);
        idx = generate_moves::<Queen, T::Checks>(pos, list, idx, us, target);
        if T::TYPE != QUIET_CHECKS && T::TYPE != EVASIONS {
            let ksq = pos.square(us, KING);
            let b = pos.attacks_from(KING, ksq) & target;
            for to in b {
                list[idx].m = Move::make(ksq, to);
                idx += 1;
            }
        }
        if T::TYPE != CAPTURES && T::TYPE != EVASIONS && pos.can_castle(us) {
            if pos.is_chess960() {
                idx = generate_castling::<Us::KingSide, T::Checks, True>(pos, list, idx, us);
                idx = generate_castling::<Us::QueenSide, T::Checks, True>(pos, list, idx, us);
            } else {
                idx = generate_castling::<Us::KingSide, T::Checks, False>(pos, list, idx, us);
                idx = generate_castling::<Us::QueenSide, T::Checks, False>(pos, list, idx, us);
            }
        }
        idx
    }
    pub fn generate_quiet_checks(pos: &Position, list: &mut [ExtMove], mut idx: usize) -> usize {
        debug_assert!(pos.checkers() == 0);
        let us = pos.side_to_move();
        let dc = pos.blockers_for_king(!us) & pos.pieces_c(us);
        for from in dc {
            let pt = pos.piece_on(from).piece_type();
            if pt == PAWN {
                continue;
            }
            let mut b = pos.attacks_from(pt, from) & !pos.pieces();
            if pt == KING {
                b &= !pseudo_attacks(QUEEN, pos.square(!us, KING));
            }
            for to in b {
                list[idx].m = Move::make(from, to);
                idx += 1;
            }
        }
        if us == WHITE {
            generate_all::<White, QuietChecks>(pos, list, idx, !pos.pieces())
        } else {
            generate_all::<Black, QuietChecks>(pos, list, idx, !pos.pieces())
        }
    }
    fn generate_evasions(pos: &Position, list: &mut [ExtMove], mut idx: usize) -> usize {
        debug_assert!(pos.checkers() != 0);
        let us = pos.side_to_move();
        let ksq = pos.square(us, KING);
        let mut slider_attacks = Bitboard(0);
        let sliders = pos.checkers() & !pos.pieces_pp(KNIGHT, PAWN);
        for check_sq in sliders {
            slider_attacks |= line_bb(check_sq, ksq) ^ check_sq;
        }
        let b = pos.attacks_from(KING, ksq) & !pos.pieces_c(us) & !slider_attacks;
        for to in b {
            list[idx].m = Move::make(ksq, to);
            idx += 1;
        }
        if more_than_one(pos.checkers()) {
            return idx;
        }
        let check_sq = lsb(pos.checkers());
        let target = between_bb(check_sq, ksq) | check_sq;
        if us == WHITE {
            generate_all::<White, Evasions>(pos, list, idx, target)
        } else {
            generate_all::<Black, Evasions>(pos, list, idx, target)
        }
    }
    fn generate_legal(pos: &Position, list: &mut [ExtMove], idx: usize) -> usize {
        let us = pos.side_to_move();
        let pinned = pos.blockers_for_king(us) & pos.pieces_c(us);
        let ksq = pos.square(us, KING);
        let pseudo = if pos.checkers() != 0 {
            generate::<Evasions>(pos, list, idx)
        } else {
            generate::<NonEvasions>(pos, list, idx)
        };
        let mut legal = idx;
        for i in idx..pseudo {
            let m = list[i].m;
            if (pinned == 0 && m.from() != ksq && m.move_type() != ENPASSANT) || pos.legal(m) {
                list[legal].m = m;
                legal += 1;
            }
        }
        legal
    }
    pub fn generate<T: GenType>(pos: &Position, list: &mut [ExtMove], idx: usize) -> usize {
        match T::TYPE {
            QUIET_CHECKS => generate_quiet_checks(pos, list, idx),
            EVASIONS => generate_evasions(pos, list, idx),
            LEGAL => generate_legal(pos, list, idx),
            _ => {
                debug_assert!(pos.checkers() == 0);
                let us = pos.side_to_move();
                let target = match T::TYPE {
                    CAPTURES => pos.pieces_c(!us),
                    QUIETS => !pos.pieces(),
                    NON_EVASIONS => !pos.pieces_c(us),
                    _ => Bitboard(0),
                };
                if us == WHITE {
                    generate_all::<White, T>(pos, list, idx, target)
                } else {
                    generate_all::<Black, T>(pos, list, idx, target)
                }
            }
        }
    }
}
pub mod movepick {
    use movegen::*;
    use position::Position;
    use search;
    use std::cell::Cell;
    use types::*;
    pub struct ButterflyHistory {
        v: [[Cell<i16>; 4096]; 2],
    }
    impl ButterflyHistory {
        pub fn get(&self, c: Color, m: Move) -> i32 {
            self.v[c.0 as usize][m.from_to() as usize].get() as i32
        }
        pub fn update(&self, c: Color, m: Move, bonus: i32) {
            let entry = &self.v[c.0 as usize][m.from_to() as usize];
            let mut val = entry.get();
            val += (bonus * 32 - val as i32 * bonus.abs() / 324) as i16;
            entry.set(val);
        }
    }
    pub struct PieceToHistory {
        v: [[Cell<i16>; 64]; 16],
    }
    impl PieceToHistory {
        pub fn get(&self, pc: Piece, s: Square) -> i32 {
            self.v[pc.0 as usize][s.0 as usize].get() as i32
        }
        pub fn update(&self, pc: Piece, s: Square, bonus: i32) {
            let entry = &self.v[pc.0 as usize][s.0 as usize];
            let mut val = entry.get();
            val += (bonus * 32 - val as i32 * bonus.abs() / 936) as i16;
            entry.set(val);
        }
    }
    pub struct CapturePieceToHistory {
        v: [[[Cell<i16>; 8]; 64]; 16],
    }
    impl CapturePieceToHistory {
        pub fn get(&self, pc: Piece, to: Square, cap: PieceType) -> i32 {
            self.v[pc.0 as usize][to.0 as usize][cap.0 as usize].get() as i32
        }
        pub fn update(&self, pc: Piece, to: Square, cap: PieceType, bonus: i32) {
            let entry = &self.v[pc.0 as usize][to.0 as usize][cap.0 as usize];
            let mut val = entry.get();
            val += (bonus * 2 - val as i32 * bonus.abs() / 324) as i16;
            entry.set(val);
        }
    }
    pub struct CounterMoveHistory {
        v: [[Cell<Move>; 64]; 16],
    }
    impl CounterMoveHistory {
        pub fn get(&self, pc: Piece, s: Square) -> Move {
            self.v[pc.0 as usize][s.0 as usize].get()
        }
        pub fn set(&self, pc: Piece, s: Square, m: Move) {
            self.v[pc.0 as usize][s.0 as usize].set(m);
        }
    }
    pub struct ContinuationHistory {
        v: [[PieceToHistory; 64]; 16],
    }
    impl ContinuationHistory {
        pub fn get(&self, pc: Piece, s: Square) -> &'static PieceToHistory {
            let p: *const PieceToHistory = &self.v[pc.0 as usize][s.0 as usize];
            unsafe { &*p }
        }
        pub fn init(&self) {
            let p = self.get(Piece(0), Square(0));
            for pc in 0..16 {
                for s in 0..64 {
                    p.v[pc][s].set(search::CM_THRESHOLD as i16 - 1);
                }
            }
        }
    }
    pub struct MovePicker {
        cur: usize,
        end_moves: usize,
        end_bad_captures: usize,
        stage: i32,
        depth: Depth,
        tt_move: Move,
        countermove: Move,
        killers: [Move; 2],
        cmh: [&'static PieceToHistory; 3],
        list: [ExtMove; MAX_MOVES as usize],
    }
    pub struct MovePickerQ {
        cur: usize,
        end_moves: usize,
        stage: i32,
        depth: Depth,
        tt_move: Move,
        recapture_square: Square,
        list: [ExtMove; MAX_MOVES as usize],
    }
    pub struct MovePickerPC {
        cur: usize,
        end_moves: usize,
        stage: i32,
        tt_move: Move,
        threshold: Value,
        list: [ExtMove; MAX_MOVES as usize],
    }
    const MAIN_SEARCH: i32 = 0;
    const CAPTURES_INIT: i32 = 1;
    const GOOD_CAPTURES: i32 = 2;
    const KILLERS: i32 = 3;
    const COUNTERMOVE: i32 = 4;
    const QUIET_INIT: i32 = 5;
    const QUIET: i32 = 6;
    const BAD_CAPTURES: i32 = 7;
    const EVASION: i32 = 8;
    const EVASIONS_INIT: i32 = 9;
    const ALL_EVASIONS: i32 = 10;
    const PROBCUT: i32 = 11;
    const PROBCUT_INIT: i32 = 12;
    const PROBCUT_CAPTURES: i32 = 13;
    const QSEARCH: i32 = 14;
    const QCAPTURES_INIT: i32 = 15;
    const QCAPTURES: i32 = 16;
    const QCHECKS: i32 = 17;
    fn partial_insertion_sort(list: &mut [ExtMove], limit: i32) {
        let mut sorted_end = 0;
        for p in 1..list.len() {
            if list[p].value >= limit {
                let tmp = list[p];
                sorted_end += 1;
                list[p] = list[sorted_end];
                let mut q = sorted_end;
                while q > 0 && list[q - 1].value < tmp.value {
                    list[q] = list[q - 1];
                    q -= 1;
                }
                list[q] = tmp;
            }
        }
    }
    fn pick_best(list: &mut [ExtMove]) -> Move {
        let mut q = 0;
        for p in 1..list.len() {
            if list[p].value > list[q].value {
                q = p;
            }
        }
        list.swap(0, q);
        list[0].m
    }
    fn score_captures(pos: &Position, list: &mut [ExtMove]) {
        for m in list.iter_mut() {
            m.value = piece_value(MG, pos.piece_on(m.m.to())).0
                + pos.capture_history.get(
                    pos.moved_piece(m.m),
                    m.m.to(),
                    pos.piece_on(m.m.to()).piece_type(),
                );
        }
    }
    fn score_quiets(pos: &Position, mp: &mut MovePicker) {
        let list = &mut mp.list[mp.cur..mp.end_moves];
        for m in list.iter_mut() {
            m.value = pos.main_history.get(pos.side_to_move(), m.m)
                + mp.cmh[0].get(pos.moved_piece(m.m), m.m.to())
                + mp.cmh[1].get(pos.moved_piece(m.m), m.m.to())
                + mp.cmh[2].get(pos.moved_piece(m.m), m.m.to());
        }
    }
    fn score_evasions(pos: &Position, list: &mut [ExtMove]) {
        for m in list.iter_mut() {
            m.value = if pos.capture(m.m) {
                piece_value(MG, pos.piece_on(m.m.to())).0
                    - pos.moved_piece(m.m).piece_type().0 as i32
            } else {
                pos.main_history.get(pos.side_to_move(), m.m) - (1 << 28)
            }
        }
    }
    impl MovePicker {
        pub fn new(pos: &Position, ttm: Move, d: Depth, ss: &[search::Stack]) -> MovePicker {
            let mut stage = if pos.checkers() != 0 {
                EVASION
            } else {
                MAIN_SEARCH
            };
            let tt_move = if ttm != Move::NONE && pos.pseudo_legal(ttm) {
                ttm
            } else {
                Move::NONE
            };
            if tt_move == Move::NONE {
                stage += 1;
            }
            let prev_sq = ss[4].current_move.to();
            MovePicker {
                cur: 0,
                end_moves: 0,
                end_bad_captures: 0,
                stage: stage,
                tt_move: ttm,
                countermove: pos.counter_moves.get(pos.piece_on(prev_sq), prev_sq),
                killers: [ss[5].killers[0], ss[5].killers[1]],
                depth: d,
                cmh: [ss[4].cont_history, ss[3].cont_history, ss[1].cont_history],
                list: [ExtMove {
                    m: Move::NONE,
                    value: 0,
                }; MAX_MOVES as usize],
            }
        }
        pub fn next_move(&mut self, pos: &Position, skip_quiets: bool) -> Move {
            loop {
                match self.stage {
                    MAIN_SEARCH | EVASION => {
                        self.stage += 1;
                        return self.tt_move;
                    }
                    CAPTURES_INIT => {
                        self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                        score_captures(pos, &mut self.list[..self.end_moves]);
                        self.stage += 1;
                    }
                    GOOD_CAPTURES => {
                        while self.cur < self.end_moves {
                            let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                            self.cur += 1;
                            if m != self.tt_move {
                                if pos.see_ge(m, Value(-55 * self.list[self.cur - 1].value / 1024))
                                {
                                    return m;
                                }
                                self.list[self.end_bad_captures].m = m;
                                self.end_bad_captures += 1;
                            }
                        }
                        self.stage += 1;
                        let m = self.killers[0];
                        if m != Move::NONE
                            && m != self.tt_move
                            && pos.pseudo_legal(m)
                            && !pos.capture(m)
                        {
                            return m;
                        }
                    }
                    KILLERS => {
                        self.stage += 1;
                        let m = self.killers[1];
                        if m != Move::NONE
                            && m != self.tt_move
                            && pos.pseudo_legal(m)
                            && !pos.capture(m)
                        {
                            return m;
                        }
                    }
                    COUNTERMOVE => {
                        self.stage += 1;
                        let m = self.countermove;
                        if m != Move::NONE
                            && m != self.tt_move
                            && m != self.killers[0]
                            && m != self.killers[1]
                            && pos.pseudo_legal(m)
                            && !pos.capture(m)
                        {
                            return m;
                        }
                    }
                    QUIET_INIT => {
                        self.cur = self.end_bad_captures;
                        self.end_moves = generate::<Quiets>(pos, &mut self.list, self.cur);
                        score_quiets(pos, self);
                        partial_insertion_sort(
                            &mut self.list[self.cur..self.end_moves],
                            -4000 * self.depth / ONE_PLY,
                        );
                        self.stage += 1;
                    }
                    QUIET => {
                        if !skip_quiets {
                            while self.cur < self.end_moves {
                                let m = self.list[self.cur].m;
                                self.cur += 1;
                                if m != self.tt_move
                                    && m != self.killers[0]
                                    && m != self.killers[1]
                                    && m != self.countermove
                                {
                                    return m;
                                }
                            }
                        }
                        self.stage += 1;
                        self.cur = 0;
                    }
                    BAD_CAPTURES => {
                        if self.cur < self.end_bad_captures {
                            let m = self.list[self.cur].m;
                            self.cur += 1;
                            return m;
                        }
                        break;
                    }
                    EVASIONS_INIT => {
                        self.cur = 0;
                        self.end_moves = generate::<Evasions>(pos, &mut self.list, 0);
                        score_evasions(pos, &mut self.list[..self.end_moves]);
                        self.stage += 1;
                    }
                    ALL_EVASIONS => {
                        while self.cur < self.end_moves {
                            let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                            self.cur += 1;
                            if m != self.tt_move {
                                return m;
                            }
                        }
                        break;
                    }
                    _ => {
                        panic!("movepick")
                    }
                }
            }
            Move::NONE
        }
    }
    impl MovePickerQ {
        pub fn new(pos: &Position, ttm: Move, d: Depth, s: Square) -> MovePickerQ {
            let mut stage = if pos.checkers() != 0 {
                EVASION
            } else {
                QSEARCH
            };
            let tt_move = if ttm != Move::NONE
                && pos.pseudo_legal(ttm)
                && (d > Depth::QS_RECAPTURES || ttm.to() == s)
            {
                ttm
            } else {
                stage += 1;
                Move::NONE
            };
            MovePickerQ {
                cur: 0,
                end_moves: 0,
                stage: stage,
                depth: d,
                tt_move: tt_move,
                recapture_square: s,
                list: [ExtMove {
                    m: Move::NONE,
                    value: 0,
                }; MAX_MOVES as usize],
            }
        }
        pub fn next_move(&mut self, pos: &Position) -> Move {
            loop {
                match self.stage {
                    EVASION | QSEARCH => {
                        self.stage += 1;
                        return self.tt_move;
                    }
                    EVASIONS_INIT => {
                        self.cur = 0;
                        self.end_moves = generate::<Evasions>(pos, &mut self.list, 0);
                        score_evasions(pos, &mut self.list[..self.end_moves]);
                        self.stage += 1;
                    }
                    ALL_EVASIONS => {
                        while self.cur < self.end_moves {
                            let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                            self.cur += 1;
                            if m != self.tt_move {
                                return m;
                            }
                        }
                        break;
                    }
                    QCAPTURES_INIT => {
                        self.cur = 0;
                        self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                        score_captures(pos, &mut self.list[..self.end_moves]);
                        self.stage += 1;
                    }
                    QCAPTURES => {
                        while self.cur < self.end_moves {
                            let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                            self.cur += 1;
                            if m != self.tt_move
                                && (self.depth > Depth::QS_RECAPTURES
                                    || m.to() == self.recapture_square)
                            {
                                return m;
                            }
                        }
                        if self.depth <= Depth::QS_NO_CHECKS {
                            break;
                        }
                        self.cur = 0;
                        self.end_moves = generate::<QuietChecks>(pos, &mut self.list, 0);
                        self.stage += 1;
                    }
                    QCHECKS => {
                        while self.cur < self.end_moves {
                            let m = self.list[self.cur].m;
                            self.cur += 1;
                            if m != self.tt_move {
                                return m;
                            }
                        }
                        break;
                    }
                    _ => {
                        panic!("movepick_q")
                    }
                }
            }
            Move::NONE
        }
    }
    impl MovePickerPC {
        pub fn new(pos: &Position, ttm: Move, threshold: Value) -> MovePickerPC {
            let tt_move;
            let stage;
            if ttm != Move::NONE
                && pos.pseudo_legal(ttm)
                && pos.capture(ttm)
                && pos.see_ge(ttm, threshold)
            {
                tt_move = ttm;
                stage = PROBCUT;
            } else {
                tt_move = Move::NONE;
                stage = PROBCUT + 1;
            }
            MovePickerPC {
                cur: 0,
                end_moves: 0,
                stage: stage,
                tt_move: tt_move,
                threshold: threshold,
                list: [ExtMove {
                    m: Move::NONE,
                    value: 0,
                }; MAX_MOVES as usize],
            }
        }
        pub fn next_move(&mut self, pos: &Position) -> Move {
            loop {
                match self.stage {
                    PROBCUT => {
                        self.stage += 1;
                        return self.tt_move;
                    }
                    PROBCUT_INIT => {
                        self.cur = 0;
                        self.end_moves = generate::<Captures>(pos, &mut self.list, 0);
                        score_captures(pos, &mut self.list[..self.end_moves]);
                        self.stage += 1;
                    }
                    PROBCUT_CAPTURES => {
                        while self.cur < self.end_moves {
                            let m = pick_best(&mut self.list[self.cur..self.end_moves]);
                            self.cur += 1;
                            if m != self.tt_move && pos.see_ge(m, self.threshold) {
                                return m;
                            }
                        }
                        break;
                    }
                    _ => {
                        panic!("movepick_pc")
                    }
                }
            }
            Move::NONE
        }
    }
}
pub mod pawns {
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
        e.pawns_on_squares[us.0 as usize][BLACK.0 as usize] =
            popcount(our_pawns & DARK_SQUARES) as i32;
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
}
pub mod position {
    use bitboard::*;
    use material;
    use movegen::*;
    use movepick::*;
    use pawns;
    use psqt;
    use search;
    use std;
    use std::sync::Arc;
    use tb;
    use threads::ThreadCtrl;
    use types::*;
    use uci;
    pub mod zobrist {
        use bitboard;
        use misc;
        use types::*;
        static mut PSQ: [[Key; 64]; 16] = [[Key(0); 64]; 16];
        static mut ENPASSANT: [Key; 8] = [Key(0); 8];
        static mut CASTLING: [Key; 16] = [Key(0); 16];
        static mut SIDE: Key = Key(0);
        static mut NO_PAWNS: Key = Key(0);
        pub fn psq(pc: Piece, s: Square) -> Key {
            unsafe { PSQ[pc.0 as usize][s.0 as usize] }
        }
        pub fn material(pc: Piece, num: i32) -> Key {
            unsafe { PSQ[pc.0 as usize][num as usize] }
        }
        pub fn enpassant(f: File) -> Key {
            unsafe { ENPASSANT[f as usize] }
        }
        pub fn castling(cr: CastlingRight) -> Key {
            unsafe { CASTLING[cr.0 as usize] }
        }
        pub fn side() -> Key {
            unsafe { SIDE }
        }
        pub fn no_pawns() -> Key {
            unsafe { NO_PAWNS }
        }
        pub fn init() {
            let mut rng = misc::Prng::new(1070372);
            unsafe {
                for i in 1..15 {
                    if i != 7 && i != 8 {
                        for s in 0..64 {
                            PSQ[i][s] = Key(rng.rand64());
                        }
                    }
                }
                for f in 0..8 {
                    ENPASSANT[f] = Key(rng.rand64());
                }
                for cr in 0..16 {
                    let b = bitboard::Bitboard(cr);
                    for s in b {
                        let k = CASTLING[1usize << s.0];
                        CASTLING[cr as usize] ^= if k.0 != 0 { k } else { Key(rng.rand64()) };
                    }
                }
                SIDE = Key(rng.rand64());
                NO_PAWNS = Key(rng.rand64());
            }
        }
    }
    #[derive(Clone)]
    pub struct StateInfo {
        pub pawn_key: Key,
        pub material_key: Key,
        pub non_pawn_material: [Value; 2],
        pub castling_rights: CastlingRight,
        pub rule50: i32,
        pub plies_from_null: i32,
        pub psq: Score,
        pub ep_square: Square,
        pub key: Key,
        pub checkers_bb: Bitboard,
        pub captured_piece: Piece,
        pub blockers_for_king: [Bitboard; 2],
        pub pinners_for_king: [Bitboard; 2],
        pub check_squares: [Bitboard; 8],
    }
    impl StateInfo {
        pub fn new() -> StateInfo {
            StateInfo {
                pawn_key: Key(0),
                material_key: Key(0),
                non_pawn_material: [Value::ZERO; 2],
                castling_rights: CastlingRight(0),
                rule50: 0,
                plies_from_null: 0,
                psq: Score::ZERO,
                ep_square: Square::NONE,
                key: Key(0),
                checkers_bb: Bitboard(0),
                captured_piece: NO_PIECE,
                blockers_for_king: [Bitboard(0); 2],
                pinners_for_king: [Bitboard(0); 2],
                check_squares: [Bitboard(0); 8],
            }
        }
        pub fn copy(&self) -> StateInfo {
            StateInfo {
                pawn_key: self.pawn_key,
                material_key: self.material_key,
                non_pawn_material: self.non_pawn_material,
                castling_rights: self.castling_rights,
                rule50: self.rule50,
                plies_from_null: self.plies_from_null,
                psq: self.psq,
                ep_square: self.ep_square,
                key: Key(0),
                checkers_bb: Bitboard(0),
                captured_piece: NO_PIECE,
                blockers_for_king: [Bitboard(0); 2],
                pinners_for_king: [Bitboard(0); 2],
                check_squares: [Bitboard(0); 8],
            }
        }
    }
    pub struct Position {
        board: [Piece; 64],
        by_color_bb: [Bitboard; 2],
        by_type_bb: [Bitboard; 8],
        piece_count: [i32; 16],
        piece_list: [[Square; 16]; 16],
        index: [i32; 64],
        castling_rights_mask: [CastlingRight; 64],
        castling_rook_square: [Square; 16],
        castling_path: [Bitboard; 16],
        game_ply: i32,
        side_to_move: Color,
        states: Vec<StateInfo>,
        chess960: bool,
        pub failed_low: bool,
        pub best_move_changes: f64,
        pub previous_time_reduction: f64,
        pub previous_score: Value,
        pub calls_cnt: i32,
        pub thread_ctrl: Option<Arc<ThreadCtrl>>,
        pub is_main: bool,
        pub thread_idx: i32,
        pub pv_idx: usize,
        pub pv_last: usize,
        pub sel_depth: i32,
        pub nmp_ply: i32,
        pub nmp_odd: i32,
        pub nodes: u64,
        pub tb_hits: u64,
        pub completed_depth: Depth,
        pub root_moves: search::RootMoves,
        pub pawns_table: Vec<std::cell::UnsafeCell<pawns::Entry>>,
        pub material_table: Vec<std::cell::UnsafeCell<material::Entry>>,
        pub counter_moves: CounterMoveHistory,
        pub main_history: ButterflyHistory,
        pub capture_history: CapturePieceToHistory,
        pub cont_history: ContinuationHistory,
    }
    impl Position {
        pub fn new() -> Position {
            Position {
                board: [NO_PIECE; 64],
                by_color_bb: [Bitboard(0); 2],
                by_type_bb: [Bitboard(0); 8],
                piece_count: [0; 16],
                piece_list: [[Square::NONE; 16]; 16],
                index: [0; 64],
                castling_rights_mask: [CastlingRight(0); 64],
                castling_rook_square: [Square::NONE; 16],
                castling_path: [Bitboard(0); 16],
                game_ply: 0,
                side_to_move: WHITE,
                states: Vec::new(),
                chess960: false,
                failed_low: false,
                best_move_changes: 0.0,
                previous_time_reduction: 0.0,
                previous_score: Value::ZERO,
                calls_cnt: 0,
                thread_ctrl: None,
                is_main: false,
                thread_idx: 0,
                pv_idx: 0,
                pv_last: 0,
                sel_depth: 0,
                nmp_ply: 0,
                nmp_odd: 0,
                nodes: 0,
                tb_hits: 0,
                completed_depth: Depth::ZERO,
                root_moves: Vec::new(),
                pawns_table: Vec::new(),
                material_table: Vec::new(),
                counter_moves: unsafe { std::mem::zeroed() },
                main_history: unsafe { std::mem::zeroed() },
                capture_history: unsafe { std::mem::zeroed() },
                cont_history: unsafe { std::mem::zeroed() },
            }
        }
        pub fn init_states(&mut self) {
            self.states.truncate(0);
            self.states.push(StateInfo::new());
        }
        fn st(&self) -> &StateInfo {
            self.states.last().unwrap()
        }
        fn st_mut(&mut self) -> &mut StateInfo {
            self.states.last_mut().unwrap()
        }
        pub fn side_to_move(&self) -> Color {
            self.side_to_move
        }
        pub fn empty(&self, s: Square) -> bool {
            self.board[s.0 as usize] == NO_PIECE
        }
        pub fn piece_on(&self, s: Square) -> Piece {
            self.board[s.0 as usize]
        }
        pub fn moved_piece(&self, m: Move) -> Piece {
            self.board[m.from().0 as usize]
        }
        pub fn pieces(&self) -> Bitboard {
            self.by_type_bb[ALL_PIECES.0 as usize]
        }
        pub fn pieces_p(&self, pt: PieceType) -> Bitboard {
            self.by_type_bb[pt.0 as usize]
        }
        pub fn pieces_pp(&self, pt1: PieceType, pt2: PieceType) -> Bitboard {
            self.pieces_p(pt1) | self.pieces_p(pt2)
        }
        pub fn pieces_c(&self, c: Color) -> Bitboard {
            self.by_color_bb[c.0 as usize]
        }
        pub fn pieces_cp(&self, c: Color, pt: PieceType) -> Bitboard {
            self.pieces_c(c) & self.pieces_p(pt)
        }
        pub fn pieces_cpp(&self, c: Color, pt1: PieceType, pt2: PieceType) -> Bitboard {
            self.pieces_c(c) & self.pieces_pp(pt1, pt2)
        }
        pub fn count(&self, c: Color, pt: PieceType) -> i32 {
            self.piece_count[Piece::make(c, pt).0 as usize]
        }
        pub fn squares(&self, c: Color, pt: PieceType) -> &[Square] {
            &self.piece_list[Piece::make(c, pt).0 as usize]
        }
        pub fn square_list(&self, c: Color, pt: PieceType) -> SquareList {
            SquareList::construct(self.squares(c, pt))
        }
        pub fn square(&self, c: Color, pt: PieceType) -> Square {
            self.squares(c, pt)[0]
        }
        pub fn ep_square(&self) -> Square {
            self.st().ep_square
        }
        pub fn has_castling_right(&self, cr: CastlingRight) -> bool {
            self.st().castling_rights & cr != 0
        }
        pub fn castling_rights(&self, c: Color) -> CastlingRight {
            self.st().castling_rights & CastlingRight(3 << (2 * c.0))
        }
        pub fn can_castle(&self, c: Color) -> bool {
            self.castling_rights(c) != 0
        }
        pub fn castling_impeded(&self, cr: CastlingRight) -> bool {
            self.pieces() & self.castling_path[cr.0 as usize] != Bitboard(0)
        }
        pub fn castling_rook_square(&self, cr: CastlingRight) -> Square {
            self.castling_rook_square[cr.0 as usize]
        }
        pub fn attacks_from_pawn(&self, s: Square, c: Color) -> Bitboard {
            pawn_attacks(c, s)
        }
        pub fn attacks_from(&self, pt: PieceType, s: Square) -> Bitboard {
            debug_assert!(pt != PAWN);
            match pt {
                BISHOP | ROOK => attacks_bb(pt, s, self.pieces()),
                QUEEN => self.attacks_from(ROOK, s) | self.attacks_from(BISHOP, s),
                _ => pseudo_attacks(pt, s),
            }
        }
        pub fn attackers_to_occ(&self, s: Square, occ: Bitboard) -> Bitboard {
            (self.attacks_from_pawn(s, BLACK) & self.pieces_cp(WHITE, PAWN))
                | (self.attacks_from_pawn(s, WHITE) & self.pieces_cp(BLACK, PAWN))
                | (self.attacks_from(KNIGHT, s) & self.pieces_p(KNIGHT))
                | (attacks_bb(ROOK, s, occ) & self.pieces_pp(ROOK, QUEEN))
                | (attacks_bb(BISHOP, s, occ) & self.pieces_pp(BISHOP, QUEEN))
                | (self.attacks_from(KING, s) & self.pieces_p(KING))
        }
        pub fn attackers_to(&self, s: Square) -> Bitboard {
            self.attackers_to_occ(s, self.by_type_bb[ALL_PIECES.0 as usize])
        }
        pub fn checkers(&self) -> Bitboard {
            self.st().checkers_bb
        }
        pub fn blockers_for_king(&self, c: Color) -> Bitboard {
            self.st().blockers_for_king[c.0 as usize]
        }
        pub fn pinners_for_king(&self, c: Color) -> Bitboard {
            self.st().pinners_for_king[c.0 as usize]
        }
        pub fn check_squares(&self, pt: PieceType) -> Bitboard {
            self.st().check_squares[pt.0 as usize]
        }
        pub fn pawn_passed(&self, c: Color, s: Square) -> bool {
            self.pieces_cp(!c, PAWN) & passed_pawn_mask(c, s) == 0
        }
        pub fn advanced_pawn_push(&self, m: Move) -> bool {
            self.moved_piece(m).piece_type() == PAWN
                && m.from().relative_rank(self.side_to_move()) > RANK_4
        }
        pub fn key(&self) -> Key {
            self.st().key
        }
        pub fn pawn_key(&self) -> Key {
            self.st().pawn_key
        }
        pub fn material_key(&self) -> Key {
            self.st().material_key
        }
        pub fn psq_score(&self) -> Score {
            self.st().psq
        }
        pub fn non_pawn_material_c(&self, c: Color) -> Value {
            self.st().non_pawn_material[c.0 as usize]
        }
        pub fn non_pawn_material(&self) -> Value {
            self.non_pawn_material_c(WHITE) + self.non_pawn_material_c(BLACK)
        }
        pub fn game_ply(&self) -> i32 {
            self.game_ply
        }
        pub fn rule50_count(&self) -> i32 {
            self.st().rule50
        }
        pub fn opposite_bishops(&self) -> bool {
            self.piece_count[W_BISHOP.0 as usize] == 1
                && self.piece_count[B_BISHOP.0 as usize] == 1
                && opposite_colors(self.square(WHITE, BISHOP), self.square(BLACK, BISHOP))
        }
        pub fn is_chess960(&self) -> bool {
            self.chess960
        }
        pub fn capture_or_promotion(&self, m: Move) -> bool {
            debug_assert!(m.is_ok());
            if m.move_type() != NORMAL {
                m.move_type() != CASTLING
            } else {
                !self.empty(m.to())
            }
        }
        pub fn capture(&self, m: Move) -> bool {
            debug_assert!(m.is_ok());
            (!self.empty(m.to()) && m.move_type() != CASTLING) || m.move_type() == ENPASSANT
        }
        pub fn captured_piece(&self) -> Piece {
            self.st().captured_piece
        }
        pub const PIECE_TO_CHAR: &'static str = " PNBRQK  pnbrqk";
        pub fn print(&mut self) {
            println!("\n +---+---+---+---+---+---+---+---+");
            for r in (0..8).rev() {
                for f in 0..8 {
                    print!(
                        " | {}",
                        Position::PIECE_TO_CHAR
                            .chars()
                            .nth(self.piece_on(Square::make(f, r)).0 as usize)
                            .unwrap()
                    );
                }
                println!(" |\n +---+---+---+---+---+---+---+---+");
            }
            println!(
                "\nFen: {}\nKey: {}\nCheckers: {}",
                self.fen(),
                self.key(),
                self.checkers()
            );
            if tb::max_cardinality() >= popcount(self.pieces())
                && !self.has_castling_right(ANY_CASTLING)
            {
                let mut s1 = 1;
                let mut s2 = 1;
                let wdl = tb::probe_wdl(self, &mut s1);
                let dtz = tb::probe_dtz(self, &mut s2);
                println!(
                    "Tablebases WDL: {} ({})\nTablebases DTZ: {} ({})",
                    wdl, s1, dtz, s2
                );
                if s1 != 0 {
                    let dtm = tb::probe_dtm(self, wdl, &mut s1);
                    println!("Tablebases DTM: {} ({})", uci::value(dtm), s1);
                }
            }
        }
        pub fn set(&mut self, fen_str: &str, is_chess960: bool) {
            for c in 0..2 {
                self.by_color_bb[c] = Bitboard(0);
            }
            for t in 0..8 {
                self.by_type_bb[t] = Bitboard(0);
            }
            for i in 0..16 {
                self.piece_count[i] = 0;
                self.castling_path[i] = Bitboard(0);
                self.castling_rook_square[i] = Square::NONE;
                for j in 0..16 {
                    self.piece_list[i][j] = Square::NONE;
                }
            }
            for i in 0..64 {
                self.board[i] = NO_PIECE;
                self.castling_rights_mask[i] = CastlingRight(0);
            }
            let mut iter = fen_str.split_whitespace();
            let pieces = iter.next().unwrap();
            let mut sq = Square::A8;
            for c in pieces.chars() {
                if let Some(d) = c.to_digit(10) {
                    sq += (d as i32) * EAST;
                } else if c == '/' {
                    sq += 2 * SOUTH;
                } else if let Some(idx) = Position::PIECE_TO_CHAR.find(c) {
                    self.put_piece(Piece(idx as u32), sq);
                    sq += EAST;
                }
            }
            let color = iter.next().unwrap();
            self.side_to_move = if color == "b" { BLACK } else { WHITE };
            let castling = iter.next().unwrap();
            if castling != "-" {
                for c in castling.chars() {
                    let color = if c.is_lowercase() { BLACK } else { WHITE };
                    let rook = Piece::make(color, ROOK);
                    let side = c.to_uppercase().next().unwrap();
                    let mut rsq;
                    if side == 'K' {
                        rsq = Square::H1.relative(color);
                        while self.piece_on(rsq) != rook {
                            rsq += WEST;
                        }
                    } else if side == 'Q' {
                        rsq = Square::A1.relative(color);
                        while self.piece_on(rsq) != rook {
                            rsq += EAST;
                        }
                    } else if side >= 'A' && side <= 'H' {
                        let file = side.to_digit(18).unwrap() - 10;
                        rsq = Square::make(file, relative_rank(color, RANK_1));
                    } else {
                        continue;
                    }
                    self.set_castling_right(color, rsq);
                }
            }
            let enpassant = iter.next().unwrap();
            self.st_mut().ep_square = Square::NONE;
            if enpassant != "-" {
                let file = enpassant.chars().nth(0).unwrap();
                let file = file.to_digit(18).unwrap() - 10;
                let rank = if self.side_to_move == WHITE { 5 } else { 2 };
                let ep_sq = Square::make(file, rank);
                if self.attackers_to(ep_sq) & self.pieces_cp(self.side_to_move, PAWN) != 0
                    && self.pieces_cp(!self.side_to_move, PAWN)
                        & (ep_sq + pawn_push(!self.side_to_move))
                        != 0
                {
                    self.st_mut().ep_square = ep_sq;
                }
            }
            if let Some(halfmove) = iter.next() {
                self.st_mut().rule50 = halfmove.parse().unwrap();
            } else {
                self.st_mut().rule50 = 0;
            }
            if let Some(fullmove) = iter.next() {
                let fullmove = fullmove.parse::<i32>().unwrap();
                self.game_ply = std::cmp::max(2 * (fullmove - 1), 0);
            } else {
                self.game_ply = 0;
            }
            if self.side_to_move == BLACK {
                self.game_ply += 1;
            }
            self.chess960 = is_chess960;
            self.set_state();
            debug_assert!(self.is_ok());
        }
        fn set_castling_right(&mut self, c: Color, rfrom: Square) {
            let kfrom = self.square(c, KING);
            let cs = if kfrom < rfrom {
                CastlingSide::KING
            } else {
                CastlingSide::QUEEN
            };
            let cr = c | cs;
            self.st_mut().castling_rights |= cr;
            self.castling_rights_mask[kfrom.0 as usize] |= cr;
            self.castling_rights_mask[rfrom.0 as usize] |= cr;
            self.castling_rook_square[cr.0 as usize] = rfrom;
            let kto = relative_square(
                c,
                if cs == CastlingSide::KING {
                    Square::G1
                } else {
                    Square::C1
                },
            );
            let rto = relative_square(
                c,
                if cs == CastlingSide::KING {
                    Square::F1
                } else {
                    Square::D1
                },
            );
            let mut s = std::cmp::min(rfrom, rto);
            while s <= std::cmp::max(rfrom, rto) {
                if s != kfrom && s != rfrom {
                    self.castling_path[cr.0 as usize] |= s;
                }
                s += EAST;
            }
            let mut s = std::cmp::min(kfrom, kto);
            while s <= std::cmp::max(kfrom, kto) {
                if s != kfrom && s != rfrom {
                    self.castling_path[cr.0 as usize] |= s;
                }
                s += EAST;
            }
        }
        fn set_check_info(&mut self) {
            let mut pinners = Bitboard(0);
            self.st_mut().blockers_for_king[WHITE.0 as usize] =
                self.slider_blockers(self.pieces_c(BLACK), self.square(WHITE, KING), &mut pinners);
            self.st_mut().pinners_for_king[WHITE.0 as usize] = pinners;
            self.st_mut().blockers_for_king[BLACK.0 as usize] =
                self.slider_blockers(self.pieces_c(WHITE), self.square(BLACK, KING), &mut pinners);
            self.st_mut().pinners_for_king[BLACK.0 as usize] = pinners;
            let ksq = self.square(!self.side_to_move(), KING);
            self.st_mut().check_squares[PAWN.0 as usize] =
                self.attacks_from_pawn(ksq, !self.side_to_move);
            self.st_mut().check_squares[KNIGHT.0 as usize] = self.attacks_from(KNIGHT, ksq);
            self.st_mut().check_squares[BISHOP.0 as usize] = self.attacks_from(BISHOP, ksq);
            self.st_mut().check_squares[ROOK.0 as usize] = self.attacks_from(ROOK, ksq);
            self.st_mut().check_squares[QUEEN.0 as usize] = self.st().check_squares
                [BISHOP.0 as usize]
                | self.st().check_squares[ROOK.0 as usize];
            self.st_mut().check_squares[KING.0 as usize] = Bitboard(0);
        }
        fn set_state(&mut self) {
            self.st_mut().key = Key(0);
            self.st_mut().material_key = Key(0);
            self.st_mut().pawn_key = zobrist::no_pawns();
            self.st_mut().non_pawn_material[WHITE.0 as usize] = Value::ZERO;
            self.st_mut().non_pawn_material[BLACK.0 as usize] = Value::ZERO;
            self.st_mut().psq = Score::ZERO;
            self.st_mut().checkers_bb = self.attackers_to(self.square(self.side_to_move, KING))
                & self.pieces_c(!self.side_to_move);
            self.set_check_info();
            for s in self.pieces() {
                let pc = self.piece_on(s);
                self.st_mut().key ^= zobrist::psq(pc, s);
                self.st_mut().psq += psqt::psq(pc, s);
            }
            if self.st_mut().ep_square != Square::NONE {
                let tmp = zobrist::enpassant(self.st().ep_square.file());
                self.st_mut().key = tmp;
            }
            if self.side_to_move == BLACK {
                self.st_mut().key ^= zobrist::side();
            }
            {
                let tmp = zobrist::castling(self.st().castling_rights);
                self.st_mut().key ^= tmp;
            }
            for s in self.pieces_p(PAWN) {
                let tmp = zobrist::psq(self.piece_on(s), s);
                self.st_mut().pawn_key ^= tmp;
            }
            for c in 0..2 {
                for pt in 2..6 {
                    let pc = Piece::make(Color(c), PieceType(pt));
                    let tmp = self.count(Color(c), PieceType(pt)) * piece_value(MG, pc);
                    self.st_mut().non_pawn_material[c as usize] += tmp;
                }
                for pt in 1..7 {
                    let pc = Piece::make(Color(c), PieceType(pt));
                    for cnt in 0..self.count(Color(c), PieceType(pt)) {
                        self.st_mut().material_key ^= zobrist::material(pc, cnt);
                    }
                }
            }
        }
        pub fn fen(&self) -> String {
            let mut ss = String::new();
            for r in (0..8).rev() {
                let mut f = 0;
                while f < 8 {
                    let mut empty_cnt = 0u8;
                    while f < 8 && self.empty(Square::make(f, r)) {
                        empty_cnt += 1;
                        f += 1;
                    }
                    if empty_cnt > 0 {
                        ss.push((48u8 + empty_cnt) as char);
                    }
                    if f < 8 {
                        let c = Position::PIECE_TO_CHAR
                            .chars()
                            .nth(self.piece_on(Square::make(f, r)).0 as usize)
                            .unwrap();
                        ss.push(c);
                        f += 1;
                    }
                }
                if r > 0 {
                    ss.push('/');
                }
            }
            ss.push_str(if self.side_to_move == WHITE {
                " w "
            } else {
                " b "
            });
            self.castle_helper(&mut ss, WHITE_OO, 'K');
            self.castle_helper(&mut ss, WHITE_OOO, 'Q');
            self.castle_helper(&mut ss, BLACK_OO, 'k');
            self.castle_helper(&mut ss, BLACK_OOO, 'q');
            if !self.has_castling_right(ANY_CASTLING) {
                ss.push('-');
            }
            if self.ep_square() == Square::NONE {
                ss.push_str(" - ");
            } else {
                ss.push(' ');
                ss.push_str(&uci::square(self.ep_square()));
                ss.push(' ');
            }
            ss.push_str(&self.rule50_count().to_string());
            ss.push(' ');
            ss.push_str(&(1 + self.game_ply() / 2).to_string());
            ss
        }
        fn castle_helper(&self, ss: &mut String, cr: CastlingRight, c: char) {
            if !self.has_castling_right(cr) {
                return;
            }
            if !self.chess960 {
                ss.push(c);
            } else {
                let f = self.castling_rook_square(cr).file();
                let r = self.castling_rook_square(cr).rank();
                let mut c = 65 + f;
                if r == RANK_8 {
                    c += 32;
                }
                ss.push((c as u8) as char);
            }
        }
        pub fn slider_blockers(
            &self,
            sliders: Bitboard,
            s: Square,
            pinners: &mut Bitboard,
        ) -> Bitboard {
            let mut blockers = Bitboard(0);
            *pinners = Bitboard(0);
            let snipers = ((pseudo_attacks(ROOK, s) & self.pieces_pp(QUEEN, ROOK))
                | (pseudo_attacks(BISHOP, s) & self.pieces_pp(QUEEN, BISHOP)))
                & sliders;
            for sniper_sq in snipers {
                let b = between_bb(s, sniper_sq) & self.pieces();
                if !more_than_one(b) {
                    blockers |= b;
                    if b & self.pieces_c(self.piece_on(s).color()) != 0 {
                        *pinners |= sniper_sq;
                    }
                }
            }
            blockers
        }
        pub fn legal(&self, m: Move) -> bool {
            debug_assert!(m.is_ok());
            let us = self.side_to_move;
            let from = m.from();
            debug_assert!(self.moved_piece(m).color() == us);
            debug_assert!(self.piece_on(self.square(us, KING)) == Piece::make(us, KING));
            if m.move_type() == ENPASSANT {
                let ksq = self.square(us, KING);
                let to = m.to();
                let capsq = to - pawn_push(us);
                let occupied = (self.pieces() ^ from ^ capsq) | to;
                debug_assert!(to == self.ep_square());
                debug_assert!(self.moved_piece(m) == Piece::make(us, PAWN));
                debug_assert!(self.piece_on(capsq) == Piece::make(!us, PAWN));
                debug_assert!(self.piece_on(to) == NO_PIECE);
                return attacks_bb(ROOK, ksq, occupied) & self.pieces_cpp(!us, QUEEN, ROOK) == 0
                    && attacks_bb(BISHOP, ksq, occupied) & self.pieces_cpp(!us, QUEEN, BISHOP)
                        == 0;
            }
            if self.piece_on(from).piece_type() == KING {
                return m.move_type() == CASTLING
                    || self.attackers_to(m.to()) & self.pieces_c(!us) == 0;
            }
            self.blockers_for_king(us) & from == 0 || aligned(from, m.to(), self.square(us, KING))
        }
        pub fn pseudo_legal(&self, m: Move) -> bool {
            let us = self.side_to_move();
            let from = m.from();
            let to = m.to();
            let pc = self.moved_piece(m);
            if m.move_type() != NORMAL {
                return MoveList::new::<Legal>(self).contains(m);
            }
            if m.promotion_type() != KNIGHT {
                return false;
            }
            if pc == NO_PIECE || pc.color() != us {
                return false;
            }
            if self.pieces_c(us) & to != 0 {
                return false;
            }
            if pc.piece_type() == PAWN {
                if to.rank() == relative_rank(us, RANK_8) {
                    return false;
                }
                if self.attacks_from_pawn(from, us) & self.pieces_c(!us) & to == 0
                    && !((from + pawn_push(us) == to) && self.empty(to))
                    && !(from + 2 * pawn_push(us) == to
                        && from.rank() == relative_rank(us, RANK_2)
                        && self.empty(to)
                        && self.empty(to - pawn_push(us)))
                {
                    return false;
                }
            } else if self.attacks_from(pc.piece_type(), from) & to == 0 {
                return false;
            }
            if self.checkers() != 0 {
                if pc.piece_type() != KING {
                    if more_than_one(self.checkers()) {
                        return false;
                    }
                    if (between_bb(lsb(self.checkers()), self.square(us, KING)) | self.checkers())
                        & to
                        == 0
                    {
                        return false;
                    }
                } else if self.attackers_to_occ(to, self.pieces() ^ from) & self.pieces_c(!us) != 0
                {
                    return false;
                }
            }
            true
        }
        pub fn gives_check(&self, m: Move) -> bool {
            debug_assert!(m.is_ok());
            debug_assert!(self.moved_piece(m).color() == self.side_to_move());
            let from = m.from();
            let to = m.to();
            if self.st().check_squares[self.piece_on(from).piece_type().0 as usize] & to != 0 {
                return true;
            }
            if self.blockers_for_king(!self.side_to_move()) & from != 0
                && !aligned(from, to, self.square(!self.side_to_move(), KING))
            {
                return true;
            }
            match m.move_type() {
                NORMAL => false,
                PROMOTION => {
                    attacks_bb(m.promotion_type(), to, self.pieces() ^ from)
                        & self.square(!self.side_to_move(), KING)
                        != 0
                }
                ENPASSANT => {
                    let capsq = Square::make(to.file(), from.rank());
                    let b = (self.pieces() ^ from ^ capsq) | to;
                    (attacks_bb(ROOK, self.square(!self.side_to_move(), KING), b)
                        & self.pieces_cpp(self.side_to_move(), QUEEN, ROOK))
                        | (attacks_bb(BISHOP, self.square(!self.side_to_move(), KING), b)
                            & self.pieces_cpp(self.side_to_move(), QUEEN, BISHOP))
                        != 0
                }
                CASTLING => {
                    let kfrom = from;
                    let rfrom = to;
                    let kto = relative_square(
                        self.side_to_move(),
                        if rfrom > kfrom {
                            Square::G1
                        } else {
                            Square::C1
                        },
                    );
                    let rto = relative_square(
                        self.side_to_move(),
                        if rfrom > kfrom {
                            Square::F1
                        } else {
                            Square::D1
                        },
                    );
                    (pseudo_attacks(ROOK, rto) & self.square(!self.side_to_move(), KING)) != 0
                        && (attacks_bb(ROOK, rto, (self.pieces() ^ kfrom ^ rfrom) | rto | kto)
                            & self.square(!self.side_to_move(), KING))
                            != 0
                }
                _ => {
                    debug_assert!(false);
                    false
                }
            }
        }
        pub fn do_move(&mut self, m: Move, gives_check: bool) {
            debug_assert!(m.is_ok());
            self.nodes += 1;
            let mut k = self.st().key ^ zobrist::side();
            let st_copy = self.st().copy();
            self.states.push(st_copy);
            self.game_ply += 1;
            self.st_mut().rule50 += 1;
            self.st_mut().plies_from_null += 1;
            let us = self.side_to_move();
            let them = !us;
            let from = m.from();
            let mut to = m.to();
            let pc = self.piece_on(from);
            let mut captured = if m.move_type() == ENPASSANT {
                Piece::make(them, PAWN)
            } else {
                self.piece_on(to)
            };
            debug_assert!(pc.color() == us);
            debug_assert!(
                captured == NO_PIECE
                    || captured.color() == if m.move_type() != CASTLING { them } else { us }
            );
            if m.move_type() == CASTLING {
                debug_assert!(pc == Piece::make(us, KING));
                debug_assert!(captured == Piece::make(us, ROOK));
                let mut rfrom = Square::A1;
                let mut rto = Square::A1;
                self.do_castling::<True>(us, from, &mut to, &mut rfrom, &mut rto);
                self.st_mut().psq += psqt::psq(captured, rto) - psqt::psq(captured, rfrom);
                k ^= zobrist::psq(captured, rfrom) ^ zobrist::psq(captured, rto);
                captured = NO_PIECE;
            }
            if captured != NO_PIECE {
                let mut capsq = to;
                if captured.piece_type() == PAWN {
                    if m.move_type() == ENPASSANT {
                        capsq -= pawn_push(us);
                        debug_assert!(pc == Piece::make(us, PAWN));
                        debug_assert!(to == self.st_mut().ep_square);
                        debug_assert!(to.relative_rank(us) == RANK_6);
                        debug_assert!(self.piece_on(to) == NO_PIECE);
                        debug_assert!(self.piece_on(capsq) == Piece::make(them, PAWN));
                        self.board[capsq.0 as usize] = NO_PIECE;
                    }
                    self.st_mut().pawn_key ^= zobrist::psq(captured, capsq);
                } else {
                    self.st_mut().non_pawn_material[them.0 as usize] -= piece_value(MG, captured);
                }
                self.remove_piece(captured, capsq);
                k ^= zobrist::psq(captured, capsq);
                {
                    let tmp = zobrist::material(captured, self.piece_count[captured.0 as usize]);
                    self.st_mut().material_key ^= tmp;
                }
                self.st_mut().psq -= psqt::psq(captured, capsq);
                self.st_mut().rule50 = 0;
            }
            k ^= zobrist::psq(pc, from) ^ zobrist::psq(pc, to);
            if self.st_mut().ep_square != Square::NONE {
                k ^= zobrist::enpassant(self.st().ep_square.file());
                self.st_mut().ep_square = Square::NONE;
            }
            if self.st_mut().castling_rights != 0
                && self.castling_rights_mask[from.0 as usize]
                    | self.castling_rights_mask[to.0 as usize]
                    != 0
            {
                let cr = self.castling_rights_mask[from.0 as usize]
                    | self.castling_rights_mask[to.0 as usize];
                k ^= zobrist::castling(self.st().castling_rights & cr);
                self.st_mut().castling_rights &= !cr;
            }
            if m.move_type() != CASTLING {
                self.move_piece(pc, from, to);
            }
            if pc.piece_type() == PAWN {
                if to.0 ^ from.0 == 16
                    && self.attacks_from_pawn(to - pawn_push(us), us) & self.pieces_cp(them, PAWN)
                        != 0
                {
                    self.st_mut().ep_square = to - pawn_push(us);
                    k ^= zobrist::enpassant(self.st().ep_square.file());
                } else if m.move_type() == PROMOTION {
                    let promotion = Piece::make(us, m.promotion_type());
                    debug_assert!(to.relative_rank(us) == RANK_8);
                    debug_assert!(
                        promotion.piece_type() >= KNIGHT && promotion.piece_type() <= QUEEN
                    );
                    self.remove_piece(pc, to);
                    self.put_piece(promotion, to);
                    k ^= zobrist::psq(pc, to) ^ zobrist::psq(promotion, to);
                    self.st_mut().pawn_key ^= zobrist::psq(pc, to);
                    {
                        let tmp = zobrist::material(
                            promotion,
                            self.piece_count[promotion.0 as usize] - 1,
                        ) ^ zobrist::material(pc, self.piece_count[pc.0 as usize]);
                        self.st_mut().material_key ^= tmp;
                    }
                    self.st_mut().psq += psqt::psq(promotion, to) - psqt::psq(pc, to);
                    self.st_mut().non_pawn_material[us.0 as usize] += piece_value(MG, promotion);
                }
                self.st_mut().pawn_key ^= zobrist::psq(pc, from) ^ zobrist::psq(pc, to);
                self.st_mut().rule50 = 0;
            }
            self.st_mut().psq += psqt::psq(pc, to) - psqt::psq(pc, from);
            self.st_mut().captured_piece = captured;
            self.st_mut().key = k;
            self.st_mut().checkers_bb = if gives_check {
                self.attackers_to(self.square(them, KING)) & self.pieces_c(us)
            } else {
                Bitboard(0)
            };
            self.side_to_move = them;
            self.set_check_info();
            debug_assert!(self.is_ok());
        }
        pub fn undo_move(&mut self, m: Move) {
            debug_assert!(m.is_ok());
            self.side_to_move = !self.side_to_move;
            let us = self.side_to_move;
            let from = m.from();
            let mut to = m.to();
            let mut pc = self.piece_on(to);
            debug_assert!(self.empty(from) || m.move_type() == CASTLING);
            debug_assert!(self.st().captured_piece.piece_type() != KING);
            if m.move_type() == PROMOTION {
                debug_assert!(to.relative_rank(us) == RANK_8);
                debug_assert!(pc.piece_type() == m.promotion_type());
                debug_assert!(pc.piece_type() >= KNIGHT && pc.piece_type() <= QUEEN);
                self.remove_piece(pc, to);
                pc = Piece::make(us, PAWN);
                self.put_piece(pc, to);
            }
            if m.move_type() == CASTLING {
                let mut rfrom = Square(0);
                let mut rto = Square(0);
                self.do_castling::<False>(us, from, &mut to, &mut rfrom, &mut rto);
            } else {
                self.move_piece(pc, to, from);
                if self.st().captured_piece != NO_PIECE {
                    let mut capsq = to;
                    if m.move_type() == ENPASSANT {
                        capsq -= pawn_push(us);
                        debug_assert!(pc.piece_type() == PAWN);
                        debug_assert!(to.relative_rank(us) == RANK_6);
                        debug_assert!(self.piece_on(capsq) == NO_PIECE);
                        debug_assert!(self.st().captured_piece == Piece::make(!us, PAWN));
                    }
                    let cap_piece = self.st().captured_piece;
                    self.put_piece(cap_piece, capsq);
                }
            }
            let new_len = self.states.len() - 1;
            self.states.truncate(new_len);
            self.game_ply -= 1;
            debug_assert!(self.is_ok());
        }
        fn do_castling<Do: Bool>(
            &mut self,
            us: Color,
            from: Square,
            to: &mut Square,
            rfrom: &mut Square,
            rto: &mut Square,
        ) {
            let king_side = *to > from;
            *rfrom = *to;
            *rto = relative_square(us, if king_side { Square::F1 } else { Square::D1 });
            *to = relative_square(us, if king_side { Square::G1 } else { Square::C1 });
            self.remove_piece(Piece::make(us, KING), if Do::BOOL { from } else { *to });
            self.remove_piece(Piece::make(us, ROOK), if Do::BOOL { *rfrom } else { *rto });
            self.board[(if Do::BOOL { from } else { *to }).0 as usize] = NO_PIECE;
            self.board[(if Do::BOOL { *rfrom } else { *rto }).0 as usize] = NO_PIECE;
            self.put_piece(Piece::make(us, KING), if Do::BOOL { *to } else { from });
            self.put_piece(Piece::make(us, ROOK), if Do::BOOL { *rto } else { *rfrom });
        }
        pub fn do_null_move(&mut self) {
            debug_assert!(self.checkers() == 0);
            let st_copy = (*self.st()).clone();
            self.states.push(st_copy);
            if self.st().ep_square != Square::NONE {
                let tmp = zobrist::enpassant(self.st().ep_square.file());
                self.st_mut().key ^= tmp;
                self.st_mut().ep_square = Square::NONE;
            }
            self.st_mut().key ^= zobrist::side();
            self.st_mut().rule50 += 1;
            self.st_mut().plies_from_null = 0;
            self.side_to_move = !self.side_to_move;
            self.set_check_info();
            debug_assert!(self.is_ok());
        }
        pub fn undo_null_move(&mut self) {
            debug_assert!(self.checkers() == 0);
            let new_len = self.states.len() - 1;
            self.states.truncate(new_len);
            self.side_to_move = !self.side_to_move;
        }
        #[allow(dead_code)]
        fn key_after(&self, m: Move) -> Key {
            let from = m.from();
            let to = m.to();
            let pc = self.piece_on(from);
            let captured = self.piece_on(to);
            let mut k = self.st().key ^ zobrist::side();
            if captured != NO_PIECE {
                k ^= zobrist::psq(captured, to);
            }
            k ^ zobrist::psq(pc, to) ^ zobrist::psq(pc, from)
        }
        pub fn see_ge(&self, m: Move, value: Value) -> bool {
            debug_assert!(m.is_ok());
            if m.move_type() != NORMAL {
                return Value::ZERO >= value;
            }
            let from = m.from();
            let to = m.to();
            let mut swap = piece_value(MG, self.piece_on(to)) - value;
            if swap < Value::ZERO {
                return false;
            }
            swap = piece_value(MG, self.piece_on(from)) - swap;
            if swap <= Value::ZERO {
                return true;
            }
            let mut occ = self.pieces() ^ from ^ to;
            let mut stm = self.piece_on(from).color();
            let mut attackers = self.attackers_to_occ(to, occ);
            let mut res = Value(1);
            loop {
                stm = !stm;
                attackers &= occ;
                let mut stm_attackers = attackers & self.pieces_c(stm);
                if stm_attackers == 0 {
                    break;
                }
                if stm_attackers & self.blockers_for_king(stm) != 0
                    && self.pinners_for_king(stm) & !occ == 0
                {
                    stm_attackers &= !self.blockers_for_king(stm);
                }
                if stm_attackers == 0 {
                    break;
                }
                res = Value(res.0 ^ 1);
                let bb = stm_attackers & self.pieces_p(PAWN);
                if bb != 0 {
                    swap = PawnValueMg - swap;
                    if swap < res {
                        break;
                    }
                    occ ^= bb & -bb;
                    attackers |= attacks_bb(BISHOP, to, occ) & self.pieces_pp(BISHOP, QUEEN);
                    continue;
                }
                let bb = stm_attackers & self.pieces_p(KNIGHT);
                if bb != 0 {
                    swap = KnightValueMg - swap;
                    if swap < res {
                        break;
                    }
                    occ ^= bb & -bb;
                    continue;
                }
                let bb = stm_attackers & self.pieces_p(BISHOP);
                if bb != 0 {
                    swap = BishopValueMg - swap;
                    if swap < res {
                        break;
                    }
                    occ ^= bb & -bb;
                    attackers |= attacks_bb(BISHOP, to, occ) & self.pieces_pp(BISHOP, QUEEN);
                    continue;
                }
                let bb = stm_attackers & self.pieces_p(ROOK);
                if bb != 0 {
                    swap = RookValueMg - swap;
                    if swap < res {
                        break;
                    }
                    occ ^= bb & -bb;
                    attackers |= attacks_bb(ROOK, to, occ) & self.pieces_pp(ROOK, QUEEN);
                    continue;
                }
                let bb = stm_attackers & self.pieces_p(QUEEN);
                if bb != 0 {
                    swap = QueenValueMg - swap;
                    if swap < res {
                        break;
                    }
                    occ ^= bb & -bb;
                    attackers |= (attacks_bb(BISHOP, to, occ) & self.pieces_pp(BISHOP, QUEEN))
                        | (attacks_bb(ROOK, to, occ) & self.pieces_pp(ROOK, QUEEN));
                    continue;
                }
                if attackers & !self.pieces_c(stm) != 0 {
                    return res == Value::ZERO;
                } else {
                    return res != Value::ZERO;
                }
            }
            res != Value::ZERO
        }
        pub fn is_draw(&self, ply: i32) -> bool {
            if self.st().rule50 > 99
                && (self.checkers() == 0 || MoveList::new::<Legal>(&self).len() != 0)
            {
                return true;
            }
            let end = std::cmp::min(self.st().rule50, self.st().plies_from_null);
            if end < 4 {
                return false;
            }
            let mut k = self.states.len() - 3;
            let mut cnt = 0;
            let mut i = 4;
            while i <= end {
                k -= 2;
                if self.states[k].key == self.st().key {
                    cnt += 1;
                    if cnt + ((ply > i) as i32) == 2 {
                        return true;
                    }
                }
                i += 2;
            }
            false
        }
        pub fn has_repeated(&self) -> bool {
            let mut l = self.states.len() - 1;
            loop {
                let mut i = 4;
                let e = std::cmp::min(self.states[l].rule50, self.states[l].plies_from_null);
                if e < i {
                    return false;
                }
                let mut k = self.states.len() - 3;
                while i <= e {
                    k -= 2;
                    if self.states[k].key == self.states[l].key {
                        return true;
                    }
                    i += 2;
                }
                l -= 2;
            }
        }
        fn put_piece(&mut self, pc: Piece, s: Square) {
            self.board[s.0 as usize] = pc;
            self.by_type_bb[ALL_PIECES.0 as usize] |= s;
            self.by_type_bb[pc.piece_type().0 as usize] |= s;
            self.by_color_bb[pc.color().0 as usize] |= s;
            self.index[s.0 as usize] = self.piece_count[pc.0 as usize];
            self.piece_count[pc.0 as usize] += 1;
            self.piece_list[pc.0 as usize][self.index[s.0 as usize] as usize] = s;
            self.piece_count[Piece::make(pc.color(), ALL_PIECES).0 as usize] += 1;
        }
        fn remove_piece(&mut self, pc: Piece, s: Square) {
            self.by_type_bb[ALL_PIECES.0 as usize] ^= s;
            self.by_type_bb[pc.piece_type().0 as usize] ^= s;
            self.by_color_bb[pc.color().0 as usize] ^= s;
            self.piece_count[pc.0 as usize] -= 1;
            let last_square =
                self.piece_list[pc.0 as usize][self.piece_count[pc.0 as usize] as usize];
            self.index[last_square.0 as usize] = self.index[s.0 as usize];
            self.piece_list[pc.0 as usize][self.index[last_square.0 as usize] as usize] =
                last_square;
            self.piece_list[pc.0 as usize][self.piece_count[pc.0 as usize] as usize] = Square::NONE;
            self.piece_count[Piece::make(pc.color(), ALL_PIECES).0 as usize] -= 1;
        }
        fn move_piece(&mut self, pc: Piece, from: Square, to: Square) {
            let from_to_bb = from.bb() ^ to.bb();
            self.by_type_bb[ALL_PIECES.0 as usize] ^= from_to_bb;
            self.by_type_bb[pc.piece_type().0 as usize] ^= from_to_bb;
            self.by_color_bb[pc.color().0 as usize] ^= from_to_bb;
            self.board[from.0 as usize] = NO_PIECE;
            self.board[to.0 as usize] = pc;
            self.index[to.0 as usize] = self.index[from.0 as usize];
            self.piece_list[pc.0 as usize][self.index[to.0 as usize] as usize] = to;
        }
        pub fn is_ok(&self) -> bool {
            if self.side_to_move() != WHITE && self.side_to_move != BLACK
                || self.piece_on(self.square(WHITE, KING)) != W_KING
                || self.piece_on(self.square(BLACK, KING)) != B_KING
                || (self.ep_square() != Square::NONE
                    && self.ep_square().relative_rank(self.side_to_move()) != RANK_6)
            {
                panic!("pos: Default");
            }
            if self.count(WHITE, KING) != 1
                || self.count(BLACK, KING) != 1
                || self.attackers_to(self.square(!self.side_to_move(), KING))
                    & self.pieces_c(self.side_to_move())
                    != 0
            {
                panic!("pos_is_ok: Kings");
            }
            if self.pieces_p(PAWN) & (RANK1_BB | RANK8_BB) != 0
                || self.count(WHITE, PAWN) > 8
                || self.count(BLACK, PAWN) > 8
            {
                panic!("pos_is_ok: Pawns");
            }
            for p1 in 1..6 {
                for p2 in 1..6 {
                    if p1 != p2 && self.pieces_p(PieceType(p1)) & self.pieces_p(PieceType(p2)) != 0
                    {
                        panic!("pos_is_ok: Bitboards");
                    }
                }
            }
            for p in 1..14 {
                if p == 7 || p == 8 {
                    continue;
                }
                let pc = Piece(p);
                if self.piece_count[pc.0 as usize]
                    != popcount(self.pieces_cp(pc.color(), pc.piece_type())) as i32
                {
                    panic!("pos_is_ok: Pieces {}", p);
                }
                for i in 0..self.piece_count[pc.0 as usize] {
                    if self.board[self.piece_list[pc.0 as usize][i as usize].0 as usize] != pc
                        || self.index[self.piece_list[pc.0 as usize][i as usize].0 as usize] != i
                    {
                        panic!("pos_is_ok: Index {}, {}", p, i);
                    }
                }
            }
            true
        }
    }
}
pub mod psqt {
    use bitboard::*;
    use std;
    use types::*;
    macro_rules! S {
        ($x:expr, $y:expr) => {
            Score(($y << 16) + $x)
        };
    }
    const BONUS: [[[Score; 4]; 8]; 6] = [
        [
            [S!(0, 0), S!(0, 0), S!(0, 0), S!(0, 0)],
            [S!(-11, 7), S!(6, -4), S!(7, 8), S!(3, -2)],
            [S!(-18, -4), S!(-2, -5), S!(19, 5), S!(24, 4)],
            [S!(-17, 3), S!(-9, 3), S!(20, -8), S!(35, -3)],
            [S!(-6, 8), S!(5, 9), S!(3, 7), S!(21, -6)],
            [S!(-6, 8), S!(-8, -5), S!(-6, 2), S!(-2, 4)],
            [S!(-4, 3), S!(20, -9), S!(-8, 1), S!(-4, 18)],
            [S!(0, 0), S!(0, 0), S!(0, 0), S!(0, 0)],
        ],
        [
            [S!(-161, -105), S!(-96, -82), S!(-80, -46), S!(-73, -14)],
            [S!(-83, -69), S!(-43, -54), S!(-21, -17), S!(-10, 9)],
            [S!(-71, -50), S!(-22, -39), S!(0, -7), S!(9, 28)],
            [S!(-25, -41), S!(18, -25), S!(43, 6), S!(47, 38)],
            [S!(-26, -46), S!(16, -25), S!(38, 3), S!(50, 40)],
            [S!(-11, -54), S!(37, -38), S!(56, -7), S!(65, 27)],
            [S!(-63, -65), S!(-19, -50), S!(5, -24), S!(14, 13)],
            [S!(-195, -109), S!(-67, -89), S!(-42, -50), S!(-29, -13)],
        ],
        [
            [S!(-44, -58), S!(-13, -31), S!(-25, -37), S!(-34, -19)],
            [S!(-20, -34), S!(20, -9), S!(12, -14), S!(1, 4)],
            [S!(-9, -23), S!(27, 0), S!(21, -3), S!(11, 16)],
            [S!(-11, -26), S!(28, -3), S!(21, -5), S!(10, 16)],
            [S!(-11, -26), S!(27, -4), S!(16, -7), S!(9, 14)],
            [S!(-17, -24), S!(16, -2), S!(12, 0), S!(2, 13)],
            [S!(-23, -34), S!(17, -10), S!(6, -12), S!(-2, 6)],
            [S!(-35, -55), S!(-11, -32), S!(-19, -36), S!(-29, -17)],
        ],
        [
            [S!(-25, 0), S!(-16, 0), S!(-16, 0), S!(-9, 0)],
            [S!(-21, 0), S!(-8, 0), S!(-3, 0), S!(0, 0)],
            [S!(-21, 0), S!(-9, 0), S!(-4, 0), S!(2, 0)],
            [S!(-22, 0), S!(-6, 0), S!(-1, 0), S!(2, 0)],
            [S!(-22, 0), S!(-7, 0), S!(0, 0), S!(1, 0)],
            [S!(-21, 0), S!(-7, 0), S!(0, 0), S!(2, 0)],
            [S!(-12, 0), S!(4, 0), S!(8, 0), S!(12, 0)],
            [S!(-23, 0), S!(-15, 0), S!(-11, 0), S!(-5, 0)],
        ],
        [
            [S!(0, -71), S!(-4, -56), S!(-3, -42), S!(-1, -29)],
            [S!(-4, -56), S!(6, -30), S!(9, -21), S!(8, -5)],
            [S!(-2, -39), S!(6, -17), S!(9, -8), S!(9, 5)],
            [S!(-1, -29), S!(8, -5), S!(10, 9), S!(7, 19)],
            [S!(-3, -27), S!(9, -5), S!(8, 10), S!(7, 21)],
            [S!(-2, -40), S!(6, -16), S!(8, -10), S!(10, 3)],
            [S!(-2, -55), S!(7, -30), S!(7, -21), S!(6, -6)],
            [S!(-1, -74), S!(-4, -55), S!(-1, -43), S!(0, -30)],
        ],
        [
            [S!(267, 0), S!(320, 48), S!(270, 75), S!(195, 84)],
            [S!(264, 43), S!(304, 92), S!(238, 143), S!(180, 132)],
            [S!(200, 83), S!(245, 138), S!(176, 167), S!(110, 165)],
            [S!(177, 106), S!(185, 169), S!(148, 169), S!(110, 179)],
            [S!(149, 108), S!(177, 163), S!(115, 200), S!(66, 203)],
            [S!(118, 95), S!(159, 155), S!(84, 176), S!(41, 174)],
            [S!(87, 50), S!(128, 99), S!(63, 122), S!(20, 139)],
            [S!(63, 9), S!(88, 55), S!(47, 80), S!(0, 90)],
        ],
    ];
    static mut PSQ: [[Score; 64]; 16] = [[Score(0); 64]; 16];
    pub fn psq(pc: Piece, s: Square) -> Score {
        unsafe { PSQ[pc.0 as usize][s.0 as usize] }
    }
    pub fn init() {
        unsafe {
            for i in 1..7 {
                let pc = Piece(i);
                let v = Score::make(piece_value(MG, pc).0, piece_value(EG, pc).0);
                for s in ALL_SQUARES {
                    let f = std::cmp::min(s.file(), FILE_H - s.file());
                    PSQ[pc.0 as usize][s.0 as usize] =
                        v + BONUS[(pc.0 - 1) as usize][s.rank() as usize][f as usize];
                    PSQ[(!pc).0 as usize][(!s).0 as usize] = -PSQ[pc.0 as usize][s.0 as usize];
                }
            }
        }
    }
}
pub mod search {
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
    fn reduction<PvNode: NodeType>(i: bool, d: Depth, mn: i32) -> Depth {
        unsafe {
            REDUCTIONS[PvNode::NT][i as usize][std::cmp::min(d / ONE_PLY, 63) as usize]
                [std::cmp::min(mn, 63) as usize]
                * ONE_PLY
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
                || (limits().depth != 0
                    && pos.is_main
                    && root_depth / ONE_PLY > limits().depth as i32)
            {
                break;
            }
            if !pos.is_main {
                let i = ((pos.thread_idx - 1) & 20) as usize;
                if ((root_depth / ONE_PLY + pos.game_ply() + SKIP_PHASE[i]) / SKIP_SIZE[i]) % 2 != 0
                {
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
                        && (threads::stop()
                            || pos.pv_idx + 1 == multi_pv
                            || timeman::elapsed() > 3000)
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
                    best_value =
                        search::<Pv>(pos, &mut stack, alpha, beta, root_depth, false, false);
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
                        unstable_pv_factor *=
                            pos.previous_time_reduction.powf(0.528) / time_reduction;
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
                return if ss[5].ply >= MAX_PLY && !in_check {
                    evaluate(pos)
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
            let eval;
            if in_check {
                ss[5].static_eval = Value::NONE;
                break;
            } else if tt_hit {
                let mut tmp = tte.eval();
                if tmp == Value::NONE {
                    tmp = evaluate(pos);
                }
                ss[5].static_eval = tmp;
                if tt_value != Value::NONE
                    && tte.bound()
                        & (if tt_value > tmp {
                            Bound::LOWER
                        } else {
                            Bound::UPPER
                        })
                        != 0
                {
                    tmp = tt_value;
                }
                eval = tmp;
            } else {
                eval = if ss[4].current_move != Move::NULL {
                    evaluate(pos)
                } else {
                    -ss[4].static_eval + 2 * evaluate::TEMPO
                };
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
                && pos.blockers_for_king(!pos.side_to_move()) & pos.pieces_c(pos.side_to_move())
                    == 0
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
                    if lmr_depth < 7
                        && !in_check
                        && ss[5].static_eval + 256 + 200 * lmr_depth <= alpha
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
            if depth >= 3 * ONE_PLY
                && move_count > 1
                && (!capture_or_promotion || move_count_pruning)
            {
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
                        -qsearch::<NonPv, True>(
                            pos,
                            &mut ss[1..],
                            -(alpha + 1),
                            -alpha,
                            Depth::ZERO,
                        )
                    } else {
                        -qsearch::<NonPv, False>(
                            pos,
                            &mut ss[1..],
                            -(alpha + 1),
                            -alpha,
                            Depth::ZERO,
                        )
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
        } else if depth >= 3 * ONE_PLY
            && pos.captured_piece() == NO_PIECE
            && ss[4].current_move.is_ok()
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
        if in_check {
            ss[5].static_eval = Value::NONE;
            best_value = -Value::INFINITE;
            futility_base = -Value::INFINITE;
        } else {
            if tt_hit {
                let mut tmp = tte.eval();
                if tmp == Value::NONE {
                    tmp = evaluate(pos);
                }
                ss[5].static_eval = tmp;
                if tt_value != Value::NONE
                    && tte.bound()
                        & (if tt_value > tmp {
                            Bound::LOWER
                        } else {
                            Bound::UPPER
                        })
                        != 0
                {
                    best_value = tt_value;
                } else {
                    best_value = tmp;
                }
            } else {
                best_value = if ss[4].current_move != Move::NULL {
                    evaluate(pos)
                } else {
                    -ss[4].static_eval + 2 * evaluate::TEMPO
                };
                ss[5].static_eval = best_value;
            }
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
        }
        let mut mp = MovePickerQ::new(pos, tt_move, depth, ss[4].current_move.to());
        loop {
            let m = mp.next_move(pos);
            if m == Move::NONE {
                break;
            }
            debug_assert!(m.is_ok());
            let gives_check = if m.move_type() == NORMAL
                && pos.blockers_for_king(!pos.side_to_move()) & pos.pieces_c(pos.side_to_move())
                    == 0
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
        ss[5].pv.truncate(0);
        ss[5].pv.push(m);
        for i in 0..ss[6].pv.len() {
            let m = ss[6].pv[i];
            ss[5].pv.push(m);
        }
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
            update_continuation_histories(
                &ss[1..],
                pos.moved_piece(quiets[i]),
                quiets[i].to(),
                -bonus,
            );
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
}
pub mod tb {
    use bitboard::*;
    use memmap::*;
    use movegen::*;
    use position::zobrist::material;
    use position::Position;
    use search::RootMoves;
    use std;
    use std::cell::UnsafeCell;
    use std::collections::HashMap;
    use std::fs;
    use std::path::Path;
    use std::slice;
    use std::sync::atomic::{AtomicBool, Ordering};
    use std::sync::Mutex;
    use types::*;
    use ucioption;
    const TB_PIECES: usize = 7;
    static mut MAX_CARDINALITY: u32 = 0;
    static mut MAX_CARDINALITY_DTM: u32 = 0;
    static mut CARDINALITY: u32 = 0;
    static mut CARDINALITY_DTM: u32 = 0;
    static mut ROOT_IN_TB: bool = false;
    static mut USE_RULE_50: bool = true;
    static mut PROBE_DEPTH: Depth = Depth(0);
    pub fn read_options() {
        unsafe {
            USE_RULE_50 = ucioption::get_bool("Syzygy50MoveRule");
            PROBE_DEPTH = ucioption::get_i32("SyzygyProbeDepth") * ONE_PLY;
            CARDINALITY = ucioption::get_i32("SyzygyProbeLimit") as u32;
            if CARDINALITY > MAX_CARDINALITY {
                CARDINALITY = MAX_CARDINALITY;
                PROBE_DEPTH = Depth::ZERO;
            }
            CARDINALITY_DTM = if ucioption::get_bool("SyzygyUseDTM") {
                std::cmp::min(CARDINALITY, MAX_CARDINALITY_DTM)
            } else {
                0
            };
        }
    }
    pub fn max_cardinality() -> u32 {
        unsafe { MAX_CARDINALITY }
    }
    pub fn cardinality() -> u32 {
        unsafe { CARDINALITY }
    }
    pub fn cardinality_dtm() -> u32 {
        unsafe { CARDINALITY_DTM }
    }
    pub fn root_in_tb() -> bool {
        unsafe { ROOT_IN_TB }
    }
    pub fn use_rule_50() -> bool {
        unsafe { USE_RULE_50 }
    }
    pub fn probe_depth() -> Depth {
        unsafe { PROBE_DEPTH }
    }
    struct EncInfo {
        precomp: Option<Box<PairsData>>,
        factor: [usize; TB_PIECES],
        pieces: [u8; TB_PIECES],
        norm: [u8; TB_PIECES],
    }
    impl EncInfo {
        pub fn new() -> EncInfo {
            EncInfo {
                precomp: None,
                factor: [0; TB_PIECES],
                pieces: [0; TB_PIECES],
                norm: [0; TB_PIECES],
            }
        }
    }
    const WDL_TO_MAP: [u32; 5] = [1, 3, 0, 2, 0];
    const PA_FLAGS: [u8; 5] = [8, 0, 0, 0, 4];
    const WDL_MAGIC: u32 = 0x5d23e871;
    const DTM_MAGIC: u32 = 0x88ac504b;
    const DTZ_MAGIC: u32 = 0xa50c66d7;
    const WDL_SUFFIX: &str = ".rtbw";
    const DTM_SUFFIX: &str = ".rtbm";
    const DTZ_SUFFIX: &str = ".rtbz";
    struct Wdl;
    struct Dtm;
    struct Dtz;
    struct PieceEnc;
    struct FileEnc;
    struct RankEnc;
    trait Encoding {
        const ENC: i32;
        type Entry: EntryInfo;
    }
    impl Encoding for PieceEnc {
        const ENC: i32 = 0;
        type Entry = PieceEntry;
    }
    impl Encoding for FileEnc {
        const ENC: i32 = 1;
        type Entry = PawnEntry;
    }
    impl Encoding for RankEnc {
        const ENC: i32 = 2;
        type Entry = PawnEntry;
    }
    trait TbType: Sized {
        type PieceTable: TbTable<Entry = PieceEntry, Type = Self>;
        type PawnTable: TbTable<Entry = PawnEntry, Type = Self>;
        type Select;
        const TYPE: i32;
        fn magic() -> u32;
        fn suffix() -> &'static str;
    }
    impl TbType for Wdl {
        type PieceTable = WdlPiece;
        type PawnTable = WdlPawn;
        type Select = ();
        const TYPE: i32 = 0;
        fn magic() -> u32 {
            WDL_MAGIC
        }
        fn suffix() -> &'static str {
            WDL_SUFFIX
        }
    }
    impl TbType for Dtm {
        type PieceTable = DtmPiece;
        type PawnTable = DtmPawn;
        type Select = bool;
        const TYPE: i32 = 1;
        fn magic() -> u32 {
            DTM_MAGIC
        }
        fn suffix() -> &'static str {
            DTM_SUFFIX
        }
    }
    impl TbType for Dtz {
        type PieceTable = DtzPiece;
        type PawnTable = DtzPawn;
        type Select = i32;
        const TYPE: i32 = 2;
        fn magic() -> u32 {
            DTZ_MAGIC
        }
        fn suffix() -> &'static str {
            DTZ_SUFFIX
        }
    }
    trait TbTable: Sized {
        type Type: TbType;
        type Entry: TbEntry<Self> + EntryInfo;
        type Enc: Encoding<Entry = Self::Entry>;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>>;
        fn ready(&self) -> &AtomicBool;
        fn num_tables() -> usize;
        fn ei(&self, t: usize, idx: usize) -> &EncInfo;
        fn ei_mut(&mut self, t: usize, idx: usize) -> &mut EncInfo;
        fn set_loss_only(&mut self, b: bool);
        fn loss_only(&self) -> bool;
        fn set_flags(&mut self, t: usize, f: u8);
        fn flags(&self, t: usize) -> u8;
        fn set_map_idx(&mut self, t: usize, i: usize, j: usize, v: u16);
        type MapType: 'static;
        fn set_map(&mut self, map: &'static [Self::MapType]);
        fn map(&self, t: usize, bside: usize, res: i32, s: <Self::Type as TbType>::Select) -> i32;
        fn set_switched(&mut self);
        fn switched(&self) -> bool;
    }
    struct WdlPiece {
        mapping: Option<Box<Mmap>>,
        ei: [EncInfo; 2],
        ready: AtomicBool,
    }
    impl TbTable for WdlPiece {
        type Type = Wdl;
        type Entry = PieceEntry;
        type Enc = PieceEnc;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn num_tables() -> usize {
            1
        }
        fn ei(&self, _t: usize, i: usize) -> &EncInfo {
            &self.ei[i]
        }
        fn ei_mut(&mut self, _t: usize, i: usize) -> &mut EncInfo {
            &mut self.ei[i]
        }
        fn set_loss_only(&mut self, _b: bool) {}
        fn loss_only(&self) -> bool {
            false
        }
        fn set_flags(&mut self, _t: usize, _f: u8) {}
        fn flags(&self, _t: usize) -> u8 {
            0
        }
        fn set_map_idx(&mut self, _t: usize, _i: usize, _j: usize, _v: u16) {}
        type MapType = ();
        fn set_map(&mut self, _map: &'static [Self::MapType]) {}
        fn map(&self, _t: usize, _b: usize, res: i32, _s: ()) -> i32 {
            res - 2
        }
        fn set_switched(&mut self) {}
        fn switched(&self) -> bool {
            false
        }
    }
    struct DtmPiece {
        mapping: Option<Box<Mmap>>,
        map: &'static [u16],
        ei: [EncInfo; 2],
        map_idx: [[u16; 2]; 2],
        ready: AtomicBool,
        loss_only: bool,
    }
    impl TbTable for DtmPiece {
        type Type = Dtm;
        type Entry = PieceEntry;
        type Enc = PieceEnc;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn num_tables() -> usize {
            1
        }
        fn ei(&self, _t: usize, i: usize) -> &EncInfo {
            &self.ei[i]
        }
        fn ei_mut(&mut self, _t: usize, i: usize) -> &mut EncInfo {
            &mut self.ei[i]
        }
        fn set_loss_only(&mut self, b: bool) {
            self.loss_only = b;
        }
        fn loss_only(&self) -> bool {
            self.loss_only
        }
        fn set_flags(&mut self, _t: usize, _f: u8) {}
        fn flags(&self, _t: usize) -> u8 {
            0
        }
        fn set_map_idx(&mut self, _t: usize, i: usize, j: usize, v: u16) {
            self.map_idx[i][j] = v;
        }
        type MapType = u16;
        fn set_map(&mut self, map: &'static [Self::MapType]) {
            self.map = map
        }
        fn map(&self, _t: usize, bside: usize, mut res: i32, won: bool) -> i32 {
            if !self.loss_only {
                let idx = self.map_idx[bside][won as usize];
                res = u16::from_le(self.map[idx as usize + res as usize]) as i32;
            }
            res
        }
        fn set_switched(&mut self) {}
        fn switched(&self) -> bool {
            false
        }
    }
    struct DtzPiece {
        mapping: Option<Box<Mmap>>,
        map: &'static [u8],
        ei: EncInfo,
        map_idx: [u16; 4],
        ready: AtomicBool,
        flags: u8,
    }
    impl TbTable for DtzPiece {
        type Type = Dtz;
        type Entry = PieceEntry;
        type Enc = PieceEnc;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn num_tables() -> usize {
            1
        }
        fn ei(&self, _t: usize, _i: usize) -> &EncInfo {
            &self.ei
        }
        fn ei_mut(&mut self, _t: usize, _i: usize) -> &mut EncInfo {
            &mut self.ei
        }
        fn set_loss_only(&mut self, _b: bool) {}
        fn loss_only(&self) -> bool {
            false
        }
        fn set_flags(&mut self, _t: usize, f: u8) {
            self.flags = f;
        }
        fn flags(&self, _t: usize) -> u8 {
            self.flags
        }
        fn set_map_idx(&mut self, _t: usize, _i: usize, j: usize, v: u16) {
            self.map_idx[j] = v;
        }
        type MapType = u8;
        fn set_map(&mut self, map: &'static [Self::MapType]) {
            self.map = map
        }
        fn map(&self, _t: usize, _b: usize, mut res: i32, wdl: i32) -> i32 {
            if self.flags & 2 != 0 {
                let idx = self.map_idx[WDL_TO_MAP[(wdl + 2) as usize] as usize];
                res = self.map[idx as usize + res as usize] as i32;
            }
            if self.flags & PA_FLAGS[(wdl + 2) as usize] == 0 || wdl & 1 != 0 {
                res *= 2;
            }
            res
        }
        fn set_switched(&mut self) {}
        fn switched(&self) -> bool {
            false
        }
    }
    trait TbEntry<T: TbTable> {
        fn table(&self) -> &T;
        fn table_mut(&self) -> &mut T;
        fn exists(&self) -> bool;
    }
    trait EntryInfo {
        fn key(&self) -> Key;
        fn lock(&self) -> &Mutex<()>;
        fn num(&self) -> u8;
        fn symmetric(&self) -> bool;
        fn kk_enc(&self) -> bool;
        fn pawns(&self, i: usize) -> u8;
    }
    struct PieceEntry {
        key: Key,
        wdl: UnsafeCell<WdlPiece>,
        dtm: UnsafeCell<DtmPiece>,
        dtz: UnsafeCell<DtzPiece>,
        lock: Mutex<()>,
        num: u8,
        symmetric: bool,
        kk_enc: bool,
        has_dtm: bool,
        has_dtz: bool,
    }
    impl<T> TbEntry<T> for PieceEntry
    where
        T: TbTable,
    {
        fn table_mut(&self) -> &mut T {
            match T::Type::TYPE {
                Wdl::TYPE => unsafe { &mut *(self.wdl.get() as *mut T) },
                Dtm::TYPE => unsafe { &mut *(self.dtm.get() as *mut T) },
                Dtz::TYPE => unsafe { &mut *(self.dtz.get() as *mut T) },
                _ => panic!("Non-existing table type"),
            }
        }
        fn table(&self) -> &T {
            self.table_mut()
        }
        fn exists(&self) -> bool {
            match T::Type::TYPE {
                Wdl::TYPE => true,
                Dtm::TYPE => self.has_dtm,
                Dtz::TYPE => self.has_dtz,
                _ => panic!("Non-existing table type"),
            }
        }
    }
    impl EntryInfo for PieceEntry {
        fn key(&self) -> Key {
            self.key
        }
        fn lock(&self) -> &Mutex<()> {
            &self.lock
        }
        fn num(&self) -> u8 {
            self.num
        }
        fn symmetric(&self) -> bool {
            self.symmetric
        }
        fn kk_enc(&self) -> bool {
            self.kk_enc
        }
        fn pawns(&self, _i: usize) -> u8 {
            0
        }
    }
    struct WdlPawn {
        mapping: Option<Box<Mmap>>,
        ei: [[EncInfo; 2]; 4],
        ready: AtomicBool,
    }
    impl TbTable for WdlPawn {
        type Type = Wdl;
        type Entry = PawnEntry;
        type Enc = FileEnc;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn num_tables() -> usize {
            4
        }
        fn ei(&self, t: usize, i: usize) -> &EncInfo {
            &self.ei[t][i]
        }
        fn ei_mut(&mut self, t: usize, i: usize) -> &mut EncInfo {
            &mut self.ei[t][i]
        }
        fn set_loss_only(&mut self, _b: bool) {}
        fn loss_only(&self) -> bool {
            false
        }
        fn set_flags(&mut self, _t: usize, _f: u8) {}
        fn flags(&self, _t: usize) -> u8 {
            0
        }
        fn set_map_idx(&mut self, _t: usize, _i: usize, _j: usize, _v: u16) {}
        type MapType = ();
        fn set_map(&mut self, _map: &'static [Self::MapType]) {}
        fn map(&self, _t: usize, _b: usize, res: i32, _s: ()) -> i32 {
            res - 2
        }
        fn set_switched(&mut self) {}
        fn switched(&self) -> bool {
            false
        }
    }
    struct DtmPawn {
        mapping: Option<Box<Mmap>>,
        map: &'static [u16],
        ei: [[EncInfo; 2]; 6],
        map_idx: [[[u16; 2]; 2]; 6],
        ready: AtomicBool,
        loss_only: bool,
        switched: bool,
    }
    impl TbTable for DtmPawn {
        type Type = Dtm;
        type Entry = PawnEntry;
        type Enc = RankEnc;
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn num_tables() -> usize {
            6
        }
        fn ei(&self, t: usize, i: usize) -> &EncInfo {
            &self.ei[t][i]
        }
        fn ei_mut(&mut self, t: usize, i: usize) -> &mut EncInfo {
            &mut self.ei[t][i]
        }
        fn set_loss_only(&mut self, b: bool) {
            self.loss_only = b;
        }
        fn loss_only(&self) -> bool {
            self.loss_only
        }
        fn set_flags(&mut self, _t: usize, _f: u8) {}
        fn flags(&self, _t: usize) -> u8 {
            0
        }
        fn set_map_idx(&mut self, t: usize, i: usize, j: usize, v: u16) {
            self.map_idx[t][i][j] = v;
        }
        type MapType = u16;
        fn set_map(&mut self, map: &'static [Self::MapType]) {
            self.map = map
        }
        fn map(&self, t: usize, bside: usize, mut res: i32, won: bool) -> i32 {
            if !self.loss_only {
                let idx = self.map_idx[t][bside][won as usize];
                res = u16::from_le(self.map[idx as usize + res as usize]) as i32;
            }
            res
        }
        fn set_switched(&mut self) {
            self.switched = true;
        }
        fn switched(&self) -> bool {
            self.switched
        }
    }
    struct DtzPawn {
        mapping: Option<Box<Mmap>>,
        map: &'static [u8],
        ei: [EncInfo; 4],
        map_idx: [[u16; 4]; 4],
        flags: [u8; 4],
        ready: AtomicBool,
    }
    impl TbTable for DtzPawn {
        type Type = Dtz;
        type Entry = PawnEntry;
        type Enc = FileEnc;
        fn ready(&self) -> &AtomicBool {
            &self.ready
        }
        fn mapping(&mut self) -> &mut Option<Box<Mmap>> {
            &mut self.mapping
        }
        fn num_tables() -> usize {
            4
        }
        fn ei(&self, t: usize, _i: usize) -> &EncInfo {
            &self.ei[t]
        }
        fn ei_mut(&mut self, t: usize, _i: usize) -> &mut EncInfo {
            &mut self.ei[t]
        }
        fn set_loss_only(&mut self, _b: bool) {}
        fn loss_only(&self) -> bool {
            false
        }
        fn set_flags(&mut self, t: usize, f: u8) {
            self.flags[t] = f;
        }
        fn flags(&self, t: usize) -> u8 {
            self.flags[t]
        }
        fn set_map_idx(&mut self, t: usize, _i: usize, j: usize, v: u16) {
            self.map_idx[t][j] = v;
        }
        type MapType = u8;
        fn set_map(&mut self, map: &'static [Self::MapType]) {
            self.map = map
        }
        fn map(&self, t: usize, _b: usize, mut res: i32, wdl: i32) -> i32 {
            if self.flags[t] & 2 != 0 {
                let idx = self.map_idx[t][WDL_TO_MAP[(wdl + 2) as usize] as usize];
                res = self.map[idx as usize + res as usize] as i32;
            }
            if self.flags[t] & PA_FLAGS[(wdl + 2) as usize] == 0 || wdl & 1 != 0 {
                res *= 2;
            }
            res
        }
        fn set_switched(&mut self) {}
        fn switched(&self) -> bool {
            false
        }
    }
    struct PawnEntry {
        key: Key,
        wdl: UnsafeCell<WdlPawn>,
        dtm: UnsafeCell<DtmPawn>,
        dtz: UnsafeCell<DtzPawn>,
        lock: Mutex<()>,
        num: u8,
        symmetric: bool,
        pawns: [u8; 2],
        has_dtm: bool,
        has_dtz: bool,
    }
    impl<T> TbEntry<T> for PawnEntry
    where
        T: TbTable,
    {
        fn table_mut(&self) -> &mut T {
            match T::Type::TYPE {
                Wdl::TYPE => unsafe { &mut *(self.wdl.get() as *mut T) },
                Dtm::TYPE => unsafe { &mut *(self.dtm.get() as *mut T) },
                Dtz::TYPE => unsafe { &mut *(self.dtz.get() as *mut T) },
                _ => panic!("Non-existing table type"),
            }
        }
        fn table(&self) -> &T {
            self.table_mut()
        }
        fn exists(&self) -> bool {
            match T::Type::TYPE {
                Wdl::TYPE => true,
                Dtm::TYPE => self.has_dtm,
                Dtz::TYPE => self.has_dtz,
                _ => panic!("Non-existing table type"),
            }
        }
    }
    impl EntryInfo for PawnEntry {
        fn key(&self) -> Key {
            self.key
        }
        fn lock(&self) -> &Mutex<()> {
            &self.lock
        }
        fn num(&self) -> u8 {
            self.num
        }
        fn symmetric(&self) -> bool {
            self.symmetric
        }
        fn kk_enc(&self) -> bool {
            false
        }
        fn pawns(&self, i: usize) -> u8 {
            self.pawns[i]
        }
    }
    #[derive(Clone)]
    enum TbHashEntry {
        Piece(usize),
        Pawn(usize),
    }
    fn prt_str(pos: &Position, flip: bool) -> String {
        let mut c = if flip { BLACK } else { WHITE };
        let mut s = String::new();
        for pt in (1..7).rev() {
            for _ in 0..pos.count(c, PieceType(pt)) {
                s.push(Position::PIECE_TO_CHAR.chars().nth(pt as usize).unwrap());
            }
        }
        s.push('v');
        c = !c;
        for pt in (1..7).rev() {
            for _ in 0..pos.count(c, PieceType(pt)) {
                s.push(Position::PIECE_TO_CHAR.chars().nth(pt as usize).unwrap());
            }
        }
        s
    }
    fn calc_key_from_pcs(pcs: &[i32; 16], flip: bool) -> Key {
        let mut key = Key(0);
        for c in 0..2 {
            for pt in 1..7 {
                let pc = Piece::make(Color(c), PieceType(pt));
                for i in 0..pcs[pc.0 as usize] {
                    key ^= material(pc ^ flip, i);
                }
            }
        }
        key
    }
    fn calc_key_from_pieces(pieces: &[u8]) -> Key {
        let mut key = Key(0);
        let mut cnt = [0; 16];
        for &k in pieces.iter() {
            let pc = Piece(k as u32);
            key ^= material(pc, cnt[k as usize]);
            cnt[k as usize] += 1;
        }
        key
    }
    static mut PATH: Option<String> = None;
    fn sep_char() -> char {
        if cfg!(target_os = "windows") {
            ';'
        } else {
            ':'
        }
    }
    fn test_tb(name: &str, suffix: &str) -> bool {
        let dirs = unsafe { PATH.as_ref().unwrap().split(sep_char()) };
        for dir in dirs {
            let file_name = format!("{}{}{}{}", dir, '/', name, suffix);
            let path = Path::new(&file_name);
            if path.is_file() {
                return true;
            }
        }
        false
    }
    fn open_tb(name: &str, suffix: &str) -> Option<fs::File> {
        let dirs = unsafe { PATH.as_ref().unwrap().split(sep_char()) };
        for dir in dirs {
            let file_name = format!("{}{}{}{}", dir, '/', name, suffix);
            if let Ok(file) = fs::File::open(file_name) {
                return Some(file);
            }
        }
        None
    }
    fn map_file(name: &str, suffix: &str) -> Option<Box<Mmap>> {
        let file = open_tb(name, suffix);
        if file.is_none() {
            return None;
        }
        let file = file.unwrap();
        match unsafe { MmapOptions::new().map(&file) } {
            Ok(mmap) => Some(Box::new(mmap)),
            Err(err) => {
                eprintln!("{:?}", err.kind());
                None
            }
        }
    }
    struct GlobalVec<T> {
        v: *mut T,
        cap: usize,
        len: usize,
    }
    impl<T> GlobalVec<T> {
        pub fn init(&mut self, cap: usize) {
            self.save(Vec::with_capacity(cap));
        }
        fn save(&mut self, mut v: Vec<T>) {
            self.v = v.as_mut_ptr();
            self.len = v.len();
            self.cap = v.capacity();
            std::mem::forget(v);
        }
        fn get_vec(&self) -> Vec<T> {
            unsafe { Vec::from_raw_parts(self.v, self.len, self.cap) }
        }
        pub fn push(&mut self, item: T) {
            let mut v = self.get_vec();
            v.push(item);
            self.save(v);
        }
        pub fn len(&self) -> usize {
            self.len
        }
        pub unsafe fn reset(&mut self) {
            let mut v = self.get_vec();
            v.truncate(0);
            self.save(v);
        }
        pub unsafe fn free(&mut self) {
            std::mem::drop(self.get_vec());
        }
    }
    impl<T> std::ops::Index<usize> for GlobalVec<T>
    where
        T: 'static,
    {
        type Output = T;
        fn index(&self, idx: usize) -> &'static T {
            unsafe {
                let elt_ref: &'static T = &*self.v.offset(idx as isize);
                elt_ref
            }
        }
    }
    static mut PIECE_ENTRIES: GlobalVec<PieceEntry> = GlobalVec {
        v: 0 as *mut PieceEntry,
        len: 0,
        cap: 0,
    };
    static mut PAWN_ENTRIES: GlobalVec<PawnEntry> = GlobalVec {
        v: 0 as *mut PawnEntry,
        len: 0,
        cap: 0,
    };
    static mut TB_MAP: *mut HashMap<Key, TbHashEntry> = 0 as *mut HashMap<Key, TbHashEntry>;
    static mut NUM_WDL: u32 = 0;
    static mut NUM_DTM: u32 = 0;
    static mut NUM_DTZ: u32 = 0;
    pub fn init_tb(name: &str) {
        if !test_tb(&name, WDL_SUFFIX) {
            return;
        }
        let has_dtm = test_tb(&name, DTM_SUFFIX);
        let has_dtz = test_tb(&name, DTZ_SUFFIX);
        let mut pcs = [0; 16];
        let mut color = 0;
        for c in name.chars() {
            match c {
                'P' => pcs[PAWN.0 as usize | color] += 1,
                'N' => pcs[KNIGHT.0 as usize | color] += 1,
                'B' => pcs[BISHOP.0 as usize | color] += 1,
                'R' => pcs[ROOK.0 as usize | color] += 1,
                'Q' => pcs[QUEEN.0 as usize | color] += 1,
                'K' => pcs[KING.0 as usize | color] += 1,
                'v' => color = 8,
                _ => {}
            }
        }
        let key = calc_key_from_pcs(&pcs, false);
        let key2 = calc_key_from_pcs(&pcs, true);
        let symmetric = key == key2;
        let num = pcs.iter().sum::<i32>() as u32;
        unsafe {
            if num > MAX_CARDINALITY {
                MAX_CARDINALITY = num;
            }
            if has_dtm && num > MAX_CARDINALITY_DTM {
                MAX_CARDINALITY_DTM = num;
            }
        }
        let mut map = unsafe { Box::from_raw(TB_MAP) };
        let tb_entry;
        if pcs[W_PAWN.0 as usize] + pcs[B_PAWN.0 as usize] == 0 {
            let entry = PieceEntry {
                key: key,
                lock: Mutex::new(()),
                num: num as u8,
                symmetric: symmetric,
                kk_enc: pcs.iter().filter(|&n| *n == 1).count() == 2,
                has_dtm: has_dtm,
                has_dtz: has_dtz,
                wdl: UnsafeCell::new(WdlPiece {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    ei: [EncInfo::new(), EncInfo::new()],
                }),
                dtm: UnsafeCell::new(DtmPiece {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    ei: [EncInfo::new(), EncInfo::new()],
                    map: &[],
                    map_idx: [[0; 2]; 2],
                    loss_only: false,
                }),
                dtz: UnsafeCell::new(DtzPiece {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    flags: 0,
                    ei: EncInfo::new(),
                    map: &[],
                    map_idx: [0; 4],
                }),
            };
            unsafe {
                PIECE_ENTRIES.push(entry);
            }
            tb_entry = TbHashEntry::Piece(unsafe { PIECE_ENTRIES.len() - 1 });
        } else {
            let mut p0 = pcs[W_PAWN.0 as usize];
            let mut p1 = pcs[B_PAWN.0 as usize];
            if p1 > 0 && (p0 == 0 || p0 > p1) {
                std::mem::swap(&mut p0, &mut p1);
            }
            let entry = PawnEntry {
                key: key,
                lock: Mutex::new(()),
                num: num as u8,
                symmetric: symmetric,
                pawns: [p0 as u8, p1 as u8],
                has_dtm: has_dtm,
                has_dtz: has_dtz,
                wdl: UnsafeCell::new(WdlPawn {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    ei: [
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                    ],
                }),
                dtm: UnsafeCell::new(DtmPawn {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    ei: [
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                        [EncInfo::new(), EncInfo::new()],
                    ],
                    map: &[],
                    map_idx: [[[0; 2]; 2]; 6],
                    loss_only: false,
                    switched: false,
                }),
                dtz: UnsafeCell::new(DtzPawn {
                    mapping: None,
                    ready: AtomicBool::new(false),
                    flags: [0; 4],
                    ei: [
                        EncInfo::new(),
                        EncInfo::new(),
                        EncInfo::new(),
                        EncInfo::new(),
                    ],
                    map: &[],
                    map_idx: [[0; 4]; 4],
                }),
            };
            unsafe {
                PAWN_ENTRIES.push(entry);
            }
            tb_entry = TbHashEntry::Pawn(unsafe { PAWN_ENTRIES.len() - 1 });
        }
        map.insert(key, tb_entry.clone());
        if key != key2 {
            map.insert(key2, tb_entry);
        }
        unsafe {
            TB_MAP = Box::into_raw(map);
            NUM_WDL += 1;
            NUM_DTM += has_dtm as u32;
            NUM_DTZ += has_dtz as u32;
        }
    }
    pub fn free() {
        unsafe {
            std::mem::drop(Box::from_raw(TB_MAP));
            PIECE_ENTRIES.free();
            PAWN_ENTRIES.free();
        }
    }
    pub fn init(path: String) {
        const P: [char; 5] = ['Q', 'R', 'B', 'N', 'P'];
        static mut INITIALIZED: bool = false;
        let max5 = std::mem::size_of::<usize>() < 8;
        unsafe {
            if !INITIALIZED {
                init_indices();
                PIECE_ENTRIES.init(if max5 { 84 } else { 254 });
                PAWN_ENTRIES.init(if max5 { 61 } else { 256 });
                TB_MAP = Box::into_raw(Box::new(HashMap::new()));
                INITIALIZED = true;
            }
            if PATH != None {
                PATH = None;
                std::mem::drop(Box::from_raw(TB_MAP));
                TB_MAP = Box::into_raw(Box::new(HashMap::new()));
                PIECE_ENTRIES.reset();
                PAWN_ENTRIES.reset();
                NUM_WDL = 0;
                NUM_DTM = 0;
                NUM_DTZ = 0;
                MAX_CARDINALITY = 0;
                MAX_CARDINALITY_DTM = 0;
            }
        }
        if path == "" || path == "<empty>" {
            return;
        }
        unsafe {
            PATH = Some(path);
        }
        for i in 0..5 {
            init_tb(&format!("K{}vK", P[i]));
        }
        for i in 0..5 {
            for j in i..5 {
                init_tb(&format!("K{}vK{}", P[i], P[j]));
            }
        }
        for i in 0..5 {
            for j in i..5 {
                init_tb(&format!("K{}{}vK", P[i], P[j]));
            }
        }
        for i in 0..5 {
            for j in i..5 {
                for k in 0..5 {
                    init_tb(&format!("K{}{}vK{}", P[i], P[j], P[k]));
                }
            }
        }
        for i in 0..5 {
            for j in i..5 {
                for k in j..5 {
                    init_tb(&format!("K{}{}{}vK", P[i], P[j], P[k]));
                }
            }
        }
        if !max5 {
            for i in 0..5 {
                for j in i..5 {
                    for k in i..5 {
                        for l in (if i == k { j } else { k })..5 {
                            init_tb(&format!("K{}{}vK{}{}", P[i], P[j], P[k], P[l]));
                        }
                    }
                }
            }
            for i in 0..5 {
                for j in i..5 {
                    for k in j..5 {
                        for l in 0..5 {
                            init_tb(&format!("K{}{}{}vK{}", P[i], P[j], P[k], P[l]));
                        }
                    }
                }
            }
            for i in 0..5 {
                for j in i..5 {
                    for k in j..5 {
                        for l in k..5 {
                            init_tb(&format!("K{}{}{}{}vK", P[i], P[j], P[k], P[l]));
                        }
                    }
                }
            }
            for i in 0..5 {
                for j in i..5 {
                    for k in j..5 {
                        for l in 0..5 {
                            for m in l..5 {
                                init_tb(&format!("K{}{}{}vK{}{}", P[i], P[j], P[k], P[l], P[m]));
                            }
                        }
                    }
                }
            }
            for i in 0..5 {
                for j in i..5 {
                    for k in j..5 {
                        for l in k..5 {
                            for m in 0..5 {
                                init_tb(&format!("K{}{}{}{}vK{}", P[i], P[j], P[k], P[l], P[m]));
                            }
                        }
                    }
                }
            }
            for i in 0..5 {
                for j in i..5 {
                    for k in j..5 {
                        for l in k..5 {
                            for m in l..5 {
                                init_tb(&format!("K{}{}{}{}{}vK", P[i], P[j], P[k], P[l], P[m]));
                            }
                        }
                    }
                }
            }
        }
        println!(
            "info string Found {} WDL, {} DTM and {} DTZ tablebase files.",
            unsafe { NUM_WDL },
            unsafe { NUM_DTM },
            unsafe { NUM_DTZ }
        );
    }
    fn subfactor(k: usize, n: usize) -> usize {
        let mut f = n;
        let mut l = 1;
        for i in 1..k {
            f *= n - i;
            l *= i + 1;
        }
        f / l
    }
    fn calc_factors<T: Encoding>(
        ei: &mut EncInfo,
        e: &T::Entry,
        order: u8,
        order2: u8,
        t: usize,
    ) -> usize {
        let mut i = ei.norm[0];
        if order2 < 0x0f {
            i += ei.norm[i as usize];
        }
        let mut n = 64 - i;
        let mut f = 1;
        let mut k = 0;
        while i < e.num() || k == order || k == order2 {
            if k == order {
                ei.factor[0] = f;
                f *= if T::ENC == PieceEnc::ENC {
                    if e.kk_enc() {
                        462
                    } else {
                        31332
                    }
                } else {
                    pfactor::<T>(ei.norm[0] as usize - 1, t)
                };
            } else if k == order2 {
                ei.factor[ei.norm[0] as usize] = f;
                f *= subfactor(
                    ei.norm[ei.norm[0] as usize] as usize,
                    48 - ei.norm[0] as usize,
                );
            } else {
                ei.factor[i as usize] = f;
                f *= subfactor(ei.norm[i as usize] as usize, n as usize);
                n -= ei.norm[i as usize];
                i += ei.norm[i as usize];
            }
            k += 1;
        }
        f
    }
    fn set_norm<T: Encoding>(ei: &mut EncInfo, e: &T::Entry) {
        let mut i;
        if T::ENC == PieceEnc::ENC {
            ei.norm[0] = if e.kk_enc() { 2 } else { 3 };
            i = ei.norm[0] as usize;
        } else {
            ei.norm[0] = e.pawns(0);
            if e.pawns(1) > 0 {
                ei.norm[e.pawns(0) as usize] = e.pawns(1);
            }
            i = (e.pawns(0) + e.pawns(1)) as usize;
        }
        while i < e.num() as usize {
            for j in i..e.num() as usize {
                if ei.pieces[j] != ei.pieces[i] {
                    break;
                }
                ei.norm[i] += 1;
            }
            i += ei.norm[i] as usize;
        }
    }
    fn setup_pieces<T: Encoding>(
        ei: &mut EncInfo,
        e: &T::Entry,
        tb: &[u8],
        s: u32,
        t: usize,
    ) -> usize {
        let j = 1 + (e.pawns(1) > 0) as usize;
        for i in 0..(e.num() as usize) {
            ei.pieces[i] = (tb[i + j] >> s) & 0x0f;
        }
        let order = (tb[0] >> s) & 0x0f;
        let order2 = if e.pawns(1) > 0 {
            (tb[1] >> s) & 0x0f
        } else {
            0x0f
        };
        set_norm::<T>(ei, e);
        calc_factors::<T>(ei, e, order, order2, t)
    }
    #[repr(packed)]
    struct IndexEntry {
        block: u32,
        offset: u16,
    }
    struct PairsData {
        index_table: &'static [IndexEntry],
        size_table: &'static [u16],
        data: &'static [u8],
        offset: &'static [u16],
        sym_len: Vec<u8>,
        sym_pat: &'static [[u8; 3]],
        block_size: u32,
        idx_bits: u32,
        min_len: u8,
        const_val: u16,
        base: Vec<u64>,
    }
    fn s1(w: &[u8; 3]) -> usize {
        (w[0] as usize) | ((w[1] as usize & 0x0f) << 8)
    }
    fn s2(w: &[u8; 3]) -> usize {
        ((w[2] as usize) << 4) | ((w[1] as usize) >> 4)
    }
    fn calc_sym_len(sym_len: &mut Vec<u8>, sym_pat: &[[u8; 3]], s: usize, tmp: &mut Vec<u8>) {
        if tmp[s] != 0 {
            return;
        }
        let w = &sym_pat[s];
        let s2 = s2(w);
        if s2 == 0x0fff {
            sym_len[s] = 0;
        } else {
            let s1 = s1(w);
            calc_sym_len(sym_len, sym_pat, s1, tmp);
            calc_sym_len(sym_len, sym_pat, s2, tmp);
            sym_len[s] = sym_len[s1] + sym_len[s2] + 1;
        }
        tmp[s] = 1;
    }
    fn setup_pairs(
        data_ref: &mut &'static [u8],
        tb_size: usize,
        size: &mut [usize],
        flags: &mut u8,
        is_wdl: bool,
    ) -> Box<PairsData> {
        let data = *data_ref;
        *flags = data[0];
        if *flags & 0x80 != 0 {
            *data_ref = &data[2..];
            return Box::new(PairsData {
                index_table: &[],
                size_table: &[],
                data: &[],
                offset: &[],
                sym_len: Vec::new(),
                sym_pat: &[],
                block_size: 0,
                idx_bits: 0,
                min_len: 0,
                const_val: if is_wdl { data[1] as u16 } else { 0 },
                base: Vec::new(),
            });
        }
        let block_size = data[1] as u32;
        let idx_bits = data[2] as u32;
        let real_num_blocks = u32::from_le(cast_slice(&data[4..], 1)[0]);
        let num_blocks = real_num_blocks + data[3] as u32;
        let max_len = data[8];
        let min_len = data[9];
        let h = (max_len - min_len + 1) as usize;
        let num_syms = u16::from_le(cast_slice(&data[10 + 2 * h..], 1)[0]) as usize;
        let mut sym_len = Vec::with_capacity(num_syms);
        for _ in 0..num_syms {
            sym_len.push(0u8);
        }
        let sym_pat = cast_slice::<[u8; 3]>(&data[12 + 2 * h..], num_syms);
        let mut tmp = Vec::with_capacity(num_syms);
        for _ in 0..num_syms {
            tmp.push(0u8);
        }
        for s in 0..num_syms {
            calc_sym_len(&mut sym_len, sym_pat, s, &mut tmp);
        }
        let num_indices = (tb_size + (1usize << idx_bits) - 1) >> idx_bits;
        size[0] = num_indices as usize;
        size[1] = num_blocks as usize;
        size[2] = (real_num_blocks as usize) << block_size;
        *data_ref = &data[12 + 2 * h + 3 * num_syms + (num_syms & 1)..];
        let offset = cast_slice::<u16>(&data[10..], h);
        let mut base = Vec::with_capacity(h);
        for _ in 0..h {
            base.push(0u64);
        }
        for i in (0..h - 1).rev() {
            let b1 = u16::from_le(offset[i]) as u64;
            let b2 = u16::from_le(offset[i + 1]) as u64;
            base[i] = (base[i + 1] + b1 - b2) / 2;
        }
        for i in 0..h {
            base[i] <<= 64 - (min_len as usize + i);
        }
        Box::new(PairsData {
            index_table: &[],
            size_table: &[],
            data: &[],
            offset: offset,
            sym_len: sym_len,
            sym_pat: sym_pat,
            block_size: block_size,
            idx_bits: idx_bits,
            min_len: min_len,
            const_val: 0,
            base: base,
        })
    }
    fn align_slice(data: &[u8], align: usize) -> &[u8] {
        let ptr1 = data.as_ptr() as usize;
        let ptr2 = (ptr1 + align - 1) & !(align - 1);
        &data[(ptr2 - ptr1)..]
    }
    fn slice<'a, T>(data: &mut &'a [u8], size: usize) -> &'a [T] {
        let ptr = data.as_ptr();
        *data = &data[size * std::mem::size_of::<T>()..];
        unsafe { slice::from_raw_parts(ptr as *const T, size) }
    }
    fn cast_slice<T>(data: &[u8], size: usize) -> &[T] {
        assert!(data.len() >= size * std::mem::size_of::<T>());
        unsafe { slice::from_raw_parts(data.as_ptr() as *const T, size) }
    }
    fn read_magic(mmap: &Option<Box<Mmap>>) -> u32 {
        let data: &[u8] = &*mmap.as_ref().unwrap();
        u32::from_le(cast_slice(data, 1)[0])
    }
    fn mmap_to_slice(mmap: &Option<Box<Mmap>>) -> &'static [u8] {
        let data: &[u8] = &*mmap.as_ref().unwrap();
        unsafe { slice::from_raw_parts(data.as_ptr(), data.len()) }
    }
    fn init_table<T: TbTable>(e: &T::Entry, name: &str) -> bool {
        let tb_map = map_file(name, T::Type::suffix());
        if tb_map.is_none() {
            return false;
        }
        if read_magic(&tb_map) != T::Type::magic() {
            eprintln!("Corrupted table: {}{}", name, T::Type::suffix());
            return false;
        }
        let tb = e.table_mut();
        *tb.mapping() = tb_map;
        let mut data = mmap_to_slice(tb.mapping());
        let split = T::Type::TYPE != Dtz::TYPE && data[4] & 0x01 != 0;
        tb.set_loss_only(data[4] & 0x04 != 0);
        data = &data[5..];
        let mut tb_size = [[0; 2]; 6];
        let num = T::num_tables();
        for t in 0..num {
            tb_size[t][0] = setup_pieces::<T::Enc>(tb.ei_mut(t, 0), e, data, 0, t);
            if split {
                tb_size[t][1] = setup_pieces::<T::Enc>(tb.ei_mut(t, 1), e, data, 4, t);
            }
            data = &data[e.num() as usize + 1 + (e.pawns(1) > 0) as usize..];
        }
        data = align_slice(data, 2);
        let mut size = [[0; 6]; 6];
        let mut flags = 0;
        for t in 0..num {
            tb.ei_mut(t, 0).precomp = Some(setup_pairs(
                &mut data,
                tb_size[t][0],
                &mut size[t][0..3],
                &mut flags,
                true,
            ));
            tb.set_flags(t, flags);
            if split {
                tb.ei_mut(t, 1).precomp = Some(setup_pairs(
                    &mut data,
                    tb_size[t][1],
                    &mut size[t][3..6],
                    &mut flags,
                    true,
                ));
            }
        }
        if T::Type::TYPE == Dtm::TYPE && !tb.loss_only() {
            let map = cast_slice(data, data.len() / 2);
            let mut idx = 0;
            for t in 0..num {
                for i in 0..2 {
                    tb.set_map_idx(t, 0, i, 1 + idx);
                    idx += 1 + u16::from_le(map[idx as usize]);
                }
                if split {
                    for i in 0..2 {
                        tb.set_map_idx(t, 1, i, 1 + idx);
                        idx += 1 + u16::from_le(map[idx as usize]);
                    }
                }
            }
            tb.set_map(slice(&mut data, idx as usize));
        }
        if T::Type::TYPE == Dtz::TYPE {
            let mut idx = 0;
            for t in 0..num {
                if tb.flags(t) & 2 != 0 {
                    for i in 0..4 {
                        tb.set_map_idx(t, 0, i, 1 + idx);
                        idx += 1 + data[idx as usize] as u16;
                    }
                }
            }
            tb.set_map(slice(&mut data, idx as usize));
            data = align_slice(data, 2);
        }
        for t in 0..num {
            tb.ei_mut(t, 0).precomp.as_mut().unwrap().index_table = slice(&mut data, size[t][0]);
            if split {
                tb.ei_mut(t, 1).precomp.as_mut().unwrap().index_table =
                    slice(&mut data, size[t][3]);
            }
        }
        for t in 0..num {
            tb.ei_mut(t, 0).precomp.as_mut().unwrap().size_table = slice(&mut data, size[t][1]);
            if split {
                tb.ei_mut(t, 1).precomp.as_mut().unwrap().size_table = slice(&mut data, size[t][4]);
            }
        }
        for t in 0..num {
            data = align_slice(data, 64);
            tb.ei_mut(t, 0).precomp.as_mut().unwrap().data = slice(&mut data, size[t][2]);
            if split {
                data = align_slice(data, 64);
                tb.ei_mut(t, 1).precomp.as_mut().unwrap().data = slice(&mut data, size[t][5]);
            }
        }
        if T::Type::TYPE == Dtm::TYPE
            && calc_key_from_pieces(&tb.ei(0, 0).pieces[0..e.num() as usize]) != e.key()
        {
            tb.set_switched();
        }
        true
    }
    fn fill_squares(
        pos: &Position,
        pc: &[u8; TB_PIECES],
        num: usize,
        flip: bool,
        p: &mut [Square; TB_PIECES],
    ) {
        let mut i = 0;
        loop {
            let piece = Piece(pc[i] as u32);
            let b = pos.pieces_cp(piece.color() ^ flip, piece.piece_type());
            for sq in b {
                p[i] = sq;
                i += 1;
            }
            if i == num as usize {
                break;
            }
        }
    }
    fn probe_helper<T: TbTable>(
        pos: &Position,
        e: &T::Entry,
        s: <T::Type as TbType>::Select,
        success: &mut i32,
    ) -> i32 {
        if !e.exists() {
            *success = 0;
            return 0;
        }
        let key = pos.material_key();
        let tb = e.table();
        if !tb.ready().load(Ordering::Acquire) {
            let _lock = e.lock().lock().unwrap();
            if !tb.ready().load(Ordering::Relaxed) {
                if !init_table::<T>(e, &prt_str(pos, e.key() != key)) {
                    *success = 0;
                    return 0;
                }
                tb.ready().store(true, Ordering::Release);
            }
        }
        let flip = if !e.symmetric() {
            (key != e.key()) != tb.switched()
        } else {
            pos.side_to_move() != WHITE
        };
        let bside = (!e.symmetric()
            && (((key != e.key()) != tb.switched()) == (pos.side_to_move() == WHITE)))
            as usize;
        let t = if T::Enc::ENC != PieceEnc::ENC {
            let color = Piece(tb.ei(0, 0).pieces[0] as u32).color();
            let b = pos.pieces_cp(color ^ flip, PAWN);
            leading_pawn_table::<T::Enc>(b, flip) as usize
        } else {
            0
        };
        let mut p: [Square; TB_PIECES] = [Square(0); TB_PIECES];
        fill_squares(pos, &tb.ei(t, bside).pieces, e.num() as usize, flip, &mut p);
        if T::Enc::ENC != PieceEnc::ENC && flip {
            for i in 0..e.num() as usize {
                p[i] = !p[i];
            }
        }
        let idx = encode::<T::Enc>(&mut p, &tb.ei(t, bside), e);
        let res = decompress_pairs(&tb.ei(t, bside).precomp.as_ref().unwrap(), idx);
        tb.map(t, bside, res, s)
    }
    fn probe_table<T: TbType>(pos: &Position, s: T::Select, success: &mut i32) -> i32 {
        let key = pos.material_key();
        if T::TYPE == Wdl::TYPE && pos.pieces() == pos.pieces_p(KING) {
            return 0;
        }
        let mut res = 0;
        let map = unsafe { Box::from_raw(TB_MAP) };
        match map.get(&key) {
            None => {
                *success = 0;
            }
            Some(&TbHashEntry::Piece(idx)) => {
                let e = unsafe { &PIECE_ENTRIES[idx] };
                res = probe_helper::<T::PieceTable>(pos, e, s, success);
            }
            Some(&TbHashEntry::Pawn(idx)) => {
                let e = unsafe { &PAWN_ENTRIES[idx] };
                res = probe_helper::<T::PawnTable>(pos, e, s, success);
            }
        }
        std::mem::forget(map);
        res
    }
    fn add_underprom_caps(pos: &Position, list: &mut [ExtMove], end: usize) -> usize {
        let mut extra = end;
        for idx in 0..end {
            let m = list[idx].m;
            if m.move_type() == PROMOTION && pos.piece_on(m.to()) != NO_PIECE {
                list[extra].m = Move(m.0 - (1 << 12));
                list[extra + 1].m = Move(m.0 - (2 << 12));
                list[extra + 2].m = Move(m.0 - (3 << 12));
                extra += 3;
            }
        }
        extra
    }
    fn probe_ab(pos: &mut Position, mut alpha: i32, beta: i32, success: &mut i32) -> i32 {
        assert!(pos.ep_square() == Square::NONE);
        let mut list: [ExtMove; 64] = [ExtMove {
            m: Move::NONE,
            value: 0,
        }; 64];
        let end = if pos.checkers() == 0 {
            let end = generate::<Captures>(pos, &mut list, 0);
            add_underprom_caps(pos, &mut list, end)
        } else {
            generate::<Evasions>(pos, &mut list, 0)
        };
        for &m in list[0..end].iter() {
            if !pos.capture(m.m) || !pos.legal(m.m) {
                continue;
            }
            let gives_check = pos.gives_check(m.m);
            pos.do_move(m.m, gives_check);
            let v = -probe_ab(pos, -beta, -alpha, success);
            pos.undo_move(m.m);
            if *success == 0 {
                return 0;
            }
            if v > alpha {
                if v >= beta {
                    return v;
                }
                alpha = v;
            }
        }
        let v = probe_table::<Wdl>(pos, (), success);
        if alpha >= v {
            alpha
        } else {
            v
        }
    }
    pub fn probe_wdl(pos: &mut Position, success: &mut i32) -> i32 {
        let mut list: [ExtMove; 64] = [ExtMove {
            m: Move::NONE,
            value: 0,
        }; 64];
        let mut end = if pos.checkers() == 0 {
            let end = generate::<Captures>(pos, &mut list, 0);
            add_underprom_caps(pos, &mut list, end)
        } else {
            generate::<Evasions>(pos, &mut list, 0)
        };
        let mut best_cap = -3;
        let mut best_ep = -3;
        for &m in list[0..end].iter() {
            if !pos.capture(m.m) || !pos.legal(m.m) {
                continue;
            }
            let gives_check = pos.gives_check(m.m);
            pos.do_move(m.m, gives_check);
            let v = -probe_ab(pos, -2, -best_cap, success);
            pos.undo_move(m.m);
            if *success == 0 {
                return 0;
            }
            if v > best_cap {
                if v == 2 {
                    *success = 2;
                    return 2;
                }
                if m.m.move_type() != ENPASSANT {
                    best_cap = v;
                } else if v > best_ep {
                    best_ep = v;
                }
            }
        }
        let v = probe_table::<Wdl>(pos, (), success);
        if *success == 0 {
            return 0;
        }
        if best_ep > best_cap {
            if best_ep > v {
                *success = 2;
                return best_ep;
            }
            best_cap = best_ep;
        }
        if best_cap >= v {
            *success = 1 + (best_cap > 0) as i32;
            return best_cap;
        }
        if best_ep > -3 && v == 0 {
            for &m in list[0..end].iter() {
                if m.m.move_type() != ENPASSANT && pos.legal(m.m) {
                    return v;
                }
            }
            if pos.checkers() == 0 {
                end = generate::<Quiets>(pos, &mut list, 0);
                for &m in list[0..end].iter() {
                    if m.m.move_type() != ENPASSANT && pos.legal(m.m) {
                        return v;
                    }
                }
            }
            *success = 2;
            return best_ep;
        }
        v
    }
    fn probe_dtm_loss(pos: &mut Position, success: &mut i32) -> Value {
        let mut best = -Value::INFINITE;
        let mut num_ep = 0;
        let mut list: [ExtMove; 64] = [ExtMove {
            m: Move::NONE,
            value: 0,
        }; 64];
        let end = if pos.checkers() == 0 {
            let end = generate::<Captures>(pos, &mut list, 0);
            add_underprom_caps(pos, &mut list, end)
        } else {
            generate::<Evasions>(pos, &mut list, 0)
        };
        for &m in list[0..end].iter() {
            if !pos.capture(m.m) || !pos.legal(m.m) {
                continue;
            }
            if m.m.move_type() == ENPASSANT {
                num_ep += 1;
            }
            let gives_check = pos.gives_check(m.m);
            pos.do_move(m.m, gives_check);
            let v = -probe_dtm_win(pos, success) + 1;
            pos.undo_move(m.m);
            best = std::cmp::max(best, v);
            if *success == 0 {
                return Value::NONE;
            }
        }
        if num_ep != 0 && MoveList::new::<Legal>(pos).len() == num_ep {
            return best;
        }
        let v = -Value::MATE + 2 * probe_table::<Dtm>(pos, false, success);
        std::cmp::max(best, v)
    }
    fn probe_dtm_win(pos: &mut Position, success: &mut i32) -> Value {
        let mut best = -Value::INFINITE;
        let mut list: [ExtMove; 256] = [ExtMove {
            m: Move::NONE,
            value: 0,
        }; 256];
        let end = if pos.checkers() != 0 {
            generate::<Evasions>(pos, &mut list, 0)
        } else {
            generate::<NonEvasions>(pos, &mut list, 0)
        };
        for &m in list[0..end].iter() {
            if !pos.legal(m.m) {
                continue;
            }
            let gives_check = pos.gives_check(m.m);
            pos.do_move(m.m, gives_check);
            let wdl = if pos.ep_square() != Square::NONE {
                probe_wdl(pos, success)
            } else {
                probe_ab(pos, -1, 0, success)
            };
            let v = if wdl < 0 && *success != 0 {
                -probe_dtm_loss(pos, success) - 1
            } else {
                -Value::INFINITE
            };
            pos.undo_move(m.m);
            best = std::cmp::max(best, v);
            if *success == 0 {
                return Value::NONE;
            }
        }
        best
    }
    pub fn probe_dtm(pos: &mut Position, wdl: i32, success: &mut i32) -> Value {
        debug_assert!(wdl != 0);
        if wdl > 0 {
            probe_dtm_win(pos, success)
        } else {
            probe_dtm_loss(pos, success)
        }
    }
    const WDL_TO_DTZ: [i32; 5] = [-1, -101, 0, 101, 1];
    pub fn probe_dtz(pos: &mut Position, success: &mut i32) -> i32 {
        let wdl = probe_wdl(pos, success);
        if *success == 0 {
            return 0;
        }
        if wdl == 0 {
            return 0;
        }
        if *success == 2 {
            return WDL_TO_DTZ[(wdl + 2) as usize];
        }
        let mut list: [ExtMove; 256] = [ExtMove {
            m: Move::NONE,
            value: 0,
        }; 256];
        let mut end = 0;
        if wdl > 0 {
            end = if pos.checkers() == 0 {
                generate::<NonEvasions>(pos, &mut list, 0)
            } else {
                generate::<Evasions>(pos, &mut list, 0)
            };
            for &m in list[0..end].iter() {
                if pos.moved_piece(m.m).piece_type() != PAWN || pos.capture(m.m) || !pos.legal(m.m)
                {
                    continue;
                }
                let gives_check = pos.gives_check(m.m);
                pos.do_move(m.m, gives_check);
                let v = -probe_wdl(pos, success);
                pos.undo_move(m.m);
                if *success == 0 {
                    return 0;
                }
                if v == wdl {
                    return WDL_TO_DTZ[(wdl + 2) as usize];
                }
            }
        }
        let dtz = probe_table::<Dtz>(pos, wdl, success);
        if *success >= 0 {
            return WDL_TO_DTZ[(wdl + 2) as usize] + if wdl > 0 { dtz } else { -dtz };
        }
        let mut best;
        if wdl > 0 {
            best = std::i32::MAX;
        } else {
            best = WDL_TO_DTZ[(wdl + 2) as usize];
            end = if pos.checkers() == 0 {
                generate::<NonEvasions>(pos, &mut list, 0)
            } else {
                generate::<Evasions>(pos, &mut list, 0)
            };
        }
        for &m in list[..end].iter() {
            if pos.capture(m.m) || pos.moved_piece(m.m).piece_type() == PAWN || !pos.legal(m.m) {
                continue;
            }
            let gives_check = pos.gives_check(m.m);
            pos.do_move(m.m, gives_check);
            let v = -probe_dtz(pos, success);
            pos.undo_move(m.m);
            if *success == 0 {
                return 0;
            }
            if wdl > 0 {
                if v > 0 && v + 1 < best {
                    best = v + 1;
                }
            } else {
                if v - 1 < best {
                    best = v - 1;
                }
            }
        }
        best
    }
    fn root_probe_dtz(pos: &mut Position, root_moves: &mut RootMoves) -> bool {
        let mut success = 1;
        let cnt50 = pos.rule50_count();
        let rep = pos.has_repeated();
        let bound = if ucioption::get_bool("Syzygy50MoveRule") {
            900
        } else {
            1
        };
        for ref mut rm in root_moves.iter_mut() {
            let m = rm.pv[0];
            let gives_check = pos.gives_check(m);
            pos.do_move(m, gives_check);
            let mut v;
            if pos.rule50_count() == 0 {
                v = -probe_wdl(pos, &mut success);
                v = WDL_TO_DTZ[(v + 2) as usize];
            } else {
                v = -probe_dtz(pos, &mut success);
                if v > 0 {
                    v += 1;
                } else if v < 0 {
                    v -= 1;
                }
            }
            if pos.checkers() != 0 && v == 2 && MoveList::new::<Legal>(pos).len() == 0 {
                v = 1;
            }
            pos.undo_move(m);
            if success == 0 {
                return false;
            }
            let r = if v > 0 {
                if v + cnt50 <= 99 && !rep {
                    1000
                } else {
                    1000 - (v + cnt50)
                }
            } else if v < 0 {
                if -v * 2 + cnt50 < 100 {
                    -1000
                } else {
                    -1000 + (-v + cnt50)
                }
            } else {
                0
            };
            rm.tb_rank = r;
            rm.tb_score = if r >= bound {
                Value::MATE - MAX_MATE_PLY - 1
            } else if r > 0 {
                std::cmp::max(3, r - 800) * PawnValueEg / 200
            } else if r == 0 {
                Value::DRAW
            } else if r > -bound {
                std::cmp::max(-3, r + 800) * PawnValueEg / 200
            } else {
                -Value::MATE + MAX_MATE_PLY + 1
            };
        }
        true
    }
    fn root_probe_wdl(pos: &mut Position, root_moves: &mut RootMoves) -> bool {
        const WDL_TO_RANK: [i32; 5] = [-1000, -899, 0, 899, 1000];
        const WDL_TO_VALUE: [Value; 5] = [
            Value(-32000 + 128 + 1),
            Value(-2),
            Value(0),
            Value(2),
            Value(32000 - 128 - 1),
        ];
        let mut success = 1;
        let move50 = ucioption::get_bool("Syzygy50MoveRule");
        for ref mut rm in root_moves.iter_mut() {
            let m = rm.pv[0];
            let gives_check = pos.gives_check(m);
            pos.do_move(m, gives_check);
            let mut v = -probe_wdl(pos, &mut success);
            pos.undo_move(m);
            if success == 0 {
                return false;
            }
            if !move50 {
                v = if v > 0 {
                    2
                } else if v < 0 {
                    -2
                } else {
                    0
                };
            }
            rm.tb_rank = WDL_TO_RANK[(v + 2) as usize];
            rm.tb_score = WDL_TO_VALUE[(v + 2) as usize];
        }
        true
    }
    fn root_probe_dtm(pos: &mut Position, root_moves: &mut RootMoves) -> bool {
        let mut success = 1;
        let mut tmp_score = Vec::new();
        for ref mut rm in root_moves.iter_mut() {
            let wdl = if rm.tb_score > PawnValueEg {
                2
            } else if rm.tb_score < -PawnValueEg {
                -2
            } else {
                0
            };
            if wdl == 0 {
                tmp_score.push(Value::ZERO);
            } else {
                let gives_check = pos.gives_check(rm.pv[0]);
                pos.do_move(rm.pv[0], gives_check);
                let v = -probe_dtm(pos, -wdl, &mut success);
                pos.undo_move(rm.pv[0]);
                if success == 0 {
                    return false;
                }
                tmp_score.push(if wdl > 0 { v - 1 } else { v + 1 });
            }
        }
        for (ref mut rm, &v) in root_moves.iter_mut().zip(tmp_score.iter()) {
            rm.tb_score = v;
            rm.tb_rank = if rm.tb_rank == 900 { 1001 } else { v.0 };
        }
        true
    }
    pub fn expand_mate(pos: &mut Position, idx: usize) {
        let mut success = 1;
        let mut chk = 0;
        let mut v = pos.root_moves[idx].score;
        let mut wdl = if v > Value::ZERO { 2 } else { -2 };
        for i in 0..pos.root_moves[idx].pv.len() {
            let m = pos.root_moves[idx].pv[i];
            v = if v > Value::ZERO { -v - 1 } else { -v + 1 };
            wdl = -wdl;
            let gives_check = pos.gives_check(m);
            pos.do_move(m, gives_check);
        }
        if popcount(pos.pieces()) <= cardinality_dtm() {
            while v != -Value::MATE {
                v = if v > Value::ZERO { -v - 1 } else { -v + 1 };
                wdl = -wdl;
                let mut best_move = Move::NONE;
                for m in MoveList::new::<Legal>(pos) {
                    let gives_check = pos.gives_check(m);
                    pos.do_move(m, gives_check);
                    if wdl < 0 {
                        chk = probe_wdl(pos, &mut success);
                    }
                    let w = if success != 0 && (wdl > 0 || chk < 0) {
                        probe_dtm(pos, wdl, &mut success)
                    } else {
                        Value::ZERO
                    };
                    pos.undo_move(m);
                    if success == 0 {
                        break;
                    }
                    if v == w {
                        best_move = m;
                        break;
                    }
                }
                if success == 0 || best_move == Move::NONE {
                    break;
                }
                pos.root_moves[idx].pv.push(best_move);
                let gives_check = pos.gives_check(best_move);
                pos.do_move(best_move, gives_check);
            }
        }
        for i in (0..pos.root_moves[idx].pv.len()).rev() {
            let m = pos.root_moves[idx].pv[i];
            pos.undo_move(m);
        }
    }
    pub fn rank_root_moves(pos: &mut Position, root_moves: &mut RootMoves) {
        let mut root_in_tb = false;
        let mut dtz_available = true;
        let mut dtm_available = false;
        if cardinality() >= popcount(pos.pieces()) && !pos.has_castling_right(ANY_CASTLING) {
            root_in_tb = root_probe_dtz(pos, root_moves);
            if !root_in_tb {
                dtz_available = false;
                root_in_tb = root_probe_wdl(pos, root_moves);
            }
            if root_in_tb && cardinality_dtm() >= popcount(pos.pieces()) {
                dtm_available = root_probe_dtm(pos, root_moves);
            }
        }
        if root_in_tb {
            root_moves.sort();
            if dtm_available || dtz_available || root_moves[0].tb_rank <= 0 {
                unsafe {
                    CARDINALITY = 0;
                }
            }
        } else {
            for ref mut rm in root_moves.iter_mut() {
                rm.tb_rank = 0;
            }
        }
        unsafe {
            ROOT_IN_TB = root_in_tb;
        }
    }
    const OFF_DIAG: [i8; 64] = [
        0, -1, -1, -1, -1, -1, -1, -1, 1, 0, -1, -1, -1, -1, -1, -1, 1, 1, 0, -1, -1, -1, -1, -1,
        1, 1, 1, 0, -1, -1, -1, -1, 1, 1, 1, 1, 0, -1, -1, -1, 1, 1, 1, 1, 1, 0, -1, -1, 1, 1, 1,
        1, 1, 1, 0, -1, 1, 1, 1, 1, 1, 1, 1, 0,
    ];
    const TRIANGLE: [u8; 64] = [
        6, 0, 1, 2, 2, 1, 0, 6, 0, 7, 3, 4, 4, 3, 7, 0, 1, 3, 8, 5, 5, 8, 3, 1, 2, 4, 5, 9, 9, 5,
        4, 2, 2, 4, 5, 9, 9, 5, 4, 2, 1, 3, 8, 5, 5, 8, 3, 1, 0, 7, 3, 4, 4, 3, 7, 0, 6, 0, 1, 2,
        2, 1, 0, 6,
    ];
    const FLIP_DIAG: [u8; 64] = [
        0, 8, 16, 24, 32, 40, 48, 56, 1, 9, 17, 25, 33, 41, 49, 57, 2, 10, 18, 26, 34, 42, 50, 58,
        3, 11, 19, 27, 35, 43, 51, 59, 4, 12, 20, 28, 36, 44, 52, 60, 5, 13, 21, 29, 37, 45, 53,
        61, 6, 14, 22, 30, 38, 46, 54, 62, 7, 15, 23, 31, 39, 47, 55, 63,
    ];
    const LOWER: [u8; 64] = [
        28, 0, 1, 2, 3, 4, 5, 6, 0, 29, 7, 8, 9, 10, 11, 12, 1, 7, 30, 13, 14, 15, 16, 17, 2, 8,
        13, 31, 18, 19, 20, 21, 3, 9, 14, 18, 32, 22, 23, 24, 4, 10, 15, 19, 22, 33, 25, 26, 5, 11,
        16, 20, 23, 25, 34, 27, 6, 12, 17, 21, 24, 26, 27, 35,
    ];
    const DIAG: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 8, 0, 1, 0, 0, 0, 0, 9, 0, 0, 0, 2, 0, 0, 10, 0, 0, 0, 0, 0, 3, 11, 0,
        0, 0, 0, 0, 0, 12, 4, 0, 0, 0, 0, 0, 13, 0, 0, 5, 0, 0, 0, 14, 0, 0, 0, 0, 6, 0, 15, 0, 0,
        0, 0, 0, 0, 7,
    ];
    const FLAP: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 12, 18, 18, 12, 6, 0, 1, 7, 13, 19, 19, 13, 7, 1, 2, 8, 14,
        20, 20, 14, 8, 2, 3, 9, 15, 21, 21, 15, 9, 3, 4, 10, 16, 22, 22, 16, 10, 4, 5, 11, 17, 23,
        23, 17, 11, 5, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    const PTWIST: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, 47, 35, 23, 11, 10, 22, 34, 46, 45, 33, 21, 9, 8, 20, 32, 44, 43,
        31, 19, 7, 6, 18, 30, 42, 41, 29, 17, 5, 4, 16, 28, 40, 39, 27, 15, 3, 2, 14, 26, 38, 37,
        25, 13, 1, 0, 12, 24, 36, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    const FLAP2: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 3, 3, 2, 1, 0, 4, 5, 6, 7, 7, 6, 5, 4, 8, 9, 10, 11, 11,
        10, 9, 8, 12, 13, 14, 15, 15, 14, 13, 12, 16, 17, 18, 19, 19, 18, 17, 16, 20, 21, 22, 23,
        23, 22, 21, 20, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    const PTWIST2: [u8; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, 47, 45, 43, 41, 40, 42, 44, 46, 39, 37, 35, 33, 32, 34, 36, 38, 31,
        29, 27, 25, 24, 26, 28, 30, 23, 21, 19, 17, 16, 18, 20, 22, 15, 13, 11, 9, 8, 10, 12, 14,
        7, 5, 3, 1, 0, 2, 4, 6, 0, 0, 0, 0, 0, 0, 0, 0,
    ];
    const KK_IDX: [[u16; 64]; 10] = [
        [
            0, 0, 0, 0, 1, 2, 3, 4, 0, 0, 0, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19,
            20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41,
            42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57,
        ],
        [
            58, 0, 0, 0, 59, 60, 61, 62, 63, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74,
            75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96,
            97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114,
            115,
        ],
        [
            116, 117, 0, 0, 0, 118, 119, 120, 121, 122, 0, 0, 0, 123, 124, 125, 126, 127, 128, 129,
            130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146,
            147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163,
            164, 165, 166, 167, 168, 169, 170, 171, 172, 173,
        ],
        [
            174, 0, 0, 0, 175, 176, 177, 178, 179, 0, 0, 0, 180, 181, 182, 183, 184, 0, 0, 0, 185,
            186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202,
            203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219,
            220, 221, 222, 223, 224, 225, 226, 227, 228,
        ],
        [
            229, 230, 0, 0, 0, 231, 232, 233, 234, 235, 0, 0, 0, 236, 237, 238, 239, 240, 0, 0, 0,
            241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 256, 257,
            258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 269, 270, 271, 272, 273, 274,
            275, 276, 277, 278, 279, 280, 281, 282, 283,
        ],
        [
            284, 285, 286, 287, 288, 289, 290, 291, 292, 293, 0, 0, 0, 294, 295, 296, 297, 298, 0,
            0, 0, 299, 300, 301, 302, 303, 0, 0, 0, 304, 305, 306, 307, 308, 309, 310, 311, 312,
            313, 314, 315, 316, 317, 318, 319, 320, 321, 322, 323, 324, 325, 326, 327, 328, 329,
            330, 331, 332, 333, 334, 335, 336, 337, 338,
        ],
        [
            0, 0, 339, 340, 341, 342, 343, 344, 0, 0, 345, 346, 347, 348, 349, 350, 0, 0, 441, 351,
            352, 353, 354, 355, 0, 0, 0, 442, 356, 357, 358, 359, 0, 0, 0, 0, 443, 360, 361, 362,
            0, 0, 0, 0, 0, 444, 363, 364, 0, 0, 0, 0, 0, 0, 445, 365, 0, 0, 0, 0, 0, 0, 0, 446,
        ],
        [
            0, 0, 0, 366, 367, 368, 369, 370, 0, 0, 0, 371, 372, 373, 374, 375, 0, 0, 0, 376, 377,
            378, 379, 380, 0, 0, 0, 447, 381, 382, 383, 384, 0, 0, 0, 0, 448, 385, 386, 387, 0, 0,
            0, 0, 0, 449, 388, 389, 0, 0, 0, 0, 0, 0, 450, 390, 0, 0, 0, 0, 0, 0, 0, 451,
        ],
        [
            452, 391, 392, 393, 394, 395, 396, 397, 0, 0, 0, 0, 398, 399, 400, 401, 0, 0, 0, 0,
            402, 403, 404, 405, 0, 0, 0, 0, 406, 407, 408, 409, 0, 0, 0, 0, 453, 410, 411, 412, 0,
            0, 0, 0, 0, 454, 413, 414, 0, 0, 0, 0, 0, 0, 455, 415, 0, 0, 0, 0, 0, 0, 0, 456,
        ],
        [
            457, 416, 417, 418, 419, 420, 421, 422, 0, 458, 423, 424, 425, 426, 427, 428, 0, 0, 0,
            0, 0, 429, 430, 431, 0, 0, 0, 0, 0, 432, 433, 434, 0, 0, 0, 0, 0, 435, 436, 437, 0, 0,
            0, 0, 0, 459, 438, 439, 0, 0, 0, 0, 0, 0, 460, 440, 0, 0, 0, 0, 0, 0, 0, 461,
        ],
    ];
    static mut BINOMIAL: [[usize; 64]; 7] = [[0; 64]; 7];
    static mut PAWN_IDX: [[usize; 24]; 6] = [[0; 24]; 6];
    static mut PFACTOR: [[usize; 4]; 6] = [[0; 4]; 6];
    static mut PAWN_IDX2: [[usize; 24]; 6] = [[0; 24]; 6];
    static mut PFACTOR2: [[usize; 6]; 6] = [[0; 6]; 6];
    fn off_diag(s: Square) -> i8 {
        OFF_DIAG[s.0 as usize]
    }
    fn is_off_diag(s: Square) -> bool {
        off_diag(s) != 0
    }
    fn triangle(s: Square) -> usize {
        TRIANGLE[s.0 as usize] as usize
    }
    fn flip_diag(s: Square) -> Square {
        Square(FLIP_DIAG[s.0 as usize] as u32)
    }
    fn lower(s: Square) -> usize {
        LOWER[s.0 as usize] as usize
    }
    fn diag(s: Square) -> usize {
        DIAG[s.0 as usize] as usize
    }
    fn skip(s1: Square, s2: Square) -> usize {
        (s1.0 > s2.0) as usize
    }
    fn flap<T: Encoding>(s: Square) -> usize {
        if T::ENC == FileEnc::ENC {
            FLAP[s.0 as usize] as usize
        } else {
            FLAP2[s.0 as usize] as usize
        }
    }
    fn ptwist<T: Encoding>(s: Square) -> usize {
        if T::ENC == FileEnc::ENC {
            PTWIST[s.0 as usize] as usize
        } else {
            PTWIST2[s.0 as usize] as usize
        }
    }
    fn kk_idx(s1: usize, s2: Square) -> usize {
        KK_IDX[s1][s2.0 as usize] as usize
    }
    fn binomial(n: usize, k: usize) -> usize {
        unsafe { BINOMIAL[k as usize][n] }
    }
    fn pawn_idx<T: Encoding>(num: usize, s: usize) -> usize {
        if T::ENC == FileEnc::ENC {
            unsafe { PAWN_IDX[num][s] }
        } else {
            unsafe { PAWN_IDX2[num][s] }
        }
    }
    fn pfactor<T: Encoding>(num: usize, s: usize) -> usize {
        if T::ENC == FileEnc::ENC {
            unsafe { PFACTOR[num][s] }
        } else {
            unsafe { PFACTOR2[num][s] }
        }
    }
    fn init_indices() {
        for i in 0..7 {
            for j in 0..64 {
                let mut f = 1;
                let mut l = 1;
                for k in 0..i {
                    f *= usize::wrapping_sub(j, k);
                    l *= k + 1;
                }
                unsafe {
                    BINOMIAL[i][j] = f / l;
                }
            }
        }
        for i in 0..6 {
            let mut s = 0;
            for j in 0..24 {
                unsafe {
                    PAWN_IDX[i][j] = s;
                }
                let k = (1 + (j % 6)) * 8 + (j / 6);
                s += binomial(ptwist::<FileEnc>(Square(k as u32)), i);
                if (j + 1) % 6 == 0 {
                    unsafe {
                        PFACTOR[i][j / 6] = s;
                    }
                    s = 0;
                }
            }
        }
        for i in 0..6 {
            let mut s = 0;
            for j in 0..24 {
                unsafe {
                    PAWN_IDX2[i][j] = s;
                }
                let k = (1 + (j / 4)) * 8 + (j % 4);
                s += binomial(ptwist::<RankEnc>(Square(k as u32)), i);
                if (j + 1) % 4 == 0 {
                    unsafe {
                        PFACTOR2[i][j / 4] = s;
                    }
                    s = 0;
                }
            }
        }
    }
    fn leading_pawn_table<T: Encoding>(pawns: Bitboard, flip: bool) -> u32 {
        if T::ENC == FileEnc::ENC {
            if pawns & (FILEA_BB | FILEB_BB | FILEG_BB | FILEH_BB) != 0 {
                if pawns & (FILEA_BB | FILEH_BB) != 0 {
                    FILE_A
                } else {
                    FILE_B
                }
            } else {
                if pawns & (FILEC_BB | FILEF_BB) != 0 {
                    FILE_C
                } else {
                    FILE_D
                }
            }
        } else {
            let b = if flip {
                Bitboard(pawns.0.swap_bytes())
            } else {
                pawns
            };
            lsb(b).rank() - 1
        }
    }
    fn encode<T: Encoding>(p: &mut [Square; TB_PIECES], ei: &EncInfo, entry: &T::Entry) -> usize {
        let n = entry.num() as usize;
        if T::ENC != PieceEnc::ENC {
            for i in 0..entry.pawns(0) {
                for j in i + 1..entry.pawns(0) {
                    if ptwist::<T>(p[i as usize]) < ptwist::<T>(p[j as usize]) {
                        p.swap(i as usize, j as usize);
                    }
                }
            }
        }
        if p[0].0 & 0x04 != 0 {
            for i in 0..n {
                p[i] = Square(p[i].0 ^ 0x07);
            }
        }
        let mut i;
        let mut idx;
        if T::ENC == PieceEnc::ENC {
            if p[0].0 & 0x20 != 0 {
                for i in 0..n {
                    p[i] = Square(p[i].0 ^ 0x38);
                }
            }
            for i in 0..n {
                if is_off_diag(p[i]) {
                    if off_diag(p[i]) > 0 && i < (if entry.kk_enc() { 2 } else { 3 }) {
                        for j in i..n {
                            p[j] = flip_diag(p[j]);
                        }
                    }
                    break;
                }
            }
            idx = if entry.kk_enc() {
                i = 2;
                kk_idx(triangle(p[0]), p[1])
            } else {
                i = 3;
                let s1 = skip(p[1], p[0]);
                let s2 = skip(p[2], p[0]) + skip(p[2], p[1]);
                if is_off_diag(p[0]) {
                    triangle(p[0]) * 63 * 62 + (p[1].0 as usize - s1) * 62 + (p[2].0 as usize - s2)
                } else if is_off_diag(p[1]) {
                    6 * 63 * 62 + diag(p[0]) * 28 * 62 + lower(p[1]) * 62 + p[2].0 as usize - s2
                } else if is_off_diag(p[2]) {
                    6 * 63 * 62
                        + 4 * 28 * 62
                        + diag(p[0]) * 7 * 28
                        + (diag(p[1]) - s1) * 28
                        + lower(p[2])
                } else {
                    6 * 63 * 62
                        + 4 * 28 * 62
                        + 4 * 7 * 28
                        + diag(p[0]) * 7 * 6
                        + (diag(p[1]) - s1) * 6
                        + (diag(p[2]) - s2)
                }
            };
            idx *= ei.factor[0];
        } else {
            let t = entry.pawns(0) as usize;
            idx = pawn_idx::<T>(t - 1, flap::<T>(p[0])) as usize;
            for i in 1..t {
                idx += binomial(ptwist::<T>(p[i]), t - i);
            }
            idx *= ei.factor[0];
            i = entry.pawns(0) as usize;
            let t = i + entry.pawns(1) as usize;
            if t > i {
                for j in i..t {
                    for k in j + 1..t {
                        if p[j].0 > p[k].0 {
                            p.swap(j, k);
                        }
                    }
                }
                let mut s = 0;
                for m in i..t {
                    let sq = p[m];
                    let mut skips = 0;
                    for k in 0..i {
                        skips += skip(sq, p[k]);
                    }
                    s += binomial(sq.0 as usize - skips - 8, m - i + 1);
                }
                idx += s * ei.factor[i];
                i = t;
            }
        }
        while i < n {
            let t = ei.norm[i] as usize;
            for j in i..i + t {
                for k in j + 1..i + t {
                    if p[j] > p[k] {
                        p.swap(j, k);
                    }
                }
            }
            let mut s = 0;
            for m in i..i + t {
                let sq = p[m];
                let mut skips = 0;
                for k in 0..i {
                    skips += skip(sq, p[k]);
                }
                s += binomial(sq.0 as usize - skips, m - i + 1);
            }
            idx += s * ei.factor[i];
            i += t;
        }
        idx
    }
    fn decompress_pairs(d: &PairsData, idx: usize) -> i32 {
        if d.idx_bits == 0 {
            return d.const_val as i32;
        }
        let main_idx = idx >> d.idx_bits;
        let mut lit_idx =
            (idx as isize & ((1isize << d.idx_bits) - 1)) - (1isize << (d.idx_bits - 1));
        let mut block = u32::from_le(d.index_table[main_idx].block) as usize;
        let idx_offset = u16::from_le(d.index_table[main_idx].offset);
        lit_idx += idx_offset as isize;
        while lit_idx < 0 {
            block -= 1;
            lit_idx += d.size_table[block] as isize + 1;
        }
        while lit_idx > d.size_table[block] as isize {
            lit_idx -= d.size_table[block] as isize + 1;
            block += 1;
        }
        let mut ptr = &d.data[block << d.block_size] as *const u8 as *const u32;
        let mut code = unsafe { u64::from_be(*(ptr as *const u64)) };
        ptr = unsafe { ptr.offset(2) };
        let mut bit_cnt = 0;
        let mut sym;
        loop {
            let mut l = 0;
            while code < d.base[l] {
                l += 1;
            }
            sym = u16::from_le(d.offset[l]) as usize;
            let l2 = l + d.min_len as usize;
            sym += ((code - d.base[l]) >> (64 - l2)) as usize;
            if lit_idx < d.sym_len[sym] as isize + 1 {
                break;
            }
            lit_idx -= d.sym_len[sym] as isize + 1;
            code <<= l2;
            bit_cnt += l2;
            if bit_cnt >= 32 {
                bit_cnt -= 32;
                code |= (unsafe { u32::from_be(*ptr) } as u64) << bit_cnt;
                ptr = unsafe { ptr.offset(1) };
            }
        }
        while d.sym_len[sym] != 0 {
            let w = &d.sym_pat[sym];
            let s1 = s1(w);
            if lit_idx < d.sym_len[s1] as isize + 1 {
                sym = s1;
            } else {
                lit_idx -= d.sym_len[s1] as isize + 1;
                sym = s2(w);
            }
        }
        s1(&d.sym_pat[sym]) as i32
    }
}
pub mod threads {
    use material;
    use movegen::*;
    use pawns;
    use position::Position;
    use search::*;
    use std;
    use std::cell::Cell;
    use std::sync::atomic::*;
    use std::sync::mpsc::*;
    use std::sync::{Arc, Condvar, Mutex, RwLock};
    use std::thread;
    use tb;
    use types::*;
    use ucioption;
    pub struct PosData {
        pub fen: String,
        pub moves: Vec<Move>,
    }
    pub struct SearchResult {
        pub depth: Depth,
        pub score: Value,
        pub pv: Vec<Move>,
    }
    pub struct ThreadState {
        pub exit: bool,
        pub searching: bool,
        pub clear: bool,
    }
    pub struct CommonState {
        pub root_moves: Arc<RootMoves>,
        pub pos_data: Arc<RwLock<PosData>>,
        pub result: Arc<Mutex<SearchResult>>,
    }
    pub struct ThreadCtrl {
        pub idx: usize,
        pub state: Mutex<ThreadState>,
        pub common: Mutex<CommonState>,
        pub cv: Condvar,
        pub nodes: Cell<u64>,
        pub tb_hits: Cell<u64>,
    }
    impl ThreadCtrl {
        pub fn new(idx: usize) -> ThreadCtrl {
            let thread_ctrl = ThreadCtrl {
                idx: idx,
                state: Mutex::new(ThreadState {
                    exit: false,
                    searching: true,
                    clear: false,
                }),
                common: Mutex::new(CommonState {
                    root_moves: Arc::new(Vec::new()),
                    pos_data: Arc::new(RwLock::new(PosData {
                        fen: String::new(),
                        moves: Vec::new(),
                    })),
                    result: Arc::new(Mutex::new(SearchResult {
                        depth: Depth::ZERO,
                        score: -Value::INFINITE,
                        pv: Vec::new(),
                    })),
                }),
                cv: Condvar::new(),
                nodes: Cell::new(0),
                tb_hits: Cell::new(0),
            };
            thread_ctrl
        }
    }
    unsafe impl Sync for ThreadCtrl {}
    type Handlers = Vec<thread::JoinHandle<()>>;
    type Threads = Vec<Arc<ThreadCtrl>>;
    static mut HANDLERS: *mut Handlers = 0 as *mut Handlers;
    static mut THREADS: *mut Threads = 0 as *mut Threads;
    static STOP: AtomicBool = AtomicBool::new(false);
    static PONDER: AtomicBool = AtomicBool::new(false);
    static STOP_ON_PONDERHIT: AtomicBool = AtomicBool::new(false);
    pub fn stop() -> bool {
        STOP.load(Ordering::Relaxed)
    }
    pub fn ponder() -> bool {
        PONDER.load(Ordering::Relaxed)
    }
    pub fn stop_on_ponderhit() -> bool {
        STOP_ON_PONDERHIT.load(Ordering::Relaxed)
    }
    pub fn set_stop(b: bool) {
        STOP.store(b, Ordering::SeqCst);
    }
    pub fn set_ponder(b: bool) {
        PONDER.store(b, Ordering::SeqCst);
    }
    pub fn set_stop_on_ponderhit(b: bool) {
        STOP_ON_PONDERHIT.store(b, Ordering::SeqCst);
    }
    pub fn init(requested: usize) {
        let handlers: Box<Handlers> = Box::new(Vec::new());
        let threads: Box<Threads> = Box::new(Vec::new());
        unsafe {
            HANDLERS = Box::into_raw(handlers);
            THREADS = Box::into_raw(threads);
        }
        set(requested);
    }
    pub fn free() {
        set(0);
        unsafe {
            std::mem::drop(Box::from_raw(HANDLERS));
            std::mem::drop(Box::from_raw(THREADS));
        }
    }
    pub fn set(requested: usize) {
        let mut handlers = unsafe { Box::from_raw(HANDLERS) };
        let mut threads = unsafe { Box::from_raw(THREADS) };
        while handlers.len() < requested {
            let idx = handlers.len();
            let (tx, rx) = channel();
            let builder = thread::Builder::new().stack_size(32 * 1024 * 1024);
            let handler = builder.spawn(move || run_thread(idx, tx)).unwrap();
            let th = rx.recv().unwrap();
            handlers.push(handler);
            threads.push(th);
        }
        while handlers.len() > requested {
            let handler = handlers.pop().unwrap();
            let th = threads.pop().unwrap();
            wake_up(&th, true, false);
            let _ = handler.join();
        }
        std::mem::forget(handlers);
        std::mem::forget(threads);
    }
    fn run_thread(idx: usize, tx: Sender<Arc<ThreadCtrl>>) {
        let mut pos = Box::new(Position::new());
        pos.pawns_table.reserve_exact(16384);
        for _ in 0..16384 {
            pos.pawns_table
                .push(std::cell::UnsafeCell::new(pawns::Entry::new()));
        }
        pos.material_table.reserve_exact(8192);
        for _ in 0..8192 {
            pos.material_table
                .push(std::cell::UnsafeCell::new(material::Entry::new()));
        }
        pos.is_main = idx == 0;
        pos.thread_idx = idx as i32;
        let th = Arc::new(ThreadCtrl::new(idx));
        tx.send(th.clone()).unwrap();
        pos.thread_ctrl = Some(th.clone());
        pos.previous_time_reduction = 1.;
        pos.cont_history.init();
        loop {
            let mut state = th.state.lock().unwrap();
            state.searching = false;
            th.cv.notify_one();
            while !state.searching {
                state = th.cv.wait(state).unwrap();
            }
            if state.exit {
                break;
            }
            if state.clear {
                if th.idx == 0 {
                    pos.previous_score = Value::INFINITE;
                    pos.previous_time_reduction = 1.;
                }
                pos.counter_moves = unsafe { std::mem::zeroed() };
                pos.main_history = unsafe { std::mem::zeroed() };
                pos.capture_history = unsafe { std::mem::zeroed() };
                pos.cont_history = unsafe { std::mem::zeroed() };
                pos.cont_history.init();
                state.clear = false;
                continue;
            }
            {
                let common = th.common.lock().unwrap();
                let pos_data = common.pos_data.read().unwrap();
                pos.init_states();
                pos.set(&pos_data.fen, ucioption::get_bool("UCI_Chess960"));
                for &m in pos_data.moves.iter() {
                    let gives_check = pos.gives_check(m);
                    pos.do_move(m, gives_check);
                }
                let fen = pos.fen();
                pos.set(&fen, ucioption::get_bool("UCI_Chess960"));
                pos.root_moves = (*common.root_moves).clone();
            }
            pos.nodes = 0;
            pos.tb_hits = 0;
            if th.idx == 0 {
                mainthread_search(&mut pos, &th);
            } else {
                thread_search(&mut pos, &th);
                let lock = th.common.lock().unwrap();
                let result = &mut lock.result.lock().unwrap();
                if pos.root_moves[0].score > result.score
                    && (pos.completed_depth >= result.depth
                        || pos.root_moves[0].score >= Value::MATE_IN_MAX_PLY)
                {
                    result.depth = pos.completed_depth;
                    result.score = pos.root_moves[0].score;
                    result.pv = pos.root_moves[0].pv.clone();
                }
            }
        }
    }
    fn wake_up(th: &ThreadCtrl, exit: bool, clear: bool) {
        let mut state = th.state.lock().unwrap();
        state.searching = true;
        state.exit = exit;
        state.clear = clear;
        th.cv.notify_one();
    }
    pub fn wake_up_slaves() {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        for th in threads.iter() {
            if th.idx != 0 {
                wake_up(th, false, false);
            }
        }
        std::mem::forget(threads);
    }
    pub fn clear_search() {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        for th in threads.iter() {
            wake_up(th, false, true);
        }
        std::mem::forget(threads);
    }
    pub fn wait_for_main() {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        for th in threads.iter() {
            if th.idx == 0 {
                let mut state = th.state.lock().unwrap();
                while state.searching {
                    state = th.cv.wait(state).unwrap();
                }
            }
        }
        std::mem::forget(threads);
    }
    pub fn wait_for_slaves() {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        for th in threads.iter() {
            if th.idx != 0 {
                let mut state = th.state.lock().unwrap();
                while state.searching {
                    state = th.cv.wait(state).unwrap();
                }
            }
        }
        std::mem::forget(threads);
    }
    pub fn wait_for_all() {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        for th in threads.iter() {
            let mut state = th.state.lock().unwrap();
            while state.searching {
                state = th.cv.wait(state).unwrap();
            }
        }
        std::mem::forget(threads);
    }
    pub fn start_thinking(
        pos: &mut Position,
        pos_data: &Arc<RwLock<PosData>>,
        limits: &LimitsType,
        searchmoves: Vec<Move>,
        ponder_mode: bool,
    ) {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        wait_for_main();
        set_stop_on_ponderhit(false);
        set_stop(false);
        set_ponder(ponder_mode);
        unsafe {
            LIMITS = (*limits).clone();
        }
        let mut root_moves = RootMoves::new();
        for m in MoveList::new::<Legal>(pos) {
            if searchmoves.is_empty() || searchmoves.iter().any(|&x| x == m) {
                root_moves.push(RootMove::new(m));
            }
        }
        tb::read_options();
        tb::rank_root_moves(pos, &mut root_moves);
        let root_moves = Arc::new(root_moves);
        let result = Arc::new(Mutex::new(SearchResult {
            depth: Depth::ZERO,
            score: -Value::INFINITE,
            pv: Vec::new(),
        }));
        for th in threads.iter() {
            th.nodes.set(0);
            th.tb_hits.set(0);
            let mut common = th.common.lock().unwrap();
            common.root_moves = root_moves.clone();
            common.pos_data = pos_data.clone();
            common.result = result.clone();
        }
        wake_up(&threads[0], false, false);
        std::mem::forget(threads);
    }
    pub fn nodes_searched() -> u64 {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        let mut nodes = 0;
        for th in threads.iter() {
            nodes += th.nodes.get();
        }
        std::mem::forget(threads);
        nodes
    }
    pub fn tb_hits() -> u64 {
        let threads: Box<Threads> = unsafe { Box::from_raw(THREADS) };
        let mut tb_hits = 0;
        for th in threads.iter() {
            tb_hits += th.tb_hits.get();
        }
        std::mem::forget(threads);
        tb_hits
    }
}
pub mod timeman {
    use search;
    use std;
    use types::*;
    use ucioption;
    static mut START_TIME: Option<std::time::Instant> = None;
    static mut OPTIMUM_TIME: i64 = 0;
    static mut MAXIMUM_TIME: i64 = 0;
    pub fn optimum() -> i64 {
        unsafe { OPTIMUM_TIME }
    }
    pub fn maximum() -> i64 {
        unsafe { MAXIMUM_TIME }
    }
    pub fn elapsed() -> i64 {
        let duration = unsafe { START_TIME.unwrap().elapsed() };
        (duration.as_secs() * 1000 + (duration.subsec_nanos() / 1000000) as u64) as i64
    }
    #[derive(PartialEq, Eq)]
    enum TimeType {
        OptimumTime,
        MaxTime,
    }
    use self::TimeType::*;
    const MOVE_HORIZON: i32 = 50;
    const MAX_RATIO: f64 = 7.3;
    const STEAL_RATIO: f64 = 0.34;
    fn importance(ply: i32) -> f64 {
        const XSCALE: f64 = 6.85;
        const XSHIFT: f64 = 64.5;
        const SKEW: f64 = 0.171;
        (1. + ((ply as f64 - XSHIFT) / XSCALE).exp()).powf(-SKEW) + std::f64::MIN_POSITIVE
    }
    fn remaining(
        my_time: i64,
        movestogo: i32,
        ply: i32,
        slow_mover: i64,
        time_type: TimeType,
    ) -> i64 {
        let max_ratio = if time_type == OptimumTime {
            1.
        } else {
            MAX_RATIO
        };
        let steal_ratio = if time_type == OptimumTime {
            0.
        } else {
            STEAL_RATIO
        };
        let move_importance = (importance(ply) * slow_mover as f64) / 100.;
        let mut other_moves_importance = 0.;
        for i in 1..movestogo {
            other_moves_importance += importance(ply + 2 * i);
        }
        let ratio1 =
            (max_ratio * move_importance) / (max_ratio * move_importance + other_moves_importance);
        let ratio2 = (move_importance + steal_ratio * other_moves_importance)
            / (move_importance + other_moves_importance);
        (my_time as f64 * ratio1.min(ratio2)) as i64
    }
    pub fn init(limits: &mut search::LimitsType, us: Color, ply: i32) {
        let min_think_time = ucioption::get_i32("Minimum Thinking Time") as i64;
        let move_overhead = ucioption::get_i32("Move Overhead") as i64;
        let slow_mover = ucioption::get_i32("Slow Mover") as i64;
        unsafe {
            START_TIME = limits.start_time;
            let time = std::cmp::max(limits.time[us.0 as usize], min_think_time);
            OPTIMUM_TIME = time;
            MAXIMUM_TIME = time;
        }
        let max_mtg = if limits.movestogo != 0 {
            std::cmp::min(limits.movestogo, MOVE_HORIZON)
        } else {
            MOVE_HORIZON
        };
        for hyp_mtg in 1..(max_mtg + 1) {
            let mut hyp_my_time = limits.time[us.0 as usize]
                + limits.inc[us.0 as usize] * (hyp_mtg - 1) as i64
                - move_overhead * (2 + std::cmp::min(hyp_mtg, 40) as i64);
            hyp_my_time = std::cmp::max(hyp_my_time, 0);
            let t1 = min_think_time + remaining(hyp_my_time, hyp_mtg, ply, slow_mover, OptimumTime);
            let t2 = min_think_time + remaining(hyp_my_time, hyp_mtg, ply, slow_mover, MaxTime);
            unsafe {
                OPTIMUM_TIME = std::cmp::min(t1, OPTIMUM_TIME);
                MAXIMUM_TIME = std::cmp::min(t2, MAXIMUM_TIME);
            }
        }
        if ucioption::get_bool("Ponder") {
            unsafe {
                OPTIMUM_TIME += OPTIMUM_TIME / 4;
            }
        }
    }
}
pub mod tt {
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
}
pub mod types {
    #![allow(dead_code)]
    use std;
    #[derive(Clone, Copy, PartialEq, Eq, Hash)]
    pub struct Key(pub u64);
    impl std::ops::BitXor<Key> for Key {
        type Output = Self;
        fn bitxor(self, rhs: Self) -> Self {
            Key(self.0 ^ rhs.0)
        }
    }
    impl std::ops::BitXorAssign<Key> for Key {
        fn bitxor_assign(&mut self, rhs: Key) {
            *self = *self ^ rhs;
        }
    }
    impl std::fmt::Display for Key {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> Result<(), std::fmt::Error> {
            write!(f, "{:X}", self.0)
        }
    }
    pub const MAX_MOVES: usize = 256;
    pub const MAX_PLY: i32 = 128;
    pub const MAX_MATE_PLY: i32 = 128;
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Color(pub u32);
    pub const WHITE: Color = Color(0);
    pub const BLACK: Color = Color(1);
    impl std::ops::Not for Color {
        type Output = Color;
        fn not(self) -> Self {
            Color(self.0 ^ 1)
        }
    }
    impl std::ops::BitXor<bool> for Color {
        type Output = Self;
        fn bitxor(self, rhs: bool) -> Self {
            Color(self.0 ^ (rhs as u32))
        }
    }
    impl Iterator for Color {
        type Item = Self;
        fn next(&mut self) -> Option<Self::Item> {
            let sq = self.0;
            self.0 += 1;
            Some(Color(sq))
        }
    }
    pub struct White;
    pub struct Black;
    pub trait ColorTrait {
        type KingSide: CastlingRightTrait;
        type QueenSide: CastlingRightTrait;
        const COLOR: Color;
    }
    impl ColorTrait for White {
        type KingSide = WhiteOO;
        type QueenSide = WhiteOOO;
        const COLOR: Color = WHITE;
    }
    impl ColorTrait for Black {
        type KingSide = BlackOO;
        type QueenSide = BlackOOO;
        const COLOR: Color = BLACK;
    }
    #[allow(non_camel_case_types)]
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum CastlingSide {
        KING,
        QUEEN,
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct CastlingRight(pub u32);
    pub const NO_CASTLING: CastlingRight = CastlingRight(0);
    pub const WHITE_OO: CastlingRight = CastlingRight(1);
    pub const WHITE_OOO: CastlingRight = CastlingRight(2);
    pub const BLACK_OO: CastlingRight = CastlingRight(4);
    pub const BLACK_OOO: CastlingRight = CastlingRight(8);
    pub const ANY_CASTLING: CastlingRight = CastlingRight(15);
    pub trait CastlingRightTrait {
        const CR: CastlingRight;
    }
    pub struct WhiteOO;
    pub struct WhiteOOO;
    pub struct BlackOO;
    pub struct BlackOOO;
    impl CastlingRightTrait for WhiteOO {
        const CR: CastlingRight = WHITE_OO;
    }
    impl CastlingRightTrait for WhiteOOO {
        const CR: CastlingRight = WHITE_OOO;
    }
    impl CastlingRightTrait for BlackOO {
        const CR: CastlingRight = BLACK_OO;
    }
    impl CastlingRightTrait for BlackOOO {
        const CR: CastlingRight = BLACK_OOO;
    }
    impl CastlingRight {
        pub fn make(c: Color, cs: CastlingSide) -> CastlingRight {
            use types::CastlingSide::*;
            match (c, cs) {
                (WHITE, KING) => WHITE_OO,
                (WHITE, _) => WHITE_OOO,
                (_, KING) => BLACK_OO,
                (_, _) => BLACK_OOO,
            }
        }
    }
    impl std::ops::BitOr<CastlingSide> for Color {
        type Output = CastlingRight;
        fn bitor(self, rhs: CastlingSide) -> CastlingRight {
            CastlingRight(1u32 << ((rhs as u32) + 2 * self.0))
        }
    }
    impl std::ops::BitAnd<CastlingRight> for CastlingRight {
        type Output = Self;
        fn bitand(self, rhs: Self) -> Self {
            CastlingRight(self.0 & rhs.0)
        }
    }
    impl std::ops::BitOr<CastlingRight> for CastlingRight {
        type Output = Self;
        fn bitor(self, rhs: Self) -> Self {
            CastlingRight(self.0 | rhs.0)
        }
    }
    impl std::ops::BitAndAssign<CastlingRight> for CastlingRight {
        fn bitand_assign(&mut self, rhs: Self) {
            *self = *self & rhs;
        }
    }
    impl std::ops::BitOrAssign<CastlingRight> for CastlingRight {
        fn bitor_assign(&mut self, rhs: Self) {
            *self = *self | rhs;
        }
    }
    impl std::ops::Not for CastlingRight {
        type Output = CastlingRight;
        fn not(self) -> Self {
            CastlingRight(!self.0)
        }
    }
    impl std::cmp::PartialEq<u32> for CastlingRight {
        fn eq(&self, rhs: &u32) -> bool {
            debug_assert!(*rhs == 0);
            self.0 == *rhs
        }
    }
    pub type Phase = i32;
    pub const PHASE_ENDGAME: Phase = 0;
    pub const PHASE_MIDGAME: Phase = 128;
    pub const MG: usize = 0;
    pub const EG: usize = 1;
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct ScaleFactor(pub i32);
    impl ScaleFactor {
        pub const DRAW: ScaleFactor = ScaleFactor(0);
        pub const ONEPAWN: ScaleFactor = ScaleFactor(48);
        pub const NORMAL: ScaleFactor = ScaleFactor(64);
        pub const MAX: ScaleFactor = ScaleFactor(128);
        pub const NONE: ScaleFactor = ScaleFactor(255);
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Bound(pub u32);
    impl Bound {
        pub const NONE: Bound = Bound(0);
        pub const UPPER: Bound = Bound(1);
        pub const LOWER: Bound = Bound(2);
        pub const EXACT: Bound = Bound(3);
    }
    impl std::ops::BitAnd<Bound> for Bound {
        type Output = Self;
        fn bitand(self, rhs: Self) -> Self {
            Bound(self.0 & rhs.0)
        }
    }
    impl std::ops::BitOr<Bound> for Bound {
        type Output = Self;
        fn bitor(self, rhs: Self) -> Self {
            Bound(self.0 | rhs.0)
        }
    }
    impl std::cmp::PartialEq<u32> for Bound {
        fn eq(&self, rhs: &u32) -> bool {
            debug_assert!(*rhs == 0);
            self.0 == *rhs
        }
    }
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct PieceType(pub u32);
    pub const NO_PIECE_TYPE: PieceType = PieceType(0);
    pub const PAWN: PieceType = PieceType(1);
    pub const KNIGHT: PieceType = PieceType(2);
    pub const BISHOP: PieceType = PieceType(3);
    pub const ROOK: PieceType = PieceType(4);
    pub const QUEEN: PieceType = PieceType(5);
    pub const KING: PieceType = PieceType(6);
    pub const QUEEN_DIAGONAL: PieceType = PieceType(7);
    pub const ALL_PIECES: PieceType = PieceType(0);
    pub struct Pawn;
    pub struct Knight;
    pub struct Bishop;
    pub struct Rook;
    pub struct Queen;
    pub struct King;
    pub trait PieceTypeTrait {
        const TYPE: PieceType;
    }
    impl PieceTypeTrait for Pawn {
        const TYPE: PieceType = PAWN;
    }
    impl PieceTypeTrait for Knight {
        const TYPE: PieceType = KNIGHT;
    }
    impl PieceTypeTrait for Bishop {
        const TYPE: PieceType = BISHOP;
    }
    impl PieceTypeTrait for Rook {
        const TYPE: PieceType = ROOK;
    }
    impl PieceTypeTrait for Queen {
        const TYPE: PieceType = QUEEN;
    }
    impl PieceTypeTrait for King {
        const TYPE: PieceType = KING;
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Piece(pub u32);
    pub const NO_PIECE: Piece = Piece(0);
    pub const W_PAWN: Piece = Piece(1);
    pub const W_KNIGHT: Piece = Piece(2);
    pub const W_BISHOP: Piece = Piece(3);
    pub const W_ROOK: Piece = Piece(4);
    pub const W_QUEEN: Piece = Piece(5);
    pub const W_KING: Piece = Piece(6);
    pub const B_PAWN: Piece = Piece(9);
    pub const B_KNIGHT: Piece = Piece(10);
    pub const B_BISHOP: Piece = Piece(11);
    pub const B_ROOK: Piece = Piece(12);
    pub const B_QUEEN: Piece = Piece(13);
    pub const B_KING: Piece = Piece(14);
    impl Piece {
        pub fn piece_type(self) -> PieceType {
            PieceType(self.0 & 7)
        }
        pub fn color(self) -> Color {
            Color(self.0 >> 3)
        }
        pub fn make(c: Color, pt: PieceType) -> Piece {
            Piece((c.0 << 3) + pt.0)
        }
    }
    impl Iterator for Piece {
        type Item = Self;
        fn next(&mut self) -> Option<Self::Item> {
            let pc = self.0;
            self.0 += 1;
            Some(Piece(pc))
        }
    }
    impl std::ops::Not for Piece {
        type Output = Self;
        fn not(self) -> Self {
            Piece(self.0 ^ 8)
        }
    }
    impl std::ops::BitXor<bool> for Piece {
        type Output = Self;
        fn bitxor(self, rhs: bool) -> Self {
            Piece(self.0 ^ ((rhs as u32) << 3))
        }
    }
    #[derive(Clone, Copy, PartialOrd, Ord, PartialEq, Eq)]
    pub struct Depth(pub i32);
    impl std::ops::Add<Depth> for Depth {
        type Output = Self;
        fn add(self, rhs: Self) -> Self {
            Depth(self.0 + rhs.0)
        }
    }
    impl std::ops::Sub<Depth> for Depth {
        type Output = Self;
        fn sub(self, rhs: Self) -> Self {
            Depth(self.0 - rhs.0)
        }
    }
    impl std::ops::AddAssign<Depth> for Depth {
        fn add_assign(&mut self, rhs: Self) {
            *self = *self + rhs;
        }
    }
    impl std::ops::SubAssign<Depth> for Depth {
        fn sub_assign(&mut self, rhs: Self) {
            *self = *self - rhs;
        }
    }
    impl std::ops::Mul<i32> for Depth {
        type Output = Self;
        fn mul(self, rhs: i32) -> Self {
            Depth(self.0 * rhs)
        }
    }
    impl std::ops::Mul<Depth> for i32 {
        type Output = Depth;
        fn mul(self, rhs: Depth) -> Depth {
            Depth(self * rhs.0)
        }
    }
    impl std::ops::Div<Depth> for Depth {
        type Output = i32;
        fn div(self, rhs: Depth) -> i32 {
            self.0 / rhs.0
        }
    }
    pub const ONE_PLY: Depth = Depth(1);
    pub const DEPTH_ZERO: Depth = Depth(0 * ONE_PLY.0);
    pub const DEPTH_QS_CHECKS: Depth = Depth(0 * ONE_PLY.0);
    pub const DEPTH_QS_NO_CHECKS: Depth = Depth(-1 * ONE_PLY.0);
    pub const DEPTH_QS_RECAPTURES: Depth = Depth(-5 * ONE_PLY.0);
    pub const DEPTH_NONE: Depth = Depth(-6 * ONE_PLY.0);
    pub const DEPTH_MAX: Depth = Depth(MAX_PLY * ONE_PLY.0);
    impl Depth {
        pub const ZERO: Depth = Depth(0 * ONE_PLY.0);
        pub const QS_CHECKS: Depth = Depth(0 * ONE_PLY.0);
        pub const QS_NO_CHECKS: Depth = Depth(-1 * ONE_PLY.0);
        pub const QS_RECAPTURES: Depth = Depth(-5 * ONE_PLY.0);
        pub const NONE: Depth = Depth(-6 * ONE_PLY.0);
        pub const MAX: Depth = Depth(MAX_PLY * ONE_PLY.0);
    }
    pub type File = u32;
    pub type Rank = u32;
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Square(pub u32);
    pub const FILE_A: File = 0;
    pub const FILE_B: File = 1;
    pub const FILE_C: File = 2;
    pub const FILE_D: File = 3;
    pub const FILE_E: File = 4;
    pub const FILE_F: File = 5;
    pub const FILE_G: File = 6;
    pub const FILE_H: File = 7;
    pub const RANK_1: Rank = 0;
    pub const RANK_2: Rank = 1;
    pub const RANK_3: Rank = 2;
    pub const RANK_4: Rank = 3;
    pub const RANK_5: Rank = 4;
    pub const RANK_6: Rank = 5;
    pub const RANK_7: Rank = 6;
    pub const RANK_8: Rank = 7;
    pub fn relative_rank(c: Color, r: Rank) -> Rank {
        r ^ (c.0 * 7)
    }
    impl Square {
        pub const A1: Square = Square(0);
        pub const B1: Square = Square(1);
        pub const C1: Square = Square(2);
        pub const D1: Square = Square(3);
        pub const E1: Square = Square(4);
        pub const F1: Square = Square(5);
        pub const G1: Square = Square(6);
        pub const H1: Square = Square(7);
        pub const A2: Square = Square(8);
        pub const B2: Square = Square(9);
        pub const C2: Square = Square(10);
        pub const D2: Square = Square(11);
        pub const E2: Square = Square(12);
        pub const F2: Square = Square(13);
        pub const G2: Square = Square(14);
        pub const H2: Square = Square(15);
        pub const A3: Square = Square(16);
        pub const B3: Square = Square(17);
        pub const C3: Square = Square(18);
        pub const D3: Square = Square(19);
        pub const E3: Square = Square(20);
        pub const F3: Square = Square(21);
        pub const G3: Square = Square(22);
        pub const H3: Square = Square(23);
        pub const A4: Square = Square(24);
        pub const B4: Square = Square(25);
        pub const C4: Square = Square(26);
        pub const D4: Square = Square(27);
        pub const E4: Square = Square(28);
        pub const F4: Square = Square(29);
        pub const G4: Square = Square(30);
        pub const H4: Square = Square(31);
        pub const A5: Square = Square(32);
        pub const B5: Square = Square(33);
        pub const C5: Square = Square(34);
        pub const D5: Square = Square(35);
        pub const E5: Square = Square(36);
        pub const F5: Square = Square(37);
        pub const G5: Square = Square(38);
        pub const H5: Square = Square(39);
        pub const A6: Square = Square(40);
        pub const B6: Square = Square(41);
        pub const C6: Square = Square(42);
        pub const D6: Square = Square(43);
        pub const E6: Square = Square(44);
        pub const F6: Square = Square(45);
        pub const G6: Square = Square(46);
        pub const H6: Square = Square(47);
        pub const A7: Square = Square(48);
        pub const B7: Square = Square(49);
        pub const C7: Square = Square(50);
        pub const D7: Square = Square(51);
        pub const E7: Square = Square(52);
        pub const F7: Square = Square(53);
        pub const G7: Square = Square(54);
        pub const H7: Square = Square(55);
        pub const A8: Square = Square(56);
        pub const B8: Square = Square(57);
        pub const C8: Square = Square(58);
        pub const D8: Square = Square(59);
        pub const E8: Square = Square(60);
        pub const F8: Square = Square(61);
        pub const G8: Square = Square(62);
        pub const H8: Square = Square(63);
        pub const NONE: Square = Square(64);
        pub fn file(self) -> File {
            self.0 & 7
        }
        pub fn rank(self) -> Rank {
            self.0 >> 3
        }
        pub fn relative(self, c: Color) -> Self {
            Square(self.0 ^ (c.0 * 56))
        }
        pub fn relative_rank(self, c: Color) -> Rank {
            relative_rank(c, self.rank())
        }
        pub fn is_ok(self) -> bool {
            self >= Square::A1 && self <= Square::H8
        }
        pub fn make(f: File, r: Rank) -> Square {
            Square((r << 3) | f)
        }
    }
    pub fn relative_square(c: Color, s: Square) -> Square {
        s.relative(c)
    }
    impl std::ops::Not for Square {
        type Output = Self;
        fn not(self) -> Self {
            Square(self.0 ^ Square::A8.0)
        }
    }
    impl std::ops::BitXor<bool> for Square {
        type Output = Self;
        fn bitxor(self, rhs: bool) -> Self {
            Square(self.0 ^ if rhs { 0x38 } else { 0 })
        }
    }
    impl Iterator for Square {
        type Item = Self;
        fn next(&mut self) -> Option<Self::Item> {
            let sq = self.0;
            *self = Square(sq + 1);
            Some(Square(sq))
        }
    }
    #[derive(Clone, Copy)]
    pub struct Squares {
        pub start: Square,
        pub end: Square,
    }
    impl Iterator for Squares {
        type Item = Square;
        fn next(&mut self) -> Option<Self::Item> {
            let s = self.start;
            if s != self.end {
                self.start += Direction(1);
                Some(s)
            } else {
                None
            }
        }
    }
    pub struct SquareList<'a> {
        list: &'a [Square],
        idx: usize,
    }
    impl<'a> SquareList<'a> {
        pub fn construct(list: &'a [Square]) -> SquareList<'a> {
            SquareList { list: list, idx: 0 }
        }
    }
    impl<'a> Iterator for SquareList<'a> {
        type Item = Square;
        fn next(&mut self) -> Option<Self::Item> {
            let s = self.list[self.idx];
            if s != Square::NONE {
                self.idx += 1;
                Some(s)
            } else {
                None
            }
        }
    }
    pub fn opposite_colors(s1: Square, s2: Square) -> bool {
        let s = s1.0 ^ s2.0;
        (((s >> 3) ^ s) & 1) != 0
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Direction(pub i32);
    impl std::ops::Neg for Direction {
        type Output = Self;
        fn neg(self) -> Self {
            Direction(-self.0)
        }
    }
    pub const NORTH: Direction = Direction(8);
    pub const EAST: Direction = Direction(1);
    pub const SOUTH: Direction = Direction(-8);
    pub const WEST: Direction = Direction(-1);
    pub const NORTH_EAST: Direction = Direction(9);
    pub const NORTH_WEST: Direction = Direction(7);
    pub const SOUTH_EAST: Direction = Direction(-7);
    pub const SOUTH_WEST: Direction = Direction(-9);
    impl std::ops::Add<Direction> for Direction {
        type Output = Self;
        fn add(self, rhs: Self) -> Self {
            Direction(self.0 + rhs.0)
        }
    }
    impl std::ops::Add<Direction> for Square {
        type Output = Square;
        fn add(self, rhs: Direction) -> Self {
            Square(u32::wrapping_add(self.0, rhs.0 as u32))
        }
    }
    impl std::ops::Sub<Direction> for Square {
        type Output = Square;
        fn sub(self, rhs: Direction) -> Self {
            Square(u32::wrapping_sub(self.0, rhs.0 as u32))
        }
    }
    impl std::ops::AddAssign<Direction> for Square {
        fn add_assign(&mut self, rhs: Direction) {
            *self = *self + rhs;
        }
    }
    impl std::ops::SubAssign<Direction> for Square {
        fn sub_assign(&mut self, rhs: Direction) {
            *self = *self - rhs;
        }
    }
    impl std::ops::Mul<Direction> for i32 {
        type Output = Direction;
        fn mul(self, rhs: Direction) -> Direction {
            Direction(self * rhs.0)
        }
    }
    pub fn pawn_push(c: Color) -> Direction {
        match c {
            WHITE => NORTH,
            _ => SOUTH,
        }
    }
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct MoveType(pub u32);
    pub const NORMAL: MoveType = MoveType(0);
    pub const PROMOTION: MoveType = MoveType(1 << 14);
    pub const ENPASSANT: MoveType = MoveType(2 << 14);
    pub const CASTLING: MoveType = MoveType(3 << 14);
    #[derive(Clone, Copy, PartialEq, Eq)]
    pub struct Move(pub u32);
    impl Move {
        pub const NONE: Move = Move(0);
        pub const NULL: Move = Move(65);
        pub fn from(self) -> Square {
            Square((self.0 >> 6) & 0x3f)
        }
        pub fn to(self) -> Square {
            Square(self.0 & 0x3f)
        }
        pub fn from_to(self) -> u32 {
            self.0 & 0xfff
        }
        pub fn move_type(self) -> MoveType {
            MoveType(self.0 & (3 << 14))
        }
        pub fn promotion_type(self) -> PieceType {
            PieceType(((self.0 >> 12) & 3) + KNIGHT.0)
        }
        pub fn is_ok(self) -> bool {
            self.from() != self.to()
        }
        pub fn make(from: Square, to: Square) -> Move {
            Move((from.0 << 6) + to.0)
        }
        pub fn make_prom(from: Square, to: Square, pt: PieceType) -> Move {
            Move(PROMOTION.0 + ((pt.0 - KNIGHT.0) << 12) + (from.0 << 6) + to.0)
        }
        pub fn make_special(mt: MoveType, from: Square, to: Square) -> Move {
            Move(mt.0 + (from.0 << 6) + to.0)
        }
    }
    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    pub struct Value(pub i32);
    impl Value {
        pub const ZERO: Value = Value(0);
        pub const DRAW: Value = Value(0);
        pub const KNOWN_WIN: Value = Value(10000);
        pub const MATE: Value = Value(32000);
        pub const INFINITE: Value = Value(32001);
        pub const NONE: Value = Value(32002);
        pub const MATE_IN_MAX_PLY: Value = Value(Value::MATE.0 - MAX_MATE_PLY - MAX_PLY);
        pub const MATED_IN_MAX_PLY: Value = Value(-Value::MATE.0 + MAX_MATE_PLY + MAX_PLY);
        pub fn abs(self) -> Value {
            Value(self.0.abs())
        }
    }
    #[allow(non_upper_case_globals)]
    pub const PawnValueMg: Value = Value(171);
    #[allow(non_upper_case_globals)]
    pub const KnightValueMg: Value = Value(764);
    #[allow(non_upper_case_globals)]
    pub const BishopValueMg: Value = Value(826);
    #[allow(non_upper_case_globals)]
    pub const RookValueMg: Value = Value(1282);
    #[allow(non_upper_case_globals)]
    pub const QueenValueMg: Value = Value(2526);
    #[allow(non_upper_case_globals)]
    pub const PawnValueEg: Value = Value(240);
    #[allow(non_upper_case_globals)]
    pub const KnightValueEg: Value = Value(848);
    #[allow(non_upper_case_globals)]
    pub const BishopValueEg: Value = Value(891);
    #[allow(non_upper_case_globals)]
    pub const RookValueEg: Value = Value(1373);
    #[allow(non_upper_case_globals)]
    pub const QueenValueEg: Value = Value(2646);
    pub const MIDGAME_LIMIT: Value = Value(15258);
    pub const ENDGAME_LIMIT: Value = Value(3915);
    const PIECE_VALUE: [[Value; 16]; 2] = [
        [
            Value::ZERO,
            PawnValueMg,
            KnightValueMg,
            BishopValueMg,
            RookValueMg,
            QueenValueMg,
            Value::ZERO,
            Value::ZERO,
            Value::ZERO,
            PawnValueMg,
            KnightValueMg,
            BishopValueMg,
            RookValueMg,
            QueenValueMg,
            Value::ZERO,
            Value::ZERO,
        ],
        [
            Value::ZERO,
            PawnValueEg,
            KnightValueEg,
            BishopValueEg,
            RookValueEg,
            QueenValueEg,
            Value::ZERO,
            Value::ZERO,
            Value::ZERO,
            PawnValueEg,
            KnightValueEg,
            BishopValueEg,
            RookValueEg,
            QueenValueEg,
            Value::ZERO,
            Value::ZERO,
        ],
    ];
    pub fn piece_value(phase: usize, pc: Piece) -> Value {
        PIECE_VALUE[phase][pc.0 as usize]
    }
    impl std::ops::Neg for Value {
        type Output = Self;
        fn neg(self) -> Self {
            Value(-self.0)
        }
    }
    impl std::ops::Add<Value> for Value {
        type Output = Self;
        fn add(self, rhs: Self) -> Self {
            Value(self.0 + rhs.0)
        }
    }
    impl std::ops::Add<i32> for Value {
        type Output = Self;
        fn add(self, rhs: i32) -> Self {
            self + Value(rhs)
        }
    }
    impl std::ops::Sub<i32> for Value {
        type Output = Self;
        fn sub(self, rhs: i32) -> Self {
            self - Value(rhs)
        }
    }
    impl std::ops::Sub<Value> for Value {
        type Output = Self;
        fn sub(self, rhs: Self) -> Self {
            Value(self.0 - rhs.0)
        }
    }
    impl std::ops::AddAssign<Value> for Value {
        fn add_assign(&mut self, rhs: Self) {
            *self = *self + rhs;
        }
    }
    impl std::ops::AddAssign<i32> for Value {
        fn add_assign(&mut self, rhs: i32) {
            *self = *self + rhs;
        }
    }
    impl std::ops::SubAssign<Value> for Value {
        fn sub_assign(&mut self, rhs: Self) {
            *self = *self - rhs;
        }
    }
    impl std::ops::SubAssign<i32> for Value {
        fn sub_assign(&mut self, rhs: i32) {
            *self = *self - rhs;
        }
    }
    impl std::ops::Mul<i32> for Value {
        type Output = Self;
        fn mul(self, rhs: i32) -> Self {
            Value(self.0 * rhs)
        }
    }
    impl std::ops::MulAssign<i32> for Value {
        fn mul_assign(&mut self, rhs: i32) {
            *self = *self * rhs;
        }
    }
    impl std::ops::Mul<Value> for i32 {
        type Output = Value;
        fn mul(self, rhs: Value) -> Value {
            Value(self * rhs.0)
        }
    }
    impl std::ops::Div<i32> for Value {
        type Output = Self;
        fn div(self, rhs: i32) -> Self {
            Value(self.0 / rhs)
        }
    }
    impl std::ops::DivAssign<i32> for Value {
        fn div_assign(&mut self, rhs: i32) {
            *self = *self / rhs;
        }
    }
    impl std::ops::Div<Value> for Value {
        type Output = i32;
        fn div(self, rhs: Self) -> i32 {
            self.0 / rhs.0
        }
    }
    pub fn mate_in(ply: i32) -> Value {
        Value::MATE - ply
    }
    pub fn mated_in(ply: i32) -> Value {
        -Value::MATE + ply
    }
    #[derive(Clone, Copy)]
    pub struct Score(pub i32);
    impl Score {
        pub const ZERO: Score = Score(0);
        pub fn eg(self) -> Value {
            Value((((self.0 + 0x8000) >> 16) as i16) as i32)
        }
        pub fn mg(self) -> Value {
            Value((self.0 as i16) as i32)
        }
        pub fn make(mg: i32, eg: i32) -> Self {
            Score((eg << 16) + mg)
        }
    }
    impl std::ops::Add<Score> for Score {
        type Output = Self;
        fn add(self, rhs: Self) -> Self {
            Score(self.0 + rhs.0)
        }
    }
    impl std::ops::AddAssign<Score> for Score {
        fn add_assign(&mut self, rhs: Self) {
            *self = *self + rhs;
        }
    }
    impl std::ops::Sub<Score> for Score {
        type Output = Self;
        fn sub(self, rhs: Self) -> Self {
            Score(self.0 - rhs.0)
        }
    }
    impl std::ops::SubAssign<Score> for Score {
        fn sub_assign(&mut self, rhs: Self) {
            *self = *self - rhs;
        }
    }
    impl std::ops::Neg for Score {
        type Output = Self;
        fn neg(self) -> Self {
            Score(-self.0)
        }
    }
    impl std::ops::Mul<i32> for Score {
        type Output = Self;
        fn mul(self, rhs: i32) -> Self {
            Score::make(rhs * self.mg().0, rhs * self.eg().0)
        }
    }
    pub struct True {}
    pub struct False {}
    pub trait Bool {
        const BOOL: bool;
    }
    impl Bool for True {
        const BOOL: bool = true;
    }
    impl Bool for False {
        const BOOL: bool = false;
    }
}
pub mod uci {
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
}
pub mod ucioption {
    use std;
    use tb;
    use threads;
    use tt;
    type OnChange = Option<fn(&OptVal)>;
    struct Opt {
        key: &'static str,
        val: OptVal,
        on_change: OnChange,
    }
    impl Opt {
        pub fn new(key: &'static str, val: OptVal, on_change: OnChange) -> Opt {
            Opt {
                key: key,
                val: val,
                on_change: on_change,
            }
        }
    }
    enum OptVal {
        StringOpt {
            def: &'static str,
            cur: String,
        },
        Spin {
            def: i32,
            cur: i32,
            min: i32,
            max: i32,
        },
        Check {
            def: bool,
            cur: bool,
        },
        Button,
        Combo {
            def: &'static str,
            cur: String,
        },
    }
    impl OptVal {
        pub fn string(def: &'static str) -> OptVal {
            OptVal::StringOpt {
                def: def,
                cur: String::from(def),
            }
        }
        pub fn spin(def: i32, min: i32, max: i32) -> OptVal {
            OptVal::Spin {
                def: def,
                cur: def,
                min: min,
                max: max,
            }
        }
        pub fn check(def: bool) -> OptVal {
            OptVal::Check { def: def, cur: def }
        }
        pub fn combo(def: &'static str) -> OptVal {
            OptVal::Combo {
                def: def,
                cur: String::from(&def[0..def.find(" var").unwrap()]).to_lowercase(),
            }
        }
    }
    fn on_clear_hash(_: &OptVal) {
        tt::clear();
    }
    fn on_hash_size(opt_val: &OptVal) {
        if let &OptVal::Spin { cur, .. } = opt_val {
            tt::resize(cur as usize);
        }
    }
    fn on_threads(opt_val: &OptVal) {
        if let &OptVal::Spin { cur, .. } = opt_val {
            threads::set(cur as usize);
        }
    }
    fn on_tb_path(opt_val: &OptVal) {
        if let &OptVal::StringOpt { ref cur, .. } = opt_val {
            tb::init(String::from(cur.as_str()));
        }
    }
    static mut OPTIONS: *mut Vec<Opt> = 0 as *mut Vec<Opt>;
    pub fn init() {
        let mut opts = Box::new(Vec::new());
        opts.push(Opt::new("Contempt", OptVal::spin(18, -100, 100), None));
        opts.push(Opt::new(
            "Analysis Contempt",
            OptVal::combo("Off var Off var White var Black"),
            None,
        ));
        opts.push(Opt::new(
            "Threads",
            OptVal::spin(1, 1, 512),
            Some(on_threads),
        ));
        opts.push(Opt::new(
            "Hash",
            OptVal::spin(16, 1, 128 * 1024),
            Some(on_hash_size),
        ));
        opts.push(Opt::new("Clear Hash", OptVal::Button, Some(on_clear_hash)));
        opts.push(Opt::new("Ponder", OptVal::check(false), None));
        opts.push(Opt::new("MultiPV", OptVal::spin(1, 1, 500), None));
        opts.push(Opt::new("Move Overhead", OptVal::spin(30, 0, 5000), None));
        opts.push(Opt::new(
            "Minimum Thinking Time",
            OptVal::spin(20, 0, 5000),
            None,
        ));
        opts.push(Opt::new("Slow Mover", OptVal::spin(84, 10, 1000), None));
        opts.push(Opt::new("UCI_AnalyseMode", OptVal::check(false), None));
        opts.push(Opt::new("UCI_Chess960", OptVal::check(false), None));
        opts.push(Opt::new(
            "SyzygyPath",
            OptVal::string("<empty>"),
            Some(on_tb_path),
        ));
        opts.push(Opt::new("SyzygyProbeDepth", OptVal::spin(1, 1, 100), None));
        opts.push(Opt::new("Syzygy50MoveRule", OptVal::check(true), None));
        opts.push(Opt::new("SyzygyProbeLimit", OptVal::spin(6, 0, 6), None));
        opts.push(Opt::new("SyzygyUseDTM", OptVal::check(true), None));
        unsafe {
            OPTIONS = Box::into_raw(opts);
        }
    }
    pub fn free() {
        let _opts = unsafe { Box::from_raw(OPTIONS) };
    }
    pub fn print() {
        let opts = unsafe { Box::from_raw(OPTIONS) };
        for opt in opts.iter() {
            print!(
                "\noption name {} type {}",
                opt.key,
                match opt.val {
                    OptVal::StringOpt { def, .. } => format!("string default {}", def),
                    OptVal::Spin { def, min, max, .. } =>
                        format!("spin default {} min {} max {}", def, min, max),
                    OptVal::Check { def, .. } =>
                        format!("check default {}", if def { true } else { false }),
                    OptVal::Button => format!("button"),
                    OptVal::Combo { def, .. } => format!("combo default {}", def),
                }
            );
        }
        print!("\n");
        std::mem::forget(opts);
    }
    pub fn set(key: &str, val: &str) {
        let mut opts = unsafe { Box::from_raw(OPTIONS) };
        if let Some(opt) = opts.iter_mut().find(|ref o| o.key == key) {
            match opt.val {
                OptVal::StringOpt { ref mut cur, .. } => *cur = String::from(val),
                OptVal::Spin { ref mut cur, .. } => *cur = val.parse().unwrap(),
                OptVal::Check { ref mut cur, .. } => *cur = val == "true",
                OptVal::Button => {}
                OptVal::Combo { ref mut cur, .. } => *cur = String::from(val).to_lowercase(),
            }
            if let Some(on_change) = opt.on_change {
                on_change(&opt.val);
            }
        } else {
            println!("No such option: {}", key);
        }
        unsafe {
            OPTIONS = Box::into_raw(opts);
        }
    }
    pub fn get_i32(key: &str) -> i32 {
        let opts = unsafe { Box::from_raw(OPTIONS) };
        let val = {
            let opt = opts.iter().find(|ref o| o.key == key).unwrap();
            if let OptVal::Spin { cur, .. } = opt.val {
                cur
            } else {
                0
            }
        };
        std::mem::forget(opts);
        val
    }
    pub fn get_bool(key: &str) -> bool {
        let opts = unsafe { Box::from_raw(OPTIONS) };
        let val = {
            let opt = opts.iter().find(|ref o| o.key == key).unwrap();
            if let OptVal::Check { cur, .. } = opt.val {
                cur
            } else {
                false
            }
        };
        std::mem::forget(opts);
        val
    }
    pub fn get_string(key: &str) -> String {
        let opts = unsafe { Box::from_raw(OPTIONS) };
        let val = {
            let opt = opts.iter().find(|ref o| o.key == key).unwrap();
            if let OptVal::StringOpt { ref cur, .. } = opt.val {
                String::from(cur.as_str())
            } else if let OptVal::Combo { ref cur, .. } = opt.val {
                String::from(cur.as_str())
            } else {
                String::new()
            }
        };
        std::mem::forget(opts);
        val
    }
}
use std::thread;
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
