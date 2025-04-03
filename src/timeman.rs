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
fn remaining(my_time: i64, movestogo: i32, ply: i32, slow_mover: i64, time_type: TimeType) -> i64 {
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
