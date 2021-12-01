mod day1;
pub use crate::day1::day1a;

fn main() {
    let day1 = day1a::day1a_sol("../input/day1.txt");
    println!("Solution to day1: {}", day1);
}
