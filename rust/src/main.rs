mod day1;
pub use crate::day1::day1sol;

fn main() {
    let day1a = day1sol::day1a_sol("../input/day1.txt");
    println!("Solution to day1 part 1: {}", day1a);
    let day1b = day1sol::day1b_sol("../input/day1.txt");
    println!("Solution to day1 part 2: {}", day1b);
}
