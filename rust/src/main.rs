mod day1;
pub use crate::day1::day1sol;
mod day2;
pub use crate::day2::day2sol;

fn main() {
    let day1a = day1sol::day1a_sol("../input/day1.txt");
    println!("Solution to day1 part 1: {}", day1a);
    let day1b = day1sol::day1b_sol("../input/day1.txt");
    println!("Solution to day1 part 2: {}", day1b);
    let day2a = day2sol::day2a_sol("../input/day2.txt");
    println!("Solution to day2 part 1: {}", day2a);
    let day2b = day2sol::day2b_sol("../input/day2.txt");
    println!("Solution to day2 part 2: {}", day2b);
}
