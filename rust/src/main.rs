mod day1;
pub use crate::day1::day1sol;
mod day2;
pub use crate::day2::day2sol;
pub mod day3;
pub use crate::day3::day3sol;
pub mod day5;
pub use crate::day5::day5sol;

fn main() {
    let day1a = day1sol::day1a_sol("../input/day1.txt");
    println!("Solution to day1 part 1: {}", day1a);
    let day1b = day1sol::day1b_sol("../input/day1.txt");
    println!("Solution to day1 part 2: {}", day1b);
    let day2a = day2sol::day2a_sol("../input/day2.txt");
    println!("Solution to day2 part 1: {}", day2a);
    let day2b = day2sol::day2b_sol("../input/day2.txt");
    println!("Solution to day2 part 2: {}", day2b);
    let day3a = day3sol::day3a_sol("../input/day3.txt");
    println!("Solution to day3 part 1: {}", day3a);
    let day5a = day5sol::day5a_sol("../input/day5.txt");
    println!("Solution to day5 part 1: {:?}", day5a);
}
