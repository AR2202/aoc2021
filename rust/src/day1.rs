pub mod day1sol {
    use std::fs;

    pub fn day1a_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let mut numbers = contents.lines().map(|line| line.parse::<i32>().unwrap());
        let mut prev = numbers.next().unwrap();

        numbers.fold(0, |acc, num| {
            if num > prev {
                prev = num;
                acc + 1
            } else {
                prev = num;
                acc
            }
        })
    }
    pub fn day1b_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let numbers: Vec<i32> = contents
            .lines()
            .map(|line| line.parse::<i32>().unwrap())
            .collect();
        let dropped3 = &numbers[3..];

        numbers
            .iter()
            .zip(dropped3.iter())
            .fold(0, |acc, (x, y)| if y > x { acc + 1 } else { acc })
    }
    // Test for day1 part1
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn day1a_test() {
            assert_eq!(day1a_sol("../input/example1.txt"), 7);
        }

        #[test]
        fn day1b_test() {
            assert_eq!(day1b_sol("../input/example1.txt"), 5);
        }
    }
}
