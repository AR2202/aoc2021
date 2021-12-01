pub mod day1a {
    use std::fs;

    pub fn day1a_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let mut numbers = contents.lines().map(|line| line.parse::<i32>().unwrap());
        let mut prev = numbers.next().unwrap();

        let n_increase = numbers.fold(0, |acc, num| {
            if num > prev {
                prev = num;
                acc + 1
            } else {
                prev = num;
                acc
            }
        });

        n_increase
    }

    // Test for day1 part1
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn day1a_test() {
            assert_eq!(day1a_sol("../input/example1.txt"), 7);
        }
    }
}
