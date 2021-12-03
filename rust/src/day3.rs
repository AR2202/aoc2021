pub mod day3sol {
    use itertools::Itertools;

    use std::fs;

    pub fn day3a_sol(filename: &str) -> u32 {
        let bits = read_input(filename);
        let transposed = transpose2(bits);
        let len = transposed[0].len();
        let ones: Vec<_> = transposed
            .iter()
            .map(|bit| bit.iter().filter(|&x| *x == '1').count())
            .collect();
        let zeros: Vec<_> = ones.iter().map(|count| len - count).collect();
        let maxes: Vec<_> = ones
            .iter()
            .zip(zeros.iter())
            .map(|(one, zero)| if zero > one { '0' } else { '1' })
            .collect();
        let mins: Vec<_> = maxes
            .iter()
            .map(|&num| if num == '0' { '1' } else { '0' })
            .collect();
        let base: u32 = 2;
        let dec_max: u32 = maxes
            .iter()
            .rev()
            .map(|c| c.to_digit(10).unwrap())
            .enumerate()
            .map(|(a, b)| base.pow(a as u32) * (b as u32))
            .sum();
        let dec_min: u32 = mins
            .iter()
            .rev()
            .map(|c| c.to_digit(10).unwrap())
            .enumerate()
            .map(|(a, b)| base.pow(a as u32) * (b as u32))
            .sum();
        dec_min * dec_max
    }

    fn read_input(filename: &str) -> Vec<Vec<char>> {
        let contents = fs::read_to_string(filename).unwrap();
        let characters: Vec<Vec<char>> = contents
            .lines()
            .map(|line| line.chars().collect())
            .collect();
        characters
    }

    fn transpose2<T>(v: Vec<Vec<T>>) -> Vec<Vec<T>> {
        let len = v[0].len();
        let mut iters: Vec<_> = v.into_iter().map(|n| n.into_iter()).collect();
        (0..len)
            .map(|_| {
                iters
                    .iter_mut()
                    .map(|n| n.next().unwrap())
                    .collect::<Vec<T>>()
            })
            .collect()
    }
    // Test for day3 part1
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn day3a_test() {
            assert_eq!(day3a_sol("../input/example3.txt"), 198);
        }
    }
}
