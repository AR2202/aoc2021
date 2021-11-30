pub mod day1a {
    use std::fs;

    pub fn day1a_sol() -> f64 {
        let filename = "../input/day1.txt";
        let contents = fs::read_to_string(filename);

        let contents2 = contents.unwrap();
        //let thelines = contents2.lines();
        println!("{}", contents2);
        5.0
    }

    // Test for day1 part1
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn day1a_test() {
            assert_eq!(day1a_sol(), 5.0);
        }
    }
}
