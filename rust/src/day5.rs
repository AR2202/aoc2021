pub mod day5sol {
    use std::cmp::{max, min};
    use std::collections::HashMap;
    use std::fs;

    #[derive(PartialEq, Eq, Hash, Debug)]
    pub struct Vent {
        x1: i32,
        y1: i32,
        x2: i32,
        y2: i32,
    }

    pub fn day5a_sol(filename: &str) -> usize {
        let hor_or_vert: Vec<(i32, i32)> = read_input(filename)
            .iter()
            .filter(|&vent| is_ho_or_vert(vent))
            .map(|vent| points_covered(vent))
            .collect::<Vec<Vec<(i32, i32)>>>()
            .concat();
        let mut hm = hor_or_vert.into_iter().fold(HashMap::new(), |mut acc, ch| {
            *acc.entry(ch).or_insert(0) += 1;
            acc
        });
        hm.retain(|_, v| *v >= 2);
        hm.keys().len()
    }
    pub fn day5b_sol(filename: &str) -> usize {
        let vents: Vec<Vent> = read_input(filename);
        let mut hor_or_vert: Vec<(i32, i32)> = read_input(filename)
            .iter()
            .filter(|&vent| is_ho_or_vert(vent))
            .map(|vent| points_covered(vent))
            .collect::<Vec<Vec<(i32, i32)>>>()
            .concat();
        let diag: Vec<(i32, i32)> = read_input(filename)
            .iter()
            .filter(|&vent| is_diagonal(vent))
            .map(|vent| points_covered_diag(vent))
            .collect::<Vec<Vec<(i32, i32)>>>()
            .concat();
        hor_or_vert.extend(diag);
        let mut hm = hor_or_vert.into_iter().fold(HashMap::new(), |mut acc, ch| {
            *acc.entry(ch).or_insert(0) += 1;
            acc
        });
        hm.retain(|_, v| *v >= 2);
        hm.keys().len()
    }
    fn read_input(filename: &str) -> Vec<Vent> {
        let contents = fs::read_to_string(filename).unwrap();
        let vents: Vec<Vent> = contents.lines().map(|line| parse_as_vent(line)).collect();
        vents
    }
    fn parse_as_vent(line: &str) -> Vent {
        let mut points = line.split(" -> ");
        let mut startpoint = points.next().expect("no start point").split(",");
        let x1 = startpoint
            .next()
            .expect("no x value")
            .parse::<i32>()
            .unwrap();
        let y1 = startpoint
            .next()
            .expect("no y value")
            .parse::<i32>()
            .unwrap();
        let mut endpoint = points.next().expect("no end point").split(",");
        let x2 = endpoint.next().expect("no x value").parse::<i32>().unwrap();
        let y2 = endpoint.next().expect("no y value").parse::<i32>().unwrap();
        Vent {
            x1: x1,
            y1: y1,
            x2: x2,
            y2: y2,
        }
    }

    fn is_horizontal(v: &Vent) -> bool {
        v.y1 == v.y2
    }
    fn is_vertical(v: &Vent) -> bool {
        v.x1 == v.x2
    }
    fn is_ho_or_vert(v: &Vent) -> bool {
        is_horizontal(v) || is_vertical(v)
    }
    fn is_ho_or_vert_or_diag(v: &Vent) -> bool {
        is_horizontal(v) || is_vertical(v) || is_diagonal(v)
    }
    fn is_diagonal(v: &Vent) -> bool {
        (v.x1 - v.x2).abs() == (v.y1 - v.y2).abs()
    }
    fn points_covered(v: &Vent) -> Vec<(i32, i32)> {
        let min_x = min(v.x1, v.x2);
        let min_y = min(v.y1, v.y2);
        let max_x = max(v.x1, v.x2);
        let max_y = max(v.y1, v.y2);
        let points: Vec<Vec<(i32, i32)>> = (min_x..max_x + 1)
            .map(|x| (min_y..max_y + 1).map(|y| (x, y)).collect())
            .collect();
        points.concat()
    }

    fn points_covered_diag(v: &Vent) -> Vec<(i32, i32)> {
        let min_x = min(v.x1, v.x2);
        let min_y = min(v.y1, v.y2);
        let max_x = max(v.x1, v.x2);
        let max_y = max(v.y1, v.y2);
        let points: Vec<(i32, i32)>;
        if (v.x1 - v.x2) == (v.y1 - v.y2) {
            points = (min_x..max_x + 1).zip(min_y..max_y + 1).collect();
        } else {
            points = (min_x..max_x + 1).rev().zip(min_y..max_y + 1).collect();
        }
        points
    }
    // Test for day5 part1
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn is_horizontal_test() {
            assert!(is_horizontal(&Vent {
                x1: 5,
                y1: 1,
                x2: 4,
                y2: 1
            }));
            assert!(!is_horizontal(&Vent {
                x1: 5,
                y1: 1,
                x2: 4,
                y2: 3
            }))
        }
        #[test]
        fn is_diagonal_test() {
            assert!(is_diagonal(&Vent {
                x1: 9,
                y1: 7,
                x2: 7,
                y2: 9
            }));
        }
        #[test]
        fn covered_test() {
            assert_eq!(
                points_covered(&Vent {
                    x1: 1,
                    y1: 1,
                    x2: 1,
                    y2: 3
                }),
                vec![(1, 1), (1, 2), (1, 3)]
            );
        }
        #[test]
        fn covered_test2() {
            assert_eq!(
                points_covered_diag(&Vent {
                    x1: 9,
                    y1: 7,
                    x2: 7,
                    y2: 9
                }),
                vec![(9, 7), (8, 8), (7, 9)]
            );
        }
        #[test]
        fn parse_test() {
            assert_eq!(
                parse_as_vent("1,1 -> 1,3"),
                Vent {
                    x1: 1,
                    y1: 1,
                    x2: 1,
                    y2: 3
                }
            );
        }
    }
}
