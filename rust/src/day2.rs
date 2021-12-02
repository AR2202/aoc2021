pub mod day2sol {
    use std::fs;
    use std::str::FromStr;

    #[derive(PartialEq, Eq, Hash, Debug)]
    enum Instruction {
        Forward(i32),
        Up(i32),
        Down(i32),
    }

    impl std::str::FromStr for Instruction {
        type Err = String;

        fn from_str(s: &str) -> Result<Self, Self::Err> {
            let mut split_s = s.split_whitespace();
            let inst = split_s.next().unwrap();
            let num = split_s.next().unwrap();
            match inst {
                "forward" => match i32::from_str(num) {
                    Ok(i) => Ok(Instruction::Forward(i)),
                    _ => Err(format!("distance has to be an integer")),
                },
                "up" => match i32::from_str(num) {
                    Ok(i) => Ok(Instruction::Up(i)),
                    _ => Err(format!("distance has to be an integer")),
                },
                "down" => match i32::from_str(num) {
                    Ok(i) => Ok(Instruction::Down(i)),
                    _ => Err(format!("distance has to be an integer")),
                },
                _ => Err(format!("'{:?}' is not a valid value for Instruction", inst)),
            }
        }
    }

    #[derive(PartialEq, Eq, Hash, Debug)]
    struct Position {
        hor: i32,
        depth: i32,
    }
    #[derive(PartialEq, Eq, Hash, Debug)]
    struct PosAim {
        pos: Position,
        aim: i32,
    }
    pub fn day2a_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let position = contents
            .lines()
            .map(|line| Instruction::from_str(line).unwrap())
            .fold(Position { hor: 0, depth: 0 }, |pos, inst| moving(inst, pos));
        position.hor * position.depth
    }

    pub fn day2b_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let position = contents
            .lines()
            .map(|line| Instruction::from_str(line).unwrap())
            .fold(
                PosAim {
                    pos: Position { hor: 0, depth: 0 },
                    aim: 0,
                },
                |p, inst| moving2(inst, p),
            );
        position.pos.hor * position.pos.depth
    }

    fn moving(inst: Instruction, p: Position) -> Position {
        match inst {
            Instruction::Forward(i) => Position {
                hor: p.hor + i,
                depth: p.depth,
            },
            Instruction::Up(i) => Position {
                hor: p.hor,
                depth: p.depth - i,
            },
            Instruction::Down(i) => Position {
                hor: p.hor,
                depth: p.depth + i,
            },
        }
    }
    fn moving2(inst: Instruction, p: PosAim) -> PosAim {
        match inst {
            Instruction::Forward(i) => PosAim {
                pos: Position {
                    hor: p.pos.hor + i,
                    depth: p.pos.depth + i * p.aim,
                },
                aim: p.aim,
            },
            Instruction::Up(i) => PosAim {
                pos: p.pos,
                aim: p.aim - i,
            },
            Instruction::Down(i) => PosAim {
                pos: p.pos,
                aim: p.aim + i,
            },
        }
    }

    // Test for day2
    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn day2a_test() {
            assert_eq!(day2a_sol("../input/example2.txt"), 150);
        }

        #[test]
        fn day2b_test() {
            assert_eq!(day2b_sol("../input/example2.txt"), 900);
        }
    }
}
