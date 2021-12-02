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

    pub fn day2a_sol(filename: &str) -> i32 {
        let contents = fs::read_to_string(filename).unwrap();
        let position = contents
            .lines()
            .map(|line| Instruction::from_str(line).unwrap())
            .fold(Position { hor: 0, depth: 0 }, |pos, inst| moving(inst, pos));
        position.hor * position.depth
    }

    fn moving(inst: Instruction, pos: Position) -> Position {
        match inst {
            Instruction::Forward(i) => Position {
                hor: pos.hor + i,
                depth: pos.depth,
            },
            Instruction::Up(i) => Position {
                hor: pos.hor,
                depth: pos.depth - i,
            },
            Instruction::Down(i) => Position {
                hor: pos.hor,
                depth: pos.depth + i,
            },
        }
    }
}
