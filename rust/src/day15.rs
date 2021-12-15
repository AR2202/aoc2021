pub mod day15sol {
    use petgraph::algo::astar;
    use petgraph::graph::node_index;
    use petgraph::graph::NodeIndex;
    use petgraph::Graph;
    use std::fs;

    pub fn graph_from_windows(v: &Vec<Vec<(usize, usize, u32)>>) {
        let len = v[0].len();
        let height = v.len();
        let edgevec: Vec<(u32, u32, u32)> = v
            .iter()
            .map(|subvec| {
                subvec
                    .windows(2)
                    .map(|weights| {
                        (
                            weights[0].0 as u32 + (weights[0].1 * len) as u32,
                            weights[1].0 as u32 + (weights[1].1 * len) as u32,
                            weights[1].2,
                        )
                    })
                    .collect()
            })
            .collect::<Vec<Vec<(u32, u32, u32)>>>()
            .concat();
        let gr = Graph::<i32, u32>::from_edges(&edgevec);
        let lastnode = (len * len) - 1;

        let path = astar(
            &gr,
            node_index(0),
            |finish| finish == node_index(lastnode),
            |e| *e.weight(),
            |_| 0,
        );
        println!("shortes path risk: {:?}", path.map(|x| x.0));
    }
    pub fn read_intlists(filename: &str) -> Vec<Vec<(usize, usize, u32)>> {
        let contents = fs::read_to_string(filename).unwrap();
        let mut ls: Vec<&str> = contents.lines().collect();
        let numlines = ls.len();
        let mut numbers: Vec<Vec<(usize, usize, u32)>> = ls
            .iter()
            .enumerate()
            .map(|(y, line)| {
                line.chars()
                    .enumerate()
                    .map(|(x, c)| (x, y, c.to_digit(10).unwrap()))
                    .collect()
            })
            .collect();
        let mut numbers_transposed = transpose2(numbers.clone());

        let mut numbers2: Vec<Vec<(usize, usize, u32)>> = ls
            .iter()
            .enumerate()
            .map(|(y, line)| {
                line.chars()
                    .rev()
                    .enumerate()
                    .map(|(x, c)| (numlines - 1 - x, y, c.to_digit(10).unwrap()))
                    .collect()
            })
            .collect();
        let contents3 = fs::read_to_string(filename).unwrap();
        let mut numbers3: Vec<Vec<(usize, usize, u32)>> = ls
            .iter()
            .rev()
            .enumerate()
            .map(|(y, line)| {
                line.chars()
                    .enumerate()
                    .map(|(x, c)| (x, numlines - 1 - y, c.to_digit(10).unwrap()))
                    .collect()
            })
            .collect();
        let mut numbers2_transposed = transpose2(numbers3);
        numbers.append(&mut numbers_transposed);
        numbers.append(&mut numbers2);
        numbers.append(&mut numbers2_transposed);
        numbers
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
}
