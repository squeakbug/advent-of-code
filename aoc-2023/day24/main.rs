use std::env;

fn part1(input: &str) -> String {
    todo!()
}

fn part2(input: &str) -> String {
    todo!()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = std::fs::read_to_string(filename).expect("Error reading file");

    let part1_solution = part1(&contents);
    let part2_solution = part2(&contents);

    println!("Part 1 solution: {}", part1_solution);
    println!("Part 2 solution: {}", part2_solution);
}