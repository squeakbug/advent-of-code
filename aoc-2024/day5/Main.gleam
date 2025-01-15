import gleam/io
import gleam/erlang

fn main() {
  // Get command-line arguments
  args = io::args()

  // Check if an argument is provided
  case args {
    [] -> io::print("No input file provided.")
    [input_file] -> {
      // Read the content of the file
      case io::read_file(input_file) {
        Ok(content) -> {
          // Process the input with part1 and part2 functions
          part1_sln = part1(content)
          part2_sln = part2(content)

          // Print the results
          io::print("part1_sln = " ++ int_to_string(part1_sln))
          io::print("part2_sln = " ++ int_to_string(part2_sln))
        }
        Err(error) -> io::print("Error reading file: " ++ error)
      }
    }
  }
}

// Define your part1 and part2 functions here
fn part1(input: String) -> Int {
  // Your logic for part1
}

fn part2(input: String) -> Int {
  // Your logic for part2
}