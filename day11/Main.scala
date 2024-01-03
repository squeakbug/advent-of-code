import scala.io.Source.fromFile

object Main {
  def isFreeRow(line: String): Boolean = {
    line.count((sym) => sym == '#') == 0
  }

  def upperFree(lines: Array[String], yPos: Int): Int = {
    lines.take(yPos).count((line) => isFreeRow(line))
  }

  def leftFree(lines: Array[String], xPos: Int): Int = {
    (0 until xPos)
      .map((pos) => lines.map((line) => line(pos)))
      .count((line) => isFreeRow(String(line)))
  }

  def solve(lines: Array[String]): Int = {
    var yRange = 0 until lines.length;
    var xRange = 0 until lines(0).length;

    yRange
      .flatMap((y) => xRange.map((x) => (y, x)))
      .filter((pos) => lines(pos._1)(pos._2) == '#')
      .map((pos) =>
        (pos._1 + upperFree(lines, pos._1),
         pos._2 + leftFree(lines, pos._2)))
      .tails
      .filter(_.nonEmpty)
      .flatMap(xs => xs.tail.map((xs.head, _)))
      .foldLeft(0)((acc, pair) =>
        acc
        + Math.abs(pair._2._1 - pair._1._1) 
        + Math.abs(pair._2._2 - pair._1._2)
      )
  }

  def part1(lines: Array[String]): Int = {
    return solve(lines);
  }

  def part2(lines: Array[String]): Int = {
    return solve(lines)
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("usage: app <input_filename>")
      return ;
    }
    val fileName = args(1);
    val lines = fromFile(fileName).getLines().toArray;

    var part1_sln = part1(lines)
    var part2_sln = part2(lines)

    println(f"part1_sln = $part1_sln")
    println(f"part2_sln = $part2_sln")
  }
}
