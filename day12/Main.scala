import scala.io.Source.fromFile
import util.matching.Regex

object Main {
  def parseNumbers(numbers: String): List[Int] = {
    numbers.split(",").map(_.toInt).toList
  }

  def parseLine(line: String): (String, List[Int]) = {
    line match
      case s"$springs $numbers" => (springs, parseNumbers(numbers))
  }

  private def generatePermuts(springs: String, list: List[String]): List[String] = {
    if springs.isEmpty then 
      list
    else
      val newList = springs.head match
        case '?' => list.flatMap(s => List(s.appended('.'), s.appended('#')))
        case x   => list.map(_.appended(x))
      generatePermuts(springs.tail, newList)
  }

  private val regex = """#+""".r

  private def procParsedLine(line: (String, List[Int])): Long = {
    val (springs, numbers) = line;
    generatePermuts(springs, List(""))
      .map(s => regex.findAllMatchIn(s))
      .map(iterator => iterator.map(m => m.toString.length))
      .count(_.toList == numbers)
  }

  private def part1(lines: List[String]): Long = {
    lines
      .map(line => parseLine(line))
      .map(procParsedLine).sum
  }

  private def isMatch(springs: String)(spring: Char)(index: Int) = {
    springs
      .take(index)
      .forall(List(spring, '?').contains(_))
  }

  private val memo = collection.mutable.Map[(String, List[Int]), Long]()

  private def memoize(springs: String)(counts: List[Int]): Long = {
    val key = (springs, counts)
    (memo.get(key), counts) match
      case (Some(value), _) => value
      case (_, Nil) =>
        val result = if isMatch(springs)('.')(springs.size) then 1L else 0L
        memo.addOne(key -> result)
        result
      case (_, head :: next) =>
        val searchRange = springs.size - counts.sum - counts.size + 1
        val result = (1 to searchRange)
          .filter(index =>
            isMatch(springs)('.')(index) &&
              isMatch(springs.drop(index))('#')(head)
          )
          .map(index => memoize(springs.drop(index + head))(next))
          .sum
        memo.addOne(key -> result)
        result
  }

  private def procParsedLine2(line: String): Long = {
    val cpyCount = 5;
    val (springs, counts) = parseLine(line)
    val unfoldedSprings = List.fill(cpyCount)(springs).mkString("?")
    val unfoldedCounts = List.fill(cpyCount)(counts).flatten
    memoize("." + unfoldedSprings)(unfoldedCounts)
  }

  private def part2(lines: List[String]): Long = {
    lines.map(procParsedLine2).sum
  }

  def main(args: Array[String]): Unit = {
    if (args.length != 2) {
      println("usage: app <input_filename>")
      return ;
    }
    val fileName = args(1);
    val lines = fromFile(fileName).getLines().toList;

    var part1_sln = part1(lines)
    var part2_sln = part2(lines)

    println(f"part1_sln = $part1_sln")
    println(f"part2_sln = $part2_sln")
  }
}
