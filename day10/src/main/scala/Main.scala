import scala.io.Source.fromFile

case class Move(dy: Int, dx: Int);
case class Pos(y: Int, x: Int);
case class State(pos: Pos, mv: Move);

object Main {
  def findS(lines: Array[String]): Pos = {
    var rows = lines.length - 1;
    var cols = lines(0).length - 1;
    var result = for {
      i <- 0 to rows;
      j <- 0 to cols
      if lines(i)(j) == 'S'
    } yield (i, j)
    return Pos(result(0)._1, result(0)._2)
  }

  def getNextState(sym: Char, curState: State): State = {
    var curPos = curState.pos;
    var move = curState.mv;
    var nextMove = sym match {
      case 'F' => (move.dy, move.dx) match {
        case (-1, 0) => Move(0, 1)
        case (0, -1) => Move(1, 0)
        case _ => Move(-1, -1)
      }
      case '7' => (move.dy, move.dx) match {
        case (-1, 0) => Move(0, -1)
        case ( 0, 1) => Move(1,  0)
        case _ => Move(-1, -1)
      }
      case 'J' => (move.dy, move.dx) match {
        case (1, 0) => Move(0, -1)
        case (0, 1) => Move(-1, 0)
        case _ => Move(-1, -1)
      }
      case 'L' => (move.dy, move.dx) match {
        case ( 1, 0) => Move(0, 1)
        case (0, -1) => Move(-1, 0)
        case _ => Move(-1, -1)
      }
      case '-' => (move.dy, move.dx) match {
        case (0,  1) => Move(0,  1)
        case (0, -1) => Move(0, -1)
        case _ => Move(-1, -1)
      }
      case '|' => (move.dy, move.dx) match {
        case ( 1, 0) => Move(1, 0)
        case (-1, 0) => Move(-1, 0)
        case _ => Move(-1, -1)
      }
      case _ => Move(-1, -1)
    }
    val nextPos = Pos(curPos.y + nextMove.dy, curPos.x + nextMove.dx)
    return State(nextPos, nextMove)
  }

  def updateWeightMatrix(weightMatrix: Array[Array[Int]], lines: Array[String], startPos: Pos, startMove: Move): Array[Array[Int]] = {
    var pos = startPos
    var move = startMove
    var state = State(pos, move);
    var counter = 1;

    if (pos.y < 0 || pos.y >= lines.length)
      return weightMatrix;
    if (pos.x < 0 || pos.x >= lines(0).length)
      return weightMatrix;

    while (lines(pos.y)(pos.x) != '.'
      && weightMatrix(pos.y)(pos.x) != 0
      && (weightMatrix(pos.y)(pos.x) == -1 || weightMatrix(pos.y)(pos.x) > counter))
    {
      val oldPos = pos
      val sym = lines(pos.y)(pos.x);
      state = getNextState(sym, state);
      pos = state.pos

      if (state.mv.dx == -1 && state.mv.dy == -1)
        return weightMatrix; 

      weightMatrix(oldPos.y)(oldPos.x) = counter;
      counter = counter + 1;
    }
    
    return weightMatrix;
  }

  def part1(lines: Array[String]): Int = {
    var posS = findS(lines);
    var weightMatrix: Array[Array[Int]] = 
      Array.tabulate(lines.length, lines(0).length)( (x,y) => -1 );
    weightMatrix(posS.y)(posS.x) = 0;
    
    weightMatrix = updateWeightMatrix(weightMatrix, lines, Pos(posS.y, posS.x + 1), Move(0,  1));
    weightMatrix = updateWeightMatrix(weightMatrix, lines, Pos(posS.y, posS.x - 1), Move(0, -1));
    weightMatrix = updateWeightMatrix(weightMatrix, lines, Pos(posS.y + 1, posS.x), Move( 1, 0));
    weightMatrix = updateWeightMatrix(weightMatrix, lines, Pos(posS.y - 1, posS.x), Move(-1, 0));

    return weightMatrix.flatten.max;
  }

  def getCounter(lines: Array[String], startPos: Pos, startMove: Move): Int = {
    var pos = startPos
    var move = startMove
    var state = State(pos, move);
    var counter = 0;
    while (lines(pos.y)(pos.x) != '.'
      && lines(pos.y)(pos.x) != 'S')
    {
      val oldPos = pos
      val sym = lines(pos.y)(pos.x);
      state = getNextState(sym, state);
      pos = state.pos
      println(f"dx = ${(pos.x - oldPos.x)}; sy = ${(pos.y + oldPos.y)}")
      counter = counter + (pos.x - oldPos.x) * (pos.y + oldPos.y);
    }
    
    return counter;
  }

  def part2(lines: Array[String]): Int = {
    val rows = lines.length - 1
    val cols = lines(0).length - 1
    
    var posS = findS(lines);
    var counter = getCounter(lines, Pos(posS.y, posS.x + 1), Move(0, 1));

    return counter;
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
