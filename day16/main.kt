import java.io.File
import java.util.*

data class Move(val dy: Int, val dx: Int);
data class Pos(val y: Int, val x: Int);
data class State(
    val pos: Pos,
    val mv: Move,
    var matrix: Array<Array<Char>>,
    var pattern: List<String>,
    val passed: MutableList<Pair<Pos, Move>>
);

fun getNextStates(s: State): List<State> {
    var (pos, mv, mat, pat, pass) = s;
    var result: MutableList<State> = mutableListOf();
    var isFinded: Boolean = false;

    val sym = pat[pos.y][pos.x];
    if (Pair(pos, mv) in pass) {
        return listOf();
    } else {
        pass.add(Pair(pos, mv));
    }

    if (sym == '.') {
        pos = Pos(pos.y + mv.dy, pos.x + mv.dx);
        isFinded = true;
    }
    if (sym == '-') {
        if (mv.dx == 1 || mv.dx == -1) {
            pos = Pos(pos.y, pos.x + mv.dx);
            isFinded = true;
        }
    }
    if (sym == '|') {
        if (mv.dy == 1 || mv.dy == -1) {
            pos = Pos(pos.y + mv.dy, pos.x);
            isFinded = true;
        }
    }
    if (sym == '\\') {
        if (mv.dx == 1) {
            pos = Pos(pos.y + 1, pos.x);
            mv = Move(dy = 1, dx = 0);
            isFinded = true;
        }
        else if (mv.dx == -1) {
            pos = Pos(pos.y - 1, pos.x);
            mv = Move(dy = -1, dx = 0);
            isFinded = true;
        }
        else if (mv.dy == -1) {
            pos = Pos(pos.y, pos.x - 1);
            mv = Move(dy = 0, dx = -1);
            isFinded = true;
        }
        else if (mv.dy == 1) {
            pos = Pos(pos.y, pos.x + 1);
            mv = Move(dy = 0, dx = 1);
            isFinded = true;
        }
    }
    if (sym == '/') {
        if (mv.dx == -1) {
            pos = Pos(pos.y + 1, pos.x);
            mv = Move(dy = 1, dx = 0);
            isFinded = true;
        }
        else if (mv.dx == 1) {
            pos = Pos(pos.y - 1, pos.x);
            mv = Move(dy = -1, dx = 0);
            isFinded = true;
        }
        else if (mv.dy == 1) {
            pos = Pos(pos.y, pos.x - 1);
            mv = Move(dy = 0, dx = -1);
            isFinded = true;
        }
        else if (mv.dy == -1) {
            pos = Pos(pos.y, pos.x + 1);
            mv = Move(dy = 0, dx = 1);
            isFinded = true;
        }
    }

    if (isFinded) {
        val newState = State(pos, mv, mat, pat, pass);
        result.add(newState);
    }

    if (sym == '-') {
        if (mv.dy == 1 || mv.dy == -1) {
            val pos1 = Pos(pos.y, pos.x - 1);
            val mv1  = Move(0, -1);
            val newState1 = State(pos1, mv1, mat, pat, pass);
            result.add(newState1);

            val pos2 = Pos(pos.y, pos.x + 1);
            val mv2  = Move(0, 1);
            val newState2 = State(pos2, mv2, mat, pat, pass);
            result.add(newState2);
        }
    }
    if (sym == '|') {
        if (mv.dx == 1 || mv.dx == -1) {
            var pos1 = Pos(pos.y - 1, pos.x);
            val mv1  = Move(-1, 0);
            val newState1 = State(pos1, mv1, mat, pat, pass);
            result.add(newState1);

            var pos2 = Pos(pos.y + 1, pos.x);
            val mv2  = Move(1, 0);
            val newState2 = State(pos2, mv2, mat, pat, pass);
            result.add(newState2);
        }
    }

    return result;
}

fun isStateFinal(s: State): Boolean {
    val (pos, _, matrix) = s;
    val cols = matrix[0].size;
    val rows = matrix.size;
    return pos.x < 0 || pos.x >= cols || pos.y < 0 || pos.y >= rows;
}

fun part1(lines: List<String>): Int {
    val queue: Queue<State> = LinkedList();
    var cols = lines[0].length;
    val matrix = Array<Array<Char>>(lines.size) { Array<Char>(cols) { '-' } };

    val initState = State(Pos(0, 0), Move(0, 1), matrix, lines, mutableListOf());
    queue.add(initState);
    while (queue.size != 0) {
        val curState = queue.remove();
        if (!isStateFinal(curState)) {
            val nextStates = getNextStates(curState);
            nextStates.forEach({ s: State -> 
                queue.add(s);
            });
            val mat = curState.matrix;
            val pos = curState.pos;
            mat[pos.y][pos.x] = '#';
            curState.matrix = mat;
        }
    }

    return matrix.fold(0, { acc: Int, row: Array<Char> -> 
        acc + row.count( { c: Char -> 
            c == '#' 
        })
    });
}

fun part2(_lines: List<String>): Long {
    return 0;
}

fun main(args: Array<String>) {
    if (args.size != 2) {
      println("usage: app <input_filename>")
      return ;
    }

    val fileName = args[1];
    val lines = File(fileName).readLines();

    var part1_sln = part1(lines)
    var part2_sln = part2(lines)

    println("part1_sln = $part1_sln")
    println("part2_sln = $part2_sln")
}
