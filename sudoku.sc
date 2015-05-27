object Sudoku {

  type XY = (Int, Int)
  type SudokuBoard = Array[Int]

  def toString(values:Iterable[Int]) =
    "\n+---+---+---+\n" +
      values.mkString("")
        .replaceAll( """\d\d\d\d\d\d\d\d\d""", "|$0\n")
        .replaceAll( """\|\d*\n\|\d*\n\|\d*\n""", "$0+---+---+---+\n")
        .replaceAll( """\d\d\d""", "$0|")
        .replaceAll(" \n", "|\n")
        .replaceAll("0", " ")


  def rowPop(brd:SudokuBoard) = Array.range(0, 9).map(x => rowValues(brd, x).filter(y => y != 0).length)
  def colPop(brd:SudokuBoard) = Array.range(0, 9).map(x => colValues(brd, x).filter(y => y != 0).length)

  def coord(i: Int) = (i % 9, i / 9)

  def index(c: XY) = c._2 * 9 + c._1

  def cell(brd:SudokuBoard, c: XY) = brd(index(c))
  def cell(brd:SudokuBoard, x:Int, y:Int):Int = cell(brd, (x, y))

  def subGrid(c: XY) = (c._1 / 3, c._2 / 3)

  def rowValues(brd:SudokuBoard, r: Int) = brd.slice(r * 9, (r + 1) * 9)

  def colValues(brd:SudokuBoard, c: Int) = brd.zipWithIndex.collect {
    case (e, i) if ((i - c) % 9 == 0) => e
  }

  def subGridCoord(c: XY) = (c._1 * 3, c._2 * 3)

  def subGridValues(brd:SudokuBoard, c: XY) = {
    val sg = subGridCoord(c)
    Array(
      cell(brd, sg._1 + 0, sg._2 + 0), cell(brd, sg._1 + 0, sg._2 + 1), cell(brd, sg._1 + 0, sg._2 + 2),
      cell(brd, sg._1 + 1, sg._2 + 0), cell(brd, sg._1 + 1, sg._2 + 1), cell(brd, sg._1 + 1, sg._2 + 2),
      cell(brd, sg._1 + 2, sg._2 + 0), cell(brd, sg._1 + 2, sg._2 + 1), cell(brd, sg._1 + 2, sg._2 + 2)
    )
  }

  def allValuesOf(brd:SudokuBoard, c: XY) = {
    rowValues(brd, c._2).union(
      colValues(brd, c._1).union(
        subGridValues(brd, subGrid(c))))
      .filter(x => x != 0).distinct
  }

  def withValue(brd:SudokuBoard, c: XY, v: Int) = {
    val newBoard = brd.clone()
    newBoard.update(index(c), v)
    newBoard
  }

  def withValue(brd:SudokuBoard, x:Int, y:Int, v:Int):SudokuBoard = withValue(brd, (x, y), v)

  def notIn(lookFor: Iterable[Int], exclude: Iterable[Int]) = {
    val exa = exclude.toArray
    lookFor.filter(x => !exa.contains(x))
  }

  def abundance[T](list: Iterable[T]): Array[(Int, T)] =
    list.groupBy(x => x).map(g => (g._2.size, g._1)).toArray
      .sortBy(x => x._1)
      .reverse

  def findEasiestCells(brd: SudokuBoard) = {
    val pairs = for (
      r <- rowPop(brd).zipWithIndex;
      c <- colPop(brd).zipWithIndex
      if cell(brd, c._2, r._2) == 0
    ) yield ((c._2, r._2), c._1 + r._1)
    pairs
      .sortBy(x => x._2).reverseMap(x => x._1)
  }

  def findBestGuesses(brd: SudokuBoard, c: XY) = {
    val excludables = allValuesOf(brd, c)
    val candidates = notIn(Array.range(1, 10), excludables).toArray
    val subGridXY = subGridCoord(subGrid(c));
    val colNeighbors =
      Array.range(subGridXY._1, subGridXY._1 + 3).filter(x => x != c._1)
        .map(x => colValues(brd, x)).flatten
    val rowNeighbors =
      Array.range(subGridXY._2, subGridXY._2 + 3).filter(y => y != c._2)
        .map(y => rowValues(brd, y)).flatten
    val neighbors = colNeighbors.union(rowNeighbors)
    val options = abundance(neighbors.filter(n => candidates.contains(n)))
      .map(x => x._2).union(candidates).distinct
    options
  }

  def solutions(brd: SudokuBoard): Iterable[SudokuBoard] = {
    if (!brd.contains(0)) {
      Array(brd).toIterable
    } else {
      val easies = findEasiestCells(brd).toStream;
      easies.toIterable
        .map(cell => (cell, findBestGuesses(brd, cell)))
        // Short circuit if any cell has no guesses.
        .takeWhile { case (cell, guesses) => {
        guesses.length != 0
      }
      }
        .map { case (cell, guesses) => {
        guesses.toIterable
          .map(guess => withValue(brd, cell, guess))
          .map(newBoard => solutions(newBoard))

      }
      }.flatten.flatten
    }
  }

  def solution(brd: SudokuBoard): SudokuBoard = {
    var boards: Iterable[SudokuBoard] = Array(brd).toIterable
    var ret: SudokuBoard = null;
    while (ret == null) {
      //val solved = boards.filter(x => !x.values.contains(0))
      //if (solved.length > 0) ret = solved(0)
      boards = boards.map(brd => {
        println(brd)

        if (!brd.contains(0)) {
          println("SUCCESS")
          ret = brd
          null
        }
        else findEasiestCells(brd).toIterable.take(3)
          .map(cell => {
          findBestGuesses(brd, cell)
            .map(guess => withValue(brd, cell, guess))
        })
      }).flatten.flatten
    }
    ret
  }

  def unsolve(brd: SudokuBoard, n:Int):SudokuBoard = {
    lazy val filledIndexes = brd.zipWithIndex.filter(_._1 != 0)
    if (n == 0 || filledIndexes.length == 0) brd
    else unsolve(withValue(brd, coord((math.random * filledIndexes.length).toInt), 0), n - 1)
  }
}
val jhvalues = Array(
  0, 2, 0, 1, 7, 8, 0, 3, 0,
  0, 4, 0, 3, 0, 2, 0, 9, 0,
  1, 0, 0, 0, 0, 0, 0, 0, 6,
  0, 0, 8, 6, 0, 3, 5, 0, 0,
  3, 0, 0, 0, 0, 0, 0, 0, 4,
  0, 0, 6, 7, 0, 9, 2, 0, 0,
  9, 0, 0, 0, 0, 0, 0, 0, 2,
  0, 8, 0, 9, 0, 1, 0, 6, 0,
  0, 1, 0, 4, 3, 6, 0, 5, 0)

val avalues = Array(
  1,2,3,4,5,6,7,8,9,
  4,5,6,7,8,9,1,2,3,
  7,8,9,1,2,3,4,5,6,
  2,3,4,5,6,7,8,9,1,
  5,6,7,8,9,1,2,3,4,
  8,9,1,2,3,4,5,6,7,
  3,4,5,6,7,8,9,1,2,
  6,7,8,9,1,2,3,4,5,
  9,1,2,3,4,5,6,7,8
)
val mvalues = Array(
  0,2,3,4,5,6,7,8,0,
  4,0,6,7,8,9,1,0,3,
  7,8,0,1,2,3,0,5,6,
  2,3,4,0,0,0,8,9,1,
  5,6,7,0,0,0,2,3,4,
  8,9,1,0,0,0,5,6,7,
  3,4,0,6,7,8,0,1,2,
  6,0,8,9,1,2,3,0,5,
  0,1,2,3,4,5,6,7,0
)

val dvalues = Array(
  0,0,8,0,3,0,0,0,0,
  2,6,0,0,0,5,3,4,0,
  0,0,0,6,0,1,7,0,8,
  3,0,0,0,4,7,2,0,0,
  8,0,0,1,0,3,0,0,7,
  0,0,9,8,5,0,0,0,1,
  6,0,5,3,0,8,0,0,0,
  0,8,7,2,0,0,0,6,3,
  0,0,0,0,9,0,8,0,0
)
val ivalues = Array(
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0,
  0,0,0,0,0,0,0,0,0
)


val values = Array(
  1,2,3,4,5,6,7,8,9,
  4,5,6,7,8,9,1,2,3,
  7,8,9,1,2,3,4,5,6,
  2,3,4,5,6,7,8,9,1,
  5,6,7,8,9,1,2,3,4,
  8,9,1,2,3,4,5,6,7,
  3,4,5,6,7,8,9,1,2,
  6,7,8,9,1,2,3,4,5,
  9,1,2,3,4,5,6,7,8
)




val board = Sudoku.unsolve(values, 20)
println(Sudoku toString board)
Sudoku.solutions(board).take(1).foreach(x=>println(Sudoku.toString(x)))
