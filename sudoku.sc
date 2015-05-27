type XY = (Int, Int)

trait Set
case class Row(r:Int) extends Set
case class Col(c:Int) extends Set
case class SubGrid(c:XY) extends Set

class SudokuBoard(val values:Array[Int]) {
  lazy val rowPop = Array.range(0,9).map(x=>rowValues(x).filter(y=>y!=0).length)
  lazy val colPop = Array.range(0,9).map(x=>colValues(x).filter(y=>y!=0).length)
  def coord(i:Int) = (i%9, i/9)
  def index(c:XY) = c._2 * 9 + c._1
  def cell(c:XY) = values(index(c))
  def subGrid(c:XY) = (c._1/3, c._2/3)
  def rowValues(r:Int) = values.slice(r*9, (r+1)*9)
  def colValues(c:Int) = values.zipWithIndex.collect{
    case(e, i) if ((i - c) % 9 == 0) => e
  }
  def subGridCoord(c:XY) = (c._1*3, c._2*3)
  def subGridValues(c:XY) = {
    val sg = subGridCoord(c)
    Array(
      cell(sg._1+0, sg._2+0),cell(sg._1+0, sg._2+1),cell(sg._1+0, sg._2+2),
      cell(sg._1+1, sg._2+0),cell(sg._1+1, sg._2+1),cell(sg._1+1, sg._2+2),
      cell(sg._1+2, sg._2+0),cell(sg._1+2, sg._2+1),cell(sg._1+2, sg._2+2)
    )
  }
  //  def values(set:Set) = set match {
  //    case Row(r) => rowValues(r)
  //    case Col(c) => colValues(c)
  //    case SubGrid(c) => subGridValues(c)
  //    case _ => ???
  //  }
  def allValuesOf(c:XY) = {
    rowValues(c._2).union(
      colValues(c._1).union(
        subGridValues(subGrid(c))))
      .filter(x=>x != 0).distinct
  }
  def withValue(c:XY, v:Int) = {
    val newValues = values.clone()
    newValues(index(c)) = v
    new SudokuBoard(newValues)
  }
  override def toString =
    "\n+---+---+---+\n" +
      values.mkString("")
        .replaceAll("""\d\d\d\d\d\d\d\d\d""", "|$0\n")
        .replaceAll("""\|\d*\n\|\d*\n\|\d*\n""", "$0+---+---+---+\n")
        .replaceAll("""\d\d\d""", "$0|")
        .replaceAll(" \n", "|\n")
        .replaceAll("0", " ")
}
def notIn(lookFor:Iterable[Int], exclude:Iterable[Int]) = {
  val exa = exclude.toArray
  lookFor.filter(x => !exa.contains(x))
}
def abundance[T](list:Iterable[T]):Array[(Int, T)] =
  list.groupBy(x=>x).map(g=>(g._2.size, g._1)).toArray
    .sortBy(x=>x._1)
    .reverse
def findEasiestCells(brd: SudokuBoard) = {
  val pairs = for (
    r <- brd.rowPop.zipWithIndex;
    c <- brd.colPop.zipWithIndex
    if brd.cell(c._2, r._2) == 0
  ) yield((c._2, r._2), c._1 + r._1)
  pairs
    .sortBy(x => x._2).reverseMap(x => x._1)
}
def findBestGuesses(brd:SudokuBoard, c:XY) = {
  val excludables = brd.allValuesOf(c)
  val candidates = notIn(Array.range(1, 10), excludables).toArray
  val subGridXY = brd.subGridCoord(brd.subGrid(c));
  val colNeighbors =
    Array.range(subGridXY._1, subGridXY._1 + 3).filter(x => x != c._1)
      .map(x => brd.colValues(x)).flatten
  val rowNeighbors =
    Array.range(subGridXY._2, subGridXY._2 + 3).filter(y => y != c._2)
      .map(y => brd.rowValues(y)).flatten
  val neighbors = colNeighbors.union(rowNeighbors)
  val options = abundance(neighbors.filter(n => candidates.contains(n)))
    .map(x => x._2).union(candidates).distinct
  options
}

def solutions(brd:SudokuBoard):Iterable[SudokuBoard] = {
  if (!brd.values.contains(0)) {
    Array(brd).toIterable
  } else {
    val easies = findEasiestCells(brd).toStream;
    easies.toIterable
      .map(cell => (cell, findBestGuesses(brd, cell)))
      // Short circuit if any cell has no guesses.
      .takeWhile{case (cell, guesses) => {
      guesses.length != 0
    }}
      .map{case (cell, guesses) => {
      guesses.toIterable
        .map(guess => brd.withValue(cell, guess))
        .map(newBoard => solutions(newBoard))

    }}.flatten.flatten
  }
}
def solution(brd:SudokuBoard):SudokuBoard = {
  var boards:Iterable[SudokuBoard] = Array(brd).toIterable
  var ret:SudokuBoard = null;
  while (ret == null) {
    //val solved = boards.filter(x => !x.values.contains(0))
    //if (solved.length > 0) ret = solved(0)
    boards = boards.map(brd => {
      println(brd)

      if (!brd.values.contains(0)) {
        println("SUCCESS")
        ret = brd
        null
      }
      else findEasiestCells(brd).toIterable.take(3)
        .map(cell => {
        findBestGuesses(brd, cell)
          .map(guess => brd.withValue(cell, guess))
      })
    }).flatten.flatten
  }
  ret
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
  4,0,2,8,1,9,0,0,0,
  7,0,5,2,4,3,0,0,0,
  1,0,8,5,7,6,0,0,0,
  3,2,0,7,0,8,4,6,5,
  6,0,4,1,3,0,7,9,8,
  0,8,7,0,6,5,1,3,2,
  0,4,3,0,2,1,0,0,0,
  8,0,6,3,5,0,9,2,1,
  2,1,0,6,0,7,3,5,4
)


val board = new SudokuBoard(values)
println(board)
solutions(board).take(1).foreach(x=>println(x))
//println(findEasiestCells(board).mkString)
//println(board.rowValues(8).mkString)
//println(board.colValues(8).mkString)
//println(board.allValuesOf(7, 8).mkString)

//board.toString
//board.cell(0, 0)
//board.cell(0, 1)
//board.cell(1, 1)
//board.colValues(0)
//board.colValues(1)
//board.colValues(8)
//board.subGridValues(2, 2)
//findBestGuesses(board, (0, 0))
//notIn(Array(1,2,3,4,5), Array(3,5,6,7,8))

//println(board.cell(8,7))