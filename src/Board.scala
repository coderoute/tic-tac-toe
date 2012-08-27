/**
 * @author vgowadia
 */
class Board(val grid: Array[Array[Char]]) {

  def get(row: Int, col: Int): Char = grid(row)(col)

  def isEmpty(row: Int, col: Int): Boolean = get(row, col) == ' '

  override def toString: String = {
    grid.foldLeft("")( (iteration_result,row) => iteration_result + rowToString(row) + "\n")
  }

  private def rowToString(row: Array[Char]) : String = {
    "| " + row.foldLeft("")((iteration_result, cell) => iteration_result + cell + " | " )
  }

  def isValid: Boolean = {
    val countX = grid.flatten.count(_ == 'X')
    val countO = grid.flatten.count(_ == 'O')
    (countX == countO) || (countX == countO + 1)
  }

  def whoHasNextMove: Char = {
    val turn = grid.flatten.count(_ != ' ')
    if (turn == 9 || hasSomeOneWon) {
      ' '
    } else {
      if (turn % 2 == 0) {
        'X'
      } else {
        'O'
      }
    }
  }

  def makeMove(row: Int, col: Int) = {
    if (!isEmpty(row, col)) throw new IllegalArgumentException("Cell Occupied")
    whoHasNextMove match {
      case 'X' => grid(row)(col) = 'X'
      case 'O' => grid(row)(col) = 'O'
      case _ =>
    }
  }

  def whoWon: Option[Char] = {
    if (isSomeWinningRow(grid))
      return findRowWinner(grid)
    if (isSomeWinningColumn(grid))
      return findColumnWinner(grid)
    None
  }

  def hasSomeOneWon: Boolean = {
    isSomeWinningRow(grid) || isSomeWinningColumn(grid)
  }

  def isSomeWinningColumn(_grid: Array[Array[Char]]): Boolean = {
    isSomeWinningRow(_grid.transpose)
  }

  def isSomeWinningRow(_grid: Array[Array[Char]]): Boolean = {
    _grid.foldLeft(false)((iteration_result, current) => iteration_result || (current(0) == current(1) && current(0) == current(2) && current(0) != ' '))
  }

  private def findRowWinner(_grid: Array[Array[Char]]): Option[Char] = {
    _grid.foreach(
      row => {
        if (isWinningRow(row, 'X'))
          return Some('X')
        if (isWinningRow(row, 'O'))
          return Some('O')
      }
    )
    None
  }

  private def findColumnWinner(_grid: Array[Array[Char]]): Option[Char] = {
    findRowWinner(_grid.transpose)
  }

  private def isWinningRow(row: Array[Char], c: Char): Boolean = {
    row.foldLeft(true)((iteration_result, current) => if (current == c) iteration_result else false)
  }
}

object Board {

  def apply(): Board = {
    val grid = Array.fill(3, 3)(' ');
    val board = new Board(grid);
    return board
  }

  def apply(rows: Array[String]): Board = {
    if (rows.length != 3 || rows.foldLeft(false)((iteration_result, row) => if (iteration_result || row.length != 3) true else false))
      throw new IllegalArgumentException("Incorrect board size")
    val grid = for (row <- rows) yield row.toCharArray
    if (grid.flatten.count(!"XO ".contains(_)) != 0)
      throw new IllegalArgumentException("Unknown cell marker used")
    val board = new Board(grid)
    if (!board.isValid)
      throw new IllegalArgumentException("Invalid board configuration")
    return board
  }
}