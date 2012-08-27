
class Game {
  def readMove: Pair[Int, Int] = {
    val line = readLine()
    readMove(line)
  }

  def readMove(line : String) : Pair[Int, Int] = {
    val parts = line.split(',');
    if (parts.length != 2) {
      throw new IllegalArgumentException("Invalid input, row and column should be separated by a comma.")
    }
    try {
      val row = parts(0).trim.toInt
      val col = parts(1).trim.toInt
      if (row < 0 || col < 0 || row > 2 || col > 2)
        throw new IllegalArgumentException("You can enter only values 0 / 1 / 2 for rows and columns");
      return Pair(row, col)
    } catch {
      case nfe : NumberFormatException => throw new IllegalArgumentException("You can enter only numeric values")
    }
  }

  def play = {
    val board = Board()
    while (!board.hasSomeOneWon && board.whoHasNextMove != ' ') {
      print("Enter row,column position to make move for player " + board.whoHasNextMove + ": ")
      try {
        val move = readMove
        board.makeMove(move._1, move._2)
      } catch {
        case iae : IllegalArgumentException => println(iae.getMessage)
      }
      println(board)
    }

    board.whoWon match {
      case Some('X') => println("X won")
      case Some('O') => println("O won")
      case _ => println("No one won, game over!")
    }

    println(board)
  }
}

object Game extends App {

  def apply() : Game = new Game

  Game().play
}
