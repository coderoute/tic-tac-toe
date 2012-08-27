import org.scalatest.FunSuite

/**
 * @author vgowadia
 */
class BoardTest extends FunSuite {
  test("can check board cell is empty") {
    val board = Board()
    assert(board.isEmpty(0, 0))
    assert(board.isEmpty(2, 2))
  }

  test("cell is out of bounds") {
    val board = Board()
    intercept[ArrayIndexOutOfBoundsException] {
      assert(board.isEmpty(0, 3))
      fail("expected excepion")
    }
  }

  test("Board creation fails if cell contains invalid character (other than blank, O, or X)") {
    intercept[IllegalArgumentException] {
      Board(Array("XY ", "   ", "   "))
    }
  }

  test("Board creation fails if number of rows specified in not 3") {
    intercept[IllegalArgumentException] {
      Board(Array("X ", "   ", "   ", "   "))
    }
    intercept[IllegalArgumentException] {
      Board(Array("X  ", "   "))
    }
    intercept[IllegalArgumentException] {
      Board(Array("X  "))
    }
  }

  test("Board creation fails if row length of any row is not 3") {
    intercept[IllegalArgumentException] {
      Board(Array("X   ", "   ", "   "))
    }
    intercept[IllegalArgumentException] {
      Board(Array("X  ", "   ", "  "))
    }
  }

  test("first move can be made by X") {
    val board = Board()
    assert(board.whoHasNextMove === 'X')
  }

  test("second move can be made by O") {
    val board = Board(Array("X  ", "   ", "   "))
    assert(board.whoHasNextMove === 'O')
  }

  test("third move can be made by X") {
    val board = Board(Array("X  ", "O  ", "   "))
    assert(board.whoHasNextMove === 'X')
  }

  test("Last move can be made by X if the board has only one place left and no one has won") {
    val board = Board(Array("XOX", "XOX", "O O"))
    assert(board.whoHasNextMove==='X')
  }

  test("No one has next move if the board is full") {
    val board = Board(Array("XOX", "XOX", "OXO"))
    assert(board.whoHasNextMove===' ')
  }

  test("No one has next move if some one has won") {
    val board = Board(Array("XXX", "O O", "   "))
    assert(board.whoHasNextMove===' ')
  }

  test("check empty board is valid") {
    val board = Board()
    assert(board.isValid)
  }

  test("check board is valid after X has made 1 move and O has made none") {
    val board = Board(Array("X  ", "   ", "   "))
    assert(board.isValid)
  }

  test("check board is valid after X has made 1 move and O has made 1 move") {
    val board = Board(Array("X  ", " O ", "   "))
    assert(board.isValid)
  }

  test("check board is invalid if X has made 1 move and O has made 2 moves") {
    intercept[IllegalArgumentException]  {
      Board(Array("X  ", " OO", "   "))
    }
  }

  test("check board is invalid if O has made 1 move and X has made 0 moves") {
    intercept[IllegalArgumentException]  {
      Board(Array("O  ", "   ", "   "))
    }
  }

  test("First move sets the target cell to X") {
    val board = Board()
    board.makeMove(0, 0)
    assert(board.get(0, 0) === 'X')
  }

  test("Move over occupied cell causes error") {
    val board = Board()
    board.makeMove(0, 0)
    intercept[IllegalArgumentException] {
      board.makeMove(0, 0)
    }
  }

  test("Second move sets the target cell to O") {
    val board = Board()
    board.makeMove(0, 0)
    board.makeMove(1, 1)
    assert(board.get(1, 1) === 'O')
  }

  test("If all chars in 1st row are X, X has won") {
    val board = Board(Array("XXX", "O O", " O "))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 2nd row are X, X has won") {
    val board = Board(Array("O O", "XXX", " O "))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 3rd row are X, X has won") {
    val board = Board(Array("O O", " O ", "XXX"))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 1st row are O, O has won") {
    val board = Board(Array("OOO", "X X", " X "))
    assert(board.whoWon.get === 'O')
  }

  test("On empty board no one has won") {
    val board = Board()
    assert(board.whoWon === None)
  }

  test("No one has Won if the board is empty") {
    val board = Board()
    assert(!board.hasSomeOneWon)
  }
  test("If all chars in 1st row are equal, someone has won") {
    val board = Board(Array("XXX", " O ", "O O"))
    assert(board.hasSomeOneWon)
  }

  test("If all chars in 2nd row are equal, someone has won") {
    val board = Board(Array("  O", "XXX", "OXO"))
    assert(board.hasSomeOneWon)
  }

  test("If all chars in 3nd row are equal, someone has won") {
    val board = Board(Array("X O", "OO ", "XXX"))
    assert(board.hasSomeOneWon)
  }

  test("If all chars in 1st column are equal, someone has won") {
    val board = Board(Array("X O", "XO ", "XOX"))
    assert(board.hasSomeOneWon)
  }

  test("If all chars in \\ diagonal are equal, someone has won") {
    val board = Board(Array("X O", "OX ", "XOX"))
    assert(board.hasSomeOneWon)
  }

  test("If all chars in / diagonal are equal, someone has won") {
    val board = Board(Array("X O", "XO ", "OX "))
    assert(board.hasSomeOneWon)
  }

  test("If board is full and no row or no column or no diagnol matches than no one has won") {
    val board = Board(Array("XOX", "XOX", "OXO"))
    assert(!board.hasSomeOneWon)
  }

  test("If all chars in 1st column are X, X has won") {
    val board = Board(Array("X O", "XO ", "XOX"))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 2nd column are X, X has won") {
    val board = Board(Array(" XO", "OX ", "OXX"))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 3rd column are X, X has won") {
    val board = Board(Array("O X", " OX", "OXX"))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in 1st column are O, O has won") {
    val board = Board(Array("O X", "OX ", "OXX"))
    assert(board.whoWon.get === 'O')
  }

  test("If all chars in 2nd column are O, O has won") {
    val board = Board(Array(" OX", "XO ", "XOX"))
    assert(board.whoWon.get === 'O')
  }

  test("If all chars in 3rd column are O, O has won") {
    val board = Board(Array("X O", " XO", "XXO"))
    assert(board.whoWon.get === 'O')
  }

  test("There is no winning diagonal in an empty board") {
    val board = Board()
    assert(board.isSomeWinningDiagonal === false)
  }

  test("If all chars in \\ diagonal are X, there is a winning diagonal") {
    val board = Board(Array("X O", " XO", "XOX"))
    assert(board.isSomeWinningDiagonal)
  }

  test("If all chars in \\ diagonal are O, there is a winning diagonal") {
    val board = Board(Array("O X", " OX", "XXO"))
    assert(board.isSomeWinningDiagonal)
  }

  test("If all chars in / diagonal are X, there is a winning diagonal") {
    val board = Board(Array("O X", " XO", "XOX"))
    assert(board.isSomeWinningDiagonal)
  }

  test("If all chars in / diagonal are O, there is a winning diagonal") {
    val board = Board(Array("X O", " OX", "OX "))
    assert(board.isSomeWinningDiagonal)
  }

  test("If all chars in \\ diagonal are X, X has won") {
    val board = Board(Array("X O", " XO", "XOX"))
    assert(board.whoWon.get === 'X')
  }

  test("If all chars in / diagonal are O, O has won") {
    val board = Board(Array("X O", "XO ", "OX "))
    assert(board.whoWon.get === 'O')
  }
}
