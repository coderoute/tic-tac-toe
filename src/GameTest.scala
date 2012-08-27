import dbc.value.Conversion.Illegal
import org.scalatest.FunSuite

/**
 * @author vgowadia
 */
class GameTest extends FunSuite {

  test("readMove should throw Exception when input line is not comma separated") {
    intercept[IllegalArgumentException] {
      Game().readMove("")
    }

    intercept[IllegalArgumentException] {
      Game().readMove("1")
    }
  }

  test("readMove should throw Exception when input line contains more than 1 comma separating integers") {
    intercept[IllegalArgumentException] {
      Game().readMove("0,1,2")
    }
  }

  test("readMove should throw Exception when input line contains non-numeric values") {
    intercept[IllegalArgumentException] {
      Game().readMove("1,b")
    }

    intercept[IllegalArgumentException] {
      Game().readMove("a,1")
    }
  }


  test("readMove should throw Exception when input line contains values less than 0") {
    intercept[IllegalArgumentException] {
      Game().readMove("1,-1")
    }

    intercept[IllegalArgumentException] {
      Game().readMove("-1,1")
    }
  }

  test("readMove should throw Exception when input line contains values greater than 2") {
    intercept[IllegalArgumentException] {
      Game().readMove("1,3")
    }

    intercept[IllegalArgumentException] {
      Game().readMove("3,1")
    }
  }

  test("readMove parses row,column value correctly") {

    val game = Game()
    val pair1 = game.readMove("0,0")
    assert(pair1._1 == 0)
    assert(pair1._2 == 0)

    val pair2 = game.readMove("2,2")
    assert(pair2._1 == 2)
    assert(pair2._2 == 2)
  }

  test("readMove ignores white space when parsing row,column value") {

    val game = Game()
    val pair1 = game.readMove(" \t 1 , \n 0 ")
    assert(pair1._1 == 1)
    assert(pair1._2 == 0)
  }
}
