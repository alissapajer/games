package tictactoe

//TODO
//need to keep track of state: whose turn it is, and the state of the board
//maybe a monoid to combine all the board state changes as new pieces are added

//set perfect and random players in motion, state accumulates
trait BoardModule {
  trait Value
  case object Ex extends Value
  case object Oh extends Value
  case object Blank extends Value

  case class Coord(i: Int, j: Int) {
    val range = 0 until 3
    assert((range contains i) && (range contains j))
  }

  type Board = Map[Coord, Value]

  val boardSpaces: Seq[Coord] = for {
    i <- 0 until 3
    j <- 0 until 3
  } yield {
    Coord(i, j)
  }

  val initialBoard: Board =
    boardSpaces.map((_, Blank)).toMap

  class Player(board: Board) {
    def playTurn(coord: Coord, value: Value): Board = {
      if (!boardSpaces.contains(coord)) {
        sys.error("illegal coordinate")
      } else {
        val currentValue = board(coord)
        currentValue match {
          case Blank => board.updated(coord, value)
          case Ex | Oh => sys.error("illegal move")
        }
      }
    }
  }
}

class Game extends BoardModule {
  val player = new Player(initialBoard)
  player.playTurn(Coord(1, 1), Ex)
}
