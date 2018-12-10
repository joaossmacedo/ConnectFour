
import scala.scalajs.js.annotation.JSExportTopLevel
import org.scalajs.dom
import dom.document
import org.scalajs.dom.raw.Element

object GameProject {
  var game_on: Boolean = true

  val player1: String = "Joao"
  val player1Color: String = "red"

  val player2: String = "Chico"
  val player2Color: String = "mediumAquaMarine"

  val tr: dom.NodeList = document.querySelectorAll("tr")
  val td: dom.NodeList = document.querySelectorAll("tr td")
  val buttons: dom.NodeList = document.querySelectorAll("button")


  def main(args: Array[String]): Unit = {
    document.querySelector("#player").innerHTML = player1
    document.querySelector("#playerColor").innerHTML = player1Color
    document.querySelector("#playerColor").setAttribute("style", "color: " + player1Color)
  }

  @JSExportTopLevel("assignEventListener")
  def assignEventListener(nd: dom.Node): Unit = {
    val index = findPos(nd)
    if (game_on) {
      // Recognize what column was chosen
      val column = findColumn(index)
      // Gets the empty(gray) button nearest to the bottom
      val row = getBottom(column)

      if (row != -1) {
        // Drop the disc
        dropDisc(row, column)

        // Change the player
        document.querySelector("#player").innerHTML = currentPlayer()
        document.querySelector("#playerColor").innerHTML = currentColor()
        document.querySelector("#playerColor").setAttribute("style", "color: " + currentColor())

        // Check for a win or a tie
        if (checkHorizontalWin() || checkVerticalWin() || checkDiagonalWin()) {
          endGame()
        } else if (countDiscs() == buttons.length) {
          endGameTie()
        }
      }


    }
  }

  def findPos(nd: dom.Node): Int = {
    def auxFindPos(nd: dom.Node, i: Int): Int = {
      if (buttons(i) == nd) {
        i
      } else {
        auxFindPos(nd, i + 1)
      }
    }

    auxFindPos(nd, 0)
  }

  // ---------------------------------- DISC ----------------------------------

  // Recognize what column was chosen
  def findColumn(index: Int): Int = {
    index % 7
  }

  // Gets the empty(gray) button nearest to the bottom
  def getBottom(column: Int): Int = {
    def auxGetBottom(row: Int, column: Int): Int = {
      if (row < 0) return -1

      val colorReport = returnColor(row, column)

      if (colorReport == "background-color: " + player1Color || colorReport == "background-color: " + player2Color) {
        auxGetBottom(row - 1, column)
      } else {
        row
      }
    }

    auxGetBottom(tr.length - 1, column)
  }

  // Place the disc in the right position
  def dropDisc(row: Int, column: Int): Unit = {
    val pos = (row * 7) + column
    buttons(pos).asInstanceOf[Element].setAttribute("style", "background-color: " + currentColor())
  }

  // ------------------------------ COLOR FUNCTIONS ------------------------------

  // Check if the inputs are the same color and not gray
  def equalColors(c1: String, c2: String, c3: String, c4: String): Boolean = {
    c1 == c2 && c2 == c3 && c3 == c4 && c1 != null
  }

  // Return color of a button
  def returnColor(row: Int, column: Int): String = {
    val pos = (row * 7) + column
    if (0 <= pos && pos < buttons.length) {
      buttons(pos).asInstanceOf[Element].getAttribute("style")
    } else {
      null
    }
  }

  // ------------------------------ CURRENT STATUS ------------------------------

  // return the current player
  def currentPlayer(): String = {
    if ((countDiscs() % 2) == 0) {
      player1
    } else {
      player2
    }
  }

  // return the current color
  def currentColor(): String = {
    if ((countDiscs() % 2) == 0) {
      player1Color
    } else {
      player2Color
    }
  }

  // count how many discs are in the table
  def countDiscs(): Int = {
    def auxCountDiscs(td: dom.NodeList, i: Int): Int = {
      if (i < td.length) {
        val nd: Element = td(i).firstChild.asInstanceOf[Element]
        val color: String = nd.getAttribute("style").toString

        if (color == "background-color: " + player1Color || color == "background-color: " + player2Color)
          1 + auxCountDiscs(td, i + 1)
        else
          auxCountDiscs(td, i + 1)

      } else {
        0
      }
    }

    auxCountDiscs(td, 0)
  }

  // ------------------------- CHECK END GAME SITUATIONS -------------------------

  def checkHorizontalWin(): Boolean = {
    // change the columns
    def auxCheckHorizontalWin2(row: Int, column: Int): Boolean = {
      if (column < (td.length / tr.length) - 3) {
        if (equalColors(returnColor(row, column), returnColor(row, column + 1), returnColor(row, column + 2), returnColor(row, column + 3))) {
          true
        } else {
          auxCheckHorizontalWin2(row, column + 1)
        }
      } else {
        false
      }
    }

    // change the rows
    def auxCheckHorizontalWin(row: Int): Boolean = {
      if (row < tr.length) {
        if (auxCheckHorizontalWin2(row, 0)) {
          true
        } else {
          auxCheckHorizontalWin(row + 1)
        }
      } else {
        false
      }
    }

    auxCheckHorizontalWin(0)
  }

  def checkVerticalWin(): Boolean = {
    // change the rows
    def auxCheckVerticalWin2(row: Int, column: Int): Boolean = {
      if (row < tr.length - 3) {
        if (equalColors(returnColor(row, column), returnColor(row + 1, column), returnColor(row + 2, column), returnColor(row + 3, column))) {
          true
        } else {
          auxCheckVerticalWin2(row + 1, column)
        }
      } else {
        false
      }
    }

    // change the columns
    def auxCheckVerticalWin(column: Int): Boolean = {
      if (column < (td.length / tr.length)) {
        if (auxCheckVerticalWin2(0, column)) {
          true
        } else {
          auxCheckVerticalWin(column + 1)
        }
      } else {
        false
      }
    }

    auxCheckVerticalWin(0)
  }

  def checkDiagonalWin(): Boolean = {
    // check if there is a diagonal win
    def auxCheckDiagonalWin3(row: Int, column: Int): Boolean = {
      if (row < tr.length - 3) {
        if (equalColors(returnColor(row, column), returnColor(row + 1, column + 1), returnColor(row + 2, column + 2), returnColor(row + 3, column + 3))) {
          return true
        }
      }
      if (row > 2) {
        if (equalColors(returnColor(row, column), returnColor(row - 1, column + 1), returnColor(row - 2, column + 2), returnColor(row - 3, column + 3))) {
          return true
        }
      }
      false
    }

    // change the rows
    def auxCheckDiagonalWin2(row: Int, column: Int): Boolean = {
      if (row < tr.length) {
        if (auxCheckDiagonalWin3(row, column)) {
          true
        } else {
          auxCheckDiagonalWin2(row + 1, column)
        }
      } else {
        false
      }
    }

    // change the columns
    def auxCheckDiagonalWin(column: Int): Boolean = {
      if (column < (td.length / tr.length) - 3) {
        if (auxCheckDiagonalWin2(0, column)) {
          true
        } else {
          auxCheckDiagonalWin(column + 1)
        }
      } else {
        false
      }
    }

    auxCheckDiagonalWin(0)
  }

  // --------------------------------- END GAME ---------------------------------

  // End game
  def endGame() {
    document.querySelector("#playerTurn").innerHTML = defineWinner() + " won. Click reset to play again."
    document.querySelector("#playerTurn").setAttribute("style", "color: " + defineColor())
    game_on = false

    def defineWinner(): String = {
      if (currentPlayer() == player1) {
        player2
      } else {
        player1
      }
    }
    def defineColor(): String = {
      if (currentPlayer() == player1) {
        player2Color
      } else {
        player1Color
      }
    }
  }

  def endGameTie() {
    document.querySelector("#playerTurn").innerHTML = "TIE"
    game_on = false
  }

}