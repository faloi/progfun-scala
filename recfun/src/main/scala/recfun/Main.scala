package recfun

import scala.annotation.tailrec

object Main extends App {
  override def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal: PartialFunction[(Int, Int), Int] = {
      case (col, row) if col == row => 1
      case (0, _) => 1
      case (col, row) => pascal(col - 1, row - 1) + pascal(col, row -1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def isBalanced(openedParenthesis: Int, chars: List[Char]): Boolean =
      openedParenthesis match {
        case x if x < 0 => false
        case x =>
          chars match {
            case Nil => true
            case c :: cs => c match {
              case '(' => isBalanced(x + 1, cs)
              case ')' => isBalanced(x - 1, cs)
              case _ => isBalanced(x, cs)
            }
          }
      }

    isBalanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange: PartialFunction[(Int, List[Int]), Int] = {
    case (0, _) => 1
    case (money, Nil) => 0
    case (money, coins @ c :: cs) =>
      val changeWithRemainingCoins = countChange(money, cs)

      if (c <= money) changeWithRemainingCoins + countChange(money - c, coins)
      else changeWithRemainingCoins
  }
}
