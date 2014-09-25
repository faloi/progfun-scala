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
  def pascal(c: Int, r: Int): Int = {
    (c, r) match {
      case (x, y) if x == y => 1
      case (0, _) => 1
      case (x, y) => pascal(x - 1, y - 1) + pascal(x, y -1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(count: Int, chars: List[Char]): Boolean = {
      if (count < 0)
        false
      else
        chars match {
          case Nil => true
          case c :: cs => c match {
            case '(' => loop(count + 1, cs)
            case ')' => loop(count - 1, cs)
            case _ => loop(count, cs)
          }
        }
    }

    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = ???
}
