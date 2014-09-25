package recfun

object Main {
  def main(args: Array[String]) {
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
  def pascal(c: Int, r: Int): Int = ???

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
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
