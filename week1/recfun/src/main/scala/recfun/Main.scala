package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if ((c == 0) || (c == r)) 1
    else pascal(c, r-1) + pascal(c-1, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], a: Int): Int =
      if (chars.isEmpty)
        a
      else if (chars.head == ')' && a == 0)
        -1
      else if (chars.head == '(')
        loop(chars.tail, a+1)
      else if (chars.head == ')')
        loop(chars.tail, a-1)
      else loop(chars.tail, a)

    loop(chars, 0) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    if (money == 0)
      1
    else if (money < 0)
      0
    else if (coins.isEmpty)
      0
    else {
      val frequencies = (0 to (money/coins.head))
      val recursiveResults = frequencies.map({
        n: Int => {
          countChange(money - ( coins.head * n ), coins.tail )
        }
      })

      recursiveResults.sum
    }
  }
}
