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
    def pascal(c: Int, r: Int): Int =
      if (r<0 || c>r) 0
      else if (c==0 || c==r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def f(chars: List[Char], open: Int): Boolean = {
      if (chars.isEmpty) {
        open == 0
      } else {
        val open_update =
          if (chars.head == '(') open + 1
          else if (chars.head == ')') open - 1
          else open
        if (open_update >= 0) f(chars.tail, open_update)
        else false
      }
    }
    f(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0 || coins.isEmpty) 0
    else if (money == coins.head) 1 + countChange(money, coins.tail)
    else if (money < coins.head) countChange(money, coins.tail)
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }

}
