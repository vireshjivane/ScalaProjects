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
  def pascal(c: Int, r: Int): Int = {
    c == 0 || c == r match {
      case true => 1
      case false => pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  }


  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def innerFunction(chars: List[Char], totalOpenPars: Int): Boolean = {
      if (!chars.nonEmpty) {
        totalOpenPars == 0
      } else {
        val headValue = chars.head
        val n =
          if (headValue == '(') totalOpenPars + 1
          else if (headValue == ')') totalOpenPars - 1
          else totalOpenPars
        if (n >= 0) innerFunction(chars.tail, n)
        else false
      }
    }
    innerFunction(chars, 0)
  }


  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def coinCounter(money: Int, coins: List[Int]): Int = {
      if (!coins.nonEmpty) 0
      else if (money - coins.head == 0) 1
      else if (money - coins.head < 0) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
    coinCounter(money, coins.sorted)
  }
}