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
      if (c == 0 || c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
    def updateBalance(balance: Int, char: Char): Int = {
      if (char == '(')
        balance + 1
      else if (char == ')')
        balance - 1
      else
        balance
    }

    def balanceInner(chars: List[Char], openedBrackets: Int): Int = {
      if (openedBrackets < 0 || chars.isEmpty) {
        openedBrackets
      } else {
        val updatedCount = updateBalance(openedBrackets, chars.head)
        balanceInner(chars.tail, updatedCount)
      }
    }

    def balance(chars: List[Char]): Boolean = {
      val unbalancedBrackets = balanceInner(chars, 0)

      if (unbalancedBrackets == 0) (true) else (false)
    }
  
  /**
   * Exercise 3
   */
    def countChangeInner(money: Int, passedCoins: List[Int]) = {
      if (money == 0)
        1
      else if (money < 0)
        0
      else
        iterateCoins(money - passedCoins.head, passedCoins)
    }

    def iterateCoins(money: Int, passedCoins: List[Int]): Int = {
      if (passedCoins.length == 0)
        0
      else {
        val innerSum = countChangeInner(money, passedCoins)
        if (money == 0)
          innerSum
        else
          innerSum + iterateCoins(money, passedCoins.tail)
      }
    }

    def countChange(money: Int, coins: List[Int]): Int = {
      iterateCoins(money, coins.sorted)
    }
  }
