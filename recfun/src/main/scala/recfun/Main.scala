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
      if (c == 0 || c == r) {
        return 1
      }
      return pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  /**
   * Exercise 2
   */
    def updateBalance(balance: Int, char: Char): Int = {
      if (char == '(')
        return balance + 1
      else if (char == ')')
        return balance - 1
      else
        return balance
    }

    def balanceInner(chars: List[Char], openedBrackets: Int): Int = {
      if (openedBrackets < 0 || chars.isEmpty) {
        return openedBrackets
      }
      
      var updatedCount = updateBalance(openedBrackets, chars.head)

      return balanceInner(chars.tail, updatedCount)
    }

    def balance(chars: List[Char]): Boolean = {
      val unbalancedBrackets = balanceInner(chars, 0)

      if (unbalancedBrackets == 0) (true) else (false)
    }
  
  /**
   * Exercise 3
   */
    def iterateCoins(money: Int, coins: List[Int]): Int = {
      var sum = 0
      var passedCoins = coins

      for (coin <- coins) {
        sum += countChange(money - coin, passedCoins)
        passedCoins = passedCoins.tail
      }
      sum
    }

    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || coins.length == 0)
        0
      else
        iterateCoins(money, coins)
    }
  }
