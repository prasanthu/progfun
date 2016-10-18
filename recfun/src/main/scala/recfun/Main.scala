package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))
    println(balance("())(".toList))
    println(countChange(4, List(1, 2)))
    println(countChange(3, List(1, 2)))
    println(countChange(300,List(500,5,50,100,20,200,10)))

  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r)
        1
      else
        pascal(c-1 ,r-1) + pascal(c ,r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def matchParentheses(remaining: Int, chars: List[Char]): Boolean = {
        if (chars.isEmpty) remaining == 0
        else if (remaining < 0) false
        else {
          val c = chars.head
          val tail = chars.tail
          if (c == '(')
            matchParentheses(remaining+1, tail)
          else if (c == ')')
            matchParentheses(remaining-1, tail)
          else matchParentheses(remaining, tail)
        }
      }
      matchParentheses(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countWays(money: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
        else {
          val coin = coins.head
          val tail = coins.tail
          if (money - coin == 0) 1
          else if (money - coin < 0) 0
          else countWays(money - coin, coins) + countWays(money, tail)
        }
      }
      countWays(money, coins.sorted)
    }
  }
