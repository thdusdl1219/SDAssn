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
  {
    if (c == 0) 1
    else if (r == 0) 1
    else if (c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = 
  {
    def loop(parnum : Int, chars:List[Char]) : Boolean = 
    {
      if(parnum < 0) false
      else if(chars.isEmpty && parnum == 0) true
      else if(chars.isEmpty && parnum != 0) false
      else if(chars.head == '(') loop(parnum + 1, chars.tail)
      else if(chars.head == ')') loop(parnum - 1, chars.tail)
      else loop(parnum, chars.tail)
    }
    loop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 
  {
    if (money == 0) return 1
    if (money < 0 || coins.isEmpty) return 0

    return countChange(money, coins.tail) + countChange(money - coins.head, coins)
  }
}
