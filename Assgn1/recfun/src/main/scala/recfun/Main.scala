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
  def pascal(c: Int, r: Int): Int = {
    if(c==r || c==0)
      1
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceloop(curr: Int, charlist: List[Char]): Int = {
      if(charlist.isEmpty) {
        curr
      } else if(charlist.head == '(') {
        balanceloop(curr + 1, charlist.tail)
      } else if(charlist.head == ')' && curr < 1) {
        -1
      } else if(charlist.head == ')') {
        balanceloop(curr - 1, charlist.tail)
      } else {
        balanceloop(curr, charlist.tail)
      }
    }

    balanceloop(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def changeloop(m: Int, c: List[Int]): Int = {
      if(m==0)
        1
      else if(m < 0 || c.length <= 0)
        0
      else
        changeloop(m - c.head, c) + changeloop(m, c.tail)
    }

    changeloop(money, coins.sorted.reverse)
  }
}
