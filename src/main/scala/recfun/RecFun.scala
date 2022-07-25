package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /** Exercise 1
    */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /** Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val i = c(chars)
    System.out.println(i);
    i == 0
  }

  def c(chars: List[Char]): Int = {
    if (chars.isEmpty) 0
    else {
      val z = c(chars.tail)
      if (z > 0) z
      else
        z + (if (chars.head == '(') 1
             else if (chars.head == ')') -1
             else 0)
    }
  }

  /** Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      val moneyMinusFirstCoin = countChange(money - coins.head, coins)
      val moneyWithRestOfCoins = countChange(money, coins.tail)
      moneyMinusFirstCoin + moneyWithRestOfCoins
    }
  }
}
