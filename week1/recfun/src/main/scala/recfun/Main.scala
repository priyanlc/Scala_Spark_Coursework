package recfun

import scala.annotation.tailrec


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println("Balence Parenthasis")

    val str1 ="This is a test ( not the only one )"
    val str2 ="This is a test (( not the only one )"
    val str3 ="This is a test (( not the only one ))"
    val str4 ="This is a test (( not the only one ). ( and a second test)"
    val str5 ="This is a test (( not the only one ))))"

    println("str1 "+balance(str1.toList))//t
    println("str2 "+balance(str2.toList))//f
    println("str3 "+balance(str3.toList))//t
    println("str4 "+balance(str4.toList))//f
    println("str5 "+balance(str5.toList))//f

    println("Count Change "+ countChange(1,List(1,2)))

   // println("Count Change 1 "+ changeAcc(0,List(1,2)))




  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
    def balance(chars: List[Char]): Boolean = {
    val open ='('
    val close =')'
    @tailrec
    def balanceHelper(chars: List[Char], stack: Int): Boolean = {
      chars match {
        case Nil=> stack==0
        case x::tail =>   if (x == open) balanceHelper(tail, stack + 1)
                          else if (x == close) stack > 0 && balanceHelper(tail, stack - 1)
                          else balanceHelper(tail, stack)
      }
    }
    balanceHelper(chars, 0)
  }

   /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0)
      0
    else
      changeAcc(money, coins.reverse)
  }

  def changeAcc(money: Int, coins: List[Int]): Int = {
    val availableCoins = coins.filter(_ <= money)

    if (money == 0)
      1
    else if (money < 0)
      0
    else if (availableCoins.length == 0)
      0
    else
      changeAcc(money - coins.head, coins) + changeAcc(money, coins.tail)
  }

}
