package recursion

import scala.annotation.tailrec

/**
  * Created by priyanchandrapala on 12/02/2017.
  */
object Recursion {

  def countdown(x: Int): Unit = {
    if (x > 0) {
      countdown(x - 1)
      println(x)
    }
    else println("The end")
  }

  def getClassAsString(x: Any): String = x match {
    case s: String => s + " is a String"
    case i: Int => "Int"
    case f: Float => "Float"
    case l: List[_] => "List"
    case _ => "Unknown"
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: tail => x + sum(tail)

  }

  def sum1(ints: List[Int]): Int = ints match {
    case Nil => 0
    case x :: tail => x + sum(tail)
  }

  def sum2(ints: List[Int]): Int = {
    @tailrec
    def sumHelper(ints: List[Int], accum: Int): Int = {
      ints match {
        case Nil => accum
        case x :: tail => sumHelper(tail, x + accum)
      }
    }

    sumHelper(ints, 0)
  }


  def sum3(ints: List[Int]): Int = {
    if (ints.isEmpty) 0
    else ints.head + sum3(ints.tail)
  }


  def product(ints: List[Int]): Int = {
    @tailrec
    def productHelper(ints: List[Int], product: Int): Int = {
      ints match {
        case Nil => product
        case x :: tail => productHelper(tail, x * product)
      }
    }

   productHelper(ints, 1)
  }


  def findMax(ints:List[Int]):Int= {
    @tailrec
    def maxHelper(ints:List[Int],theMax:Int):Int = {
          ints match {
            case Nil=> theMax
            case x::tail =>
              val maxValue:Int=if(x>theMax) x else theMax
                  maxHelper(tail,maxValue)
          }
    }
    maxHelper(ints,0)
  }


  def findMax2(ints:List[Int]):Int= {
    @tailrec
    def maxHelper(ints:List[Int],theMax:Int):Int = {
      if(ints.isEmpty) theMax
      else {
        val newMax= if(ints.head>theMax) ints.head else theMax
        maxHelper(ints.tail,newMax)
      }
    }
    maxHelper(ints,0)
  }


  /*
     def sum(xs:List[Int]):Int={
         sum()
     }


     def product(xs:): Int = {
     }

     */

}
