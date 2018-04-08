package recursion

/**
  * Created by priyanchandrapala on 12/02/2017.
  */
object Fibonacci extends App{
  println(fib(1, 2))

  def fib(prevPrev: Int, prev: Int) {
    val next = prevPrev + prev
    println(next)
    if (next > 1000000) System.exit(0)
    fib(prev, next)
  }


}
