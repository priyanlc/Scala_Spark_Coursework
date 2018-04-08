class Coder(code: => Unit) {
  def exec = {
    println("before")
    code
    println("after")}
}

val brackets = new Coder {println("testing")}


class Scheduled(val time: Int, val callback: () => Unit) {
  def doit = callback()
}

object Scheduled {
  def apply(time: Int, callback: => Unit) =
    new Scheduled(time, { () => callback })
}


Scheduled(1234, println("x"))


Scheduled(1234, println("x")).doit
