object overrides{
println("Welcome to Scala worksheet")
}

abstract class Base {
def foo =1
  def bar :Int

}

abstract class sub extends Base {
override def foo=2
  def bar =3

}