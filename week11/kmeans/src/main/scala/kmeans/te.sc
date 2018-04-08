def prependIfLong(candidate: Any, elems:Any*):Seq[Any] = {
  if (candidate.toString.length > 1){
    candidate +: elems

  } else {println("in2")
    elems

  }
}

println(prependIfLong(" I", "love", "Scala"))

val prepended = prependIfLong(" I", "love", "Scala")

println(prepended(1))



def prependIfLongRefac(candidate: Any)(elems:Any*):Seq[Any] = {
  if (candidate.toString.length > 1)
    candidate +: elems
  else elems
}

println(prependIfLongRefac("I", "love", "Scala")(0))


def method[T](x: T): T = x

method(1)


val ll= method(1, 2)

println(ll)





val option1 = Option(String,String)

val option2 = Option(String)

