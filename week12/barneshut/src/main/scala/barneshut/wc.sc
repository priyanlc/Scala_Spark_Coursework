import scala.reflect.runtime.universe._

object Recognizer {
  def recognize[T](x: T)(implicit tag: TypeTag[T]): String =
    tag.tpe match {
      case TypeRef(utype, usymbol, args) =>
        List(utype, usymbol, args).mkString("\n")
    }
}

val list: List[Int] = List(1, 2)

val result = Recognizer.recognize(list)

println(result)