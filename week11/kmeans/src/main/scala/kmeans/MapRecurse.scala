package kmeans

/**
  * Created by priyanchandrapala on 12/05/2017.
  */
class MapRecurse {
// use the fork join framework

  def mapRecurse(mp:Map[String,Any]):Unit ={

    for(m<-mp){
        m._2 match {
          case Nil =>()
          case (x:String)::(xs:List[String]) =>println(x); xs.foreach(a=>println(a))
          case (x:Map[String,Any])::(xs:List[Map[String,Any]]) =>mapRecurseHelper(x,xs)
          case _ =>()
        }
    }

    def mapRecurseHelper(head:Map[String,Any],tail:List[Map[String,Any]]):Unit = {
          // create thread for head and recursively call mapRecurseHelper for the tail
    }
  }

}
