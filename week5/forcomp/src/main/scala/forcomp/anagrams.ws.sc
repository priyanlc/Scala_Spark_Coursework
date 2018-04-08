import forcomp.loadDictionary

/** A word is simply a `String`. */
type Word = String

/** A sentence is a `List` of words. */
type Sentence = List[Word]

type Occurrences = List[(Char, Int)]


val dictionary: List[Word] = loadDictionary


def wordOccurrences(w: Word): Occurrences = {

  (w.toLowerCase().toCharArray.groupBy((c:Char)=>c) map {
    case (k,v) => (k,v.length)
  }).toList.sorted

}

/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString)


//val l = List(("A", 1, 4), ("A", 2, 5), ("A", 3, 6), ("B", 1, 4), ("B", 2, 5), ("B", 3, 6))

//l groupBy (_._1) map { case (k,v) =>(k, v map { case (v1,v2,v3) => (v2,v3)})}

val xx:Occurrences =List(('a', 1), ('d', 1), ('l', 1), ('r', 1))

val yy:Occurrences =List(('r', 1))


def subtractCheck(ab:(Char,Int)):Boolean= {
  val xm=xx.toMap
  var v1:Int= -1
  if(xm.contains(ab._1))
    if(xm.get(ab._1).isDefined) v1=xm.get(ab._1).get
    else v1= -1

  if(v1 >=ab._2) true
  else false
}

yy.map(subtractCheck)

yy.map(subtractCheck).exists(x=>(x==false))


def subtract(ab:(Char,Int)):(Char,Int)= {
  val xm=xx.toMap
  var v1:Int= -1
  v1=xm.get(ab._1).getOrElse(-1)
  (ab._1,v1-ab._2)
}

yy.map(subtract)

yy.map(subtract).filter(a=>a._2>0)

//subtract(xx,yy)

