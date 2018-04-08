package wikipedia

import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD

/**
  * Created by priyanchandrapala on 27/02/2018.
  */


  case class WikipediaArticle(title:String, text:String) {
          def mentionsLanguage(lang:String) : Boolean = text.split(' ').contains(lang)
  }

  object WikipediaRanking {

    val langs = List(
      "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
      "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Wiki App")
    val sc: SparkContext = new SparkContext(conf)

    // Hint: use a combination of `sc.textFile`, `WikipediaData.filePath` and `WikipediaData.parse`
      val wikiRdd: RDD[WikipediaArticle] =sc.textFile(WikipediaData.filePath).map(l=>WikipediaData.parse(l))


    /** Returns the number of articles on which the language `lang` occurs.
      *  Hint1: consider using method `aggregate` on RDD[T].
      *  Hint2: consider using method `mentionsLanguage` on `WikipediaArticle`
      */
    def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int =  rdd.filter(l=>l.mentionsLanguage(lang)).count().toInt



    /* (1) Use `occurrencesOfLang` to compute the ranking of the languages
   *     (`val langs`) by determining the number of Wikipedia articles that
   *     mention each language at least once. Don't forget to sort the
   *     languages by their occurrence, in decreasing order!
   *
   *   Note: this operation is long-running. It can potentially run for
   *   several seconds.
   */
    def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] = {

      langs.map(l=>(l,occurrencesOfLang(l,rdd))).sortBy(-_._2)


    }

    def seqOp = (i:Int,y:Boolean) => if(y) i+1 else i

    def combOp = (a:Int,b:Int)=>(a+b)
    /** Returns the number of articles on which the language `lang` occurs.
      *  Hint1: consider using method `aggregate` on RDD[T].
      *  Hint2: consider using method `mentionsLanguage` on `WikipediaArticle`
      */
    def occurrencesOfLang(lang: String, rdd: RDD[WikipediaArticle]): Int = rdd.map(a=>a.mentionsLanguage(lang)).aggregate(0)(seqOp,combOp)

    /* (1) Use `occurrencesOfLang` to compute the ranking of the languages
     *     (`val langs`) by determining the number of Wikipedia articles that
     *     mention each language at least once. Don't forget to sort the
     *     languages by their occurrence, in decreasing order!
     *
     *   Note: this operation is long-running. It can potentially run for
     *   several seconds.
     */
    def rankLangs(langs: List[String], rdd: RDD[WikipediaArticle]): List[(String, Int)] =  langs.map(l=>(l,occurrencesOfLang(l,rdd))).sortBy(a=>a._2).reverse

    /* Compute an inverted index of the set of articles, mapping each language
    * to the Wikipedia pages in which it occurs.
    */
    def makeIndex(langs: List[String], rdd:RDD[WikipediaArticle]):RDD[(String,Iterable[WikipediaArticle])]={
      val lr=for {
        r<-rdd
        l<-langs
        if(r.mentionsLanguage(l))
      } yield(l,r)
      lr.groupByKey()
    }

    //wikiRdd.filter(l=>l.mentionsLanguage("Scala")).count()


    def main (args:Array[String]): Unit = {

      }

}
