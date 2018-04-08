/**
  * Created by priyanchandrapala on 20/02/2017.
  */
abstract class IntSet {

  def incl(x:Int):IntSet
  def contains(x:Int):Boolean
  def union(other:IntSet):IntSet

}
