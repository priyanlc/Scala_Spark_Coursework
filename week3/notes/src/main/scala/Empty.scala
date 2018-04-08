/**
  * Created by priyanchandrapala on 20/02/2017.
  */
object Empty extends IntSet{

  def contains(x:Int):Boolean=false
  def incl(x:Int) :IntSet = new NonEmpty(x,Empty,Empty)
  def union(other:IntSet)=other
  override def toString= "."

}
