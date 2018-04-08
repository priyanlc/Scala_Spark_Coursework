/**
  * Created by priyanchandrapala on 20/02/2017.
  */
class NonEmpty(elem:Int,left:IntSet,right:IntSet) extends IntSet {

  def contains(x:Int):Boolean= {
    if(x<elem) left contains x
    else if (x>elem) right contains x
    else true
  }

  def incl(x:Int):IntSet= {
    if(x<elem) new NonEmpty(elem,left incl x,right)
    else if (x>elem) new NonEmpty(elem,left,right incl x)
    else this
  }

  def union(other:IntSet):IntSet = {
    ((left union right) union other  ) incl elem
  }

  override def toString= "{" +left + elem + right + "}"

}
