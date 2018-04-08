package higherorderfunctions

/**
  * Created by priyanchandrapala on 15/02/2017.
  */
class Rational (n:Int,d:Int){
  require(d!=0, "The denorminator should be nonzero")

  def numer=n
  def denom=d

  def this(n:Int)=this(n,1)

  private def gcd(a:Int,b:Int):Int = if (b==0) a else gcd(b,a%b)

  def add(that:Rational):Rational =
    new Rational(that.numer*denom+that.denom*numer,that.denom*denom)

  def subtract(that:Rational): Rational = add(that.neg)

  def neg:Rational =new Rational(-numer,denom)

  def less(that:Rational)= numer*that.denom<that.numer*denom

  def max(that:Rational)=if(this.less(that)) that else this

  override def toString = {
    val g =gcd(n,d)
    numer/g + "/" + denom/g
  }

}
