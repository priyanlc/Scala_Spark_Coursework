package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal(b()*b()-4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    // (-b ± √Δ) / (2a)
    val valDeltaRoot = Signal(math.sqrt(computeDelta(a,b,c)()))
    val negB = Signal(-1*b())
    val twoA = Signal(2*a())

    Signal{
      if(valDeltaRoot()<0) Set()
      else Set((negB()+valDeltaRoot())/twoA(),(negB()-valDeltaRoot())/twoA())
    }
  }
}
