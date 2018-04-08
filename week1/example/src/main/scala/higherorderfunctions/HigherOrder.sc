def id(x:Int):Int =x

def cube (a:Int):Int= a*a*a

def fact(a:Int):Int =
  if (a==0) 1 else a*fact(a-1)


//def sum(f:Int=>Int,a:Int,b:Int):Int={
//  if(a>b) 0
//  else f(a)+ sum(f,a+1,b)
//}

def sum(f:Int=>Int):(Int,Int)=>Int ={
  def sumF(a:Int,b:Int):Int =
    if(a>b) 0
    else f(a)+sumF(a+1,b)
  sumF
}

def sumInts = sum(x=>x)
//if(a>b) 0 else id(a)+sumints(a+1,b)

def sumCube =sum(x=>x*x*x)
//if(a>b) 0 else cube(a)+sumcube(a+1,b)

def sumFact =sum(fact)
//if(a>b) 0 else fact(a) +sumfact(a+1,b)


sumInts(1,6)
sumCube(2,4)
sumFact(1,6)








