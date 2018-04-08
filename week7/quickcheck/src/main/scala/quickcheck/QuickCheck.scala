package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop.BooleanOperators
import Prop.forAll
import Prop.collect

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {


  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      a<- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(a,h)
  )

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a:Int) =>
    val h = insert(a, empty)
    val h2 = insert(2*a, h)
    findMin(h2) == a
  }

  property("min3") = forAll { (a:Int) =>
    val h = insert(a, empty)
    deleteMin(h)==empty
  }

/* 1. I need a generator to generate some random integers maybe 20 in total, I need this in an container
   2. Insert the random integers to the heap from the container, this function may be recursive
   3. A recursive function to get find minimum and delete the minimum and add to a list
   4. The list should be equivalent to a sorted list
 */
  property("find sorted list ") = forAll { (a:Int) =>
    val h = insert(a, empty)
    deleteMin(h)==empty
  }


  property("dummy generator") = forAll(Gen.choose(1,10)) { n =>



  }


}
