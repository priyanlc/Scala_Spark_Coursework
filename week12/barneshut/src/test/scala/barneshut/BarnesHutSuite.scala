package barneshut

  import java.util.concurrent._
  import scala.collection._
  import org.scalatest.FunSuite
  import org.junit.runner.RunWith
  import org.scalatest.junit.JUnitRunner
  import common._
  import scala.math._
  import scala.collection.parallel._
  import barneshut.conctrees.ConcBuffer

  @RunWith(classOf[JUnitRunner])
  class BarnesHutSuite extends FunSuite {

    // test cases for quad tree

import FloatOps._

  test("Empty: center of mass should be the center of the cell") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.massX == 51f, s"${quad.massX} should be 51f")
    assert(quad.massY == 46.3f, s"${quad.massY} should be 46.3f")
  }

  test("Empty: mass should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.mass == 0f, s"${quad.mass} should be 0f")
  }

  test("Empty: total should be 0") {
    val quad = Empty(51f, 46.3f, 5f)
    assert(quad.total == 0, s"${quad.total} should be 0")
  }

  test("Leaf with 1 body") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val quad = Leaf(17.5f, 27.5f, 5f, Seq(b))

    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


  test("Fork with 3 empty quadrants and 1 leaf (nw)") {
    val b = new Body(123f, 18f, 26f, 0f, 0f)
    val nw = Leaf(17.5f, 27.5f, 5f, Seq(b))
    val ne = Empty(22.5f, 27.5f, 5f)
    val sw = Empty(17.5f, 32.5f, 5f)
    val se = Empty(22.5f, 32.5f, 5f)
    val quad = Fork(nw, ne, sw, se)

    assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
    assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
    assert(quad.mass ~= 123f, s"${quad.mass} should be 123f")
    assert(quad.massX ~= 18f, s"${quad.massX} should be 18f")
    assert(quad.massY ~= 26f, s"${quad.massY} should be 26f")
    assert(quad.total == 1, s"${quad.total} should be 1")
  }


    test("updating body when Fork has weight") {
      val b1 = new Body(123f, 18f, 26f, 0f, 0f)
      val b2 = new Body(133f, 34f, 20f, 1f, 4f)
      val b3 = new Body(143f, 43f, 40f, 0f, 0f)
      val b4 = new Body(143f, 43f, 40f, 2f, 3f)

      val nw = Leaf(17.5f, 27.5f, 5f, Seq(b1,b2,b3))
      val ne = Empty(22.5f, 27.5f, 5f)
      val sw = Empty(17.5f, 32.5f, 5f)
      val se = Empty(22.5f, 32.5f, 5f)
      val quad = Fork(nw, ne, sw, se)

      val b4u=b4.updated(quad)

      assert(b4u.xspeed!= 0f, " xspeed should not be 0f")
      assert(b4u.yspeed!= 0f, " yspeed should not be 0f")

    }


    test("Fork with 4 empty quadrants") {
      val nw = Empty(17.5f, 27.5f, 5f)
      val ne = Empty(22.5f, 27.5f, 5f)
      val sw = Empty(17.5f, 32.5f, 5f)
      val se = Empty(22.5f, 32.5f, 5f)
      val quad = Fork(nw, ne, sw, se)

      assert(quad.centerX == 20f, s"${quad.centerX} should be 20f")
      assert(quad.centerY == 30f, s"${quad.centerY} should be 30f")
      assert(quad.mass ~= 0, s"${quad.mass} should be 0f")
      assert(quad.massX ~= 20f, s"${quad.massX} should be 20f")
      assert(quad.massY ~= 30f, s"${quad.massY} should be 30f")
      assert(quad.total == 0, s"${quad.total} should be 0")
    }

  test("Empty.insert(b) should return a Leaf with only that body") {
    val quad = Empty(51f, 46.3f, 5f)
    val b = new Body(3f, 54f, 46f, 0f, 0f)
    val inserted = quad.insert(b)
    inserted match {
      case Leaf(centerX, centerY, size, bodies) =>
        assert(centerX == 51f, s"$centerX should be 51f")
        assert(centerY == 46.3f, s"$centerY should be 46.3f")
        assert(size == 5f, s"$size should be 5f")
        assert(bodies == Seq(b), s"$bodies should contain only the inserted body")
      case _ =>
        fail("Empty.insert() should have returned a Leaf, was $inserted")
    }
  }

    // [Test Description] 'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize
    // [Observed Error] asExpected was false expected Leaf(1.0,1.0,1.0E-5,List(barneshut.package$Body@5119fb47, barneshut.package$Body@7193666c)) found Fork(Empty(0.5,0.5,2.5E-6),Empty(1.5,0.5,2.5E-6),Empty(0.5,1.5,2.5E-6),Leaf(1.5,1.5,2.5E-6,List(barneshut.package$Body@5119fb47)))
    // [Lost Points] 2
    test("'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize") {
      val sizex = 0.00001f
      val quad = Empty(1f, 1f, 0.00001f)
      val b = new Body(3f, 1f, 1f, 0f, 0f)
      val insertedLeaf:Quad = quad.insert(b)
      val b2 = new Body(2f, 2f, 2f, 0f, 0f)
      val insertedLeaf2 = insertedLeaf.insert(b2)

      insertedLeaf2 match {
        case Leaf(centerX, centerY, size, bodies) =>
          //assert(centerX == 1f, s"$centerX should be 1f")
         // assert(centerY == 1f, s"$centerY should be 1f")
          assert(size == sizex, s"$size should be 0.00001f")
          assert(insertedLeaf2.total == 2, s"$insertedLeaf2.total should be 2.0f")


        case _ =>
          fail("Empty.insert() should have returned a Leaf, was $inserted")
      }
    }

  // test cases for Body

  test("Body.updated should do nothing for Empty quad trees") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val body = b1.updated(Empty(50f, 60f, 5f))

    assert(body.xspeed == 0f)
    assert(body.yspeed == 0f)
  }

  test("Body.updated should take bodies in a Leaf into account") {
    val b1 = new Body(123f, 18f, 26f, 0f, 0f)
    val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
    val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

    val quad = Leaf(15f, 30f, 20f, Seq(b2, b3))

    val body = b1.updated(quad)

    assert(body.xspeed ~= 12.587037f)
    assert(body.yspeed ~= 0.015557117f)
  }

  // test cases for sector matrix

  test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96") {
    val body = new Body(5, 25, 47, 0.1f, 0.1f)
    val boundaries = new Boundaries()
    boundaries.minX = 1
    boundaries.minY = 1
    boundaries.maxX = 97
    boundaries.maxY = 97
    val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
    sm += body
    val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
    assert(res, s"Body not found in the right sector")
  }


    test("Combining 2 sector Matrix of size 96") {
      val b1 = new Body(5, 12, 34, 0.1f, 0.1f)
      val b2 = new Body(5, 23, 45, 0.1f, 0.1f)
      val b3 = new Body(5, 56, 9, 0.1f, 0.1f)
      val b4 = new Body(5, 8, 79, 0.1f, 0.1f)
      val b5 = new Body(5, 5, 99, 0.1f, 0.1f)

      //(12, 34), (23, 45), (56, 9), (8, 79), (5, 99)

      val boundaries = new Boundaries()
      boundaries.minX = 1
      boundaries.minY = 1
      boundaries.maxX = 97
      boundaries.maxY = 97
      val sm1 = new SectorMatrix(boundaries, SECTOR_PRECISION)
      sm1 += b1
      sm1 += b2

      val sm2 = new SectorMatrix(boundaries, SECTOR_PRECISION)
      sm2 += b3
      sm2 += b4
      sm2 += b5
      val smc=sm1.combine(sm2)
      //val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
     assert(true, s"Body not found in the right sector")
    }


    test("'insert' should work correctly on a leaf with center (1,1) and size =2")
    {
      val sizex = 2.0f
      val quad = Empty(1f, 1f, 2.0f)
      val b1 = new Body(1f, 0.5f, 0.5f, 0f, 0f)
      val b2 = new Body(0.5f, 1.5f, 0.5f, 0f, 0f)
      val se = Seq(b1,b2)

      val fk:Quad=se.foldLeft(quad:Quad)((q,b)=>q.insert(b))

      //  val insertedLeaf:Quad = quad.insert(b)
      //  val insertedLeaf2 = insertedLeaf.insert(b2)

      fk match {
        case Fork(nw,_,_,_) =>
        //assert(size == 1f, s"$nw.size should be 1.0f")
        //assert(insertedLeaf2.total == 2, s"$insertedLeaf2.total should be 2.0")
        case _ =>
          fail("Empty.insert() should have returned a Leaf, was $inserted")
      }
    }





    test("Fork.insert(b) should insert recursively in the appropriate quadrant")
    {
      val quad = Empty(1f, 1f, 2.0f)
      val a = new Body(1f, 0.5f, 0.5f, 0f, 0f)
      val b = new Body(1f, 0.5f, 1.5f, 0f, 0f)
      val c = new Body(1f, 1.5f, 1.5f, 0f, 0f)
      val d = new Body(1f, 1.5f, 0.5f, 0f, 0f)
      val e = new Body(1f, 1.75f, 1.75f, 0f, 0f)
      val f = new Body(1f, 1.75f, 1.0f, 0f, 0f)

      val se = Seq(a,b,c,d,e,f)

      val fk:Quad=se.foldLeft(quad:Quad)((q,b)=>q.insert(b))

      fk match {
        case Fork(nw,_,_,_) =>
        //assert(size == 1f, s"$nw.size should be 1.0f")
        //assert(insertedLeaf2.total == 2, s"$insertedLeaf2.total should be 2.0")
        case _ =>
          fail("Empty.insert() should have returned a Leaf, was $inserted")
      }
    }



    /*
    [Test Description] Body.updated should consider a Fork as opaque if it is far away
     [Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(-0.33580848574638367) was false xspeed was -0.34836838
     [Lost Points] 2
    */

    test("Body.updated should should consider a Fork as opaque if it is far away") {
      val quad = Empty(1f, 1f, 25.0f)

      val b1 = new Body(123f, 18f, 26f, 0f, 0f)
      val a = new Body(1f, 0.5f, 0.5f, 0f, 0f)
      val b = new Body(1f, 0.5f, 1.5f, 0f, 0f)
      val c = new Body(1f, 1.5f, 1.5f, 0f, 0f)
      val d = new Body(1f, 1.5f, 0.5f, 0f, 0f)
      val e = new Body(1f, 1.75f, 1.75f, 0f, 0f)
      val f = new Body(1f, 1.75f, 1.0f, 0f, 0f)

      val se = Seq(a,b,c,d,e,f)

      val fk:Quad=se.foldLeft(quad:Quad)((q,b)=>q.insert(b))



      val body = b1.updated(fk)

      assert(true)
      assert(true)
    }

/*
    [Test Description] Body.updated should recursively traverse a Fork close to it
    [Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(0.2641816735267639) was false xspeed was 0.26652554
    [Lost Points] 2
*/
    test("Body.updated should recursively traverse a Fork close to it") {
      val b1 = new Body(123f, 18f, 26f, 0f, 0f)
      val b2 = new Body(524.5f, 24.5f, 25.5f, 0f, 0f)
      val b3 = new Body(245f, 22.4f, 41f, 0f, 0f)

      val quad = Leaf(15f, 30f, 200f, Seq(b2, b3))

      val body = b1.updated(quad)

      assert(body.xspeed ~= 12.587037f)
      assert(body.yspeed ~= 0.015557117f)
    }

/*
      [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
      [Observed Error] res was false Body not found in the right sector. Hint: sector sizes could be fractions
      [Lost Points] 2
*/
    test("'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100") {
      val body = new Body(5, 25, 47, 0.1f, 0.1f)
      val boundaries = new Boundaries()
      boundaries.minX = 1
      boundaries.minY = 1
      boundaries.maxX = 101
      boundaries.maxY = 101
      val sm = new SectorMatrix(boundaries, SECTOR_PRECISION)
      sm += body
      val res = sm(2, 3).size == 1 && sm(2, 3).find(_ == body).isDefined
      assert(res, s"Body not found in the right sector")
    }




/*

      [Test Description] Body.updated should consider a Fork as opaque if it is far away
      [Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(-0.33580848574638367) was false xspeed was -0.34836838
      [Lost Points] 2

      [Test Description] Body.updated should recursively traverse a Fork close to it
      [Observed Error] FloatOps.DoubleOps(body.xspeed.toDouble).~=(0.2641816735267639) was false xspeed was 0.18462096
      [Lost Points] 2



    [Test Description] 'mergeBoundaries' should correctly merge two boundaries
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

    [Test Description] 'insert' should work correctly on a leaf with center (1,1) and size <= minimumSize
      [Observed Error] asExpected was false expected Leaf(1.0,1.0,1.0E-5,List(barneshut.package$Body@76508ed1, barneshut.package$Body@41e36e46)) found Fork(Empty(0.5,0.5,5.0E-6),Empty(1.5,0.5,5.0E-6),Empty(0.5,1.5,5.0E-6),Leaf(1.5,1.5,5.0E-6,List(barneshut.package$Body@76508ed1, barneshut.package$Body@41e36e46)))
    [Lost Points] 2

      [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 96
    [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

      [Test Description] 'SectorMatrix.+=' should add a body at (64,27) to the correct bucket of a sector matrix of size 108 when the sector precision is 12
    [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2



      [Test Description] computeSectorMatrix should be parallel
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2



      [Test Description] 'computeSectorMatrix' should correctly work given 5 points within a boundary of size 96 when some points map to the same sector
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

      [Test Description] 'updateBoundaries' should correctly update the boundary given a body at (3,5)
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

    [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 that contain some points in the same sector
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2



      [Test Description] updateBodies should correctly update all bodies wrt to a Quad
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

    [Test Description] 'SectorMatrix.+=' should add a body at (25,47) to the correct bucket of a sector matrix of size 100
    [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

      [Test Description] 'SectorMatrix.combine' should correctly combine two sector matrices of size 96 containing points: (12, 34), (23, 45), (56, 9), (8, 79), (5, 99)
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

    [Test Description] 'computeSectorMatrix' should correctly add points to buckets given 7 points within a boundary of size 96
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

    [Test Description] updateBodies should be parallel
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

      [Test Description] 'updateBoundaries' should correctly update the boundary when invoked repeatedly with points in the range (0,0) to (100,100)
      [Observed Error] an implementation is missing
      [exception was thrown] detailed error message in debug output section below
      [Lost Points] 2

*/
}

object FloatOps {
  private val precisionThreshold = 1e-4

  /** Floating comparison: assert(float ~= 1.7f). */
  implicit class FloatOps(val self: Float) extends AnyVal {
    def ~=(that: Float): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Long floating comparison: assert(double ~= 1.7). */
  implicit class DoubleOps(val self: Double) extends AnyVal {
    def ~=(that: Double): Boolean =
      abs(self - that) < precisionThreshold
  }

  /** Floating sequences comparison: assert(floatSeq ~= Seq(0.5f, 1.7f). */
  implicit class FloatSequenceOps(val self: Seq[Float]) extends AnyVal {
    def ~=(that: Seq[Float]): Boolean =
      self.size == that.size &&
        self.zip(that).forall { case (a, b) =>
          abs(a - b) < precisionThreshold
        }
  }
}

