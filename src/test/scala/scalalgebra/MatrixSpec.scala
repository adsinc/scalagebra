package scalalgebra

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scalalgebra.Matrix.zeros

class MatrixSpec extends FlatSpec with Matchers {
  val RectRows = 3
  val RectCols = 4
  val SqRows = 3

  lazy val squareM = Matrix.random(SqRows, SqRows)

  lazy val rm1 = Matrix.random(RectRows, RectCols)
  lazy val rm2 = Matrix.random(RectRows, RectCols)
  lazy val rm3 = Matrix.random(RectRows, RectCols)

  lazy val rm = Matrix.random(RectCols, RectRows)

  lazy val matrices = Seq(squareM, rm1)

  "A Matrix" should "have size equals rows * cols" in {
    val ms = Seq(
      Matrix.random(1, 1),
      Matrix.random(1, 5),
      Matrix.random(3, 4)
    )
    ms foreach (m => m.size should be(m.rows * m.cols))
  }

  it should "throw IllegalArgumentException if rows or cols is less then 1" in {
    a[IllegalArgumentException] should be thrownBy {
      Matrix(Vector(Vector()))
    }
    a[IllegalArgumentException] should be thrownBy {
      Matrix(Vector(Vector(), Vector()))
    }
  }

  it should "throw IllegalArgumentException if not all rows are equal" in {
    a[IllegalArgumentException] should be thrownBy {
      Matrix(Vector(
        Vector(1, 2, 3),
        Vector()
      ))
    }
  }

  it should "be equal to another instance with same elements" in {
    Matrix(Vector(Vector(1))) should be(Matrix(Vector(Vector(1))))
    Matrix(Vector(Vector(1, 2, 3, 4))) should be(Matrix(Vector(Vector(1, 2, 3, 4))))
  }

  it should "be not equal to another instance with different elements or another object" in {
    Matrix(Vector(Vector(1))) should not be Matrix(Vector(Vector(2)))
    Matrix(Vector(Vector(1, 2, 3, 4))) should not be Matrix(Vector(Vector(3, 2, 3, 4)))
    Matrix(Vector(Vector(1, 2, 3, 4))) should not be Matrix(Vector(Vector(1, 2), Vector(3, 4)))
    matrices foreach(_ == Vector() should be (false))
  }

  it should "correctly return elements by index" in {
    matrices foreach { m =>
      val data = extract2dData(m)
      0 until m.rows foreach { r =>
        0 until m.cols foreach { c =>
          m(r, c) should be (data(r)(c))
        }
      }
      a[NoSuchElementException] should be thrownBy m(m.rows + 1, 0)
      a[NoSuchElementException] should be thrownBy m(0, m.cols + 1)
      a[NoSuchElementException] should be thrownBy m(m.rows + 1, m.cols + 1)
      a[NoSuchElementException] should be thrownBy m(-1, 1)
      a[NoSuchElementException] should be thrownBy m(1, -1)
    }
  }

  it should "correctly return row by index" in {
    matrices foreach { m =>
      val data = extract2dData(m)
      0 until m.rows foreach { r =>
        m.row(r) should be(Matrix(Vector(data(r))))
      }
      a[NoSuchElementException] should be thrownBy m.row(m.rows + 1)
      a[NoSuchElementException] should be thrownBy m.row(-1)
    }
  }

  it should "correctly return column by index" in {
    matrices foreach { m =>
      val data = extract2dData(m)
      0 until m.cols foreach { c =>
        m.col(c) should be(Matrix(data map (row => Vector(row(c)))))
      }
      a[NoSuchElementException] should be thrownBy m.col(m.cols + 1)
    }
  }

  "Zero matrix" should "be zero for all elements" in {
    val m = zeros(3, 4)
    for {
      r <- 0 until m.rows
      c <- 0 until m.cols
    } m(r, c) should be (0)
  }

  "Unary minus" should "change each element to -element" in
    testForEachElement(matrixFn = -_, elementFn = -_)


  it should "applied twice return the equals object" in {
    matrices foreach { m => m should be(-(-m)) }
  }

  "Plus, minus and multiplicate  operator for scalar" should "add and subtract multiply each element on scalar" in {
    val scalar = Random.nextDouble()
    testForEachElement(_ + scalar, _ + scalar)
    testForEachElement(_ - scalar, _ - scalar)
    testForEachElement(_ * scalar, _ * scalar)
  }

  "Minus operator for scalar" should "be equal + (- element)" in {
    val scalar = Random.nextDouble()
    matrices foreach { m => m - scalar should be(m + (-scalar)) }
  }

  "Sum and sub" should "throw exception when add matrix different size" in {
    a[IllegalArgumentException] should be thrownBy squareM + rm1
    a[IllegalArgumentException] should be thrownBy squareM - rm1
  }

  "Sum" should "be commutative" in {
    val a = rm1 + rm2
    val b = rm2 + rm1
    a should be(b)
  }

  it should "be associative" in {
    val a = (rm1 + rm2) + rm3
    val b = rm1 + (rm2 + rm3)
    a should be (b)
  }

  it should "not change matrix when add zero matrix" in
    testForEachElement(m => m + zeros(m.rows, m.cols), e => e)

  it should "be zero matrix when add -matrix" in {
    matrices foreach { m =>
      m + (-m) should be (zeros(m.rows, m.cols))
    }
  }

  "Multiplication" should "throw IllegalArgumentException if left arg cols is not equal right argument rows" in {
    a[IllegalArgumentException] should be thrownBy rm1 * rm2
  }

  it should "result matrix (left arg rows)X(right argument cols)" in {
    val p = rm1 * rm
    p.rows should be (rm1.rows)
    p.cols should be (rm.cols)
  }

  it should "matrix with one matrix should not change it" in {
    val one: Matrix = Matrix.one(squareM.rows)
    squareM * one should be (squareM)
    one * squareM should be (squareM)
  }

  it should "be associative" in {
    rm1 * (rm * rm2) should be ((rm1 * rm) * rm2)
  }

  it should "be distributive" in {
    rm * (rm1 + rm2) should be(rm * rm1 + rm * rm2)
    (rm1 + rm2) * rm should be(rm1 * rm + rm2 * rm)
  }

  it should "be zero when one of arguments is zero" in {
    matrices foreach { m =>
      val z = zeros(m.cols, m.rows)
      m * z should be (zeros(m.rows, z.cols))
      z * m should be (zeros(z.rows, m.cols))
    }
  }

  "Transpose" should "change rows by columns" in {
    for {
      m <- matrices
      mt = m.transpose
      i <- 0 until m.rows
    } m.row(i).elements should be(mt.col(i).elements)
  }

  it should "not change matrix if applied twice" in {
    matrices foreach (m => m.t.t should be(m))
  }

  it should "return MxN matrix when applied to NxM matrix" in {
    matrices foreach { m =>
      val mt = m.t
      m.rows should be(mt.cols)
      m.cols should be(mt.rows)
    }
  }

  def testForEachElement(matrixFn: Matrix => Matrix,
                         elementFn: Double => Double): Unit = {
    for {
      m <- matrices
      newM = matrixFn(m)
      r <- 0 until m.rows
      c <- 0 until m.cols
    } newM(r, c) should be(elementFn(m(r, c)))
  }

  private def extract2dData(m: Matrix) = m.elements.grouped(m.cols).toVector
}
