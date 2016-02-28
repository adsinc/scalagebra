package scalalgebra

import org.scalatest.{FlatSpec, Matchers}

import scalalgebra.Matrix.zeros

class MatrixSpec extends FlatSpec with Matchers {
  lazy val squareData: Vector[Vector[Double]] = Vector(
    Vector(1, 2, 3),
    Vector(4, 5, 6),
    Vector(7, 8, 9)
  )

  lazy val rectangleData: Vector[Vector[Double]] = Vector(
    Vector(1, 2, 3, 2),
    Vector(4, 5, 6, 3),
    Vector(7, 8, 9, 3)
  )

  lazy val rectangleData2: Vector[Vector[Double]] = Vector(
    Vector(12, 12, 33, 22),
    Vector(43, 15, 236, 32),
    Vector(72, 18, 93, 33)
  )

  lazy val rectangleData3: Vector[Vector[Double]] = Vector(
    Vector(1, 12, 323, 222),
    Vector(4, 5, 236, 32),
    Vector(7, 18, 3, 33)
  )

  lazy val rm1 = Matrix(rectangleData)
  lazy val rm2 = Matrix(rectangleData2)
  lazy val rm3 = Matrix(rectangleData3)
  lazy val squareMatrix = Matrix(squareData)

  lazy val datas = Seq(squareData, rectangleData)

  "A Matrix" should "have size equals rows * cols" in {
    val ms = Seq(
      Matrix(Vector(Vector(1))),
      Matrix(Vector(Vector(1, 2, 3))),
      Matrix(Vector(
        Vector(1, 2, 3),
        Vector(1, 3, 4)
      ))
    )
    ms foreach (m => m.size should be(m.rows * m.cols))
  }

  it should "throw IllegalArgumentException if rows or cols is less then 1" in {
    a[IllegalArgumentException] should be thrownBy {
      Matrix(Vector())
      Matrix(Vector(Vector()))
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

  it should "be not equal to another instance with different elements" in {
    Matrix(Vector(Vector(1))) should not be Matrix(Vector(Vector(2)))
    Matrix(Vector(Vector(1, 2, 3, 4))) should not be Matrix(Vector(Vector(3, 2, 3, 4)))
    Matrix(Vector(Vector(1, 2, 3, 4))) should not be Matrix(Vector(Vector(1, 2), Vector(3, 4)))
  }

  it should "correctly return elements by index" in {
    datas foreach { data =>
      val m = Matrix(data)
      val elements = 0 until m.rows map { r =>
        0 until m.cols map (m(r, _))
      }
      elements should be(data)
    }
  }

  it should "correctly return row by index" in {
    datas foreach { data =>
      val m = Matrix(data)
      0 until m.rows foreach { r =>
        m.row(r) should be(Matrix(Vector(data(r))))
      }
    }
  }

  it should "correctly return column by index" in {
    datas foreach { data =>
      val m = Matrix(data)
      0 until m.cols foreach { c =>
        m.col(c) should be(Matrix(data map (row => Vector(row(c)))))
      }
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
    datas map Matrix.apply foreach { m => m should be(-(-m)) }
  }

  "Plus and minus operator for scalar" should "add and subtract scalar to each element" in {
    val scalar = 10
    testForEachElement(_ + scalar, _ + scalar)
    testForEachElement(_ - scalar, _ - scalar)
  }

  "Minus operator for scalar" should "be equal + (- element)" in {
    datas map Matrix.apply foreach { m => m - 10 should be(m + (-10)) }
  }

  "Sum and sub" should "throw exception when add matrix different size" in {
    a[IllegalArgumentException] should be thrownBy squareMatrix + rm1
    a[IllegalArgumentException] should be thrownBy squareMatrix - rm1
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
    Seq(squareMatrix, rm1) foreach { m =>
      m + (-m) should be (zeros(m.rows, m.cols))
    }
  }

  def testForEachElement(matrixFn: Matrix => Matrix,
                         elementFn: Double => Double): Unit = {
    for {
      data <- datas
      m = Matrix(data)
      newM = matrixFn(m)
      r <- 0 until m.rows
      c <- 0 until m.cols
    } newM(r, c) should be(elementFn(m(r, c)))
  }
}
