package scalalgebra

class Matrix(elems: Vector[Vector[Double]]) {
  val rows = elems.length
  require(rows > 0)
  require(elems forall (r => r.length == elems.head.length))
  val cols = elems.head.length
  require(cols > 0)
  private val data: Vector[Vector[Double]] = elems
  val size = rows * cols

  def row(row: Int): Matrix = Matrix(Vector(data(row)))

  def col(col: Int): Matrix = Matrix(data map (row => Vector(row(col))))

  def apply(row: Int, col: Int): Double = elems(row)(col)

  def unary_- = Matrix(data map (_ map (-_)))

  def + (other: Matrix) = ???

  def +(number: Double) = Matrix(data map (_ map (_ + number)))

  def - (other: Matrix) = ???

  def -(number: Double) = this + (-number)

  def compareSize(other: Matrix): Boolean =
    other.rows == rows && other.cols == cols

  override def equals(that: scala.Any): Boolean = that match {
    case m: Matrix => compareSize(m) && m.data == data
    case _ => false
  }
}

object Matrix {
  def apply(elems: Vector[Vector[Double]]) = new Matrix(elems)
}
