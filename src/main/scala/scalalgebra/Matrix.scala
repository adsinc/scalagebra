package scalalgebra

import scala.util.Random
import scalalgebra.Matrix.Precision

case class Matrix(elements: Vector[Double], rows: Int, cols: Int)(implicit precision: Precision) {
  val size = rows * cols

  require(rows > 0, s"Rows = $rows but should be > 0")
  require(cols > 0, s"Cols = $cols but should be > 0")
  require(elements.length == size, s"Elements length = ${elements.length} != $rows * $cols ($size)")

  def row(row: Int): Matrix = {
    validateRow(row)
    copy(elements = elements slice(row * cols, cols * (row + 1)), rows = 1)
  }

  def col(col: Int): Matrix = {
    validateColumn(col)
    copy(elements = (col until(size, cols) map elements.apply).toVector, cols = 1)
  }

  def apply(row: Int, col: Int): Double = {
    validateRow(row)
    validateColumn(col)
    elements(row * cols + col)
  }

  private def validateColumn(col: Int) = validateIndex(col, cols)

  private def validateRow(row: Int) = validateIndex(row, rows)

  private def validateIndex(index: Int, maxValue: Int) =
    if (index >= maxValue || index < 0) {
      throw new NoSuchElementException(s"Index $index not in interval [0; $maxValue]")
    }

  def unary_-(): Matrix = copy(elements = elements map (-_))

  def +(other: Matrix): Matrix = elementByElementOp(other, _ + _)

  def -(other: Matrix): Matrix = elementByElementOp(other, _ - _)

  private def elementByElementOp(other: Matrix,
                                 fn: (Double, Double) => Double) = {
    require(equalsSize(other), "Matrices should be equals size")
    copy(elements = elements zip other.elements map fn.tupled)
  }

  def *(other: Matrix): Matrix = {
    require(cols == other.rows, "To allow A * B A.cols should be equal B.rows")
    Matrix(Vector.tabulate(rows, other.cols) { (r, c) =>
      val re = row(r).elements
      val ce = other.col(c).elements
      (re zip ce map (p => p._1 * p._2)).sum
    })
  }

  def +(number: Double): Matrix = applyToEach(_ + number)

  def -(number: Double): Matrix = applyToEach(_ - number)

  def *(number: Double): Matrix = applyToEach(_ * number)

  private def applyToEach(fn: (Double) => Double) = copy(elements = elements map fn)

  def equalsSize(other: Matrix): Boolean =
    other.rows == rows && other.cols == cols

  override def equals(that: scala.Any): Boolean = that match {
    case m: Matrix => equalsSize(m) && {
      m.elements zip elements forall (p => (p._1 - p._2).abs <= precision.value)
    }
    case _ => false
  }

  override def toString: String =
    elements grouped cols map (_ mkString " ") mkString(s"\n$rows x $cols\n", "\n", "\n")
}

object Matrix {
  val DefaultPrecision = 0.001
  case class Precision(value: Double)
  implicit val precision = Precision(DefaultPrecision)

  def apply(data: Vector[Vector[Double]]): Matrix = {
    Matrix(
      elements = data.flatten,
      rows = data.length,
      cols = data.head.length
    )
  }

  def zeros(rows: Int, cols: Int): Matrix = {
    Matrix(Vector.fill(rows, cols)(0))
  }

  def one(size: Int): Matrix = {
    Matrix(Vector.tabulate(size, size)((r, c) => if(r == c) 1 else 0))
  }

  def random(rows: Int, cols: Int): Matrix =
    Matrix(Vector.fill(rows, cols)(Random.nextDouble()))
}
