package scalalgebra

import scala.util.Random

case class Matrix(elements: Vector[Double], rows: Int, cols: Int, precision: Double) {
  val size = rows * cols

  //todo messages for all require
  require(rows > 0 && cols > 0)
  require(elements.length == size)

  def row(row: Int): Matrix = {
    validateRow(row)
    copy(elements = elements slice(row * cols, cols * (row + 1)), rows = 1)
  }

  def col(col: Int): Matrix = {
    validateColumn(col)
    copy(elements = (col until (size, cols) map elements.apply).toVector, cols = 1)
  }

  def apply(row: Int, col: Int): Double = {
    validateRow(row)
    validateColumn(col)
    elements(row * cols + col)
  }

  private def validateColumn(col: Int) = validateIndex(col, cols)

  private def validateRow(row: Int) = validateIndex(row, rows)

  private def validateIndex(index: Int, maxValue: Int) =
    if(index >= maxValue || index < 0) {
      throw new NoSuchElementException(s"Index $index not in interval [0; $maxValue]")
    }

  def unary_-(): Matrix = copy(elements = elements map (-_))

  def +(other: Matrix): Matrix = elementByElementOp(other, _ + _)

  def -(other: Matrix): Matrix = elementByElementOp(other, _ - _)

  private def elementByElementOp(other: Matrix,
                                 fn: (Double, Double) => Double) = {
    require(equalsSize(other))
    copy(elements = elements zip other.elements map fn.tupled)
  }

  def +(number: Double): Matrix = copy(elements = elements map (_ + number))

  def -(number: Double): Matrix = copy(elements = elements map (_ - number))

  def equalsSize(other: Matrix): Boolean =
    other.rows == rows && other.cols == cols

  override def equals(that: scala.Any): Boolean = that match {
    case m: Matrix => equalsSize(m) && {
      m.elements zip elements forall (p => (p._1 - p._2).abs <= precision)
    }
    case _ => false
  }

  override def toString: String =
    elements grouped cols map (_ mkString " ") mkString (s"\n$rows x $cols\n", "\n", "\n")
}

object Matrix {
  val DefaultPrecision = 0.001

  def apply(data: Vector[Vector[Double]], precision: Double = DefaultPrecision): Matrix = {
    Matrix(
      elements = data.flatten,
      rows = data.length,
      cols = data.head.length,
      precision = precision
    )
  }

  def zeros(rows: Int, cols: Int, precision: Double = DefaultPrecision): Matrix = {
    Matrix(Vector.fill(rows, cols)(0), precision = precision)
  }

  def random(rows: Int, cols: Int, precision: Double = DefaultPrecision): Matrix =
    Matrix(Vector.fill(rows, cols)(Random.nextDouble()), precision = precision)
}
