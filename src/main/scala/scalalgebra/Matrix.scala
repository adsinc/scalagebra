package scalalgebra

import scala.language.postfixOps
import scala.util.Random
import scalalgebra.Matrix.DefaultPrecision

class Matrix(val elements: Vector[Double], val rows: Int, val cols: Int, val precision: Double) {
  //todo remove
  val data: Vector[Vector[Double]] = (elements grouped cols).toVector
  require(rows > 0 && cols > 0)
  val size = rows * cols
  require(elements.length == size)

  def this(data: Vector[Vector[Double]], precision: Double = DefaultPrecision) {
    this(
      elements = data.flatten,
      rows = data.length,
      cols = data.head.length,
      precision = precision
    )
  }

  def row(row: Int): Matrix = {
    validateRow(row)
    new Matrix(
      elements = elements slice(row * cols, cols * (row + 1)),
      rows = 1,
      cols = cols,
      precision = precision
    )
  }

  def col(col: Int): Matrix = {
    validateColumn(col)
    new Matrix(
      elements = col until (size, cols) map elements.apply toVector,
      rows = rows,
      cols = 1,
      precision = precision
    )
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


  def unary_-(): Matrix = new Matrix(
    elements = elements map (-_),
    rows = rows,
    cols = cols,
    precision = precision
  )

  def +(other: Matrix): Matrix = elementByElementOp(other, _ + _)

  def -(other: Matrix): Matrix = elementByElementOp(other, _ - _)

  private def elementByElementOp(other: Matrix,
                                 fn: (Double, Double) => Double) = {
    require(compareSize(other))
    Matrix(0 until rows map { r =>
      0 until cols map { c =>
        fn(this(r, c), other(r, c))
      } toVector
    } toVector)
  }

  def +(number: Double): Matrix = Matrix(data map (_ map (_ + number)))

  def -(number: Double): Matrix = Matrix(data map (_ map (_ - number)))

  def compareSize(other: Matrix): Boolean =
    other.rows == rows && other.cols == cols

  override def equals(that: scala.Any): Boolean = that match {
    case m: Matrix => compareSize(m) && {
      //todo refactor?
      m.data.flatten zip data.flatten forall (p => (p._1 - p._2).abs <= precision)
    }
    case _ => false
  }

  override def toString: String =
    data map (_ mkString " ") mkString(s"\n$rows x $cols\n", "\n", "\n")
}

object Matrix {
  val DefaultPrecision = 0.001

  def apply(elems: Vector[Vector[Double]], precision: Double = DefaultPrecision): Matrix = new Matrix(elems)

  def zeros(rows: Int, cols: Int): Matrix = {
    val row = {0 until cols map (_ => 0.0)}.toVector
    Matrix(0 until rows map (_ => row) toVector)
  }

  def random(rows: Int, cols: Int): Matrix =
    Matrix(Vector.fill(rows, cols)(Random.nextDouble()))
}
