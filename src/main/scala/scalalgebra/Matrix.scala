package scalalgebra

import scala.language.postfixOps
import scala.util.Random

class Matrix(val data: Vector[Vector[Double]], val precision: Double = 0.001) {
  val rows = data.length
  require(rows > 0)
  require(data forall (r => r.length == data.head.length))
  val cols = data.head.length
  require(cols > 0)
  val size = rows * cols

  def row(row: Int): Matrix = Matrix(Vector(data(row)))

  def col(col: Int): Matrix = Matrix(data map (row => Vector(row(col))))

  def apply(row: Int, col: Int): Double = data(row)(col)

  def unary_-(): Matrix = Matrix(data map (_ map (-_)))

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
  def apply(elems: Vector[Vector[Double]]): Matrix = new Matrix(elems)

  def zeros(rows: Int, cols: Int): Matrix = {
    val row = {0 until cols map (_ => 0.0)}.toVector
    Matrix(0 until rows map (_ => row) toVector)
  }

  def random(rows: Int, cols: Int): Matrix =
    Matrix(Vector.fill(rows, cols)(Random.nextDouble()))
}
