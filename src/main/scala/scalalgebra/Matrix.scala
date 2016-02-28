package scalalgebra

import scala.language.postfixOps

class Matrix(elems: Vector[Vector[Double]]) {
  val rows = elems.length
  require(rows > 0)
  require(elems forall (r => r.length == elems.head.length))
  val cols = elems.head.length
  require(cols > 0)
  private val data = elems
  val size = rows * cols

  def row(row: Int): Matrix = Matrix(Vector(data(row)))

  def col(col: Int): Matrix = Matrix(data map (row => Vector(row(col))))

  def apply(row: Int, col: Int): Double = elems(row)(col)

  def unary_-(): Matrix = Matrix(data map (_ map (-_)))

  def +(other: Matrix): Matrix = {
    require(compareSize(other))
    //todo refactor it!
    Matrix(0 until rows map { r =>
      0 until cols map { c =>
        this (r, c) + other(r, c)
      } toVector
    } toVector)
  }

  def -(other: Matrix): Matrix = this + (-other)

  def +(number: Double): Matrix = Matrix(data map (_ map (_ + number)))

  def -(number: Double): Matrix = Matrix(data map (_ map (_ - number)))

  def compareSize(other: Matrix): Boolean =
    other.rows == rows && other.cols == cols

  override def equals(that: scala.Any): Boolean = that match {
    case m: Matrix => compareSize(m) && m.data == data
    case _ => false
  }
}

object Matrix {
  def apply(elems: Vector[Vector[Double]]): Matrix = new Matrix(elems)
}
