package scalalgebra

class Matrix(elems: Vector[Vector[Double]]) {
  val rows = elems.length

  require(rows > 0)
  require(elems forall (r => r.length == elems.head.length))

  val cols = elems.head.length

  require(cols > 0)

  private val data: Vector[Double] = elems.flatten

  val size = data.length

  override def equals(obj: scala.Any): Boolean = true
}

object Matrix {
  def apply(elems: Vector[Vector[Double]]) = new Matrix(elems)
}
