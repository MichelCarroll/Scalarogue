package primitives

import scala.math.Ordering


object NonNeg {

  import scala.language.implicitConversions

  class NonNegInt private (val value: Int) extends AnyVal

  object NonNegInt {
    def apply(v: Int) = {
      require(v >= 0, "NonNegInt forbids negative integer values")
      new NonNegInt(v)
    }

    implicit def toNonNegInt(v: Int): NonNegInt = NonNegInt(v)
  }

  implicit def toInt(nn: NonNegInt): Int = nn.value
  implicit def toDouble(nn: NonNegInt): Double = nn.value.toDouble

  implicit object NonNegOrdering extends Ordering[NonNegInt] {
    def compare(x: NonNegInt, y: NonNegInt) =
      if (x < y) -1
      else if (x == y) 0
      else 1
  }

}

case class Ratio(value: Double) {
  require(value >= 0 && value <= 1, "NonNegInt forbids negative integer values")
}