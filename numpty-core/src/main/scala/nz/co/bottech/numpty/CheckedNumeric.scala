package nz.co.bottech.numpty

import simulacrum.typeclass

@typeclass trait CheckedNumeric[T] {

  def plus(x: T, y: T): Option[T]
  def minus(x: T, y: T): Option[T]
  def times(x: T, y: T): Option[T]
  def negate(x: T): Option[T]
  def fromInt(x: Int): Option[T]
  def toInt(x: T): Option[Int]
  def toLong(x: T): Option[Long]
  def toFloat(x: T): Option[Float]
  def toDouble(x: T): Option[Double]

  def zero = fromInt(0)
  def one = fromInt(1)

  def abs(x: T): T = if (lt(x, zero)) negate(x) else x
  def signum(x: T): Int =
    if (lt(x, zero)) -1
    else if (gt(x, zero)) 1
    else 0
}
