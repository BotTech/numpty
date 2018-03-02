package nz.co.bottech.numpty

import nz.co.bottech.numpty.BoundedNumeric.NumericError

sealed trait BoundedNumeric[T] {

  def validValue: Option[T]

  def valueOrError: Either[NumericError[T], T]
}

object BoundedNumeric {

  // TODO: Make this a T Refined Interval.Closed
  case class WithinBounds[T](value: T, bounds: NumericBounds[T])
                            (implicit numeric: Numeric[T]) extends BoundedNumeric[T] {

    import bounds._
    import numeric._

    require(lower <= upper, s"The lower bound ($lower) must be less than or equal to the upper bound ($upper)")
    require(lower <= value && value <= upper, s"The value ($value) must be within the bounds ($lower, $upper)")

    override val validValue: Option[T] = Some(value)

    override def valueOrError: Either[NumericError[T], T] = Right(value)
  }

  sealed trait NumericError[T] extends BoundedNumeric[T] {

    override val validValue: Option[T] = None

    override def valueOrError: Either[NumericError[T], T] = Left(this)
  }

  sealed trait ArithmeticError[T] extends NumericError[T]

  case class Overflow[T](value: T, bounds: NumericBounds[T]) extends ArithmeticError[T]

  case class Underflow[T](value: T, bounds: NumericBounds[T]) extends ArithmeticError[T]

  sealed trait BoundsError[T] extends NumericError[T]

  case class BelowLowerBound[T](value: T, bounds: NumericBounds[T]) extends BoundsError[T]

  case class AboveUpperBound[T](value: T, bounds: NumericBounds[T]) extends BoundsError[T]

  case class InvalidNumber[T](value: T) extends NumericError[T]

  def apply[T](x: T)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import bounds._
    import numeric._
    if (x < lower) BelowLowerBound(x, bounds)
    else if (x > upper) AboveUpperBound(x, bounds)
    else if (x >= lower && x <= upper) WithinBounds(x, bounds)
    else InvalidNumber(x)
  }

  // TODO: We can make a Numeric[BoundedNumeric] if we use a fundep:
  // - encode the bounds into the type of BoundedNumeric
  // At this point it should work with the same BoundedNumeric type but to go further:
  // - implicit that creates a new BoundedNumeric type that has the larger of the two bounds
  // - implicitly convert from x and y to the new BoundedNumeric type
  // - operate on this wider BoundedNumeric.
}
