package nz.co.bottech.numpty

sealed trait NumericError

object NumericError {

  sealed trait ArithmeticError extends NumericError

  case object Overflow extends ArithmeticError

  case object Underflow extends ArithmeticError

  sealed trait BoundsError extends NumericError

  case class BelowLowerBound[T](value: T) extends BoundsError

  case class AboveUpperBound[T](value: T) extends BoundsError

  case class InvalidNumber[T](value: T) extends NumericError

}
