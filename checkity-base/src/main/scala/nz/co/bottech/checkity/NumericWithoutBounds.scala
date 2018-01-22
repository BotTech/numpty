package nz.co.bottech.checkity

trait NumericWithoutBounds[T] {

  implicit val num: Numeric[T]
}

object NumericWithoutBounds {

  abstract class NumericWithoutBoundsBase[T](implicit numeric: Numeric[T]) extends NumericWithoutBounds[T] {

    override implicit val num: Numeric[T] = numeric
  }

  implicit object BigDecimalBounds extends NumericWithoutBoundsBase[BigDecimal]

  implicit object BigIntBoundsBase extends NumericWithoutBoundsBase[BigInt]
}
