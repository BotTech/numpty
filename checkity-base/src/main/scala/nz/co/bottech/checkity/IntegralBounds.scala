package nz.co.bottech.checkity

import scala.math.Numeric.BigDecimalAsIfIntegral

sealed trait IntegralBounds[T] {

  implicit val num: Integral[T]
}

object IntegralBounds {

  trait IntegralWithoutBounds[T] extends NumericWithoutBounds[T] with IntegralBounds[T]

  trait BoundedIntegral[T] extends NumericBounds[T] with IntegralBounds[T]

  abstract class BoundedIntegralBase[T](implicit integral: Integral[T]) extends BoundedIntegral[T] {

    override val num: Integral[T] = integral
  }

  implicit object BigDecimalIntegralBounds extends IntegralWithoutBounds[BigDecimal] {

    // This is not the default Numeric for BigDecimal but it works better with NumericRange
    override val num: Integral[BigDecimal] = BigDecimalAsIfIntegral
  }

  implicit object ByteBounds extends BoundedIntegralBase[Byte] {

    override val lower: Byte = Byte.MinValue
    override val upper: Byte = Byte.MaxValue
  }

  implicit object CharBounds extends BoundedIntegralBase[Char] {

    override val lower: Char = Char.MinValue
    override val upper: Char = Char.MaxValue
  }

  implicit object IntBounds extends BoundedIntegralBase[Int] {

    override val lower: Int = Int.MinValue
    override val upper: Int = Int.MaxValue
  }

  implicit object LongBounds extends BoundedIntegralBase[Long] {

    override val lower: Long = Long.MinValue
    override val upper: Long = Long.MaxValue
  }

  implicit object ShortBounds extends BoundedIntegralBase[Short] {

    override val lower: Short = Short.MinValue
    override val upper: Short = Short.MaxValue
  }

}
