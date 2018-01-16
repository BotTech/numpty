package nz.co.bottech.checkity

import nz.co.bottech.checkity.NumericBounds.BoundedNumeric._
import nz.co.bottech.checkity.NumericBounds.{BoundedNumeric, UnboundedNumeric}
import org.scalactic.Requirements._

import scala.annotation.tailrec
import scala.math.Numeric.{BigDecimalAsIfIntegral, BigDecimalIsFractional, BigIntIsIntegral, ByteIsIntegral, CharIsIntegral, DoubleIsFractional, FloatIsFractional, IntIsIntegral, LongIsIntegral, ShortIsIntegral}

// TODO: Make this a real typeclass and get rid of the unbounded numeric
sealed trait NumericBounds[T] {

  implicit val num: Numeric[T]
}

object NumericBounds {

  trait BoundedNumeric[T] extends NumericBounds[T] {

    def lower: T

    def upper: T

    import num._

    def plus(x: T, y: T): Either[NumericError, T] = {
      val result = x + y
      if (signum(y) > 0) checkIncrement(x, y, result)
      else checkDecrement(x, y, result)
    }

    def minus(x: T, y: T): Either[NumericError, T] = {
      val result = x - y
      if (signum(y) < 0) checkIncrement(x, y, result)
      else checkDecrement(x, y, result)
    }

    def times(x: T, y: T): Either[NumericError, T] = {
      def fractional(z: T): Boolean = {
        if (signum(z) >= 0) z <= one
        else z >= -one
      }

      def countWithSmallPosStep: Either[NumericError, (T, T)] = {
        if (signum(x) >= 0) {
          if (signum(y) >= 0) {
            if (x > y) Right(y, x) else Right(x, y)
          } else {
            for {
              negX <- negate(x)
              posY <- negate(y)
            } yield if (x > posY) (y, x) else (negX, posY)
          }
        } else {
          if (signum(y) >= 0) {
            for {
              posX <- negate(x)
              negY <- negate(y)
            } yield if (posX > y) (negY, posX) else (x, y)
          } else {
            for {
              posX <- negate(x)
              posY <- negate(y)
            } yield if (posX > posY) (posY, posX) else (posX, posY)
          }
        }
      }

      @tailrec
      def doubles(step: T, count: T, previous: List[(T, T)], end: T): Either[NumericError, List[(T, T)]] = {
        assert(count <= end)
        if (count == end) Right(previous)
        else plus(count, count) match {
          case Right(doubleCount) if doubleCount <= end => plus(step, step) match {
            case Left(error)       => Left(error)
            case Right(doubleStep) => doubles(doubleStep, doubleCount, (doubleStep, doubleCount) :: previous, end)
          }
          case _                                        => Right(previous)
        }
      }

      @tailrec
      def addDoubles(acc: T, num: T, dbls: List[(T, T)], end: T, firstStep: T): Either[NumericError, T] = {
        assert(num <= end)
        if (num == end) Right(acc)
        else dbls match {
          case (step, count) :: tail => plus(num, count) match {
            case Right(nextNum) if nextNum <= end => plus(acc, step) match {
              case Left(error)    => Left(error)
              case Right(nextAcc) => addDoubles(nextAcc, nextNum, tail, end, firstStep)
            }
            case _                                => addDoubles(acc, num, tail, end, firstStep)
          }
          case _                     => minus(end, num).flatMap { remaining =>
            assert(remaining < one)
            assert(remaining > -one)
            plus(firstStep * remaining, acc)
          }
        }
      }

      if (fractional(x) || fractional(y)) checkBounds(x * y)
      else for {
        countAndStep <- countWithSmallPosStep
        (count, step) = countAndStep
        dbls <- doubles(step, one, (step, one) :: Nil, count)
        total <- addDoubles(zero, zero, dbls, count, step)
      } yield total
    }

    def negate(x: T): Either[NumericError, T] = minus(zero, x)

    def fromInt(x: Int): Either[NumericError, T] = checkBounds(num.fromInt(x))

    def toInt(x: T): Int = num.toInt(x)

    def toLong(x: T): Long = num.toLong(x)

    def toFloat(x: T): Float = num.toFloat(x)

    def toDouble(x: T): Double = num.toDouble(x)

    protected def checkIncrement(x: T, y: T, result: T): Either[NumericError, T] = {
      if (result < x) Left(Overflow)
      else checkBounds(result)
    }

    protected def checkDecrement(x: T, y: T, result: T): Either[NumericError, T] = {
      if (result > x) Left(Underflow)
      else checkBounds(result)
    }

    protected def checkBounds(x: T): Either[NumericError, T] = {
      if (x < lower) Left(BelowLowerBound(x))
      else if (x > upper) Left(AboveUpperBound(x))
      else if (x >= lower && x <= upper) Right(x)
      else Left(InvalidNumber(x))
    }

    override final def equals(obj: Any): Boolean = obj match {
      case other: BoundedNumeric[T] => equiv(lower, other.lower) && equiv(upper, other.upper)
      case _                        => false
    }

    override def toString: String = s"BoundedNumeric(lower = $lower, upper = $upper)"
  }

  object BoundedNumeric {

    def apply[T](low: T, high: T)(implicit numeric: Numeric[T]): BoundedNumeric[T] = {
      import Ordered._
      require(low <= high)
      new BoundedNumeric[T] {
        override implicit val num: Numeric[T] = numeric
        override val lower: T = low
        override val upper: T = high
      }
    }

    sealed trait NumericError

    sealed trait ArithmeticError extends NumericError

    case object Overflow extends ArithmeticError

    case object Underflow extends ArithmeticError

    sealed trait BoundsError extends NumericError

    case class BelowLowerBound[T](value: T) extends BoundsError

    case class AboveUpperBound[T](value: T) extends BoundsError

    case class InvalidNumber[T](value: T) extends NumericError

  }

  trait UnboundedNumeric[T] extends NumericBounds[T]

  implicit object BigDecimalBounds extends UnboundedNumeric[BigDecimal] {

    override implicit val num: Numeric[BigDecimal] = BigDecimalIsFractional
  }

  implicit object DoubleBounds extends BoundedNumeric[Double] with ApproachesInfinity[Double] {

    override val lower: Double = Double.MinValue
    override val upper: Double = Double.MaxValue
    override implicit val num: Numeric[Double] = DoubleIsFractional

    override protected val positiveInfinity: Double = Double.PositiveInfinity
    override protected val negativeInfinity: Double = Double.NegativeInfinity
  }

  implicit object FloatBounds extends BoundedNumeric[Float] with ApproachesInfinity[Float] {

    override val lower: Float = Float.MinValue
    override val upper: Float = Float.MaxValue
    override implicit val num: Numeric[Float] = FloatIsFractional

    override protected val positiveInfinity: Float = Float.PositiveInfinity
    override protected val negativeInfinity: Float = Float.NegativeInfinity
  }

  trait ApproachesInfinity[T] {
    this: BoundedNumeric[T] =>

    protected def positiveInfinity: T

    protected def negativeInfinity: T

    override def times(x: T, y: T): Either[NumericError, T] = checkResult(x, y, num.times)

    protected def checkResult(x: T, y: T, operation: (T, T) => T): Either[NumericError, T] = {
      val result = operation(x, y)
      if (result == positiveInfinity) Left(AboveUpperBound(result))
      else if (result == negativeInfinity) Left(BelowLowerBound(result))
      else checkBounds(result)
    }
  }

}

trait IntegralBounds[T] extends NumericBounds[T] {

  override implicit val num: Integral[T]
}

object IntegralBounds {

  trait BoundedIntegral[T] extends BoundedNumeric[T] with IntegralBounds[T]

  implicit object BigDecimalBounds extends UnboundedNumeric[BigDecimal] {

    // Note that this is not the default Numeric for BigDecimal but it works better with NumericRange
    override implicit val num: Numeric[BigDecimal] = BigDecimalAsIfIntegral
  }

  implicit object BigIntBounds extends UnboundedNumeric[BigInt] {

    override implicit val num: Numeric[BigInt] = BigIntIsIntegral
  }

  implicit object ByteBounds extends BoundedIntegral[Byte] {

    override val lower: Byte = Byte.MinValue
    override val upper: Byte = Byte.MaxValue
    override implicit val num: Integral[Byte] = ByteIsIntegral
  }

  implicit object CharBounds extends BoundedIntegral[Char] {

    override val lower: Char = Char.MinValue
    override val upper: Char = Char.MaxValue
    override implicit val num: Integral[Char] = CharIsIntegral
  }

  implicit object IntBounds extends BoundedIntegral[Int] {

    override val lower: Int = Int.MinValue
    override val upper: Int = Int.MaxValue
    override implicit val num: Integral[Int] = IntIsIntegral
  }

  implicit object LongBounds extends BoundedIntegral[Long] {

    override val lower: Long = Long.MinValue
    override val upper: Long = Long.MaxValue
    override implicit val num: Integral[Long] = LongIsIntegral
  }

  implicit object ShortBounds extends BoundedIntegral[Short] {

    override val lower: Short = Short.MinValue
    override val upper: Short = Short.MaxValue
    override implicit val num: Integral[Short] = ShortIsIntegral
  }

  case class PositiveBigDecimal(value: BigDecimal) {
  }

}