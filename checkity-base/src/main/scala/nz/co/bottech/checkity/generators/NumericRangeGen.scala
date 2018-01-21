package nz.co.bottech.checkity.generators

import nz.co.bottech.checkity.IntegralBounds
import nz.co.bottech.checkity.IntegralBounds.{BoundedIntegral, UnboundedIntegral}
import nz.co.bottech.checkity.NumericBounds._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.collection.immutable.NumericRange

object NumericRangeGen {

  implicit def arbitraryNumericRanges[T: Arbitrary : Choose](implicit precision: IntegralBounds[T]): Arbitrary[NumericRange[T]] = {
    import precision.num
    precision match {
      case _: UnboundedIntegral[_]     => arbitraryUnboundedNumericRanges[T]
      case bounded: BoundedIntegral[T] =>
        implicit val boundedPrecision: BoundedIntegral[T] = bounded
        arbitraryBoundedNumericRanges[T]
    }
  }

  private def arbitraryUnboundedNumericRanges[T: Arbitrary : Choose](implicit integral: Integral[T]): Arbitrary[NumericRange[T]] = {
    Arbitrary {
      import integral._
      Gen.sized { size =>
        for {
          start <- arbitrary[T]
          step <- arbitrary[T].filter(_ != zero)
          inclusive <- arbitrary[Boolean]
          end <- chooseEnd(fromInt(size), start, step, inclusive)
        } yield {
          val range = if (inclusive) NumericRange.inclusive(start, end, step)
          else NumericRange(start, end, step)
          range
        }
      }
    }
  }

  private def arbitraryBoundedNumericRanges[T: Arbitrary : Choose](implicit precision: BoundedIntegral[T]): Arbitrary[NumericRange[T]] = {
    Arbitrary {
      import precision._
      import num._
      Gen.sized { size =>
        val numSize = num.fromInt(size)
        for {
          inclusive <- arbitrary[Boolean]
          step <- chooseStep(numSize)
          start <- chooseStart(numSize, inclusive)
          numberOfSteps = num.fromInt(size)
          oneSmaller = if (signum(step) < 0) -one else one
          (lower, upper) = if (inclusive) {
            (start + step * (numberOfSteps - one), start + step * numberOfSteps - oneSmaller)
          } else {
            (start + step * (numberOfSteps - one) + oneSmaller, start + step * numberOfSteps)
          }
          end <- Gen.chooseNum(min(lower, upper), max(lower, upper))
        } yield {
          val range = if (inclusive) NumericRange.inclusive(start, end, step)
          else NumericRange(start, end, step)
          range
        }
      }
    }
  }

  private[checkity] def chooseStep[T: Choose](size: T)(implicit bounds: BoundedNumeric[T]) = {
    import bounds._
    import num._
    // TODO: signum of zero is zero
    if (equiv(size, one) || signum(lower) == signum(upper)) {
      Gen.chooseNum(lower, upper)
    } else {
      // TODO: Extract this logic and test on its own
      // What we know:
      // - Size is greater than one
      // - Lower is negative
      // - Upper is zero or positive
      // - There is a zero
      // - We can subtract anything that has the same sign and is the same size or smaller
      // - We can add anything that has the same sign and is at most half the bound
      // - Since there is a zero any other addition or subtraction is guaranteed to
      //   overflow/underflow at most once
      // - If addition of two positive numbers results in overflow then the result will
      //   be less than (or equal) to both numbers. The opposite is true for subtraction.
      // - This means that we can reliably detect overflow/underflow
      // - Negation of any number other than zero can overflow/underflow but if it does it
      //   will either have the wrong sign or be outside the bounds (if the bounds are different
      //   from the bounds of the underlying value)
      ???
      //      val lowerStepSize = lower / size // At most half lower bound
      //      val upperStepSize = upper / size // At most half upper bound
      //      val negatedLowerStepSize = negateNegative(lowerStepSize)
      //      val negatedUpperStepSize = negatePositive(upperStepSize)
      //      // TODO: What about the remainder?
      //      val oneStepUp = lower - lowerStepSize
      //      val oneStepDown = upper - upperStepSize
      //      val minStep = lowerStepSize + max(oneStepUp, negate(upperStepSize))
      //      val maxStep = upperStepSize + min(abs(lowerStepSize), oneStepDown)
      // TODO: overflow/underflow
      //      val minStep = lowerStepSize + negatedUpperStepSize
      //      val maxStep = upperStepSize + negatedLowerStepSize
      //      Gen.chooseNum(minStep, maxStep)
    }
  }

  private def chooseStart[T: Choose](size: T, inclusive: Boolean)(implicit bounds: BoundedNumeric[T]) = {
    import bounds._
    import num._
    val max = if (inclusive) upper - size + one else upper - size
    Gen.chooseNum(lower, max)
  }

  private def chooseEnd[T: Choose](size: T, start: T, step: T, inclusive: Boolean)(implicit num: Numeric[T]): Gen[T] = {
    import num._
    val oneSmaller = if (signum(step) < 0) -one else one
    val (lower, upper) = if (inclusive) {
      (start + step * (size - one), start + step * size - oneSmaller)
    } else {
      (start + step * (size - one) + oneSmaller, start + step * size)
    }
    Gen.chooseNum(min(lower, upper), max(lower, upper))
  }

  implicit def numericRangeShrinker[T](implicit num: Numeric[T]): Shrink[NumericRange[T]] = Shrink { range =>
    if (range.isEmpty) Stream.empty
    else {
      import num._
      import range._
      val nextStep = if (signum(step) > 0) step + one else step - one
      copy(start + step, end, step) #::
        copy(start, end - step, step) #::
        copy(start, end, nextStep) #::
        Stream.empty[NumericRange[T]]
    }
  }
}
