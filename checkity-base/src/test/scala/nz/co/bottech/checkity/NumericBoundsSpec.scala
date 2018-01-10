package nz.co.bottech.checkity

import nz.co.bottech.checkity.NumericBounds.BoundedNumeric.{AboveUpperBound, BelowLowerBound}
import org.scalacheck.Gen.Choose
import org.scalacheck.{Gen, Shrink}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{EitherValues, Matchers, PropSpec}

class NumericBoundsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with EitherValues {

  import NumericBounds._

  // TODO: Use custom bounds that is the square root of the int limits to detect overflow/underflow

  // TODO: Fractional tests

  // TODO: Shared properties

  private def noShrink[T]: Shrink[T] = Shrink[T](_ => Stream.empty)

  // Shrinking obscures the real failure.
  private implicit val noShrinkDouble: Shrink[Double] = noShrink[Double]

  // The smallest number that can always be added to a double and not be lost to rounding.
  private val smallestDoubleStep = 1E292

  // The smallest number that can always be multiplied by a double and not be lost to rounding.
  private val smallestDoubleFactor = 1.0000000000000002

  property("Big Decimal bounds is unbounded") {
    BigDecimalBounds shouldBe an[UnboundedNumeric[_]]
  }

  property("Big Decimal numeric is the same instance as the implicit numeric") {
    BigDecimalBounds.num shouldBe theSameInstanceAs(implicitly[Numeric[BigDecimal]])
  }

  property("Double bounds is bounded") {
    DoubleBounds shouldBe a[BoundedNumeric[_]]
  }

  property("Double numeric is the same instance as the implicit numeric") {
    DoubleBounds.num shouldBe theSameInstanceAs(implicitly[Numeric[Double]])
  }

  property("Lower double bound is the minimum Double") {
    DoubleBounds.lower shouldBe Double.MinValue
  }

  property("Double upper bound is the maximum Double") {
    DoubleBounds.upper shouldBe Double.MaxValue
  }

  // TODO: These tests do not use infinity as an operand

  property("A large sum of doubles is positive infinity") {
    forAll(operandsOfLargeSum(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.plus(x, y).left.value shouldBe AboveUpperBound(Double.PositiveInfinity)
    }
  }

  property("A small sum of doubles is negative infinity") {
    forAll(operandsOfSmallSum(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.plus(x, y).left.value shouldBe BelowLowerBound(Double.NegativeInfinity)
    }
  }

  property("A sum of doubles within bounds is the same as double addition") {
    forAll(operandsOfSumWithinBounds[Double]) { case (x, y) =>
      DoubleBounds.plus(x, y).right.value shouldBe x + y
    }
  }

  property("A large product of doubles is positive infinity") {
    forAll(operandsOfLargeProduct[Double](smallestDoubleFactor, Math.nextUp)) { case (x, y) =>
      DoubleBounds.times(x, y).left.value shouldBe AboveUpperBound(Double.PositiveInfinity)
    }
  }

  property("A small product of doubles is negative infinity") {
    forAll(operandsOfSmallProduct[Double](smallestDoubleFactor, Math.nextUp)) { case (x, y) =>
      DoubleBounds.times(x, y).left.value shouldBe BelowLowerBound(Double.NegativeInfinity)
    }
  }

  property("A product of doubles within bounds is the same as double multiplication") {
    forAll(operandsOfProductWithinBounds[Double]) { case (x, y) =>
      DoubleBounds.times(x, y).right.value shouldBe x * y
    }
  }

  private def operandsOfLargeSum[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.chooseNum(step, upper)
      y <- Gen.chooseNum(min(upper - x + step, upper), upper)
    } yield (x, y)
  }

  private def operandsOfSmallSum[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.chooseNum(lower, -step)
      y <- Gen.chooseNum(lower, max(lower + x - step, lower))
    } yield (x, y)
  }

  private def operandsOfSumWithinBounds[T: Choose](implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    val aNonNegative = for {
      nonNegative <- Gen.chooseNum(zero, upper)
      y <- Gen.chooseNum(lower, upper - nonNegative)
    } yield (nonNegative, y)
    val aNonPositive = for {
      nonPositive <- Gen.chooseNum(lower, zero)
      y <- Gen.chooseNum(lower - nonPositive, upper)
    } yield (nonPositive, y)
    Gen.oneOf(aNonNegative, aNonPositive)
  }

  private def operandsOfLargeProduct[T: Choose](factor: T, increment: T => T)(implicit bounds: BoundedNumeric[T], fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    for {
      x <- Gen.chooseNum(factor, upper)
      y <- Gen.chooseNum(min(increment(upper / x), upper), upper)
    } yield (x, y)
  }

  private def operandsOfSmallProduct[T: Choose](factor: T, increment: T => T)(implicit bounds: BoundedNumeric[T], fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    for {
      x <- Gen.chooseNum(lower, -factor)
      y <- Gen.chooseNum(factor, max(increment(lower / x), upper))
    } yield (x, y)
  }

  private def operandsOfProductWithinBounds[T: Choose](implicit bounds: BoundedNumeric[T], fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    val aNonNegative = for {
      nonNegative <- Gen.chooseNum(zero, upper)
      y <- Gen.chooseNum(lower / nonNegative, upper / nonNegative)
    } yield (nonNegative, y)
    val aNonPositive = for {
      nonPositive <- Gen.chooseNum(lower, zero)
      y <- Gen.chooseNum(lower / nonPositive, upper / nonPositive)
    } yield (nonPositive, y)
    Gen.oneOf(aNonNegative, aNonPositive)
  }
}
