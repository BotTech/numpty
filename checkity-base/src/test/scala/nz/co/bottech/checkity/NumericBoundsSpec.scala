package nz.co.bottech.checkity

import nz.co.bottech.checkity.NumericBounds.BoundedNumeric.{AboveUpperBound, BelowLowerBound, InvalidNumber, NumericError}
import nz.co.bottech.checkity.generators.FloatingPointGen
import org.scalacheck.Gen.Choose
import org.scalacheck.{Gen, Shrink}
import org.scalactic.Equality
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.reflect.ClassTag

class NumericBoundsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import FloatingPointGen._
  import NumericBounds._

  private def noShrink[T]: Shrink[T] = Shrink[T](_ => Stream.empty)

  // Shrinking obscures the real failure.
  private implicit val noShrinkDouble: Shrink[Double] = noShrink[Double]

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 100000)

  private implicit val doubleEquality: Equality[Double] = {
    case (x: Double, y: Double) => x == y || (x.isNaN && y.isNaN)
    case _ => false
  }

  private implicit val doubleResultEquality: Equality[Either[NumericError, Double]] = sameResult(_, _)

  // The smallest number that can always be added to a double and not be lost to rounding.
  private val smallestDoubleStep = 1E292

  // The smallest number larger than one that can always be multiplied to a double and not be lost to rounding.
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

  property("Lower double bound is the minimum value") {
    DoubleBounds.lower shouldBe Double.MinValue
  }

  property("Double upper bound is the maximum value") {
    DoubleBounds.upper shouldBe Double.MaxValue
  }

  property("Double plus is symmetric") {
    forAll { (x: Double, y: Double) =>
      DoubleBounds.plus(x, y) shouldEqual DoubleBounds.plus(y, x)
    }
  }

  property("Double positive infinity plus a valid number is positive infinity") {
    forAll(Gen.choose(Double.MinValue, Double.PositiveInfinity)) { x =>
      DoubleBounds.plus(Double.PositiveInfinity, x) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("Double negative infinity plus a valid number is positive infinity") {
    forAll(Gen.choose(Double.NegativeInfinity, Double.MaxValue)) { x =>
      DoubleBounds.plus(Double.NegativeInfinity, x) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("Double positive infinity plus a negative infinity is not a number") {
    DoubleBounds.plus(Double.PositiveInfinity, Double.NegativeInfinity) shouldEqual Left(InvalidNumber(Double.NaN))
  }

  property("Double not a number plus any number is not a number") {
    forAll { x: Double =>
      DoubleBounds.plus(Double.NaN, x) shouldEqual Left(InvalidNumber(Double.NaN))
    }
  }
  
  property("A large sum of doubles is positive infinity") {
    forAll(operandsOfLargeSum(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.plus(x, y) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("A small sum of doubles is negative infinity") {
    forAll(operandsOfSmallSum(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.plus(x, y) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("A sum of doubles within bounds is the same as addition") {
    forAll(operandsOfSumWithinBounds(nextToZero)) { case (x, y) =>
      DoubleBounds.plus(x, y) shouldBe Right(x + y)
    }
  }

  property("Double positive infinity minus a valid number is positive infinity") {
    forAll(Gen.choose(Double.NegativeInfinity, Double.MaxValue)) { x =>
      DoubleBounds.minus(Double.PositiveInfinity, x) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("A valid double minus positive infinity is negative infinity") {
    forAll(Gen.choose(Double.NegativeInfinity, Double.MaxValue)) { x =>
      DoubleBounds.minus(x, Double.PositiveInfinity) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("Double positive infinity minus positive infinity is not a valid number") {
    DoubleBounds.minus(Double.PositiveInfinity, Double.PositiveInfinity) shouldEqual Left(InvalidNumber(Double.NaN))
  }

  property("Double negative infinity minus a valid number is negative infinity") {
    forAll(Gen.choose(Double.MinValue, Double.PositiveInfinity)) { x =>
      DoubleBounds.minus(Double.NegativeInfinity, x) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("A valid double minus negative infinity is negative infinity") {
    forAll(Gen.choose(Double.MinValue, Double.PositiveInfinity)) { x =>
      DoubleBounds.minus(x, Double.NegativeInfinity) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("Double negative infinity minus negative infinity is not a number") {
    DoubleBounds.minus(Double.NegativeInfinity, Double.NegativeInfinity) shouldEqual Left(InvalidNumber(Double.NaN))
  }

  property("Any double minus not a number is not a number") {
    forAll { x: Double =>
      DoubleBounds.minus(x, Double.NaN) shouldEqual Left(InvalidNumber(Double.NaN))
    }
  }

  property("Double not a number minus a double is not a number") {
    forAll { x: Double =>
      DoubleBounds.minus(Double.NaN, x) shouldEqual Left(InvalidNumber(Double.NaN))
    }
  }

  property("A large difference of doubles is positive infinity") {
    forAll(operandsOfLargeDifference(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.minus(x, y) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("A small difference of doubles is negative infinity") {
    forAll(operandsOfSmallDifference(smallestDoubleStep)) { case (x, y) =>
      DoubleBounds.minus(x, y) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("A difference of doubles within bounds is the same as subtraction") {
    forAll(operandsOfDifferenceWithinBounds(nextToZero)) { case (x, y) =>
      DoubleBounds.minus(x, y) shouldBe Right(x - y)
    }
  }

  property("Double times is symmetric") {
    forAll { (x: Double, y: Double) =>
      DoubleBounds.times(x, y) shouldEqual DoubleBounds.times(y, x)
    }
  }

  property("Double positive infinity times a positive number is positive infinity") {
    forAll(Gen.choose(Double.MinPositiveValue, Double.PositiveInfinity)) { x =>
      DoubleBounds.times(Double.PositiveInfinity, x) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("Double positive infinity times zero is not a number") {
    DoubleBounds.times(Double.PositiveInfinity, 0) shouldEqual Left(InvalidNumber(Double.NaN))
  }

  property("Double positive infinity times a negative number is negative infinity") {
    forAll(Gen.choose(Double.NegativeInfinity, -Double.MinPositiveValue)) { x =>
      DoubleBounds.times(Double.PositiveInfinity, x) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("Double negative infinity times a negative number is positive infinity") {
    forAll(Gen.choose(Double.NegativeInfinity, -Double.MinPositiveValue)) { x =>
      DoubleBounds.times(Double.NegativeInfinity, x) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("Double negative infinity times zero is not a number") {
    DoubleBounds.times(Double.NegativeInfinity, 0) shouldEqual Left(InvalidNumber(Double.NaN))
  }

  property("Double negative infinity times a positive number is negative infinity") {
    forAll(Gen.choose(Double.MinPositiveValue, Double.PositiveInfinity)) { x =>
      DoubleBounds.times(Double.NegativeInfinity, x) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("Any double times not a number is not a number") {
    forAll { x: Double =>
      DoubleBounds.times(x, Double.NaN) shouldEqual Left(InvalidNumber(Double.NaN))
    }
  }

  property("A large product of doubles is positive infinity") {
    forAll(operandsOfLargeProduct(smallestDoubleFactor)(Math.nextUp)) { case (x, y) =>
      DoubleBounds.times(x, y) shouldBe Left(AboveUpperBound(Double.PositiveInfinity))
    }
  }

  property("A small product of doubles is negative infinity") {
    forAll(operandsOfSmallProduct(smallestDoubleFactor)(Math.nextUp)) { case (x, y) =>
      DoubleBounds.times(x, y) shouldBe Left(BelowLowerBound(Double.NegativeInfinity))
    }
  }

  property("A product of doubles within bounds is the same as double multiplication") {
    forAll(operandsOfProductWithinBounds(nextToZero)) { case (x, y) =>
      DoubleBounds.times(x, y) shouldBe Right(x * y)
    }
  }

  private def sameResult[T: ClassTag](x: Either[NumericError, T], y: Any)(implicit eq: Equality[T]): Boolean = y match {
    case Left(InvalidNumber(yNum)) => x match {
      case Left(InvalidNumber(xNum: T)) => eq.areEqual(xNum, yNum)
      case _ => false
    }
    case _ => x == y
  }

  private def operandsOfLargeSum[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.choose(step, upper)
      y <- Gen.choose(min(upper - x + step, upper), upper)
    } yield (x, y)
  }

  private def operandsOfSmallSum[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.choose(lower, -step)
      y <- Gen.choose(lower, max(lower + x - step, lower))
    } yield (x, y)
  }

  private def operandsOfSumWithinBounds[T: Choose](nextToZero: T => T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    val aNonNegative = for {
      nonNegative <- Gen.choose(zero, upper)
      y <- Gen.choose(lower, nextToZero(upper - nonNegative))
    } yield (nonNegative, y)
    val aNonPositive = for {
      nonPositive <- Gen.choose(lower, zero)
      y <- Gen.choose(lower - nonPositive, upper)
    } yield (nonPositive, y)
    Gen.oneOf(aNonNegative, aNonPositive)
  }

  private def operandsOfLargeDifference[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.choose(step, upper)
      y <- Gen.choose(lower, x - upper - step)
    } yield (x, y)
  }

  private def operandsOfSmallDifference[T: Choose](step: T)(implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    for {
      x <- Gen.choose(lower, -step)
      y <- Gen.choose(x - lower + step, upper)
    } yield (x, y)
  }

  private def operandsOfDifferenceWithinBounds[T: Choose](nextToZero: T => T)
                                                         (implicit bounds: BoundedNumeric[T]): Gen[(T, T)] = {
    import bounds._
    import num._
    val aNonNegative = for {
      nonNegative <- Gen.choose(zero, upper)
      y <- Gen.choose(nextToZero(lower + nonNegative), upper)
    } yield (nonNegative, y)
    val aNonPositive = for {
      nonPositive <- Gen.choose(lower, zero)
      y <- Gen.choose(lower, upper + nonPositive)
    } yield (nonPositive, y)
    Gen.oneOf(aNonNegative, aNonPositive)
  }

  private def operandsOfLargeProduct[T: Choose](factor: T)(increment: T => T)
                                               (implicit bounds: BoundedNumeric[T],
                                                fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    for {
      x <- Gen.choose(factor, upper)
      y <- Gen.choose(min(increment(upper / x), upper), upper)
    } yield (x, y)
  }

  private def operandsOfSmallProduct[T: Choose](factor: T)(increment: T => T)
                                               (implicit bounds: BoundedNumeric[T],
                                                fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    for {
      x <- Gen.choose(lower, -factor)
      y <- Gen.choose(min(increment(lower / x), upper), upper)
    } yield (x, y)
  }

  private def operandsOfProductWithinBounds[T: Choose](shrink: T => T)
                                                      (implicit bounds: BoundedNumeric[T],
                                                       fractional: Fractional[T]): Gen[(T, T)] = {
    import bounds._
    import fractional._
    val aPositive = for {
      positive <- Gen.choose(one, upper)
      y <- Gen.choose(shrink(lower / positive), shrink(upper / positive))
    } yield (positive, y)
    val aNegative = for {
      negative <- Gen.choose(lower, -one)
      yHigh = shrink(lower / negative)
      y <- Gen.choose(shrink(upper / negative), if (yHigh > zero) yHigh else upper)
    } yield (negative, -y)
    val aNonNegativeFraction = for {
      fraction <- Gen.choose(zero, one)
      y <- Gen.choose(shrink(lower * fraction), shrink(upper * fraction))
    } yield (fraction, y)
    val aNonPositiveFraction = for {
      fraction <- Gen.choose(-one, zero)
      y <- Gen.choose(shrink(upper * fraction), shrink(lower * fraction))
    } yield (fraction, -y)
    Gen.oneOf(aPositive, aNegative, aNonNegativeFraction, aNonPositiveFraction)
  }

  private def nextToZero(x: Double): Double = {
    if (x < 0) Math.nextUp(x)
    else if (x > 0) Math.nextDown(x)
    else x
  }
}
