package nz.co.bottech.checkity.generators

import nz.co.bottech.checkity.NumericBounds.BoundedNumeric
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, OptionValues, PropSpec}

import scala.collection.immutable.NumericRange

class NumericRangeGenSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with OptionValues {

  import NumericBoundsGen._
  import NumericRangeGen._

  property("step must be within bounds") {
    forAll { (size: Int, bounds: BoundedNumeric[Int]) =>
      whenever(size > 0) {
        implicit val boundedPrecision: BoundedNumeric[Int] = bounds
        val stepGenerator = chooseStep[Int](size)
        stepGenerator.sample.value should (be >= bounds.lower and be <= bounds.upper)
      }
    }
  }

  property("step must not be zero") {
    forAll { (size: Int, bounds: BoundedNumeric[Int]) =>
      whenever(size > 0) {
        implicit val boundedPrecision: BoundedNumeric[Int] = bounds
        val stepGenerator = chooseStep[Int](size)
        stepGenerator.sample.value should not be 0
      }
    }
  }

  property("step must not be zero 2") {
    implicit val boundedPrecision: BoundedNumeric[Int] = BoundedNumeric(-2050164641, 1)
    val stepGenerator = chooseStep[Int](3)
    stepGenerator.sample.value should not be 0
  }

  property("step should account for bounds and desired size when the size is within the bounds") {
    forAll { (desiredSize: Int, bounds: BoundedNumeric[Int]) =>
      whenever(desiredSize > 0 && withinBounds(desiredSize, bounds)) {
        implicit val boundedPrecision: BoundedNumeric[Int] = bounds
        val stepGenerator = chooseStep[Int](desiredSize)
        val step = stepGenerator.sample.value
        val lowerSize = math.abs(bounds.lower / step)
        val lowerRem = math.abs(bounds.lower % step)
        val upperSize = math.abs(bounds.upper / step)
        val upperRem = math.abs(bounds.upper % step)
        val remSize = (lowerRem + upperRem + 1) / step
        val totalSize = lowerSize + upperSize + remSize
        desiredSize should be <= totalSize
      }
    }
  }

  property("step should one when the size is outside of the bounds") {
    forAll { (desiredSize: Int, bounds: BoundedNumeric[Int]) =>
      whenever(desiredSize > 0 && !withinBounds(desiredSize, bounds)) {
        implicit val boundedPrecision: BoundedNumeric[Int] = bounds
        val stepGenerator = chooseStep[Int](desiredSize)
        val step = stepGenerator.sample.value
        step shouldBe 1
      }
    }
  }

  property("ranges must be the same size as the generator") {
    // FIXME
    forAll { (sizeX: Int) =>
      val size = 1
      val rangeGenerator = arbitraryNumericRanges[Int].arbitrary
      val resized = Gen.resize(size, rangeGenerator)
      resized.sample match {
        case Some(range) => {
          if (range.length != size) {
            println(s"start = ${range.start}, end = ${range.end}, step = ${range.step}")
          }
          range should have length size
        }
        case None        => size shouldBe 0
      }
    }
  }

  property("ranges must shrink in size") {
    forAll { (range: NumericRange[Int]) =>
      val shrinker = numericRangeShrinker[Int]
      shrinker.shrink(range).foreach { shrunk =>
        shrunk.length should be < range.length
      }
    }
  }

  def withinBounds(x: Int, bounds: BoundedNumeric[Int]): Boolean = {
    import bounds._
    if (math.signum(lower) == math.signum(upper)) x <= upper - lower + 1
    else x <= upper || lower <= x - upper - 1
  }
}
