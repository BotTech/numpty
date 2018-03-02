package nz.co.bottech.numpty

import nz.co.bottech.numpty.NumericBounds._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class NumericBoundsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("Double bounds is bounded") {
    DoubleBounds shouldBe a[NumericBounds[_]]
  }

  property("Lower double bound is the minimum value") {
    DoubleBounds.lower shouldBe Double.MinValue
  }

  property("Double upper bound is the maximum value") {
    DoubleBounds.upper shouldBe Double.MaxValue
  }

  // TODO: Use discipline to test the laws of equality here.
  // Reflexive
  // Symmetric
  // Transitive
  // Consistent
  property("Double bounds is equal to itself") {
    DoubleBounds shouldBe DoubleBounds
  }

  property("Double bounds is not equal to another bounds with a different type") {
    DoubleBounds should not be IntBounds
  }

  property("Double bounds is not equal to something else") {
    DoubleBounds should not be ((DoubleBounds.lower, DoubleBounds.upper))
  }

  property("Double bounds should print as numeric bounds") {
    DoubleBounds.toString shouldBe s"NumericBounds(lower = ${DoubleBounds.lower}, upper = ${DoubleBounds.upper})"
  }
}
