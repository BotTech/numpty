package nz.co.bottech.numpty

import nz.co.bottech.numpty.NumericWithoutBounds._
import org.scalatest.{Matchers, PropSpec}

class NumericWithoutBoundsSpec extends PropSpec with Matchers {

  property("Big Decimal bounds is unbounded") {
    BigDecimalBounds shouldBe an[NumericWithoutBounds[_]]
  }

  property("Big Decimal numeric is the same instance as the implicit numeric") {
    BigDecimalBounds.num shouldBe theSameInstanceAs(implicitly[Numeric[BigDecimal]])
  }
}
