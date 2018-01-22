package nz.co.bottech.checkity

import nz.co.bottech.checkity.NumericWithoutBounds._
import org.scalatest.{Matchers, PropSpec}

class NumericWithoutBoundsSpec extends PropSpec with Matchers {

  property("Big Decimal bounds is unbounded") {
    BigDecimalBounds shouldBe an[NumericWithoutBounds[_]]
  }

  property("Big Decimal numeric is the same instance as the implicit numeric") {
    BigDecimalBounds.num shouldBe theSameInstanceAs(implicitly[Numeric[BigDecimal]])
  }
}
