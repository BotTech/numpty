package nz.co.bottech.checkity

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.util.matching.Regex

class RegexImplicitsSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import RegexImplicits._

  property("should match itself literally") {
    forAll { (str: String) =>
      Regex.quote(str).r.matches(str) shouldBe true
    }
  }

  property("should not match itself with anything prepended") {
    forAll { (str: String, prepended: String) =>
      whenever(prepended.nonEmpty) {
        Regex.quote(prepended + str).r.matches(str) shouldBe false
      }
    }
  }

  property("should not match itself with anything appended") {
    forAll { (str: String, appended: String) =>
      whenever(appended.nonEmpty) {
        Regex.quote(str + appended).r.matches(str) shouldBe false
      }
    }
  }
}
