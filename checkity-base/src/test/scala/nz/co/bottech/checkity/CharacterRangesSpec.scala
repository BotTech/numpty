package nz.co.bottech.checkity

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CharacterRangesSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import CharacterRanges._

  property("unicode contains all valid characters") {
    forAll { (c: Char) =>
      unicodeCharacters should contain(c)
    }
  }

  private val invalidUnicodeCharacters = Gen.oneOf(('\uD800' until '\uE000') ++ Seq('\uFFFE', '\uFFFF'))

  property("unicode excludes all invalid characters") {
    forAll(invalidUnicodeCharacters) { (c: Char) =>
      unicodeCharacters should not contain c
    }
  }
}
