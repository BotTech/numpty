package nz.co.bottech.checkity.macros

import org.scalatest.{Matchers, WordSpec}

import scala.language.existentials

class CharacterMacrosSpec extends WordSpec with Matchers {

  "matchingChars macro" should {
    "expand into a set with all the characters matching the regular expression" in {
      val range = CharacterMacros.matchingChars("""[a-j]""")
      range should contain theSameElementsAs('a' to 'j')
    }
    "expand into all characters" in {
      val range = CharacterMacros.matchingChars(""".*""")
      range should have size 5000
    }
  }
}
