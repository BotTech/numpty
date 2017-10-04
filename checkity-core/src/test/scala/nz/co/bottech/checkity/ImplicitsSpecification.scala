package nz.co.bottech.checkity

import nz.co.bottech.checkity.Hexadecimal._
import nz.co.bottech.checkity.matchers.HexadecimalMatchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ImplicitsSpecification extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with HexadecimalMatchers {

  import Implicits._

  property("codePoint should have four characters") {
    forAll { (c: Char) =>
      c.codePoint should have length 4
    }
  }

  property("codePoint can only contain hexadecimal characters") {
    forAll { (c: Char) =>
      all(c.codePoint) shouldBe hexChar
    }
  }

  property("codePoint can be converted back to the decimal value") {
    forAll { (c: Char) =>
      c.toInt shouldBe hexToInt(c.codePoint)
    }
  }

  property("escape must start with '\\u'") {
    forAll { (c: Char) =>
      c.escape should startWith("\\u")
    }
  }

  property("escape must end with the codePoint") {
    forAll { (c: Char) =>
      c.escape should endWith(c.codePoint)
    }
  }

  property("escape length must be the code point length plus two") {
    forAll { (c: Char) =>
      c.escape should have length (c.codePoint.length + 2)
    }
  }

  property("""literal must start with '""") {
    forAll { (c: Char) =>
      c.literal should startWith("""'""")
    }
  }

  property("""literal must end with '""") {
    forAll { (c: Char) =>
      c.literal should endWith("""'""")
    }
  }

  property("test 1") {
    "abcd" should contain inOrder('a', 'b')
  }

  property("test 2") {
    "abcd" should contain inOrder('b', 'c')
  }

  property("test 3") {
    "abcd" should not contain inOrder('a', 'c')
  }

  property("test 4") {
    // Is this a bug?
    "abcdb" should contain inOrder('b', 'c')
  }

  property("literal must contain the escape") {
    forAll { (c: Char) =>
      c.literal should contain inOrderElementsOf c.escape
    }
  }

  property("literal length must be the escape length plus two") {
    forAll { (c: Char) =>
      c.literal should have length (c.escape.length + 2)
    }
  }

}
