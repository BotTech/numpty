package nz.co.bottech.checkity.generators

import org.scalacheck.{Arbitrary, Gen, Shrink}

object NonPositiveIntGen {

  implicit val arbitraryNonPositiveInts: Arbitrary[Int] = Arbitrary {
    Gen.chooseNum(Int.MinValue, 0)
  }

  // TODO: Refactor common code from NonNegativeIntGen

  implicit val nonPositiveIntShrinker: Shrink[Int] = Shrink { x =>
    def halves(x: Int): Stream[Int] = {
      val q = x / 2
      if (q == 0) Stream(0)
      else q #:: halves(q)
    }

    if (x == 0) Stream.empty else halves(x)
  }
}
