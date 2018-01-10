package nz.co.bottech.checkity.generators

import org.scalacheck.{Arbitrary, Gen, Shrink}

object NegativeIntGen {

  implicit val arbitraryNegativeInts: Arbitrary[Int] = Arbitrary {
    Gen.chooseNum(Int.MinValue, -1)
  }

  implicit val negativeIntShrinker: Shrink[Int] = Shrink { x =>
    def halves(x: Int): Stream[Int] = {
      val q = x / 2
      if (q >= -1) Stream(-1)
      else q #:: halves(q)
    }

    if (x >= -1) Stream.empty else halves(x)
  }
}
