package nz.co.bottech.numpty.generators

import org.scalacheck.{Arbitrary, Gen, Shrink}

object PositiveIntGen {

  implicit val arbitraryNegativeInts: Arbitrary[Int] = Arbitrary {
    Gen.chooseNum(1, Int.MaxValue)
  }

  implicit val positiveIntShrinker: Shrink[Int] = Shrink { x =>
    def halves(x: Int): Stream[Int] = {
      val q = x / 2
      if (q <= 1) Stream(1)
      else q #:: halves(q)
    }

    if (x <= 1) Stream.empty else halves(x)
  }
}
