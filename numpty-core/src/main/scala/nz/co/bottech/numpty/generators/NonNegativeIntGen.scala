package nz.co.bottech.numpty.generators

import org.scalacheck.{Arbitrary, Gen, Shrink}

import scala.math.Numeric.IntIsIntegral

object NonNegativeIntGen {

  // TODO: Find out if there is a better way to combine a generator and a shrinker together

  // TODO: Generalise this for Integrals

  implicit val arbitraryNonNegativeInts: Arbitrary[Int] = Arbitrary {
    Gen.chooseNum(0, Int.MaxValue)
  }

  implicit val nonNegativeIntShrinker: Shrink[Int] = Shrink { x =>
    def halves(x: Int): Stream[Int] = {
      val q = x / 2
      if (q == 0) Stream(0)
      else q #:: halves(q)
    }

    if (x == 0) Stream.empty else halves(x)
  }
}
