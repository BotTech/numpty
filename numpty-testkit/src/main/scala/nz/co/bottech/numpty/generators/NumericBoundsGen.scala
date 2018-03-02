package nz.co.bottech.numpty.generators

import nz.co.bottech.numpty.NumericBounds
import nz.co.bottech.numpty.helpers.StreamHelper
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Shrink}

object NumericBoundsGen {

  implicit def arbitraryNumericBounds[T: Arbitrary](implicit numeric: Numeric[T]): Arbitrary[NumericBounds[T]] = {
    Arbitrary {
      for {
        a <- arbitrary[T]
        b <- arbitrary[T]
      } yield NumericBounds(numeric.min(a, b), numeric.max(a, b))
    }
  }

  implicit def boundedNumericShrinker[T](implicit numeric: Numeric[T],
                                         shrinker: Shrink[T]): Shrink[NumericBounds[T]] = {
    Shrink { bounds =>
      import bounds._
      import numeric._
      if (upper >= lower) Stream.empty
      else {
        val shrinkLower = shrinker.shrink(lower).filter(_ >= upper)
        val shrinkUpper = shrinker.shrink(upper).filter(_ <= lower)
        val shrunkLower = shrinkLower.map(NumericBounds(_, upper))
        val shrunkUpper = shrinkUpper.map(NumericBounds(lower, _))
        StreamHelper.interleave(shrunkLower, shrunkUpper)
      }
    }
  }
}
