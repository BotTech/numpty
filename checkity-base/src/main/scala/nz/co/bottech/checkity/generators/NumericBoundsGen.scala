package nz.co.bottech.checkity.generators

import nz.co.bottech.checkity.NumericBounds
import nz.co.bottech.checkity.helpers.StreamHelper
import org.scalacheck.{Arbitrary, Shrink}
import org.scalacheck.Arbitrary.arbitrary

object NumericBoundsGen {

  implicit def arbitraryBoundedNumeric[T: Arbitrary](implicit num: Numeric[T]): Arbitrary[NumericBounds[T]] = {
    Arbitrary {
      for {
        a <- arbitrary[T]
        b <- arbitrary[T]
      } yield NumericBounds(num.min(a, b), num.max(a, b))
    }
  }

  implicit def boundedNumericShrinker[T](implicit shrinker: Shrink[T]): Shrink[NumericBounds[T]] = {
    Shrink { bounds =>
      import bounds._
      import bounds.num._
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
