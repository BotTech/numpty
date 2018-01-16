package nz.co.bottech.checkity.generators

import org.scalacheck.Gen._
import org.scalacheck.{Arbitrary, Gen}

object FloatingPointGen {

  private val validSpecials = Set(
    Double.MinValue,
    Double.MaxValue,
    0.0,
    Math.nextUp(0.0),
    Math.nextDown(0.0),
    Double.NegativeInfinity,
    Double.PositiveInfinity
  )

  private val allSpecials = validSpecials + Double.NaN

  implicit val arbDouble: Arbitrary[Double] = Arbitrary(allDoublesWith(allSpecials))

  val validDoubles: Gen[Double] = allDoublesWith(validSpecials)

  private def allDoublesWith(specials: Set[Double]): Gen[Double] = {
    val weightedSpecials = specials.toSeq.map(d => (1, const(d)))
    val other = (weightedSpecials.size, Arbitrary.arbDouble.arbitrary)
    val allGens = other +: weightedSpecials
    frequency(allGens: _*)
  }

  implicit val chooseDouble: Choose[Double] = chooseDouble(_, _)

  // Cannot include NaN
  private def chooseDouble(min: Double, max: Double, specials: Double*): Gen[Double] = {
    val basics = validSpecials + min + max
    val basicsAndSpecials = for {
      d <- (basics ++ specials).toSeq
      if d >= min && d <= max
    } yield (1, const(d))
    // Do not use infinity as the bounds for choose.
    // See https://github.com/rickynils/scalacheck/issues/379.
    val chooseMin = if (min.isNegInfinity) Double.MinValue else min
    val chooseMax = if (max.isPosInfinity) Double.MaxValue else max
    val other = (basicsAndSpecials.size, Choose.chooseDouble.choose(chooseMin, chooseMax))
    val allGens = other +: basicsAndSpecials
    frequency(allGens: _*)
  }
}
