package nz.co.bottech.checkity

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.rng.Seed

object Arbitraries {

  implicit val arbSeed: Arbitrary[Seed] = Arbitrary(
    arbitrary[Long] flatMap Seed.apply
  )
}
