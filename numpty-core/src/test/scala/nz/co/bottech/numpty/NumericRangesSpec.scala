package nz.co.bottech.numpty

import nz.co.bottech.numpty.generators.NumericRangeGen
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.collection.immutable.NumericRange

class NumericRangesSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import NumericRanges._
  import NumericRangeGen._

  implicit val boundedInt: Arbitrary[Int] = Arbitrary {
    Gen.oneOf(
      Gen.chooseNum(-10, -1),
      Gen.chooseNum(1, 10)
    )
  }

  property("minimized ranges must be sorted") {
    forAll { (ranges: Seq[NumericRange[Int]]) =>
      minimize(ranges) shouldBe sorted
    }
  }

  property("minimized ranges must have no overlaps") {
    forAll { (ranges: Seq[NumericRange[Int]]) =>
      val minimizedRanges = minimize(ranges)
      val (noOverlaps, _) = minimizedRanges.foldLeft((true, None: Option[NumericRange[Int]])) {
        case (acc@(false, _), _)         => acc
        case ((_, None), next)           => (true, Some(next))
        case ((_, Some(previous)), next) => (previous.step != next.step || previous.end < next.start, Some(next))
      }
      noOverlaps shouldBe true
    }
  }

  property("empty ranges must be empty") {
    NumericRanges.empty[Int] shouldBe empty
  }

  property("new ranges must have minimized ranges") {
    forAll { (ranges: Seq[NumericRange[Int]]) =>
      NumericRanges(ranges).ranges should contain theSameElementsInOrderAs minimize(ranges)
    }
  }

  property("new ranges must be balanced") {
    forAll { (ranges: Seq[NumericRange[Int]]) =>
      val tree = NumericRanges(ranges).tree
      val isBalanced = tree.balanced
      println(ranges)
      println(tree)
      println(s"isBalanced: $isBalanced")
      isBalanced shouldBe true
    }
  }
}
