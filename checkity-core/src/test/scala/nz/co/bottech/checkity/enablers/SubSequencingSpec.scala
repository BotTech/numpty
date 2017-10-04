package nz.co.bottech.checkity.enablers

import nz.co.bottech.checkity.generators.SequenceGen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SubSequencingSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import SequenceGen._
  import SubSequencing._

  property("array contains sub-sequence in order") {
    forAll(arrayWithSub[String, Seq]) {
      case SequenceWithSub(sequence, sub) =>
        val sequencing = sequencingNatureOfArray[String]
        sequencing.containsSubSequenceInOrder(sequence, sub) shouldBe true
    }
  }
}
