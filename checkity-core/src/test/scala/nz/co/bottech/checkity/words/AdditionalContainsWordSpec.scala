package nz.co.bottech.checkity.words

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{Matchers, PropSpec}

import scala.collection.GenTraversable

class AdditionalContainsWordSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  import AdditionalContainsWord._

//  property("should contain itself as a sub-sequence") {
//    forAll { (eles: GenTraversable[_]) =>
//      eles should contain subSequenceOf eles
//    }
//  }
}
