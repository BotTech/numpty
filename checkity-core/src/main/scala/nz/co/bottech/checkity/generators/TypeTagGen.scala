package nz.co.bottech.checkity.generators

import nz.co.bottech.checkity.macros.TypeTagMacros.randomClassTag
import org.scalacheck.{Arbitrary, Gen}

import scala.language.existentials
import scala.reflect.ClassTag

private[checkity] object TypeTagGen {

  val anyClassTag: Gen[ClassTag[T]] forSome {type T} = Gen.const(randomClassTag)

  implicit val arbClassTag: Arbitrary[ClassTag[T]] forSome {type T}  = Arbitrary(anyClassTag)
}
