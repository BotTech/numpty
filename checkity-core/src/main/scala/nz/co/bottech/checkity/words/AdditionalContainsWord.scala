package nz.co.bottech.checkity.words

import nz.co.bottech.checkity.enablers.SubSequencing
import org.scalatest.Assertion
import org.scalatest.words.ResultOfContainWord

import scala.collection.GenTraversable

object AdditionalContainsWord extends AdditionalContainsWord

trait AdditionalContainsWord {

  implicit class AdditionalResultOfContainWord[L](result: ResultOfContainWord[L]) {

    def subSequence(firstEle: Any, secondEle: Any, remainingEles: Any*)(implicit sequencing: SubSequencing[L]): Assertion = {
      result.inOrder(firstEle, secondEle, remainingEles: _*)
    }

    def subSequenceOf(right: GenTraversable[_])(implicit sequencing: SubSequencing[L]): Assertion = {
      result.inOrderElementsOf(right)
    }
  }
}
