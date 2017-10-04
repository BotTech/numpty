package nz.co.bottech.checkity.generators

import nz.co.bottech.checkity.Sliceable
import org.scalacheck.{Arbitrary, Gen}

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object SequenceGen {

  case class SequenceWithSub[T, U](sequence: T, sub: U)

  def sequenceWithSub[E, T <: TraversableLike[E, T], S[_]](implicit arb: Arbitrary[T],
                                                           cbf: CanBuildFrom[Nothing, E, S[E]]): Gen[SequenceWithSub[T, S[E]]] = {
    sequenceWithSub(arb.arbitrary)
  }

  def sequenceWithSub[E, T <: TraversableLike[E, T], S[_]](gen: Gen[T])
                                                          (implicit cbf: CanBuildFrom[Nothing, E, S[E]]): Gen[SequenceWithSub[T, S[E]]] = {
    sliceableWithSub[E, T, S](gen)
  }

  def arrayWithSub[E, S[_]](implicit arb: Arbitrary[Array[E]],
                            cbf: CanBuildFrom[Nothing, E, S[E]]): Gen[SequenceWithSub[Array[E], S[E]]] = {
    arrayWithSub(arb.arbitrary)
  }

  def arrayWithSub[E, S[_]](gen: Gen[Array[E]])
                           (implicit cbf: CanBuildFrom[Nothing, E, S[E]]): Gen[SequenceWithSub[Array[E], S[E]]] = {
    sliceableWithSub[E, Array[E], S](gen)
  }

  def sliceableWithSub[E, T, S[_]](implicit arb: Arbitrary[T],
                                   cbf: CanBuildFrom[Nothing, E, S[E]],
                                   sliceable: Sliceable[E, T]): Gen[SequenceWithSub[T, S[E]]] = {
    sliceableWithSub(arb.arbitrary)
  }

  def sliceableWithSub[E, T, S[_]](gen: Gen[T])
                                  (implicit cbf: CanBuildFrom[Nothing, E, S[E]],
                                   sliceable: Sliceable[E, T]): Gen[SequenceWithSub[T, S[E]]] = {
    for {
      sequenceSize: Int <- Gen.size
      sequence <- gen
      subSize <- Gen.chooseNum(0, sequenceSize)
      subStart <- Gen.chooseNum(0, sequenceSize - subSize)
    } yield {
      val sub: S[E] = sliceable.slice(sequence, subStart, sequenceSize - subSize)
      SequenceWithSub(sequence, sub)
    }
  }
}
