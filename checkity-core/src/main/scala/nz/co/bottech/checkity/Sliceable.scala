package nz.co.bottech.checkity

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait Sliceable[E, T] {
  def slice[S[_]](from: T, start: Int, end: Int)(implicit cbf: CanBuildFrom[Nothing, E, S[E]]): S[E]
}

object Sliceable {

  implicit def sliceableTraversableLike[E, T <: TraversableLike[E, T]]: SliceableTraversableLike[E, T] = new SliceableTraversableLike[E, T]

  implicit def sliceableArray[E]: SliceableArray[E] = new SliceableArray[E]

  implicit def sliceableString[E]: SliceableString = new SliceableString

  class SliceableTraversableLike[E, T <: TraversableLike[E, T]] extends Sliceable[E, T] {
    override def slice[S[_]](sequence: T, from: Int, until: Int)
                            (implicit cbf: CanBuildFrom[Nothing, E, S[E]]): S[E] =
      sequence.slice(from, until).to[S]
  }

  class SliceableArray[E] extends Sliceable[E, Array[E]] {
    override def slice[S[_]](sequence: Array[E], from: Int, until: Int)
                            (implicit cbf: CanBuildFrom[Nothing, E, S[E]]): S[E] =
      sequence.slice(from, until).to[S]
  }

  class SliceableString extends Sliceable[Char, String] {
    override def slice[S[_]](sequence: String, from: Int, until: Int)
                            (implicit cbf: CanBuildFrom[Nothing, Char, S[Char]]): S[Char] =
      sequence.slice(from, until).to[S]
  }

}
