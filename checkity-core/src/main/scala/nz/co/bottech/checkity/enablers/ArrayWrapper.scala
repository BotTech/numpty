package nz.co.bottech.checkity.enablers

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * This wrapper gives better toString (Array(x, x, x)) as compared to Scala default one (WrappedArray(x, x, x)).
  *
  * This also extends [[mutable.WrappedArray]] so that we can get ScalaTest to prettify it for us.
  *
  * @see [[org.scalatest.words.ArrayWrapper]]
  */
private[enablers] class ArrayWrapper[T](underlying: Array[T]) extends mutable.WrappedArray[T] with Traversable[T] {

  protected val delegate: mutable.WrappedArray[T] = mutable.WrappedArray.make(underlying)

  override def foreach[U](f: (T) => U): Unit = {
    var index = 0
    while (index < underlying.length) {
      index += 1
      f(underlying(index - 1))
    }
  }

  override def elemTag: ClassTag[T] = delegate.elemTag

  override def length: Int = delegate.length

  override def apply(index: Int): T = delegate.apply(index)

  override def update(index: Int, elem: T): Unit = delegate.update(index, elem)

  override def array: Array[T] = delegate.array
}

