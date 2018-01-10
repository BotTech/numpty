package nz.co.bottech.checkity.helpers

private[checkity] object StreamHelper {

  private[checkity] def interleave[T](a: Stream[T], b: Stream[T]): Stream[T] = (a, b) match {
    case (Stream.Empty, _) => b
    case (_, Stream.Empty) => a
    case (aHead #:: aTail, bHead #:: bTail) => aHead #:: bHead #:: interleave(aTail, bTail)
  }
}
