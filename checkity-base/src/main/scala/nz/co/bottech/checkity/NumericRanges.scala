package nz.co.bottech.checkity

import nz.co.bottech.checkity.NumericRanges._
import sun.plugin.dom.exception.InvalidStateException

import scala.Ordered._
import scala.annotation.tailrec
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable.NumericRange
import scala.collection.{AbstractSeq, IndexedSeqLike, mutable}

class NumericRanges[A] protected(private[checkity] val tree: Tree[A],
                                 val ranges: List[NumericRange[A]],
                                 val length: Int)(implicit num: Integral[A]) extends AbstractSeq[A]
  with IndexedSeq[A]
  with IndexedSeqLike[A, NumericRanges[A]] {

  override protected[this] def newBuilder: mutable.Builder[A, NumericRanges[A]] = NumericRanges.newBuilder

  override def lengthCompare(len: Int): Int = this.length - len

  override def apply(idx: Int): A = tree(idx)

  override def contains[A1 >: A](elem: A1): Boolean = tree.contains(elem)

  // The first element may not be the smallest anymore...
  override def drop(n: Int): NumericRanges[A] = tree.drop(n).toRanges

  // The first element may not be the smallest anymore...
  override def take(n: Int): NumericRanges[A] = tree.take(n).toRanges

  override def foreach[U](f: (A) => U): Unit = ranges.foreach(_.foreach(f))

  override def iterator: Iterator[A] = ranges.foldLeft[Iterator[A]](Iterator.empty)(_ ++ _.iterator)

  override def min[B >: A](implicit ord: Ordering[B]): A = {
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) ranges.head.min
    else super.min(ord)
  }

  override def max[B >: A](implicit ord: Ordering[B]): A = {
    if ((ord eq num) || defaultOrdering.get(num).exists(ord eq _)) ranges.last.max
    else super.max(ord)
  }
}

object NumericRanges {

  private val defaultOrdering = Map[Numeric[_], Ordering[_]](
    Numeric.BigIntIsIntegral -> Ordering.BigInt,
    Numeric.IntIsIntegral -> Ordering.Int,
    Numeric.ShortIsIntegral -> Ordering.Short,
    Numeric.ByteIsIntegral -> Ordering.Byte,
    Numeric.CharIsIntegral -> Ordering.Char,
    Numeric.LongIsIntegral -> Ordering.Long,
    Numeric.FloatAsIfIntegral -> Ordering.Float,
    Numeric.DoubleAsIfIntegral -> Ordering.Double,
    Numeric.BigDecimalAsIfIntegral -> Ordering.BigDecimal
  )

  def empty[A: Integral]: NumericRanges[A] = new NumericRanges[A](Empty, Nil, 0)

  def apply[A: Integral](ranges: Seq[NumericRange[A]]): NumericRanges[A] = {
    applyMinimized(minimize(ranges))
  }

  private def applyMinimized[A: Integral](minimized: List[NumericRange[A]]): NumericRanges[A] = {
    val length = minimized.foldLeft(0)(_ + _.length)
    new NumericRanges[A](Tree(minimized), minimized, length)
  }

  implicit def startThenEnd[A: Ordering]: Ordering[NumericRange[A]] =
    Ordering[(A, A)].on(range => (range.start, range.end))

  /**
    * Minimizes the number of ranges by combining overlapping ranges with the same step into a single range.
    *
    * @param ranges the ranges to minimize, in any order
    * @return The combined ranges in order of smallest to largest.
    */
  private[checkity] def minimize[A: Ordering](ranges: Seq[NumericRange[A]]): List[NumericRange[A]] = {
    def overlap(smaller: NumericRange[A], larger: NumericRange[A]): Boolean =
      larger.step == smaller.step && smaller.end >= larger.start

    ranges.sorted.foldLeft(List.empty[NumericRange[A]]) { (acc, larger) =>
      acc match {
        case Nil                                         => larger :: Nil
        case smaller :: tail if overlap(smaller, larger) => larger.copy(smaller.start, larger.end, larger.step) :: tail
        case _                                           => larger :: acc
      }
    }.reverse
  }

  private[checkity] sealed trait Tree[+A] extends AbstractSeq[A] with IndexedSeq[A] with IndexedSeqLike[A, Tree[A]] {

    protected[this] implicit def integral: Integral[A]

    override protected[this] def newBuilder: mutable.Builder[A, Tree[A]] =
      new NumericRangeBuilder[A]().mapResult(ranges => Tree(ranges.ranges))

    private[checkity] def balanced: Boolean
  }

  protected object Tree {

    def apply[A: Integral](ranges: List[NumericRange[A]]): Tree[A] = {

      case class SubTree(root: Node[A], left: List[Node[A]], right: List[Node[A]], subsSize: Int)

      def split(nodes: List[Node[A]], size: Int): SubTree = nodes match {
        case Nil         => throw new IllegalArgumentException("Cannot split an empty list of nodes")
        case node :: Nil => SubTree(node, Nil, Nil, 0)
        case _           =>
          val subTreeSize = size / 2
          val (subLeft, subRoot :: subRight) = nodes.splitAt(subTreeSize - 1)
          SubTree(subRoot, subLeft, subRight, subTreeSize)
      }

      @tailrec
      def loop(trees: List[SubTree]): Tree[A] = trees match {
        case Nil                                                                     => Empty
        case SubTree(tree, Nil, Nil, _) :: Nil                                       => tree
        case SubTree(right, Nil, Nil, _) :: SubTree(parent, Nil, Nil, size) :: tail  =>
          loop(SubTree(parent.copy(larger = right), Nil, Nil, size) :: tail)
        case SubTree(left, Nil, Nil, _) :: SubTree(parent, Nil, right, size) :: tail =>
          loop(SubTree(parent.copy(smaller = left), Nil, right, size) :: tail)
        case SubTree(root, Nil, right, size) :: tail                                 =>
          loop(split(right, size) :: SubTree(root, Nil, Nil, size) :: tail)
        case SubTree(root, left, right, size) :: tail                                =>
          loop(split(left, size) :: SubTree(root, Nil, right, size) :: tail)
      }

      val trees = ranges.map(Node(_, Empty, Empty)) match {
        case Nil   => Nil
        case nodes => split(nodes, nodes.length) :: Nil
      }

      loop(trees)
    }

    def newBuilder[B: Integral]: mutable.Builder[B, Tree[B]] =
      new NumericRangeBuilder[B].mapResult[Tree[B]](ranges => Tree(ranges.ranges))

    implicit class TreeConverter[A: Integral](tree: Tree[A]) {

      def toRanges: NumericRanges[A] = tree match {
        case node: Node[A] => applyMinimized(node.rangeIterator.toList)
        case Empty         => NumericRanges.empty[A]
      }
    }

  }

  private[checkity] case class Node[A: Integral](range: NumericRange[A],
                                                 smaller: Tree[A],
                                                 larger: Tree[A]) extends Tree[A] {

    override protected[this] def integral: Integral[A] = implicitly[Integral[A]]

    override val length: Int = range.length + smaller.length + larger.length

    override private[checkity] val balanced: Boolean = smaller.length == larger.length && smaller.balanced && larger.balanced

    override def lengthCompare(len: Int): Int = this.length - len

    override def apply(idx: Int): A = {
      if (idx < smaller.length) smaller(idx)
      else {
        val rangeIdx = idx - smaller.length
        if (rangeIdx < range.length) {
          range(rangeIdx)
        } else {
          val largerIdx = rangeIdx - range.length
          if (largerIdx < larger.length) {
            larger(largerIdx)
          } else {
            throw new IndexOutOfBoundsException(idx.toString)
          }
        }
      }
    }

    override def contains[A1 >: A](elem: A1): Boolean = {
      try containsTyped(elem.asInstanceOf[A])
      catch {
        case _: ClassCastException => false
      }
    }

    def containsTyped(x: A): Boolean = {
      def treeContains(tree: Tree[A]): Boolean = tree match {
        case Empty         => false
        case node: Node[A] => node.containsTyped(x)
      }

      // If it is not smaller then it may be in an overlapping range with a different step so check both.
      if (x < range.start) treeContains(smaller)
      else range.containsTyped(x) || treeContains(larger)
    }

    // Unbalanced
    override def drop(n: Int): Tree[A] = {
      def rebuild(trees: List[Node[A]]): Tree[A] = trees.foldLeft[Tree[A]](Empty) {
        case (child, parent) => parent.copy(smaller = child)
      }

      @tailrec
      def loop(remaining: Int, next: Tree[A], trees: List[Node[A]]): Tree[A] = next match {
        case Empty                                             => rebuild(trees)
        case _ if remaining <= 0                               => rebuild(trees)
        case _ if remaining >= next.length                     => rebuild(trees)
        case Node(r, s, l) if remaining >= r.length + s.length => loop(remaining - r.length - s.length, l, trees)
        case Node(r, s, l) if remaining >= s.length            =>
          rebuild(Node(r.drop(remaining - s.length), Empty, l) :: trees)
        case node@Node(_, s, _)                                => loop(remaining, s, node :: trees)
      }

      loop(n, this, Nil)
    }

    // Unbalanced
    override def take(n: Int): Tree[A] = {
      def rebuild(trees: List[Node[A]]): Tree[A] = trees.foldLeft[Tree[A]](Empty) {
        case (child, parent) => parent.copy(larger = child)
      }

      @tailrec
      def loop(remaining: Int, next: Tree[A], trees: List[Node[A]]): Tree[A] = next match {
        case Empty                                                  => rebuild(trees)
        case _ if remaining <= 0                                    => rebuild(trees)
        case node: Node[A] if remaining >= next.length              => rebuild(node :: trees)
        case node@Node(r, s, l) if remaining >= r.length + s.length =>
          loop(remaining - r.length - s.length, l, node :: trees)
        case Node(r, s, _) if remaining >= s.length                 =>
          rebuild(Node(r.take(remaining - s.length), s, Empty) :: trees)
        case Node(_, s, _)                                          => loop(remaining, s, trees)
      }

      loop(n, this, Nil)
    }

    override def iterator: Iterator[A] = rangeIterator.foldLeft[Iterator[A]](Iterator.empty)(_ ++ _.iterator)

    def rangeIterator: Iterator[NumericRange[A]] = new Node.NodeIterator[A](this).map(_.range)
  }

  protected object Node {

    // Not thread safe
    protected class NodeIterator[A: Integral](node: Node[A]) extends Iterator[Node[A]] {

      private var nodes: List[Node[A]] = node :: Nil

      override def hasNext: Boolean = nodes.nonEmpty

      override def next(): Node[A] = {
        @tailrec
        def loop(): Node[A] = nodes match {
          case Nil                                           => throw new NoSuchElementException(
            "Cannot get next of empty iterator")
          case (head@Node(_, Empty, Empty)) :: tail          =>
            nodes = tail
            head
          case (head@Node(_, Empty, right: Node[A])) :: tail =>
            nodes = right :: tail
            head
          case (head@Node(_, left: Node[A], _)) :: tail      =>
            nodes = left :: head.copy(smaller = Empty) :: tail
            loop()
        }

        loop()
      }
    }

  }

  private[checkity] case object Empty extends Tree[Nothing] {

    override protected[this] def integral: Integral[Nothing] =
      throw new UnsupportedOperationException("Cannot get an integral of nothing")

    override val length: Int = 0

    override private[checkity] val balanced: Boolean = true

    override def apply(idx: Int): Nothing = throw new IndexOutOfBoundsException(idx.toString)
  }

  def newBuilder[B: Integral]: mutable.Builder[B, NumericRanges[B]] = new NumericRangeBuilder

  // Not thread safe
  protected class NumericRangeBuilder[B](implicit num: Integral[B]) extends mutable.Builder[B, NumericRanges[B]] {

    import num._

    private val rangeBuilder: mutable.Builder[NumericRange[B], NumericRanges[B]] =
      Seq.newBuilder[NumericRange[B]].mapResult(NumericRanges(_))

    private var previous: Either[List[B], NumericRange[B]] = Left(Nil)

    override def +=(elem: B): this.type = previous match {
      case Left(elem4 :: elem3 :: elem2 :: elem1 :: Nil) => start(elem1, elem2, elem3, elem4)
      case Left(elems)                                   => stash(elem, elems)
      case Right(range) if canExtend(range, elem)        => extend(range, elem)
      case Right(range)                                  => end(range, elem)
    }

    private def start(elem1: B, elem2: B, elem3: B, elem4: B): this.type = {
      // No guarantees but this attempts to reproduce the same ranges based on the assumption that they
      val step2to1 = elem2 - elem1
      val step3to2 = elem3 - elem2
      val step4to3 = elem4 - elem3
      val (next, done) = if (step4to3 == step3to2) {
        if (step3to2 == step2to1) (NumericRange.inclusive(elem1, elem4, step2to1), None)
        else (NumericRange.inclusive(elem2, elem4, step3to2), Some(NumericRange.inclusive(elem1, elem1, one)))
      } else if (step3to2 == step2to1) {
        (NumericRange.inclusive(elem4, elem4, one), Some(NumericRange.inclusive(elem1, elem3, step2to1)))
      } else (NumericRange.inclusive(elem3, elem4, step4to3), Some(NumericRange.inclusive(elem1, elem2, step2to1)))
      done.foreach(rangeBuilder += _)
      previous = Right(next)
      this
    }

    private def stash(elem: B, elems: List[B]): this.type = {
      previous = Left(elem :: elems)
      this
    }

    private def canExtend(range: NumericRange[B], elem: B): Boolean = {
      range.length == 1 || elem == range.end + range.step
    }

    private def extend(range: NumericRange[B], elem: B): this.type = {
      previous = Right(NumericRange.inclusive(range.start, range.end, elem - range.end))
      this
    }

    private def end(range: NumericRange[B], elem: B): this.type = {
      rangeBuilder += range
      previous = Right(NumericRange.inclusive(elem, elem, one))
      this
    }

    override def clear(): Unit = {
      rangeBuilder.clear()
      previous = Left(Nil)
    }

    override def result(): NumericRanges[B] = {
      previous match {
        case Left(elem3 :: elem2 :: elem1 :: Nil) => consume(elem1, elem2, elem3)
        case Left(elem2 :: elem1 :: Nil)          => consume(elem1, elem2, elem2)
        case Left(elem1 :: Nil)                   => consume(elem1, elem1, elem1)
        case Left(Nil)                            =>
        case Left(_)                              =>
          throw new InvalidStateException("Four or more elements had been stashed")
        case Right(range)                         => rangeBuilder += range
      }
      rangeBuilder.result
    }

    private def consume(elem1: B, elem2: B, elem3: B): Unit = {
      val range = NumericRange.inclusive(elem1, elem2, max(elem2 - elem1, one))
      if (canExtend(range, elem3)) {
        rangeBuilder += NumericRange.inclusive(range.start, range.end, elem3 - range.end)
      } else {
        rangeBuilder += range
        rangeBuilder += NumericRange.inclusive(elem3, elem3, one)
      }
    }
  }

  implicit def canBuildFrom[B: Integral]: CanBuildFrom[NumericRanges[_], B, NumericRanges[B]] = {
    new CanBuildFrom[NumericRanges[_], B, NumericRanges[B]] {
      override def apply(): mutable.Builder[B, NumericRanges[B]] = newBuilder

      override def apply(from: NumericRanges[_]): mutable.Builder[B, NumericRanges[B]] = newBuilder
    }
  }
}
