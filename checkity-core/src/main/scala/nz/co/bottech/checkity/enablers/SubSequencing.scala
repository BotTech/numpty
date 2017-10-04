package nz.co.bottech.checkity.enablers

import org.scalactic.{Equality, Every}
import org.scalatest.enablers.Sequencing

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.JavaConverters._

trait SubSequencing[-S] extends Sequencing[S] {

  // We cannot declare our own functions since we cannot get the "left" out of ScalaTest so we have to just reuse the
  // functions from Sequencing.

  def containsSubSequenceInOrder(sequence: S, eles: Seq[Any]): Boolean
}

object SubSequencing {

  // Taken from ScalaTest 3.0.4

  private def checkTheSameElementsInOrderAs[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {

    // It does make any sense to change this so just use the same implementation as Sequencing.

    @tailrec
    def checkEqual(left: Iterator[T], right: Iterator[Any]): Boolean = {
      if (left.hasNext && right.hasNext) {
        val nextLeft = left.next
        val nextRight = right.next
        if (!equality.areEqual(nextLeft, nextRight))
          false
        else
          checkEqual(left, right)
      }
      else
        left.isEmpty && right.isEmpty
    }

    checkEqual(left.toIterator, right.toIterator)
  }

  private def checkInOrderOnly[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {

    // It does make any sense to change this so just use the same implementation as Sequencing.

    @tailrec
    def checkEqual(left: T, right: Any, leftItr: Iterator[T], rightItr: Iterator[Any]): Boolean = {
      if (equality.areEqual(left, right)) { // The first time in, left must equal right
        // Now need to iterate through the left while it is equal to the right
        @tailrec
        def checkNextLeftAgainstCurrentRight(): Option[T] = { // Returns first left that doesn't match the current right, or None, if all remaining lefts matched current right
          if (leftItr.hasNext) {
            val nextLeft = leftItr.next
            if (equality.areEqual(nextLeft, right))
              checkNextLeftAgainstCurrentRight()
            else
              Some(nextLeft)
          }
          else None // No more lefts
        }

        val nextLeftOption = checkNextLeftAgainstCurrentRight()
        nextLeftOption match {
          case Some(nextLeft) =>
            if (rightItr.hasNext) {
              checkEqual(nextLeft, rightItr.next, leftItr, rightItr)
            }
            else false
          case None => !rightItr.hasNext // No more lefts remaining, so we're good so long as no more rights remaining either.
        }
      }
      else false
    }

    val leftItr: Iterator[T] = left.toIterator
    val rightItr: Iterator[Any] = right.toIterator
    if (leftItr.hasNext && rightItr.hasNext)
      checkEqual(leftItr.next, rightItr.next, leftItr, rightItr)
    else left.isEmpty && right.isEmpty
  }

  private def checkSubInOrder[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {

    sealed trait StartsWithResult
    case object StartsWith extends StartsWithResult
    case object ElementNotEqual extends StartsWithResult
    case object LeftTooShort extends StartsWithResult

    @tailrec
    def checkStartsWith(left: Iterator[T], right: Iterator[Any]): StartsWithResult = {
      if (left.hasNext && right.hasNext) {
        val nextLeft = left.next
        val nextRight = right.next
        if (equality.areEqual(nextLeft, nextRight)) checkStartsWith(left, right)
        else ElementNotEqual
      } else if (left.isEmpty && right.nonEmpty) {
        LeftTooShort
      } else {
        StartsWith
      }
    }

    @tailrec
    def checkContains(left: GenTraversable[T], right: GenTraversable[Any]) : Boolean = {
      checkStartsWith(left.toIterator, right.toIterator) match {
        case StartsWith => true
        case ElementNotEqual => checkContains(left.tail, right)
        case LeftTooShort => false
      }
    }

    checkContains(left, right)
  }

  import scala.language.higherKinds

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>scala.collection.GenSeq</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.GenSeq</code>
    * @tparam E   the type of the element in the <code>scala.collection.GenSeq</code>
    * @tparam SEQ any subtype of <code>scala.collection.GenSeq</code>
    * @return <code>SubSequencing[SEQ[E]]</code> that supports <code>scala.collection.GenSeq</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfGenSeq[E, SEQ[e] <: scala.collection.GenSeq[e]](implicit equality: Equality[E]): SubSequencing[SEQ[E]] =
    new SubSequencing[SEQ[E]] {

      def containsInOrder(seq: SEQ[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(seq, elements, equality)
      }

      def containsInOrderOnly(seq: SEQ[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](seq, elements, equality)
      }

      // TODO: Make elements a SubSequencing
      def containsTheSameElementsInOrderAs(seq: SEQ[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](seq, elements, equality)
      }

      def containsSubSequenceInOrder(seq: SEQ[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(seq, elements, equality)
      }
    }

  import scala.language.implicitConversions

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>SEQ[E]</code>, where <code>SEQ</code> is a subtype of <code>scala.collection.GenSeq</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (List("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[List[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E   type of elements in the <code>scala.collection.GenSeq</code>
    * @tparam SEQ subtype of <code>scala.collection.GenSeq</code>
    * @return <code>SubSequencing</code> of type <code>SEQ[E]</code>
    */
  implicit def convertEqualityToGenSeqSubSequencing[E, SEQ[e] <: scala.collection.GenSeq[e]](equality: Equality[E]): SubSequencing[SEQ[E]] =
    sequencingNatureOfGenSeq(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>scala.collection.SortedSet</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.SortedSet</code>
    * @tparam E   the type of the element in the <code>scala.collection.SortedSet</code>
    * @tparam SET any subtype of <code>scala.collection.SortedSet</code>
    * @return <code>SubSequencing[SET[E]]</code> that supports <code>scala.collection.SortedSet</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfSortedSet[E, SET[e] <: scala.collection.SortedSet[e]](implicit equality: Equality[E]): SubSequencing[SET[E]] =
    new SubSequencing[SET[E]] {

      def containsInOrder(set: SET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(set, elements, equality)
      }

      def containsInOrderOnly(set: SET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](set, elements, equality)
      }

      def containsTheSameElementsInOrderAs(set: SET[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](set, elements, equality)
      }

      def containsSubSequenceInOrder(set: SET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(set, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>SET[E]</code>, where <code>SET</code> is a subtype of <code>scala.collection.SortedSet</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (SortedSet("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[SortedSet[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E   type of elements in the <code>scala.collection.SortedSet</code>
    * @tparam SET subtype of <code>scala.collection.SortedSet</code>
    * @return <code>SubSequencing</code> of type <code>SET[E]</code>
    */
  implicit def convertEqualityToSortedSetSubSequencing[E, SET[e] <: scala.collection.SortedSet[e]](equality: Equality[E]): SubSequencing[SET[E]] =
    sequencingNatureOfSortedSet(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>scala.collection.SortedMap</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.SortedMap</code>
    * @tparam K   the type of the key in the <code>scala.collection.SortedMap</code>
    * @tparam V   the type of the value in the <code>scala.collection.SortedMap</code>
    * @tparam MAP any subtype of <code>scala.collection.SortedMap</code>
    * @return <code>SubSequencing[MAP[K, V]]</code> that supports <code>scala.collection.SortedMap</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfSortedMap[K, V, MAP[k, v] <: scala.collection.SortedMap[k, v]](implicit equality: Equality[(K, V)]): SubSequencing[MAP[K, V]] =
    new SubSequencing[MAP[K, V]] {

      def containsInOrder(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(map, elements, equality)
      }

      def containsInOrderOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(map, elements, equality)
      }

      def containsTheSameElementsInOrderAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map, elements, equality)
      }

      def containsSubSequenceInOrder(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(map, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>(K, V)</code>
    * into <code>SubSequencing</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.SortedMap</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * // lowerCased needs to be implemented as Normalization[(K, V)]
    * (SortedMap("hi" -> "hi", "he" -> "he") should contain inOrderOnly ("HI" -> "HI", "HE" -> "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[SortedMap[String, String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>(K, V)</code>
    * @tparam K   the type of the key in the <code>scala.collection.SortedMap</code>
    * @tparam V   the type of the value in the <code>scala.collection.SortedMap</code>
    * @tparam MAP subtype of <code>scala.collection.SortedMap</code>
    * @return <code>SubSequencing</code> of type <code>MAP[K, V]</code>
    */
  implicit def convertEqualityToSortedMapSubSequencing[K, V, MAP[k, v] <: scala.collection.SortedMap[k, v]](equality: Equality[(K, V)]): SubSequencing[MAP[K, V]] =
    sequencingNatureOfSortedMap(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>Array</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
    * @tparam E the type of the element in the <code>Array</code>
    * @return <code>SubSequencing[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfArray[E](implicit equality: Equality[E]): SubSequencing[Array[E]] =
    new SubSequencing[Array[E]] {

      def containsInOrder(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(new ArrayWrapper(array), elements, equality)
      }

      def containsInOrderOnly(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(new ArrayWrapper(array), elements, equality)
      }

      def containsTheSameElementsInOrderAs(array: Array[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](new ArrayWrapper(array), elements, equality)
      }

      def containsSubSequenceInOrder(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(new ArrayWrapper(array), elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>Array[E]</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (Array("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[Array[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Array</code>
    * @return <code>SubSequencing</code> of type <code>Array[E]</code>
    */
  implicit def convertEqualityToArraySubSequencing[E](equality: Equality[E]): SubSequencing[Array[E]] =
    sequencingNatureOfArray(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>java.util.List</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.List</code>
    * @tparam E     the type of the element in the <code>java.util.List</code>
    * @tparam JLIST any subtype of <code>java.util.List</code>
    * @return <code>SubSequencing[JLIST[E]]</code> that supports <code>java.util.List</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfJavaList[E, JLIST[e] <: java.util.List[e]](implicit equality: Equality[E]): SubSequencing[JLIST[E]] =
    new SubSequencing[JLIST[E]] {

      def containsInOrder(col: JLIST[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(col.asScala, elements, equality)
      }

      def containsInOrderOnly(col: JLIST[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(col.asScala, elements, equality)
      }

      def containsTheSameElementsInOrderAs(col: JLIST[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(col.asScala, elements, equality)
      }

      def containsSubSequenceInOrder(col: JLIST[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(col.asScala, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>JLIST[E]</code>, where <code>JLIST</code> is a subtype of <code>java.util.List</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val javaList = new java.util.ArrayList[String]()
    * javaList.add("hi", "he")
    * (javaList should contain ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[java.util.ArrayList[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E     type of elements in the <code>java.util.List</code>
    * @tparam JLIST subtype of <code>java.util.List</code>
    * @return <code>SubSequencing</code> of type <code>JLIST[E]</code>
    */
  implicit def convertEqualityToJavaListSubSequencing[E, JLIST[e] <: java.util.List[e]](equality: Equality[E]): SubSequencing[JLIST[E]] =
    sequencingNatureOfJavaList(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>java.util.SortedSet</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.SortedSet</code>
    * @tparam E    the type of the element in the <code>java.util.SortedSet</code>
    * @tparam JSET any subtype of <code>java.util.SortedSet</code>
    * @return <code>SubSequencing[JSET[E]]</code> that supports <code>java.util.SortedSet</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfJavaSortedSet[E, JSET[e] <: java.util.SortedSet[e]](implicit equality: Equality[E]): SubSequencing[JSET[E]] =
    new SubSequencing[JSET[E]] {

      def containsInOrder(set: JSET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(set.iterator.asScala.toVector, elements, equality)
      }

      def containsInOrderOnly(set: JSET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](set.iterator.asScala.toVector, elements, equality)
      }

      def containsTheSameElementsInOrderAs(set: JSET[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](set.iterator.asScala.toVector, elements, equality)
      }

      def containsSubSequenceInOrder(set: JSET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(set.iterator.asScala.toVector, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>JSET[E]</code>, where <code>JSET</code> is a subtype of <code>java.util.SortedSet</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val javaSet = new java.util.TreeSet[String]()
    * javaSet.add("hi", "he")
    * (javaSet should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[java.util.TreeSet[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E    type of elements in the <code>java.util.List</code>
    * @tparam JSET subtype of <code>java.util.List</code>
    * @return <code>SubSequencing</code> of type <code>JLIST[E]</code>
    */
  implicit def convertEqualityToJavaSortedSetSubSequencing[E, JSET[e] <: java.util.SortedSet[e]](equality: Equality[E]): SubSequencing[JSET[E]] =
    sequencingNatureOfJavaSortedSet(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>java.util.SortedMap</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.SortedMap</code>
    * @tparam K    the type of the key in the <code>java.util.SortedMap</code>
    * @tparam V    the type of the value in the <code>java.util.SortedMap</code>
    * @tparam JMAP any subtype of <code>java.util.SortedMap</code>
    * @return <code>SubSequencing[JMAP[K, V]]</code> that supports <code>java.util.SortedMap</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfJavaSortedMap[K, V, JMAP[k, v] <: java.util.SortedMap[k, v]](implicit equality: Equality[java.util.Map.Entry[K, V]]): SubSequencing[JMAP[K, V]] =
    new SubSequencing[JMAP[K, V]] {

      def containsInOrder(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(map.entrySet.iterator.asScala.toVector, elements, equality)
      }

      def containsInOrderOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(map.entrySet.iterator.asScala.toVector, elements, equality)
      }

      def containsTheSameElementsInOrderAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map.entrySet.iterator.asScala.toVector, elements, equality)
      }

      def containsSubSequenceInOrder(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(map.entrySet.iterator.asScala.toVector, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    * into <code>SubSequencing</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.SortedMap</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val javaMap = new java.util.TreeMap[Int, String]()
    * javaMap.put(1, "one")
    * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
    * (javaMap should contain inOrderOnly (Entry(1, "ONE"))) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
    * and this implicit conversion will convert it into <code>Aggregating[java.util.TreeMap[Int, String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    * @tparam K    the type of the key in the <code>java.util.SortedMap</code>
    * @tparam V    the type of the value in the <code>java.util.SortedMap</code>
    * @tparam JMAP subtype of <code>java.util.SortedMap</code>
    * @return <code>SubSequencing</code> of type <code>JMAP[K, V]</code>
    */
  implicit def convertEqualityToJavaSortedMapSubSequencing[K, V, JMAP[k, v] <: java.util.SortedMap[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): SubSequencing[JMAP[K, V]] =
    sequencingNatureOfJavaSortedMap(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>String</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
    * @return <code>SubSequencing[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
    */
  implicit def sequencingNatureOfString(implicit equality: Equality[Char]): SubSequencing[String] =
    new SubSequencing[String] {

      def containsInOrder(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(s, elements, equality)
      }

      def containsInOrderOnly(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(s, elements, equality)
      }

      def containsTheSameElementsInOrderAs(s: String, elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(s, elements, equality)
      }

      def containsSubSequenceInOrder(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkSubInOrder(s, elements, equality)
      }
    }

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    * into <code>SubSequencing</code> of type <code>String</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * // lowerCased needs to be implemented as Normalization[Char]
    * ("hi hello" should contain inOrderOnly ('E')) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[String]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    * @return <code>SubSequencing</code> of type <code>String</code>
    */
  implicit def convertEqualityToStringSubSequencing(equality: Equality[Char]): SubSequencing[String] =
    sequencingNatureOfString(equality)

  /**
    * Implicit to support <code>SubSequencing</code> nature of <code>Every</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
    * @tparam E the type of the element in the <code>Every</code>
    * @return <code>SubSequencing[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
    **/
  implicit def sequencingNatureOfEvery[E](implicit equality: Equality[E]): SubSequencing[Every[E]] =
    new SubSequencing[Every[E]] {

      def containsInOrder(every: Every[E], elements: scala.collection.Seq[Any]): Boolean =
        checkSubInOrder(every, elements, equality)

      def containsInOrderOnly(every: Every[E], elements: scala.collection.Seq[Any]): Boolean =
        checkInOrderOnly(every, elements, equality)

      def containsTheSameElementsInOrderAs(every: Every[E], elements: GenTraversable[Any]): Boolean =
        checkTheSameElementsInOrderAs[E](every, elements, equality)

      def containsSubSequenceInOrder(every: Every[E], elements: scala.collection.Seq[Any]): Boolean =
        checkSubInOrder(every, elements, equality)
}

  /**
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>SubSequencing</code> of type <code>Every[E]</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (Every("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>SubSequencing[Every[String]]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Every</code>
    * @return <code>SubSequencing</code> of type <code>Every[E]</code>
    */
  implicit def convertEqualityToEverySubSequencing[E](equality: Equality[E]): SubSequencing[Every[E]] =
    sequencingNatureOfEvery(equality)

}
