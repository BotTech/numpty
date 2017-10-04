package nz.co.bottech.checkity.enablers

import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

import scala.collection.mutable

class ArrayWrapperSpec extends PropSpec with GeneratorDrivenPropertyChecks with Matchers {

  property("foreach calls function the same number of times") {
    forAll { (array: Array[Int]) =>
      var count = 0
      new ArrayWrapper(array).foreach(_ => count += 1)
      array should have length count
    }
  }

  property("foreach calls function in order") {
    forAll { (array: Array[Int]) =>
      val builder = mutable.ArrayBuilder.make[Int]()
      new ArrayWrapper(array).foreach(builder += _)
      array should contain theSameElementsInOrderAs builder.result()
    }
  }

  property("class tag is the same as the common library wrapped array") {
    val array = new Array[String](0)
    new ArrayWrapper(array).elemTag shouldBe theSameInstanceAs(mutable.WrappedArray.make(array).elemTag)
  }

  property("length same as array") {
    forAll { (array: Array[Int]) =>
      // TODO: Add should have same length as XXX
      new ArrayWrapper(array) should have length array.length
    }
  }

  // TODO: Test pretty printing
}
