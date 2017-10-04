package nz.co.bottech.checkity.macros

import org.scalatest.{Matchers, WordSpec}

import scala.language.existentials
import scala.reflect.ClassTag

class TypeTagMacrosSpec extends WordSpec with Matchers {

  "randomClassTag macro" should {
    "expand into an expression" in {
      assertCompiles(
        """val classTag = TypeTagMacros.randomClassTag"""
      )
    }
    "expand into an existential class tag" in {
      assertCompiles(
        """val classTag: ClassTag[T] forSome { type T } = TypeTagMacros.randomClassTag"""
      )
    }
    "not expand into an invariant class tag" in {
      assertDoesNotCompile(
        """val classTag: ClassTag[Any] = TypeTagMacros.randomClassTag"""
      )
    }
    "not expand into a parameterised class tag" in {
      assertDoesNotCompile(
        """def classTag[T]: ClassTag[T] = TypeTagMacros.randomClassTag"""
      )
    }
  }
}
