package nz.co.bottech.checkity.matchers

import nz.co.bottech.checkity.Hexadecimal._
import org.scalatest.matchers.{BeMatcher, MatchResult}

trait HexadecimalMatchers {

  val hexChar = new HexCharMatcher

  class HexCharMatcher extends BeMatcher[Char] {
    override def apply(left: Char): MatchResult = MatchResult(
      isHex(left),
      s"$left was not a hexadecimal character",
      s"$left was a hexadecimal character"
    )
  }

  val hexString = new HexCharMatcher

  class HexStringMatcher extends BeMatcher[String] {
    override def apply(left: String): MatchResult = MatchResult(
      isHex(left),
      s"$left was not a hexadecimal string",
      s"$left was a hexadecimal string"
    )
  }

}

object HexadecimalMatchers extends HexadecimalMatchers
