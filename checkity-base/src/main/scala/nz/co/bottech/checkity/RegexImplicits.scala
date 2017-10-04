package nz.co.bottech.checkity

import scala.util.matching.Regex

object RegexImplicits {

  implicit class RegexMatches(val regex: Regex) extends AnyVal {

    def matches(input: CharSequence): Boolean = regex.pattern.matcher(input).matches()
  }

}
