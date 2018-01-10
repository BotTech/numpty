package nz.co.bottech.checkity

import scala.collection.immutable.NumericRange

object CharacterRanges {

  type CharacterRange = NumericRange[Char]

  type CharacterRanges = NumericRanges[Char]

  implicit val startThenEnd: Ordering[CharacterRange] = Ordering[(Char, Char)].on(range => (range.start, range.end))

  def apply(ranges: CharacterRange*): CharacterRanges = NumericRanges(ranges)

  def unicodeCharacters: CharacterRanges = CharacterRanges('\u0000' to '\uD7FF', '\uE000' to '\uFFFD')

}
