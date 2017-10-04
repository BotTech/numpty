package nz.co.bottech.checkity

import scala.collection.immutable

object CharacterRanges {

  def unicodeCharacters: immutable.IndexedSeq[Char] = ('\u0000' to '\uD7FF') ++ ('\uE000' to '\uFFFD')
}
