package nz.co.bottech.checkity

object Hexadecimal {

  def isHex(c: Char): Boolean = CharacterSets.PosixAscii.isAlphaNumeric(c)

  def isHex(str: String): Boolean = str.forall(isHex)

  def hexToInt(hex: String): Int = Integer.parseInt(hex, 16)
}
