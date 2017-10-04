package nz.co.bottech.checkity

object Implicits {

  implicit class CharConversions(val c: Char) extends AnyVal {

    def codePoint: String = f"${c.toInt}%04x"

    def escape: String = s"\\u$codePoint"

    def literal: String = s"'$escape'"

    def isHex: Boolean = Hexadecimal.isHex(c)
  }

}
