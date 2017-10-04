package nz.co.bottech.checkity.macros

import nz.co.bottech.checkity.CharacterRanges
import nz.co.bottech.checkity.RegexImplicits._

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object CharacterMacros {

  def matchingChars(regex: String): Set[Char] = macro matchingCharsImpl

  def matchingCharsImpl(c: blackbox.Context)(regex: c.Expr[String]): c.Expr[Set[Char]] = {
    import c.universe._
    val regexStr = regex.tree match {
      case Literal(Constant(r: String)) => r
      case _ => c.abort(c.enclosingPosition, "Required a regular expression as a String literal")
    }
    val r = regexStr.r
    val allChars = CharacterRanges.unicodeCharacters
    val matchingChars = allChars.filter(c => r.matches(c.toString))
    val charSetTree = q"Set(..$matchingChars)"
    c.Expr(charSetTree)
  }

  def charClass: Set[Char] = macro charClassImpl

  def charClassImpl(c: blackbox.Context): c.Expr[Set[Char]] = {
    import c.universe._
    val owner = c.internal.enclosingOwner
    if (owner.isTerm) {
      val name = owner.asTerm.name.toString.trim
      val regex = s"\\p{$name}"
      matchingCharsImpl(c)(c.Expr(Literal(Constant(regex))))
    } else {
      c.abort(owner.pos, "Must be assigned directly to a variable")
    }
  }
}
