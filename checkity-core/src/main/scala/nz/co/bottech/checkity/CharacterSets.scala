package nz.co.bottech.checkity

import nz.co.bottech.checkity.macros.CharacterMacros

object CharacterSets {

  trait WhitespaceClass {

    def whitespace: Set[Char]

    def isWhitespace(c: Char): Boolean = whitespace.contains(c)
  }

  trait NonPrintableClass {

    def nonPrintable: Set[Char]

    def isNonPrintable(c: Char): Boolean = nonPrintable.contains(c)
  }

  trait InvisibleClass {

    def invisible: Set[Char]

    def isInvisible(c: Char): Boolean = invisible.contains(c)
  }

  trait AlphabetClass {

    def lowerCase: Set[Char]

    def isLowerCase(c: Char): Boolean = lowerCase.contains(c)

    def upperCase: Set[Char]

    def isUpperCase(c: Char): Boolean = upperCase.contains(c)

    def alphabetic: Set[Char] = lowerCase ++ upperCase

    def isAlphabetic(c: Char): Boolean = isLowerCase(c) || isUpperCase(c)
  }

  trait NumericClass {

    def numeric: Set[Char]

    def isNumeric(c: Char): Boolean = numeric.contains(c)
  }

  trait AlphanumericClass {
    this: AlphabetClass with NumericClass =>

    def alphaNumeric: Set[Char] = alphabetic ++ numeric

    def isAlphaNumeric(c: Char): Boolean = isAlphabetic(c) || isNumeric(c)
  }

  trait BasicCharacterSet extends AlphabetClass with NumericClass with AlphanumericClass with WhitespaceClass with NonPrintableClass with InvisibleClass {

    override def invisible: Set[Char] = nonPrintable ++ whitespace
  }

  object PosixAscii extends BasicCharacterSet {

    // A lower-case alphabetic character: [a-z]
    def Lower: Set[Char] = CharacterMacros.charClass

    override def lowerCase: Set[Char] = Lower

    override def isLowerCase(c: Char): Boolean = ('a' to 'z').containsTyped(c)

    // An upper-case alphabetic character:[A-Z]
    def Upper: Set[Char] = CharacterMacros.charClass

    override def upperCase: Set[Char] = Upper

    override def isUpperCase(c: Char): Boolean = ('A' to 'Z').containsTyped(c)

    // All ASCII:[\x00-\x7F]
    def ASCII: Set[Char] = CharacterMacros.charClass

    // An alphabetic character:[\p{Lower}\p{Upper}]
    def Alpha: Set[Char] = CharacterMacros.charClass

    override def alphabetic: Set[Char] = Alpha

    // A decimal digit: [0-9]
    def Digit: Set[Char] = CharacterMacros.charClass

    override def numeric: Set[Char] = Digit

    override def isNumeric(c: Char): Boolean = ('0' to '9').containsTyped(c)

    // An alphanumeric character:[\p{Alpha}\p{Digit}]
    def Alnum: Set[Char] = CharacterMacros.charClass

    // Punctuation: One of !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    def Punct: Set[Char] = CharacterMacros.charClass

    // A visible character: [\p{Alnum}\p{Punct}]
    def Graph: Set[Char] = CharacterMacros.charClass

    // A printable character: [\p{Graph}\x20]
    def Print: Set[Char] = CharacterMacros.charClass

    // A space or a tab: [ \t]
    def Blank: Set[Char] = CharacterMacros.charClass

    // A control character: [\x00-\x1F\x7F]
    def Cntrl: Set[Char] = CharacterMacros.charClass

    // A hexadecimal digit: [0-9a-fA-F]
    def XDigit: Set[Char] = CharacterMacros.charClass

    // A whitespace character: [ \t\n\x0B\f\r]
    def Space: Set[Char] = CharacterMacros.charClass

    override def whitespace: Set[Char] = Space

    override def nonPrintable: Set[Char] = Cntrl
  }

  object Java extends BasicCharacterSet {

    override def whitespace: Set[Char] = ('\u0009' to '\u000d').toSet ++
      ('\u001c' to '\u0020') ++ Set(
      '\u1680',
      '\u180e') ++
      ('\u2000' to '\u200a') ++ Set(
      '\u2028',
      '\u2029',
      '\u205f',
      '\u3000'
    )

    override def isWhitespace(c: Char): Boolean = c.isWhitespace

    override def nonPrintable: Set[Char] = ???

    override def numeric: Set[Char] = ???

    override def lowerCase: Set[Char] = ???

    override def upperCase: Set[Char] = ???
  }

  object Unicode extends BasicCharacterSet {

    // For the latest list see http://www.unicode.org/Public/UCD/latest/ucd/PropList.txt
    override def whitespace: Set[Char] = ('\u0009' to '\u000d').toSet ++ Set(
      '\u0020',
      '\u0085',
      '\u00a0',
      '\u1680') ++
      ('\u2000' to '\u200a') ++ Set(
      '\u2028',
      '\u2029',
      '\u202f',
      '\u205f',
      '\u3000'
    )

    // Taken from https://en.wikipedia.org/wiki/Unicode_character_property#Whitespace
    def whitespaceRelated: Set[Char] = Set(
      '\u180e',
      '\u200b',
      '\u200c',
      '\u200d',
      '\u2060',
      '\ufeff'
    )

    override def nonPrintable: Set[Char] = ???

    override def numeric: Set[Char] = ???

    override def lowerCase: Set[Char] = ???

    override def upperCase: Set[Char] = ???
  }

  object Regex extends BasicCharacterSet {

    def horizontalWhitespace: Set[Char] = Set(
      '\u0009',
      '\u0020',
      '\u00a0',
      '\u1680',
      '\u180e') ++
      ('\u2000' to '\u200a') ++ Set(
      '\u202f',
      '\u205f',
      '\u3000'
    )

    def verticalWhitespace: Set[Char] = ('\u000a' to '\u000d').toSet ++ Set(
      '\u0085',
      '\u2028',
      '\u2029'
    )

    override def whitespace: Set[Char] = PosixAscii.Space

    override def nonPrintable: Set[Char] = ???

    override def numeric: Set[Char] = ???

    override def lowerCase: Set[Char] = ???

    override def upperCase: Set[Char] = ???
  }

  def anyWhitespace: Set[Char] = ???

  def isAnyWhitespace(c: Char): Boolean = anyWhitespace.contains(c)
}
