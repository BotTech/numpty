package nz.co.bottech.numpty.math

import spire.macros.Checked
import spire.math.{DecimalNumber, FloatNumber, IntNumber, Natural, Number, Rational, RationalNumber, SafeLong}
import spire.util.Opt

import scala.math.ScalaNumericConversions

sealed trait CheckedNumber extends ScalaNumericConversions with Serializable {

  def abs: CheckedNumber

  def withinInt: Boolean
  def withinLong: Boolean
  def withinDouble: Boolean

  def canBeInt: Boolean
  def canBeLong: Boolean
  def isExact: Boolean

  def unary_-(): CheckedNumber

  def toNumber: Opt[Number]

  def +(rhs: CheckedNumber): CheckedNumber
  def *(rhs: CheckedNumber): CheckedNumber
  def -(rhs: CheckedNumber): CheckedNumber
  def /(rhs: CheckedNumber): CheckedNumber

  def pow(rhs: CheckedNumber): CheckedNumber
  final def **(rhs: CheckedNumber): CheckedNumber = pow(rhs)

  def ===(rhs: CheckedNumber): Boolean
  def =!=(rhs: CheckedNumber): Boolean = !(this === rhs)

  def compare(rhs: CheckedNumber): Int
  def min(rhs: CheckedNumber): CheckedNumber = if (this < rhs) this else rhs
  def max(rhs: CheckedNumber): CheckedNumber = if (this > rhs) this else rhs

  final def <(rhs: CheckedNumber): Boolean = compare(rhs) < 0
  final def <=(rhs: CheckedNumber): Boolean = compare(rhs) <= 0
  final def >(rhs: CheckedNumber): Boolean = compare(rhs) > 0
  final def >=(rhs: CheckedNumber): Boolean = compare(rhs) >= 0

  def &(rhs: CheckedNumber): CheckedNumber
  def |(rhs: CheckedNumber): CheckedNumber
  def ^(rhs: CheckedNumber): CheckedNumber
  def <<(rhs: CheckedNumber): CheckedNumber
  def >>(rhs: CheckedNumber): CheckedNumber

  def floor: CheckedNumber
  def ceil: CheckedNumber
  def round: CheckedNumber
}

object CheckedNumber {

  implicit def apply(n: Int): CheckedNumber = CheckedIntNumber(n)
  implicit def apply(n: Long): CheckedNumber = CheckedLongNumber(n)
}

case object InvalidNumber extends CheckedNumber {

  override def abs: InvalidNumber.type = this

  override def withinInt: Boolean = false
  override def withinLong: Boolean = false
  override def withinDouble: Boolean = false

  override def canBeInt: Boolean = false
  override def canBeLong: Boolean = false
  override def isExact: Boolean = false

  override def unary_-(): CheckedNumber = this

  override def toNumber: Opt[Number] = Opt.empty

  override def +(rhs: CheckedNumber): CheckedNumber = this
  override def *(rhs: CheckedNumber): CheckedNumber = this
  override def -(rhs: CheckedNumber): CheckedNumber = this
  override def /(rhs: CheckedNumber): CheckedNumber = this

  override def pow(rhs: CheckedNumber): CheckedNumber = this

  override def ===(rhs: CheckedNumber): Boolean = false
  override def compare(rhs: CheckedNumber): Int = Int.MinValue

  override def &(rhs: CheckedNumber): CheckedNumber = this
  override def |(rhs: CheckedNumber): CheckedNumber = this
  override def ^(rhs: CheckedNumber): CheckedNumber = this
  override def <<(rhs: CheckedNumber): CheckedNumber = this
  override def >>(rhs: CheckedNumber): CheckedNumber = this

  override def floor: CheckedNumber = this
  override def ceil: CheckedNumber = this
  override def round: CheckedNumber = this

  override def underlying(): AnyRef = this

  override def isWhole(): Boolean = false
  override def intValue(): Int = throw new NoSuchElementException("InvalidNumber.intValue")
  override def longValue(): Long = throw new NoSuchElementException("InvalidNumber.longValue")
  override def floatValue(): Float = throw new NoSuchElementException("InvalidNumber.floatValue")
  override def doubleValue(): Double = throw new NoSuchElementException("InvalidNumber.doubleValue")
}

/**
  * CheckedNumber with an underlying Int representation.
  */
private[math] case class CheckedIntNumber(n: Int) extends CheckedNumber { lhs =>

  import CheckedIntNumber._

  override def toString: String = n.toString

  def abs: CheckedNumber = Checked.option(n.abs).toNumber
  def signum: Int = n.signum

  def withinInt: Boolean = true
  def withinLong: Boolean = true
  def withinDouble: Boolean = true

  def canBeInt: Boolean = true
  def canBeLong: Boolean = true
  def isExact: Boolean = true

  def toBigInt: BigInt = BigInt(n)
  def toBigDecimal: BigDecimal = BigDecimal(n)
  def toRational: Rational = Rational(n)

  def underlying: java.lang.Object = n.underlying

  def isWhole: Boolean = true
  def doubleValue: Double = n.doubleValue
  def floatValue: Float = n.floatValue
  def longValue: Long = n.longValue
  def intValue: Int = n.intValue

  def compare(rhs: CheckedNumber): Int = rhs match {
    case CheckedIntNumber(m) => n.compare(m)
    case t => -t.compare(lhs)
  }

  override def equals(that: Any): Boolean = that match {
      case number: CheckedNumber => this === number
      case other => n == other
    }

  def ===(that: CheckedNumber): Boolean = that match {
      case CheckedIntNumber(n2) => n == n2
      case other => other === this
    }

  def unary_- : CheckedNumber = CheckedNumber(-n)

  def +(rhs: CheckedNumber): CheckedNumber = rhs match {
    case CheckedIntNumber(m) => Checked.option(n + m).toNumber
    case t => t + lhs
  }
  def *(rhs: CheckedNumber): CheckedNumber = rhs match {
    case CheckedIntNumber(m) => Checked.option(n * m).toNumber
    case t => t * lhs
  }
  def -(rhs: CheckedNumber): CheckedNumber = rhs match {
    case CheckedIntNumber(m) => Checked.option(n - m).toNumber
    case t => t r_- lhs
  }
  def /(rhs: Number): Number = rhs match {
    case IntNumber(m) => n match {
      case Long(x) => m match {
        case Long(y) => Number(x.toDouble / y.toDouble)
        case BigInteger(y) => DecimalNumber(BigDecimal(x) / BigDecimal(y))
      }
      case BigInteger(x) => Number(BigDecimal(x) / m.toBigDecimal)
    }
    case t => t r_/ lhs
  }

  private[math] def r_-(lhs: CheckedNumber): CheckedNumber = lhs match {
    case CheckedIntNumber(m) => Checked.option(m - n).toNumber
    case t => t - lhs
  }
  private[math] def r_/(lhs: Number): Number = lhs match {
    case IntNumber(m) => n match {
      case Long(x) => m match {
        case Long(y) => Number(y.toDouble / x.toDouble)
        case BigInteger(y) => DecimalNumber(BigDecimal(y) / BigDecimal(x))
      }
      case BigInteger(x) => Number(m.toBigDecimal / BigDecimal(x))
    }
    case t => t / lhs
  }
  /* TODO: move to TruncatedDivision
  private[math] def r_/~(lhs: Number): Number = lhs match {
    case IntNumber(m) => IntNumber(m / n)
    case t => t /~ lhs
  }
  private[math] def r_%(lhs: Number): Number = lhs match {
    case IntNumber(m) => IntNumber(m % n)
    case t => t % lhs
  }
  private[math] def r_/%(lhs: Number): (Number, Number) = lhs match {
    case IntNumber(m) => (IntNumber(m / n), IntNumber(m % n))
    case t => t /% lhs
  }
   */

  def pow(rhs: Number): Number = rhs match {
    case _ if rhs.canBeInt => Number(n.pow(rhs.intValue))
    case FloatNumber(m) if (withinDouble) => Number(spire.math.pow(doubleValue, m))
    case _ => Number(spire.math.pow(lhs.toBigDecimal, rhs.toBigDecimal))
  }

  override def &(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n & x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def |(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n | x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def ^(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n ^ x)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def <<(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n << x.toInt)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }
  override def >>(rhs: Number): Number = rhs match {
    case IntNumber(x) => IntNumber(n >> x.toInt)
    case _ => throw new IllegalArgumentException("%s not an integer" format rhs)
  }

  def sqrt: Number =
    if (withinDouble)
      Number(Math.sqrt(n.toDouble))
    else
      Number(n.toBigDecimal.sqrt)

  def nroot(k: Int): Number =
    if (withinDouble)
      Number(Math.pow(n.toDouble, 1.0 / k))
    else
      Number(n.toBigDecimal.nroot(k))

  def floor: Number = this
  def ceil: Number = this
  def round: Number = this
}

private[math] object CheckedIntNumber {

  implicit class Checker(val maybeInt: Option[Int]) extends AnyVal {
    def toNumber = maybeInt match {
      case Some(n) => CheckedIntNumber(n)
      case None => InvalidNumber
    }
  }
}
