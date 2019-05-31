package nz.co.bottech.numpty.math

import java.math.BigInteger

import spire.util.Opt

import scala.math.{ScalaNumber, ScalaNumericConversions}
import spire.macros.Checked
import spire.algebra.{CRing, Eq, EuclideanRing, GCDRing, IsIntegral, NRoot, Order, Signed}
import spire.math.NumberTag
import spire.std.long._
import spire.std.bigInteger._

import scala.annotation.tailrec

// TODO: Some operations might be valid if we use a SafeLong and then check if the result is within range
// TODO: Should we use Opt? Checked doesn't use it...

//scalastyle:off equals.hash.code
/**
  * Provides a type to do safe long arithmetic. This type will never overflow,
  * but rather convert the underlying long to a BigInteger as need and back down
  * to a Long when possible.
  */
sealed abstract class CheckedLong extends ScalaNumber with ScalaNumericConversions with Ordered[CheckedLong] { lhs =>

  def isZero: Boolean

  def isOne: Boolean

  def isOdd: Boolean

  def isEven: Boolean

  def signum: Int

  final def +(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs + n
      case CheckedLongBigInteger(n) => lhs + n
    }

  final def -(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs - n
      case CheckedLongBigInteger(n) => lhs - n
    }

  final def *(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs * n
      case CheckedLongBigInteger(n) => lhs * n
    }

  final def /(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs / n
      case CheckedLongBigInteger(n) => lhs / n
    }

  final def %(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs % n
      case CheckedLongBigInteger(n) => lhs % n
    }

  final def /~(rhs: CheckedLong): Option[CheckedLong] =
    lhs / rhs

  final def /%(rhs: CheckedLong): Option[(CheckedLong, CheckedLong)] =
    rhs match {
      case CheckedLongLong(n)       => lhs /% n
      case CheckedLongBigInteger(n) => lhs /% n
    }

  final def &(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs & n
      case CheckedLongBigInteger(n) => lhs & n
    }

  final def |(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs | n
      case CheckedLongBigInteger(n) => lhs | n
    }

  final def ^(rhs: CheckedLong): Option[CheckedLong] =
    rhs match {
      case CheckedLongLong(n)       => lhs ^ n
      case CheckedLongBigInteger(n) => lhs ^ n
    }

  def ===(that: CheckedLong): Boolean =
    this == that

  def =!=(that: CheckedLong): Boolean =
    !(this === that)

  def +(rhs: Long): Option[CheckedLong]
  def -(rhs: Long): Option[CheckedLong]
  def *(rhs: Long): Option[CheckedLong]
  def /(rhs: Long): Option[CheckedLong]
  def %(rhs: Long): Option[CheckedLong]
  def /%(rhs: Long): Option[(CheckedLong, CheckedLong)]
  def &(rhs: Long): Option[CheckedLong]
  def |(rhs: Long): Option[CheckedLong]
  def ^(rhs: Long): Option[CheckedLong]

  final def +(rhs: BigInt): Option[CheckedLong] = this + rhs.bigInteger
  final def -(rhs: BigInt): Option[CheckedLong] = this - rhs.bigInteger
  final def *(rhs: BigInt): Option[CheckedLong] = this * rhs.bigInteger
  final def /(rhs: BigInt): Option[CheckedLong] = this / rhs.bigInteger
  final def %(rhs: BigInt): Option[CheckedLong] = this % rhs.bigInteger
  final def /%(rhs: BigInt): Option[(CheckedLong, CheckedLong)] =  this /% rhs.bigInteger
  final def &(rhs: BigInt): Option[CheckedLong] = this & rhs.bigInteger
  final def |(rhs: BigInt): Option[CheckedLong] = this | rhs.bigInteger
  final def ^(rhs: BigInt): Option[CheckedLong] = this ^ rhs.bigInteger

  private[math] def +(rhs: BigInteger): Option[CheckedLong]
  private[math] def -(rhs: BigInteger): Option[CheckedLong]
  private[math] def *(rhs: BigInteger): Option[CheckedLong]
  private[math] def /(rhs: BigInteger): Option[CheckedLong]
  private[math] def %(rhs: BigInteger): Option[CheckedLong]
  private[math] def /%(rhs: BigInteger): Option[(CheckedLong, CheckedLong)]
  private[math] def &(rhs: BigInteger): Option[CheckedLong]
  private[math] def |(rhs: BigInteger): Option[CheckedLong]
  private[math] def ^(rhs: BigInteger): Option[CheckedLong]

  final def min(that: CheckedLong): CheckedLong =
    if (this < that) this else that

  final def max(that: CheckedLong): CheckedLong =
    if (this > that) this else that

  def <<(n: Int): Option[CheckedLong]
  def >>(n: Int): Option[CheckedLong]

  /**
    * Exponentiation function, e.g. x ** y
    *
    * If base ** exponent doesn't fit in a Long, the result will overflow (unlike
    * scala.math.pow which will return +/- Infinity).
    */
  final def **(k: Int): Option[CheckedLong] = pow(k)

  final def pow(k: Int): Option[CheckedLong] = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    // TODO: Does this work?
    // TODO: Can we use for comprehension here?
    @tailrec
    def loop(total: CheckedLong, base: CheckedLong, exp: Int): Option[CheckedLong] = {
      if (exp == 0) Some(total)
      else if ((exp & 1) == 1) {
        total * base match {
          case Some(newTotal) => base * base match {
            case Some(newBase) => loop(newTotal, newBase, exp >> 1)
            case None => None
          }
          case None => None
        }
      } else {
        base * base match {
          case Some(newBase) => loop(total, newBase, exp >> 1)
          case None => None
        }
      }
    }

    loop(CheckedLong.one, this, k)
  }

  final def modPow(k: Int, mod: CheckedLong): Option[CheckedLong] = {
    if (k < 0) throw new IllegalArgumentException(s"negative exponent: $k")

    // TODO: Does this work?
    // TODO: Can we use for comprehension here?
    @tailrec
    def loop(total: CheckedLong, base: CheckedLong, k: Int, mod: CheckedLong): Option[CheckedLong] = {
      if (k == 0) Some(total)
      else if ((k & 1) == 1) {
        total * base match {
          case Some(newTotal) => newTotal % mod match {
            case Some(newTotal2) => base * base match {
                case Some(newBase) => newBase % mod match {
                  case Some(newBase2) => loop(newTotal2, newBase2, k >> 1, mod)
                  case None => None
                }
                case None => None
              }
            case None => None
          }
          case None => None
        }
      }
      else {
        base * base match {
          case Some(newBase) => newBase % mod match {
            case Some(newBase2) => loop(total, newBase2, k >> 1, mod)
            case None => None
          }
          case None => None
        }
      }
    }

    (CheckedLong.one % mod).flatMap(loop(_, this, k, mod))
  }

  def abs: Option[CheckedLong]

  def gcd(that: CheckedLong): Option[CheckedLong]
  def lcm(that: CheckedLong): Option[CheckedLong] = {
    if (this.isZero || that.isZero) Some(CheckedLong.zero)
    else {
      for {
        x <- this gcd that
        y <- this / x
        z <- y * that
      } yield z
    }
  }

  def unary_-(): Option[CheckedLong]

  def isValidLong: Boolean
  def getLong: Opt[Long]

  override def toByte: Byte = toLong.toByte
  override def toShort: Short = toLong.toShort
  override def toInt: Int = toLong.toInt
  final def toBigInt: BigInt = toBigInteger
  def toBigDecimal: BigDecimal
  private[math] def toBigInteger: BigInteger

  override def toString: String =
    this match {
      case CheckedLongLong(n)       => n.toString
      case CheckedLongBigInteger(n) => n.toString
    }

  final def isWhole: Boolean = true

  final def isProbablePrime(c: Int): Boolean =
    toBigInteger.isProbablePrime(c)

  def bitLength: Int
}

object CheckedLong extends SafeLongInstances {

  final val minusOne: CheckedLong = CheckedLongLong(-1L)
  final val zero: CheckedLong = CheckedLongLong(0L)
  final val one: CheckedLong = CheckedLongLong(1L)
  final val two: CheckedLong = CheckedLongLong(2L)
  final val three: CheckedLong = CheckedLongLong(3L)
  final val ten: CheckedLong = CheckedLongLong(10L)

  private[spire] final val big64: BigInteger = BigInteger.ONE.shiftLeft(63)
  private[spire] final val safe64: CheckedLong = CheckedLong(big64)

  implicit def apply(x: Long): CheckedLong = CheckedLongLong(x)

  implicit def apply(x: BigInt): CheckedLong =
    if (x.isValidLong) CheckedLongLong(x.toLong) else CheckedLongBigInteger(x.bigInteger)

  private[math] def apply(s: String): CheckedLong =
    try {
      CheckedLong(java.lang.Long.parseLong(s))
    } catch {
      case _: Exception => CheckedLong(new BigInteger(s))
    }

  def longGcd(x: Long, y: Long): CheckedLong = {
    def absWrap(x: Long): CheckedLong =
      if (x >= 0) CheckedLong(x)
      else if (x == Long.MinValue) CheckedLong.safe64
      else CheckedLong(-x)

    if (x == 0) absWrap(y)
    else if (y == 0) absWrap(x)
    else if (x == Long.MinValue) {
      if (y == Long.MinValue) CheckedLong.safe64
      else spire.math.gcd(y, x % y)
    } else if (y == Long.MinValue) CheckedLongLong(spire.math.gcd(x, y % x))
    else CheckedLongLong(spire.math.gcd(x, y % x))
  }

  def mixedGcd(x: Long, y: BigInteger): CheckedLong =
    if (y.signum == 0) {
      if (x >= 0) CheckedLongLong(x)
      else if (x == Long.MinValue) CheckedLong.safe64
      else CheckedLongLong(-x)
    } else if (x == 0L) {
      CheckedLong(y.abs)
    } else if (x == Long.MinValue) {
      CheckedLong(CheckedLong.big64 gcd y)
    } else {
      CheckedLongLong(spire.math.gcd(x, (y remainder BigInteger.valueOf(x)).longValue))
    }
}

private[math] final case class CheckedLongLong(x: Long) extends CheckedLong {

  def isZero: Boolean = x == 0L
  def isOne: Boolean = x == 1L
  def isOdd: Boolean = (x & 1L) != 0
  def isEven: Boolean = (x & 1L) == 0
  def signum: Int = java.lang.Long.signum(x)

  def +(y: Long): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(x + y))(CheckedLongBigInteger(BigInteger.valueOf(x) add BigInteger.valueOf(y)))

  def -(y: Long): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(x - y))(CheckedLongBigInteger(BigInteger.valueOf(x) subtract BigInteger.valueOf(y)))

  def *(y: Long): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(x * y))(CheckedLongBigInteger(BigInteger.valueOf(x) multiply BigInteger.valueOf(y)))

  def /(y: Long): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(x / y))(CheckedLong.safe64)

  def %(y: Long): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(x % y))(CheckedLong.zero)

  def /%(y: Long): (CheckedLong, CheckedLong) =
    if (x == Long.MinValue && y == -1L)
      (CheckedLong.safe64, CheckedLong.zero)
    else
      (CheckedLongLong(x / y), CheckedLongLong(x % y))

  def &(y: Long): CheckedLong = CheckedLongLong(x & y)
  def |(y: Long): CheckedLong = CheckedLongLong(x | y)
  def ^(y: Long): CheckedLong = CheckedLongLong(x ^ y)

  def +(y: BigInteger): CheckedLong =
    if (y.bitLength <= 63) this + y.longValue
    else CheckedLong(BigInteger.valueOf(x) add y)

  def -(y: BigInteger): CheckedLong =
    if (y.bitLength <= 63) this - y.longValue
    else CheckedLong(BigInteger.valueOf(x) subtract y)

  def *(y: BigInteger): CheckedLong =
    if (y.bitLength <= 63) this * y.longValue
    else CheckedLong(BigInteger.valueOf(x) multiply y)

  def /(y: BigInteger): CheckedLong =
    if (y.bitLength <= 63) this / y.longValue
    else if (x == Long.MinValue && (y equals CheckedLong.big64)) CheckedLong.minusOne
    else CheckedLong.zero

  def %(y: BigInteger): CheckedLong =
    if (y.bitLength <= 63) this % y.longValue
    else if (x == Long.MinValue && (y equals CheckedLong.big64)) CheckedLong.zero
    else this

  def /%(y: BigInteger): (CheckedLong, CheckedLong) =
    if (y.bitLength <= 63) this /% y.longValue
    else if (x == Long.MinValue && (y equals CheckedLong.big64)) (CheckedLong.minusOne, CheckedLong.zero)
    else (CheckedLong.zero, this)

  def &(y: BigInteger): CheckedLong = CheckedLong(BigInteger.valueOf(x) and y)
  def |(y: BigInteger): CheckedLong = CheckedLong(BigInteger.valueOf(x) or y)
  def ^(y: BigInteger): CheckedLong = CheckedLong(BigInteger.valueOf(x) xor y)

  def unary_-(): CheckedLong =
    Checked.tryOrReturn[CheckedLong](CheckedLongLong(-x))(CheckedLongBigInteger(BigInteger.valueOf(x).negate()))

  override def <(that: CheckedLong): Boolean =
    that match {
      case CheckedLongLong(y)       => x < y
      case CheckedLongBigInteger(y) => y.signum > 0
    }

  override def <=(that: CheckedLong): Boolean =
    that match {
      case CheckedLongLong(y)       => x <= y
      case CheckedLongBigInteger(y) => y.signum > 0
    }

  override def >(that: CheckedLong): Boolean =
    that match {
      case CheckedLongLong(y)       => x > y
      case CheckedLongBigInteger(y) => y.signum < 0
    }

  override def >=(that: CheckedLong): Boolean =
    that match {
      case CheckedLongLong(y)       => x >= y
      case CheckedLongBigInteger(y) => y.signum < 0
    }

  def compare(that: CheckedLong): Int =
    that match {
      case CheckedLongLong(y)       =>
        x compare y
      case CheckedLongBigInteger(y) =>
        -y.signum
    }

  def <<(n: Int): CheckedLong = {
    if (x == 0) return this
    if (n < 0) return this >> -n
    if (n < 64) {
      if (x >= 0) {
        if (x <= (0x7fffffffffffffffL >> n)) return CheckedLongLong(x << n)
      } else {
        if (x >= (0x8000000000000000L >> n)) return CheckedLongLong(x << n)
      }
    }
    CheckedLongBigInteger(BigInteger.valueOf(x).shiftLeft(n))
  }

  def >>(n: Int): CheckedLong =
    if (n >= 64) (if (x >= 0) CheckedLong.zero else CheckedLong.minusOne)
    else if (n >= 0) CheckedLongLong(x >> n)
    else if (n == Int.MinValue) throw new ArithmeticException(">> MinValue not supported")
    else this << -n

  override def equals(that: Any): Boolean =
    that match {
      case CheckedLongLong(y)       => x == y
      case CheckedLongBigInteger(y) => false
      case that: BigInt             => if (that.bitLength > 63) false else that.toLong == x
      case that                     => that == x
    }

  def abs: CheckedLong =
    if (x >= 0) this
    else if (x == Long.MinValue) CheckedLong.safe64
    else CheckedLong(-x)

  def gcd(that: CheckedLong): CheckedLong =
    that match {
      case CheckedLongLong(y)       => CheckedLong.longGcd(x, y)
      case CheckedLongBigInteger(y) => CheckedLong.mixedGcd(x, y)
    }

  def doubleValue: Double = x.toDouble
  def floatValue: Float = x.toFloat
  def longValue: Long = x.toLong
  def intValue: Int = x.toInt

  def underlying: java.lang.Long = new java.lang.Long(x)
  def isValidLong: Boolean = true
  def getLong: Opt[Long] = Opt(x)

  override def toLong: Long = x
  def toBigInteger: BigInteger = BigInteger.valueOf(x)
  def toBigDecimal: BigDecimal = BigDecimal(x)

  def bitLength: Int = 64 - java.lang.Long.numberOfLeadingZeros(x)
}

private[math] final case class CheckedLongBigInteger(x: BigInteger) extends CheckedLong {

  def isZero: Boolean = false // 0 will always be represented as a SafeLongLong
  def isOne: Boolean = false // 1 will always be represented as a SafeLongLong
  def isOdd: Boolean = x.testBit(0)
  def isEven: Boolean = !x.testBit(0)
  def signum: Int = x.signum

  def +(y: Long): CheckedLong =
    if ((x.signum ^ y) < 0) CheckedLong(x add BigInteger.valueOf(y)) else CheckedLongBigInteger(x add BigInteger.valueOf(y))

  def -(y: Long): CheckedLong =
    if ((x.signum ^ y) >= 0) CheckedLong(x subtract BigInteger.valueOf(y)) else CheckedLongBigInteger(x subtract BigInteger.valueOf(y))

  def *(y: Long): CheckedLong = CheckedLong(x multiply BigInteger.valueOf(y))

  def /(y: Long): CheckedLong = CheckedLong(x divide BigInteger.valueOf(y))

  def %(y: Long): CheckedLong = CheckedLong(x remainder BigInteger.valueOf(y))

  def /%(y: Long): (CheckedLong, CheckedLong) = {
    val Array(q, r) = x.divideAndRemainder(BigInteger.valueOf(y))
    (CheckedLong(q), CheckedLong(r))
  }

  def &(y: Long): CheckedLong = CheckedLong(x and BigInteger.valueOf(y))
  def |(y: Long): CheckedLong = CheckedLong(x or BigInteger.valueOf(y))
  def ^(y: Long): CheckedLong = CheckedLong(x xor BigInteger.valueOf(y))

  def +(y: BigInteger): CheckedLong =
    if ((x.signum ^ y.signum) < 0) CheckedLong(x add y) else CheckedLongBigInteger(x add y)

  def -(y: BigInteger): CheckedLong =
    if ((x.signum ^ y.signum) < 0) CheckedLongBigInteger(x subtract y) else CheckedLong(x subtract y)

  def *(y: BigInteger): CheckedLong = CheckedLong(x multiply y)

  def /(y: BigInteger): CheckedLong = CheckedLong(x divide y)

  def %(y: BigInteger): CheckedLong = CheckedLong(x remainder y)

  def /%(y: BigInteger): (CheckedLong, CheckedLong) = {
    val Array(q, r) = x divideAndRemainder y
    (CheckedLong(q), CheckedLong(r))
  }

  def &(y: BigInteger): CheckedLong = CheckedLong(x and y)
  def |(y: BigInteger): CheckedLong = CheckedLong(x or y)
  def ^(y: BigInteger): CheckedLong = CheckedLong(x xor y)

  def unary_-(): CheckedLong = CheckedLong(x.negate())

  def compare(that: CheckedLong): Int =
    that match {
      case CheckedLongLong(y)       =>
        x.signum
      case CheckedLongBigInteger(y) =>
        x compareTo y
    }

  def <<(n: Int): CheckedLong = CheckedLong(x.shiftLeft(n))
  def >>(n: Int): CheckedLong = CheckedLong(x.shiftRight(n))

  override def equals(that: Any): Boolean =
    that match {
      case CheckedLongLong(y)       => false
      case CheckedLongBigInteger(y) => x == y
      case that: BigInt             => x equals that.bigInteger
      case that                     => that == BigInt(x)
    }

  def abs: CheckedLong =
    if (x.signum >= 0) this
    else CheckedLongBigInteger(x.negate())

  def gcd(that: CheckedLong): CheckedLong =
    that match {
      case CheckedLongLong(y)       => CheckedLong.mixedGcd(y, x)
      case CheckedLongBigInteger(y) => CheckedLong(x gcd y)
    }

  def doubleValue: Double = x.doubleValue
  def floatValue: Float = x.floatValue
  def longValue: Long = x.longValue
  def intValue: Int = x.intValue
  override def isValidByte: Boolean = false
  override def isValidShort: Boolean = false
  override def isValidInt: Boolean = false
  override def isValidLong: Boolean = false
  override def isValidChar: Boolean = false

  def underlying: BigInt = BigInt(x)

  def getLong: Opt[Long] = Opt.empty[Long]

  override def toLong: Long = x.longValue
  def toBigInteger: BigInteger = x
  def toBigDecimal: BigDecimal = BigDecimal(x)

  def bitLength: Int = x.bitLength
}

trait SafeLongInstances {
  @SerialVersionUID(1L)
  implicit object SafeLongAlgebra extends SafeLongIsEuclideanRing with SafeLongIsNRoot with Serializable

  @SerialVersionUID(1L)
  implicit object SafeLongIsReal extends SafeLongIsReal with Serializable

  implicit final val SafeLongTag = new NumberTag.LargeTag[CheckedLong](NumberTag.Integral, CheckedLong.zero)
}

private[math] trait SafeLongIsCRing extends CRing[CheckedLong] {
  override def minus(a:CheckedLong, b:CheckedLong): CheckedLong = a - b
  def negate(a:CheckedLong): CheckedLong = -a
  val one: CheckedLong = CheckedLong.one
  def plus(a:CheckedLong, b:CheckedLong): CheckedLong = a + b
  override def pow(a:CheckedLong, b:Int): CheckedLong = a pow b
  override def times(a:CheckedLong, b:CheckedLong): CheckedLong = a * b
  val zero: CheckedLong = CheckedLong.zero

  override def fromInt(n: Int): CheckedLong = CheckedLong(n)
}

private[math] trait SafeLongIsGCDRing extends GCDRing[CheckedLong] with SafeLongIsCRing {
  def lcm(a:CheckedLong, b:CheckedLong)(implicit ev: Eq[CheckedLong]): CheckedLong = a lcm b
  def gcd(a:CheckedLong, b:CheckedLong)(implicit ev: Eq[CheckedLong]): CheckedLong = a gcd b
}

private[math] trait SafeLongIsEuclideanRing extends EuclideanRing[CheckedLong] with SafeLongIsGCDRing {
  def euclideanFunction(a:CheckedLong): BigInt = a.abs.toBigInt
  def quot(a:CheckedLong, b:CheckedLong): CheckedLong = a / b
  def mod(a:CheckedLong, b:CheckedLong): CheckedLong = a % b
  override def quotmod(a:CheckedLong, b:CheckedLong): (CheckedLong, CheckedLong) = a /% b
  override def lcm(a:CheckedLong, b:CheckedLong)(implicit ev: Eq[CheckedLong]): CheckedLong = a lcm b
  override def gcd(a:CheckedLong, b:CheckedLong)(implicit ev: Eq[CheckedLong]): CheckedLong = a gcd b
}

private[math] trait SafeLongIsNRoot extends NRoot[CheckedLong] {
  def nroot(a: CheckedLong, k: Int): CheckedLong =
    a match {
      case CheckedLongLong(n)       => CheckedLong(NRoot[Long].nroot(n, k))
      case CheckedLongBigInteger(n) => CheckedLong(NRoot[BigInteger].nroot(n, k))
    }

  def fpow(a: CheckedLong, b: CheckedLong): CheckedLong =
    if (b.isValidInt) a.pow(b.toInt)
    else CheckedLong(NRoot[BigInteger].fpow(a.toBigInteger, b.toBigInteger))
}

private[math] trait SafeLongOrder extends Order[CheckedLong] {
  override def eqv(x: CheckedLong, y: CheckedLong): Boolean = x == y
  override def neqv(x: CheckedLong, y: CheckedLong): Boolean = x != y
  override def gt(x: CheckedLong, y: CheckedLong): Boolean = x > y
  override def gteqv(x: CheckedLong, y: CheckedLong): Boolean = x >= y
  override def lt(x: CheckedLong, y: CheckedLong): Boolean = x < y
  override def lteqv(x: CheckedLong, y: CheckedLong): Boolean = x <= y
  def compare(x: CheckedLong, y: CheckedLong): Int = x compare y
}

private[math] trait SafeLongSigned extends Signed[CheckedLong] with SafeLongOrder {
  override def signum(a: CheckedLong): Int = a.signum
  override def abs(a: CheckedLong): CheckedLong = a.abs
}

private[math] trait SafeLongIsReal extends IsIntegral[CheckedLong] with SafeLongSigned {
  def toDouble(n: CheckedLong): Double = n.toDouble
  def toBigInt(n: CheckedLong): BigInt = n.toBigInt
}
