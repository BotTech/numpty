package nz.co.bottech.numpty

import nz.co.bottech.numpty.NumericError._

import scala.annotation.tailrec
import simulacrum._

trait NumericBoundsNewBase[T] extends NumericBoundsNew[T] {

  implicit val num: Numeric[T]

  import num._

  // TODO: Need to make the results some WithinBounds[T] that has the value and bounds.
  // TODO: Should we provide a Numeric[WithinBounds[T]] so it can be used as a Numeric?

  def plus(x: T, y: T): Either[NumericError, T] = {
    val result = x + y
    if (signum(y) > 0) checkIncrement(x, result)
    else checkDecrement(x, result)
  }

  def minus(x: T, y: T): Either[NumericError, T] = {
    val result = x - y
    if (signum(y) < 0) checkIncrement(x, result)
    else checkDecrement(x, result)
  }

  def times(x: T, y: T): Either[NumericError, T] = {
    def fractional(z: T): Boolean = {
      if (signum(z) >= 0) z <= one
      else z >= -one
    }

    def countWithSmallPosStep: Either[NumericError, (T, T)] = {
      if (signum(x) >= 0) {
        if (signum(y) >= 0) {
          if (x > y) Right((y, x)) else Right((x, y))
        } else {
          for {
            negX <- negate(x)
            posY <- negate(y)
          } yield if (x > posY) (y, x) else (negX, posY)
        }
      } else {
        if (signum(y) >= 0) {
          for {
            posX <- negate(x)
            negY <- negate(y)
          } yield if (posX > y) (negY, posX) else (x, y)
        } else {
          for {
            posX <- negate(x)
            posY <- negate(y)
          } yield if (posX > posY) (posY, posX) else (posX, posY)
        }
      }
    }

    @tailrec
    def doubles(step: T, count: T, previous: List[(T, T)], end: T): Either[NumericError, List[(T, T)]] = {
      assert(count <= end)
      if (count == end) Right(previous)
      else plus(count, count) match {
        case Right(doubleCount) if doubleCount <= end => plus(step, step) match {
          case Left(error)       => Left(error)
          case Right(doubleStep) => doubles(doubleStep, doubleCount, (doubleStep, doubleCount) :: previous, end)
        }
        case _                                        => Right(previous)
      }
    }

    @tailrec
    def addDoubles(acc: T, num: T, dbls: List[(T, T)], end: T, firstStep: T): Either[NumericError, T] = {
      assert(num <= end)
      if (num == end) Right(acc)
      else dbls match {
        case (step, count) :: tail => plus(num, count) match {
          case Right(nextNum) if nextNum <= end => plus(acc, step) match {
            case Left(error)    => Left(error)
            case Right(nextAcc) => addDoubles(nextAcc, nextNum, tail, end, firstStep)
          }
          case _                                => addDoubles(acc, num, tail, end, firstStep)
        }
        case _                     => minus(end, num).flatMap { remaining =>
          assert(remaining < one)
          assert(remaining > -one)
          plus(firstStep * remaining, acc)
        }
      }
    }

    if (fractional(x) || fractional(y)) checkBounds(x * y)
    else for {
      countAndStep <- countWithSmallPosStep
      (count, step) = countAndStep
      dbls <- doubles(step, one, (step, one) :: Nil, count)
      total <- addDoubles(zero, zero, dbls, count, step)
    } yield total
  }

  def negate(x: T): Either[NumericError, T] = minus(zero, x)

  def fromInt(x: Int): Either[NumericError, T] = checkBounds(num.fromInt(x))

  protected def checkIncrement(x: T, result: T): Either[NumericError, T] = {
    if (result < x) Left(Overflow)
    else checkBounds(result)
  }

  protected def checkDecrement(x: T, result: T): Either[NumericError, T] = {
    if (result > x) Left(Underflow)
    else checkBounds(result)
  }

  // TODO: Have a way of putting a T into a WithinBounds[T].
  protected def checkBounds(x: T): Either[NumericError, T] = {
    if (x < lower) Left(BelowLowerBound(x))
    else if (x > upper) Left(AboveUpperBound(x))
    else if (x >= lower && x <= upper) Right(x)
    else Left(InvalidNumber(x))
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[NumericBoundsNew[_]]

  override def equals(obj: Any): Boolean = obj match {
    case that: NumericBoundsNew[_] => that.canEqual(this) && lower == that.lower && upper == that.upper && num == that.num
    case _                      => false
  }

  override def hashCode(): Int = {
    val prime = 31
    ((prime + lower.hashCode()) * prime + upper.hashCode()) * prime + num.hashCode()
  }

  override def toString: String = s"NumericBoundsNew(lower = $lower, upper = $upper)"
}


