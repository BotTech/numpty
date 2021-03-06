package nz.co.bottech.numpty

import nz.co.bottech.numpty.BoundedNumeric._

import scala.annotation.tailrec

object BoundedNumericOps {

  // TODO: We could use lots of refined types in here.

  def plus[T](x: T, y: T)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import numeric._
    val result = x + y
    if (signum(y) > 0) checkIncrement(x, result)
    else checkDecrement(x, result)
  }

  def minus[T](x: T, y: T)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import numeric._
    val result = x - y
    if (signum(y) < 0) checkIncrement(x, result)
    else checkDecrement(x, result)
  }

  def times[T](x: T, y: T)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import numeric._

    def fractional(z: T): Boolean = {
      if (signum(z) >= 0) z <= one
      else z >= -one
    }

    // The count may be positive or negative but the step must be positive
    def countWithSmallPosStep: Either[NumericError[T], (T, T)] = {
      // TODO: These should be either greater than one or less than negative one
      if (signum(x) >= 0) {
        if (signum(y) >= 0) {
          if (x > y) {
            Right((y, x))
          } else {
            Right((x, y))
          }
        } else {
          // x > 1, y < -1, result will be <= y and <= -x.
          // Negating x is safe.
          // Negating y is only safe once we know that -y <= x.
          this.negate(x).valueOrError.flatMap { negX =>
            if (negX >= y) {
              Right((y, x))
            } else {
              this.negate(y).valueOrError.map { posY =>
                (negX, posY)
              }
            }
          }
        }
      } else {
        if (signum(y) >= 0) {
          // x < -1, y > 1, result will be <= x and <= -y.
          // Negating y is safe.
          // Negating x is only safe once we know that -x <= y.
          this.negate(y).valueOrError.flatMap { negY =>
            if (negY >= x) {
              Right((x, y))
            } else {
              this.negate(x).valueOrError.map { posX =>
                (negY, posX)
              }
            }
          }
        } else {
          // x <= -1, y <= -1, result will be >= -x and >= -y.
          // Negating x and y is safe.
          for {
            posX <- this.negate(x).valueOrError
            posY <- this.negate(y).valueOrError
          } yield if (posX > posY) (posY, posX) else (posX, posY)
        }
      }
    }

    // TODO: Can we use a tupled BoundedNumeric here?
    @tailrec
    def doubles(step: T, count: T, previous: List[(T, T)], end: T): Either[NumericError[T], List[(T, T)]] = {
      assert(count <= end)
      if (count == end) Right(previous)
      else this.plus(count, count).valueOrError match {
        case Right(doubleCount) if doubleCount <= end => this.plus(step, step).valueOrError match {
          case Left(error)       => Left(error)
          case Right(doubleStep) => doubles(doubleStep, doubleCount, (doubleStep, doubleCount) :: previous, end)
        }
        case _                                        => Right(previous)
      }
    }

    @tailrec
    def addDoubles(acc: T, num: T, dbls: List[(T, T)], end: T, firstStep: T): BoundedNumeric[T] = {
      assert(num <= end)
      if (num == end) BoundedNumeric(acc)
      else dbls match {
        case (step, count) :: tail => this.plus(num, count).valueOrError match {
          case Right(nextNum) if nextNum <= end => this.plus(acc, step).valueOrError match {
            case Left(error)    => error
            case Right(nextAcc) => addDoubles(nextAcc, nextNum, tail, end, firstStep)
          }
          case _                                => addDoubles(acc, num, tail, end, firstStep)
        }
        case _                     => this.minus(end, num).valueOrError.map { remaining =>
          assert(remaining < one)
          assert(remaining > -one)
          this.plus(firstStep * remaining, acc)
        }.merge
      }
    }

    if (fractional(x) || fractional(y)) {
      BoundedNumeric(x * y)
    } else {
      val result = for {
        countAndStep <- countWithSmallPosStep
        (count, step) = countAndStep
        dbls <- doubles(step, one, (step, one) :: Nil, count)
      } yield addDoubles(zero, zero, dbls, count, step)
      result.merge
    }
  }

  def negate[T](x: T)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    minus(numeric.zero, x)
  }

  def fromInt[T](x: Int)(implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    BoundedNumeric(numeric.fromInt(x))
  }

  private def checkIncrement[T](x: T, result: T)
                               (implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import numeric._
    if (result < x) Overflow(result, bounds)
    else BoundedNumeric(result)
  }

  private def checkDecrement[T](x: T, result: T)
                               (implicit numeric: Numeric[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import numeric._
    if (result > x) Underflow(result, bounds)
    else BoundedNumeric(result)
  }
}
