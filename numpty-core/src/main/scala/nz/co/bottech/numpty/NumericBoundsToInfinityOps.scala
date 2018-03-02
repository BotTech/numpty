package nz.co.bottech.numpty

import nz.co.bottech.numpty.BoundedNumeric._
import nz.co.bottech.numpty.NumericBounds.NumericBoundsToInfinity

object NumericBoundsToInfinityOps {

  def times[T](x: T, y: T)(implicit numeric: Numeric[T], bounds: NumericBoundsToInfinity[T]): BoundedNumeric[T] = {
    checkResult(x, y, numeric.times)
  }

  private def checkResult[T](x: T, y: T, operation: (T, T) => T)
                            (implicit numeric: Numeric[T], bounds: NumericBoundsToInfinity[T]): BoundedNumeric[T] = {
    import bounds._
    val result = operation(x, y)
    if (result == positiveInfinity) AboveUpperBound(result, bounds)
    else if (result == negativeInfinity) BelowLowerBound(result, bounds)
    else BoundedNumeric(result)
  }
}
