package nz.co.bottech.numpty

import nz.co.bottech.numpty.BoundedNumeric.Overflow

object BoundedIntegralOps {

  // TODO: We could use lots of refined types in here.

  def quot[T](x: T, y: T)(implicit numeric: Integral[T], bounds: NumericBounds[T]): BoundedNumeric[T] = {
    import bounds._
    import numeric._
    val lowerNonPositive = signum(lower) <= 0
    val upperNonNegative = signum(upper) >= 0
    if (BoundedNumericOps.fractional(y)) {
      // 0 <= y <= 1 so the result will be >= x (ignoring y = 0 for now)
      // If upper / inverse(y) < x then the result will be > upper
      // upper / inverse(y) = upper * y
      if (lowerNonPositive && upperNonNegative) {
        // lower <= 0 <= upper
        // 0 <= upper * y <= upper so we can use normal multiplication
        if (upper * y <= x) {
          BoundedNumeric(x / y)
        } else {
          Overflow(x / y, bounds)
        }
      } else {
        // TODO: How can this be done?
        ???
      }
    } else {
      // |y| > 1 so the result will be between 0 and x
      if (lowerNonPositive && upperNonNegative) {
        // lower <= 0 <= upper
        // Either lower <= x <= 0 <= upper
        // or lower <= 0 <= x <= upper
        // Since the result is between 0 and x we can use normal division
        BoundedNumeric(x / y)
      } else if (upperNonNegative) {
        // 0 <= lower <= x <= upper
        // and 0 <= lower <= y <= upper
        // so x / y will be >= 0 which is maybe within the bounds
        // if lower * y <= upper then lower <= x / y <= upper
        BoundedNumericOps.times(lower, y)
          .valueOrError
          .map(_ => BoundedNumeric(x / y))
          .merge
      } else {
        // lower <= x <= upper <= 0
        // and lower <= y <= upper <= 0
        // so x / y will be >= 0 which is > upper
        Overflow(x / y, bounds)
      }
    }
  }
}
