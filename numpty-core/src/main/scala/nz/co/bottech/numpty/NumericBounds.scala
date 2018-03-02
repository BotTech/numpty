package nz.co.bottech.numpty

// TODO: Replace this with an Interval
trait NumericBounds[T] {

  def upper: T

  def lower: T
}

object NumericBounds {

  // TODO: Refined already has a Min and Max!

  implicit object ByteBounds extends NumericBounds[Byte] {

    override val lower: Byte = Byte.MinValue
    override val upper: Byte = Byte.MaxValue
  }

  implicit object CharBounds extends NumericBounds[Char] {

    override val lower: Char = Char.MinValue
    override val upper: Char = Char.MaxValue
  }

  implicit object IntBounds extends NumericBounds[Int] {

    override val lower: Int = Int.MinValue
    override val upper: Int = Int.MaxValue
  }

  implicit object LongBounds extends NumericBounds[Long] {

    override val lower: Long = Long.MinValue
    override val upper: Long = Long.MaxValue
  }

  implicit object ShortBounds extends NumericBounds[Short] {

    override val lower: Short = Short.MinValue
    override val upper: Short = Short.MaxValue
  }

  implicit object DoubleBounds extends NumericBounds[Double] with ApproachesInfinity[Double] {

    override val lower: Double = Double.MinValue
    override val upper: Double = Double.MaxValue

    override val positiveInfinity: Double = Double.PositiveInfinity
    override val negativeInfinity: Double = Double.NegativeInfinity
  }

  implicit object FloatBounds extends NumericBounds[Float] with ApproachesInfinity[Float] {

    override val lower: Float = Float.MinValue
    override val upper: Float = Float.MaxValue

    override val positiveInfinity: Float = Float.PositiveInfinity
    override val negativeInfinity: Float = Float.NegativeInfinity
  }

  trait ApproachesInfinity[T] {

    def positiveInfinity: T

    def negativeInfinity: T
  }

  type NumericBoundsToInfinity[T] = NumericBounds[T] with ApproachesInfinity[T]

  def apply[T](lower: T, upper: T): NumericBounds[T] = {
    val low = lower
    val up = upper
    new NumericBounds[T] {
      override val lower: T = low
      override val upper: T = up
    }
  }
}
