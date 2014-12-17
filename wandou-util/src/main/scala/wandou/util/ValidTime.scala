package wandou.util

/**
 * A class to get the valid time of something
 *
 * @param ref       What the valid time is talking about
 * @param validFrom the time valid from, included
 * @param validTo   the time valid to, included.
 *
 * @author Caoyuan Deng
 */
final case class ValidTime[T](ref: T, var validFrom: Long, var validTo: Long) extends Ordered[ValidTime[T]] {

  /**
   * time >= validFrom && (validTo == 0 || time <= validTo)
   */
  def isValid(time: Long): Boolean = time >= validFrom && (validTo == 0 || time <= validTo)
  def nonValid(time: Long): Boolean = !isValid(time)

  def isIn(prevTime: Long, time: Long): Boolean = nonValid(prevTime) && isValid(time)
  def isOut(prevTime: Long, time: Long): Boolean = isValid(prevTime) && nonValid(time)

  override def hashCode = {
    var hash = 7
    hash = 59 * hash + ref.hashCode
    hash = 59 * hash + (validFrom ^ (validFrom >>> 32)).toInt
    hash = 59 * hash + (validTo ^ (validTo >>> 32)).toInt
    hash
  }

  override def equals(a: Any) = a match {
    case that: ValidTime[T] => that.ref == this.ref && that.validFrom == this.validFrom && that.validTo == this.validTo
    case _                  => false
  }

  def compare(that: ValidTime[T]): Int = {
    if (validFrom == that.validFrom) {
      validTo compare that.validTo
    } else if (validFrom < that.validFrom) {
      -1
    } else {
      1
    }
  }

  override def toString = "" + validFrom + " - " + validTo + ": " + ref
}
