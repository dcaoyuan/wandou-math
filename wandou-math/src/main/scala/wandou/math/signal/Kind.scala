package wandou.math.signal

/**
 *
 * @author Caoyuan Deng
 */
class Kind(protected[signal] val id: Int) {
  def this() = this(0) /* for serializable */

  def isSide: Boolean = id > 0
  def isCorner: Boolean = id < 0

  override def hashCode = id

  override def equals(a: Any) = a match {
    case x: Kind => x.id == id
    case _       => false
  }

  override def toString = id match {
    case 1  => "Enter long"
    case 2  => "Exit long"
    case 3  => "Enter short"
    case 4  => "Exit short"
    case 5  => "Enter picking"
    case 6  => "Exit picking"
    case 7  => "Cut loss"
    case 8  => "Take profit"

    case -1 => "Upper"
    case -2 => "Lower"
  }
}

object Kind {
  def withId(id: Int): Kind = id match {
    case 1  => Side.EnterLong
    case 2  => Side.ExitLong
    case 3  => Side.EnterShort
    case 4  => Side.ExitShort
    case 5  => Side.EnterPicking
    case 6  => Side.ExitPicking
    case 7  => Side.CutLoss
    case 8  => Side.TakeProfit

    case -1 => Corner.Upper
    case -2 => Corner.Lower
  }
}

final class Side(_id: => Int) extends Kind(_id) {
  def this() = this(0) /* for serializable */
}

object Side {
  val EnterLong = new Side(1)
  val ExitLong = new Side(2)
  val EnterShort = new Side(3)
  val ExitShort = new Side(4)
  val EnterPicking = new Side(5)
  val ExitPicking = new Side(6)
  val CutLoss = new Side(7)
  val TakeProfit = new Side(8)

  def withId(id: Int): Side = Kind.withId(id).asInstanceOf[Side]
}

final class Corner(_id: => Int) extends Kind(_id) {
  def this() = this(0) /* for serializable */
}

object Corner {
  val Upper = new Corner(-1)
  val Lower = new Corner(-2)

  def withId(id: Int): Corner = Kind.withId(id).asInstanceOf[Corner]
}

