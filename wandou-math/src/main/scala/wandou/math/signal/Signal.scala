package wandou.math.signal

import akka.actor.Actor
import akka.actor.ActorLogging
import wandou.actors.Publisher
import wandou.math.indicator.SignalIndicator
import java.awt.Color

/**
 *
 * @author Caoyuan Deng
 */
final case class SignalEvent(source: SignalIndicator, signal: Signal)
final case class SignalsEvent(source: SignalIndicator, signals: Array[Signal])

/** Helper classes to carray full information of signal/signals */
final case class SignalX(symbol: String, name: String, freq: String, signal: Signal) {
  def this() = this(null, null, null, null) /* for serializable */
}
final case class SignalsX(symbol: String, name: String, freq: String, signals: Array[Signal]) {
  def this() = this(null, null, null, null) /* for serializable */
}

/**
 * @note We have to write Signal and Sign/Mark as here for avro serializer:
 *   Signal coundn't be abstract class
 *   Don't write Sign/Mark as case class
 */
final class Sign private (time: => Long,
                          kind: => Side,
                          id: => Int,
                          text: => String,
                          color: => Color) extends Signal(time, kind, id, text, color) {
  def this() = this(0L, Side.EnterLong, 0, null, null) /* for serializable */

  override def kind = super.kind.asInstanceOf[Side]
}

object Sign {
  def apply(time: Long, kind: Side, id: Int = 0, text: String = null, color: Color = null) = new Sign(time, kind, id, text, color)
  def unapply(v: Signal): Option[(Long, Side, Int, String, Color)] = v.kind match {
    case side: Side => Some((v.time, side, v.id, v.text, v.color))
    case _          => None
  }
}

final class Mark private (time: => Long,
                          kind: => Corner,
                          id: => Int,
                          text: => String,
                          color: => Color) extends Signal(time, kind, id, text, color) {
  def this() = this(0L, Corner.Lower, 0, null, null) /* for serializable */

  override def kind = super.kind.asInstanceOf[Corner]
}

object Mark {
  def apply(time: Long, kind: Corner, id: Int = 0, text: String = null, color: Color = null) = new Mark(time, kind, id, text, color)
  def unapply(v: Signal): Option[(Long, Corner, Int, String, Color)] = v.kind match {
    case corner: Corner => Some((v.time, corner, v.id, v.text, v.color))
    case _              => None
  }
}

class Signal(val time: Long, _kind: Kind, val id: Int = 0, val text: String = null, @transient val color: Color = null) {
  def this() = this(0L, null) /* for serializable */

  def kind: Kind = _kind

  def isSign = kind.isSide
  def isMark = kind.isCorner

  def hasText = text != null

  override def hashCode: Int = {
    var h = 17
    h = 37 * h + (time ^ (time >>> 32)).toInt
    h = 37 * h + kind.hashCode
    h = 37 * h + id
    if (text != null) h = 37 * h + text.hashCode
    if (color != null) h = 37 * h + color.hashCode
    h
  }

  override def equals(a: Any): Boolean = {
    a match {
      case x: Signal => x.time == time && x.kind == kind && x.id == id && x.text == text && x.color == color
      case _         => false
    }
  }

  override def toString = {
    val sb = new StringBuilder()
    sb.append(this.getClass.getSimpleName).append("(")
    sb.append(time).append(",").append(kind).append(",").append(id).append(",").append(text).append(",").append(color)
    sb.append(")").toString
  }
}

object Signal extends Actor with ActorLogging with Publisher {

  def receive = publisherBehavior

  // --- simple test
  def main(args: Array[String]) {
    try {
      val corner = new Corner(-1)
      val matchCorner = corner match {
        case Corner.Lower => false
        case Corner.Upper => true
      }
      assert(matchCorner)
      println(matchCorner)

      val side = new Side(4)
      val matchSide = side match {
        case Side.EnterLong => false
        case Side.ExitShort => true
      }
      assert(matchSide)
      println(matchSide)

      val sign = new Signal(0, Side.EnterLong)
      val matchSign = sign match {
        case Mark(time, kind, _, _, _) =>
          println(kind); false
        case Sign(time, kind, _, _, _) =>
          println(kind); true
        case _ => false
      }
      assert(matchSign)
      println(matchSign)

      val mark = new Signal(0, Corner.Lower)
      val matchMark = mark match {
        case Sign(time, kind, _, _, _) =>
          println(kind); false
        case Mark(time, kind, _, _, _) =>
          println(kind); true
        case _ => false
      }
      assert(matchMark)
      println(matchMark)

      System.exit(0)
    } catch {
      case ex: Throwable => ex.printStackTrace; System.exit(1)
    }
  }

}
