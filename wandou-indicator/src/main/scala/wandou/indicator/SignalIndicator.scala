package wandou.indicator

import java.awt.Color
import wandou.math.signal.Corner
import wandou.math.signal.Kind
import wandou.math.signal.Mark
import wandou.math.signal.Side
import wandou.math.signal.Sign
import wandou.math.signal.Signal
import wandou.math.signal.SignalEvent
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TSerEvent

/**
 * Abstract Signal Indicator
 *
 * @author Caoyuan Deng
 */
abstract class SignalIndicator(_baseSer: TBaseSer) extends Indicator(_baseSer) with wandou.math.indicator.SignalIndicator {

  isOverlapping = true

  //val signalVar = new SparseTVar[Signal](SignalIndicator.SIGNAL_VAR_KEY, Plot.Signal)
  val signalVar = TVar[List[Signal]](SignalIndicator.SIGNAL_VAR_KEY)

  def this() = this(null)

  /**
   * @return (signal, is new one)
   */
  private def signal[T <: Signal](idx: Int, kind: Kind, id: Int, text: String, color: Color): (T, Boolean) = {
    val time = baseSer.timestamps(idx)

    val signal = kind match {
      case x: Side   => Sign(time, x, id, text, color)
      case x: Corner => Mark(time, x, id, text, color)
    }

    signalVar(idx) match {
      case null =>
        signalVar(idx) = List(signal)

        (signal.asInstanceOf[T], true)
      case xs =>
        val (existOnes, others) = xs partition (_ == signal)
        signalVar(idx) = signal :: others

        (signal.asInstanceOf[T], existOnes.isEmpty)
    }
  }

  protected def mark(idx: Int, corner: Corner): Mark = {
    mark(idx, corner, 0, null, null)
  }

  protected def mark(idx: Int, corner: Corner, id: Int): Mark = {
    mark(idx, corner, id, null, null)
  }

  protected def mark(idx: Int, corner: Corner, color: Color): Mark = {
    mark(idx, corner, 0, null, color)
  }

  protected def mark(idx: Int, corner: Corner, id: Int, color: Color): Mark = {
    mark(idx, corner, id, null, color)
  }

  protected def mark(idx: Int, corner: Corner, text: String): Mark = {
    mark(idx, corner, 0, text, null)
  }

  protected def mark(idx: Int, corner: Corner, id: Int, text: String): Mark = {
    mark(idx, corner, id, text, null)
  }

  protected def mark(idx: Int, corner: Corner, text: String, color: Color): Mark = {
    signal[Mark](idx, corner, 0, text, color)._1
  }

  protected def mark(idx: Int, corner: Corner, id: Int = 0, text: String = null, color: Color = null): Mark = {
    signal[Mark](idx, corner, id, text, color)._1
  }

  protected def sign(idx: Int, side: Side): Sign = {
    sign(idx, side, 0, null, null)
  }

  protected def sign(idx: Int, side: Side, id: Int): Sign = {
    sign(idx, side, id, null, null)
  }

  protected def sign(idx: Int, side: Side, color: Color): Sign = {
    sign(idx, side, 0, null, color)
  }

  protected def sign(idx: Int, side: Side, id: Int, color: Color): Sign = {
    sign(idx, side, id, null, color)
  }

  protected def sign(idx: Int, side: Side, text: String): Sign = {
    sign(idx, side, 0, text, null)
  }

  protected def sign(idx: Int, side: Side, id: Int, text: String): Sign = {
    sign(idx, side, id, text, null)
  }

  protected def sign(idx: Int, side: Side, text: String, color: Color): Sign = {
    sign(idx, side, 0, text, color)
  }

  protected def sign(idx: Int, side: Side, id: Int = 0, text: String = null, color: Color = null): Sign = {
    val (sign, isNewOne) = signal[Sign](idx, side, id, text, color)
    if (isNewOne) {
      log.info("Signal: " + sign)
      Signal.publish(SignalEvent(this, sign))
    }
    sign
  }

  protected def remove(idx: Int) {
    signalVar(idx) = null
  }

  protected def remove(idx: Int, kind: Kind) {
    remove(idx, kind, 0, null, null)
  }

  protected def remove(idx: Int, id: Int) {
    remove(idx, null, 0, null, null)
  }

  protected def remove(idx: Int, text: String) {
    remove(idx, null, 0, text, null)
  }

  protected def remove(idx: Int, color: Color) {
    remove(idx, null, 0, null, color)
  }

  protected def remove(idx: Int, kind: Kind, id: Int) {
    remove(idx, kind, id, null, null)
  }

  protected def remove(idx: Int, kind: Kind, text: String) {
    remove(idx, kind, 0, text, null)
  }

  protected def remove(idx: Int, kind: Kind, color: Color) {
    remove(idx, kind, 0, null, color)
  }

  protected def remove(idx: Int, kind: Kind, id: Int, text: String) {
    remove(idx, kind, id, text, null)
  }

  protected def remove(idx: Int, kind: Kind, id: Int, color: Color) {
    remove(idx, kind, id, null, color)
  }

  protected def remove(idx: Int, kind: Kind, text: String, color: Color) {
    remove(idx, kind, 0, text, color)
  }

  protected def remove(idx: Int, kind: Kind = null, id: Int = 0, text: String = null, color: Color = null) {
    signalVar(idx) match {
      case null =>
      case xs => signalVar(idx) = xs filterNot { x =>
        x.id == id &&
          (if (kind == null) true else x.kind == kind) &&
          (if (text == null) true else x.text == text) &&
          (if (color == null) true else x.color == color)
      }
    }
  }

  protected def addSignal(signal: Signal) {
    val time = signal.time
    if (exists(time)) {
      signalVar(time) = signalVar(time) match {
        case null => List(signal)
        case xs =>
          val (existOnes, others) = xs partition (_ == signal)
          signal :: others
      }
    }

    publish(TSerEvent.Computed(this, null, time, time, null, null))
  }

  protected def addSignals(signals: Array[Signal]) {
    var frTime = Long.MaxValue
    var toTime = Long.MinValue
    var i = 0
    while (i < signals.length) {
      val signal = signals(i)
      val time = signal.time
      if (exists(time)) {
        signalVar(time) = signalVar(time) match {
          case null => List(signal)
          case xs =>
            val (existOnes, others) = xs partition (_ == signal)
            signal :: others
        }
      }

      frTime = math.min(frTime, time)
      toTime = math.max(toTime, time)

      i += 1
    }

    if (signals.length > 0) {
      publish(TSerEvent.Computed(this, null, frTime, toTime, null, null))
    }
  }

}

object SignalIndicator {
  val SIGNAL_VAR_KEY = "*"
}

