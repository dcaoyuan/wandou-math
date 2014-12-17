package wandou.indicator

import akka.pattern.ask
import wandou.indicator.function._
import wandou.math.indicator.Factor
import wandou.math.indicator.IndicatorHelper
import wandou.math.signal.Side
import wandou.math.timeseries.{ DefaultTSer, TVar, TBaseSer, ThingSer }
import scala.concurrent.duration._

/**
 * @param base series to compute this, not null.
 */
abstract class Indicator(val baseSer: TBaseSer, _factors: Factor*) extends DefaultTSer(baseSer.freq)
    with wandou.math.indicator.Indicator
    with IndicatorHelper {

  def context = baseSer.context

  /**
   * @Note
   * IndicatorHelper should be created here, because it will be used to
   * inject Factor(s): new Factor() will call addFac which delegated
   * by indicatorHelper.addFac(..)
   */
  private var _computedTime = Long.MinValue

  /** To store values of open, high, low, close, volume, amount, closed: */
  protected var O: TVar[Double] = _
  protected var H: TVar[Double] = _
  protected var L: TVar[Double] = _
  protected var C: TVar[Double] = _
  protected var V: TVar[Double] = _
  protected var A: TVar[Double] = _
  protected var E: TVar[Boolean] = _

  // share same timestamps with baseSer, should be care of ReadWriteLock
  attach(baseSer.timestamps)

  val baseSerBehavior = createBaseSerBehavior(baseSer)

  initPredefinedVarsOfBaseSer
  factors = _factors.toArray

  listenTo(baseSer)

  reactions += baseSerBehavior

  private var _identifier: Option[String] = None
  def identifier = _identifier
  def identifier_=(identifier: String) {
    _identifier = identifier match {
      case null | "" => None
      case _         => Some(identifier)
    }
  }

  /**
   * override this method to define your predefined vars
   */
  protected def initPredefinedVarsOfBaseSer {
    baseSer match {
      case x: ThingSer =>
        O = x.open
        H = x.high
        L = x.low
        C = x.close

        V = x.volume
        A = x.amount
        E = x.isClosed

      case _ =>
    }
  }

  def computedTime: Long = _computedTime

  /**
   * @NOTE
   * It's better to fire ser change events or fac change event instead of
   * call me directly. But, in case of baseSer has been loaded, there may
   * be no more ser change events fired, so when first create, call computeFrom(0)
   * is a safe maner.
   *
   * @TODO
   * Should this method synchronized?
   * As each seriesProvider has its own indicator instance, and indicator instance
   * usually called by chartview, that means, they are called usually in same
   * thread: awt.event.thread.
   *
   *
   * @param begin time to be computed
   */
  def computeFrom(fromTime: Long) {
    if (baseSer != null) {
      Indicator.setSessionId()

      try {
        timestamps.readLock.lock

        val fromIdx = super.preComputeFrom(fromTime)
        /**
         * @Note
         * It's better to pass Size as param to compute(...) instead of keep it as instance field,
         * so, we do not need to worry about if field _Size will be changed concurrent by another
         * thread
         */
        val size = timestamps.size

        compute(fromIdx, size)

        _computedTime = timestamps.lastOccurredTime
        super.postComputeFrom

      } finally {
        timestamps.readLock.unlock
      }
    }
  }

  protected def compute(fromIdx: Int, size: Int)

  /**
   * Define functions
   * --------------------------------------------------------------------
   */

  /**
   * Functions
   * ----------------------------------------------------------------------
   */

  // ----- Functions for test
  final protected def crossOver(idx: Int, var1: TVar[Double], var2: TVar[Double]): Boolean = {
    if (idx > 0) {
      if (var1(idx) >= var2(idx) &&
        var1(idx - 1) < var2(idx - 1)) {
        return true
      }
    }
    false
  }

  final protected def crossOver(idx: Int, var1: TVar[Double], value: Double): Boolean = {
    if (idx > 0) {
      if (var1(idx) >= value &&
        var1(idx - 1) < value) {
        return true
      }
    }
    false
  }

  final protected def crossUnder(idx: Int, var1: TVar[Double], var2: TVar[Double]): Boolean = {
    if (idx > 0) {
      if (var1(idx) < var2(idx) &&
        var1(idx - 1) >= var2(idx - 1)) {
        return true
      }
    }
    false
  }

  final protected def crossUnder(idx: Int, var1: TVar[Double], value: Double): Boolean = {
    if (idx > 0) {
      if (var1(idx) < value &&
        var1(idx - 1) >= value) {
        true
      }
    }
    false
  }

  final protected def turnUp(idx: Int, var1: TVar[Double]): Boolean = {
    if (idx > 1) {
      if (var1(idx) > var1(idx - 1) &&
        var1(idx - 1) <= var1(idx - 2)) {
        return true
      }
    }
    false
  }

  final protected def turnDown(idx: Int, var1: TVar[Double]): Boolean = {
    if (idx > 1) {
      if (var1(idx) < var1(idx - 1) &&
        var1(idx - 1) >= var1(idx - 2)) {
        return true
      }
    }
    false
  }

  // ----- End of functions for test
  import Indicator.sessionId

  final protected def sum(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[SUMFunction], baseVar, period).sum(sessionId, idx)
  }

  final protected def max(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[MAXFunction], baseVar, period).max(sessionId, idx)
  }

  final protected def min(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[MINFunction], baseVar, period).min(sessionId, idx)
  }

  final protected def ma(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[MAFunction], baseVar, period).ma(sessionId, idx)
  }

  final protected def ema(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[EMAFunction], baseVar, period).ema(sessionId, idx)
  }

  final protected def stdDev(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[STDDEVFunction], baseVar, period).stdDev(sessionId, idx)
  }

  final protected def probMass(idx: Int, baseVar: TVar[Double], period: Factor, nInterval: Factor): Array[Array[Double]] = {
    baseSer.function(classOf[PROBMASSFunction], baseVar, null, period, nInterval).probMass(sessionId, idx)
  }

  final protected def probMass(idx: Int, baseVar: TVar[Double], weight: TVar[Double], period: Factor, nInterval: Factor): Array[Array[Double]] = {
    baseSer.function(classOf[PROBMASSFunction], baseVar, weight, period, nInterval).probMass(sessionId, idx)
  }

  final protected def tr(idx: Int): Double = {
    baseSer.function(classOf[TRFunction]).tr(sessionId, idx)
  }

  final protected def dmPlus(idx: Int): Double = {
    baseSer.function(classOf[DMFunction]).dmPlus(sessionId, idx)
  }

  final protected def dmMinus(idx: Int): Double = {
    baseSer.function(classOf[DMFunction]).dmMinus(sessionId, idx)
  }

  final protected def diPlus(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[DIFunction], period).diPlus(sessionId, idx)
  }

  final protected def diMinus(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[DIFunction], period).diMinus(sessionId, idx)
  }

  final protected def dx(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[DXFunction], period).dx(sessionId, idx)
  }

  final protected def adx(idx: Int, periodDi: Factor, periodAdx: Factor): Double = {
    baseSer.function(classOf[ADXFunction], periodDi, periodAdx).adx(sessionId, idx)
  }

  final protected def adxr(idx: Int, periodDi: Factor, periodAdx: Factor): Double = {
    baseSer.function(classOf[ADXRFunction], periodDi, periodAdx).adxr(sessionId, idx)
  }

  final protected def bollMiddle(idx: Int, baseVar: TVar[_], period: Factor, alpha: Factor): Double = {
    baseSer.function(classOf[BOLLFunction], baseVar, period, alpha).bollMiddle(sessionId, idx)
  }

  final protected def bollUpper(idx: Int, baseVar: TVar[_], period: Factor, alpha: Factor): Double = {
    baseSer.function(classOf[BOLLFunction], baseVar, period, alpha).bollUpper(sessionId, idx)
  }

  final protected def bollLower(idx: Int, baseVar: TVar[_], period: Factor, alpha: Factor): Double = {
    baseSer.function(classOf[BOLLFunction], baseVar, period, alpha).bollLower(sessionId, idx)
  }

  final protected def cci(idx: Int, period: Factor, alpha: Factor): Double = {
    baseSer.function(classOf[CCIFunction], period, alpha).cci(sessionId, idx)
  }

  final protected def macd(idx: Int, baseVar: TVar[_], periodSlow: Factor, periodFast: Factor): Double = {
    baseSer.function(classOf[MACDFunction], baseVar, periodSlow, periodFast).macd(sessionId, idx)
  }

  final protected def mfi(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[MFIFunction], period).mfi(sessionId, idx)
  }

  final protected def mtm(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[MTMFunction], baseVar, period).mtm(sessionId, idx)
  }

  final protected def obv(idx: Int): Double = {
    baseSer.function(classOf[OBVFunction]).obv(sessionId, idx)
  }

  final protected def roc(idx: Int, baseVar: TVar[_], period: Factor): Double = {
    baseSer.function(classOf[ROCFunction], baseVar, period).roc(sessionId, idx)
  }

  final protected def rsi(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[RSIFunction], period).rsi(sessionId, idx)
  }

  final protected def sar(idx: Int, initial: Factor, step: Factor, maximum: Factor): Double = {
    baseSer.function(classOf[SARFunction], initial, step, maximum).sar(sessionId, idx)
  }

  final protected def sarSide(idx: Int, initial: Factor, step: Factor, maximum: Factor): Side = {
    baseSer.function(classOf[SARFunction], initial, step, maximum).sarSide(sessionId, idx)
  }

  final protected def stochK(idx: Int, period: Factor, periodK: Factor): Double = {
    baseSer.function(classOf[STOCHKFunction], period, periodK).stochK(sessionId, idx)
  }

  final protected def stochD(idx: Int, period: Factor, periodK: Factor, periodD: Factor): Double = {
    baseSer.function(classOf[STOCHDFunction], period, periodK, periodD).stochD(sessionId, idx)
  }

  final protected def stochJ(idx: Int, period: Factor, periodK: Factor, periodD: Factor): Double = {
    baseSer.function(classOf[STOCHJFunction], period, periodK, periodD).stochJ(sessionId, idx)
  }

  final protected def wms(idx: Int, period: Factor): Double = {
    baseSer.function(classOf[WMSFunction], period).wms(sessionId, idx)
  }

  final protected def zigzag(idx: Int, percent: Factor): Double = {
    baseSer.function(classOf[ZIGZAGFunction], percent).zigzag(sessionId, idx)
  }

  final protected def pseudoZigzag(idx: Int, percent: Factor): Double = {
    baseSer.function(classOf[ZIGZAGFunction], percent).pseudoZigzag(sessionId, idx)
  }

  final protected def zigzagSide(idx: Int, percent: Factor): Side = {
    baseSer.function(classOf[ZIGZAGFunction], percent).zigzagSide(sessionId, idx)
  }

  /**
   * ----------------------------------------------------------------------
   * End of Functions
   */

}

/**
 *
 * @author Caoyuan Deng
 */
object Indicator {
  /** a static global session id */
  protected var sessionId: Long = _

  protected def setSessionId() {
    sessionId += 1
  }
}
