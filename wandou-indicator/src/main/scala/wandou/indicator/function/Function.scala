package wandou.indicator.function

import wandou.math.timeseries.{ DefaultTSer, TBaseSer, TVar, Null, ThingSer }
import wandou.math.indicator.Factor
import wandou.math.signal.Side
import scala.reflect.ClassTag

/**
 *
 * @author Caoyuan Deng
 */
object Function {
  /**
   * a helper function for keeping the same functin form as Function, don't be
   * puzzled by the name, it actully will return function instance
   */
  protected def apply[T <: wandou.math.indicator.Function: ClassTag](clazz: Class[T], baseSer: TBaseSer, args: Any*): T = {
    baseSer.function(clazz, args: _*)
  }
}

abstract class Function(val baseSer: TBaseSer, args: Any*) extends DefaultTSer(baseSer.freq) with wandou.math.indicator.Function {

  def context = baseSer.context

  /**
   * Use computing session to avoid redundant computation on same idx of same
   * function instance by different callers.
   *
   * A session is a series of continuant computing usally called by Indicator
   * It may contains a couple of functions that being called during it.
   *
   * The sessionId is injected in by the caller.
   */
  private var sessionId = Long.MinValue
  protected var computedIdx = Int.MinValue

  /** To store values of open, high, low, close, volume: */
  protected var O: TVar[Double] = _
  protected var H: TVar[Double] = _
  protected var L: TVar[Double] = _
  protected var C: TVar[Double] = _
  protected var V: TVar[Double] = _
  protected var E: TVar[Boolean] = _

  attach(baseSer.timestamps)
  initPredefinedVarsOfBaseSer

  def set(args: Any*) {}

  /** override this method to define your own pre-defined vars if necessary */
  protected def initPredefinedVarsOfBaseSer {
    baseSer match {
      case x: ThingSer =>
        O = x.open
        H = x.high
        L = x.low
        C = x.close

        V = x.volume
        E = x.isClosed
      case _ =>
    }
  }

  /**
   * This method will compute from computedIdx <b>to</b> idx.
   *
   * and AbstractIndicator.compute(final long begTime) will compute <b>from</b>
   * begTime to last data
   *
   * @param sessionId, the sessionId usally is controlled by outside caller,
   *        such as an indicator
   * @param idx, the idx to be computed to
   */
  def computeTo(sessionId: Long, idx: Int) {
    try {
      timestamps.readLock.lock

      preComputeTo(sessionId, idx)

      /**
       * if in same session and idx has just been computed, do not do
       * redundance computation
       */
      if (this.sessionId == sessionId && idx <= computedIdx) {
        return
      }

      this.sessionId = sessionId

      // computedIdx itself has been computed, so, compare computedIdx + 1 with idx */
      var fromIdx = math.min(computedIdx + 1, idx)
      if (fromIdx < 0) {
        fromIdx = 0
      }

      // fill with clear data from fromIdx
      if (this ne baseSer) {
        validate
      }

      // call computeSpot(i)
      val size = timestamps.size
      val toIdx = math.min(idx, size - 1)
      var i = fromIdx
      while (i <= toIdx) {
        computeSpot(i)
        i += 1
      }

      computedIdx = toIdx

      postComputeTo(sessionId, toIdx)

    } finally {
      timestamps.readLock.unlock
    }
  }

  /**
   * override this method to do something before computeTo, such as set computedIdx etc.
   */
  protected def preComputeTo(sessionId: Long, idx: Int) {
  }

  /**
   * override this method to do something post computeTo
   */
  protected def postComputeTo(sessionId: Long, idx: Int) {
  }

  /**
   * @param i, idx of spot
   */
  protected def computeSpot(i: Int)

  /**
   * Define functions
   * --------------------------------------------------------------------
   */

  /**
   * Functions of helper
   * ----------------------------------------------------------------------
   */

  protected def indexOfLastValidValue(var1: TVar[_]): Int = {
    val values = var1.values
    var i = values.size - 1
    while (i > 0) {
      val value = values(i)
      if (value != null && !(value.isInstanceOf[Double] && Null.is(value.asInstanceOf[Double]))) {
        return baseSer.indexOfOccurredTime(timestamps(i))
      }

      i -= 1
    }
    -1
  }

  /**
   * ---------------------------------------------------------------------
   * End of functions of helper
   */

  /**
   * Functions from FunctionSereis
   * ----------------------------------------------------------------------
   */

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
   * End of Functions from FunctionSereis
   */
}
