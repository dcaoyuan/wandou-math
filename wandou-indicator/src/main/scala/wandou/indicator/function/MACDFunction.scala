package wandou.indicator.function

import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TVar
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class MACDFunction(_baseSer: TBaseSer, var baseVar: TVar[Double], var periodSlow: Factor, var periodFast: Factor) extends Function(_baseSer) {

  val _emaFast = TVar[Double]()
  val _emaSlow = TVar[Double]()

  val _macd = TVar[Double]()

  override def set(args: Any*): Unit = {
    baseVar = args(0).asInstanceOf[TVar[Double]]
    periodSlow = args(1).asInstanceOf[Factor]
    periodFast = args(2).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _emaFast(i) = ema(i, baseVar, periodFast)
    _emaSlow(i) = ema(i, baseVar, periodSlow)

    _macd(i) = _emaFast(i) - _emaSlow(i)
  }

  def macd(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _macd(idx)
  }

}

