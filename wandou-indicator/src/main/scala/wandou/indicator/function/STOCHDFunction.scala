package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class STOCHDFunction(_baseSer: TBaseSer, var period: Factor, var periodK: Factor, var periodD: Factor) extends Function(_baseSer) {

  val _stochK = TVar[Double]
  val _stochD = TVar[Double]

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
    periodK = args(1).asInstanceOf[Factor]
    periodD = args(2).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _stochK(i) = stochK(i, period, periodK)

    if (i < periodD.value - 1) {

      _stochD(i) = Null.Double

    } else {

      /** smooth stochK, periodD */
      _stochD(i) = ma(i, _stochK, periodD)

    }
  }

  def stochD(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _stochD(idx)
  }

}

