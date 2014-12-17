package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class STOCHKFunction(_baseSer: TBaseSer, var period: Factor, var periodK: Factor) extends Function(_baseSer) {

  val _elementK = TVar[Double]()
  val _stochK = TVar[Double]()

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
    periodK = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i < period.value - 1) {

      _elementK(i) = Null.Double

      _stochK(i) = Null.Double

    } else {

      val h_max_i = max(i, H, period)
      val l_min_i = min(i, L, period)

      _elementK(i) = (C(i) - l_min_i) / (h_max_i - l_min_i) * 100f

      /** smooth elementK, periodK */
      _stochK(i) = ma(i, _elementK, periodK)

    }
  }

  def stochK(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _stochK(idx)
  }

}

