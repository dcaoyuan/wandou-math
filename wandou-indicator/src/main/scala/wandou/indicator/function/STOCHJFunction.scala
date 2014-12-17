package wandou.indicator.function

import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class STOCHJFunction(_baseSer: TBaseSer, var period: Factor, var periodK: Factor, var periodD: Factor) extends Function(_baseSer) {

  val _stochK = TVar[Double]()
  val _stochD = TVar[Double]()

  val _stochJ = TVar[Double]()

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
    periodK = args(1).asInstanceOf[Factor]
    periodD = args(2).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _stochK(i) = stochK(i, period, periodK)
    _stochD(i) = stochD(i, period, periodK, periodD)

    _stochJ(i) = _stochD(i) + 2 * (_stochD(i) - _stochK(i))
  }

  def stochJ(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _stochJ(idx)
  }

}

