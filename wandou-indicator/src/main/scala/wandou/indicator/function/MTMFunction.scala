package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TVar
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class MTMFunction(_baseSer: TBaseSer, var baseVar: TVar[Double], var period: Factor) extends Function(_baseSer) {

  val _mtm = TVar[Double]()

  override def set(args: Any*): Unit = {
    baseVar = args(0).asInstanceOf[TVar[Double]]
    period = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i < period.value - 1) {

      _mtm(i) = Null.Double

    } else {

      _mtm(i) = (baseVar(i) / baseVar(i - period.value.toInt)) * 100f

    }
  }

  def mtm(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _mtm(idx)
  }

}

