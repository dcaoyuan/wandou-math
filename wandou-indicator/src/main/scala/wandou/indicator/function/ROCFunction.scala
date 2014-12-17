package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TVar
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class ROCFunction(_baseSer: TBaseSer, var baseVar: TVar[Double], var period: Factor) extends Function(_baseSer) {

  val _roc = TVar[Double]()

  override def set(args: Any*): Unit = {
    baseVar = args(0).asInstanceOf[TVar[Double]]
    period = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i < period.value - 1) {

      _roc(i) = Null.Double

    } else {

      val var_j = baseVar(i - period.value.toInt)

      val roc_i = if (var_j == 0) 0f else ((baseVar(i) - var_j) / var_j) * 100

      _roc(i) = roc_i

    }
  }

  def roc(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _roc(idx)
  }

}

