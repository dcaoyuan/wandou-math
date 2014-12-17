package wandou.indicator.function

import wandou.math.StatsFunctions
import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TVar
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class MINFunction(_baseSer: TBaseSer, var baseVar: TVar[Double], var period: Factor) extends Function(_baseSer) {

  final protected def imin(idx: Int, baseVar: TVar[Double], period: Double, prev: Double): Double = {
    StatsFunctions.imin(idx, baseVar.values, period.toInt, prev)
  }

  val _min = TVar[Double]()

  override def set(args: Any*): Unit = {
    baseVar = args(0).asInstanceOf[TVar[Double]]
    period = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i < period.value - 1) {

      _min(i) = Null.Double

    } else {

      _min(i) = imin(i, baseVar, period.value, _min(i - 1))

    }
  }

  def min(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _min(idx)
  }

}

