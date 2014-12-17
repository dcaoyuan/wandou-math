package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class ADXRFunction(_baseSer: TBaseSer, var periodDi: Factor, var periodAdx: Factor) extends Function(_baseSer) {

  val _adx = TVar[Double]()
  val _adxr = TVar[Double]()

  override def set(args: Any*): Unit = {
    periodDi = args(0).asInstanceOf[Factor]
    periodAdx = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int) {
    _adx(i) = adx(i, periodDi, periodAdx)

    if (i < periodDi.value - 1 || i < periodAdx.value - 1) {

      _adxr(i) = Null.Double

    } else {

      val adx_i = _adx(i)
      val adx_j = _adx(i - periodAdx.value.toInt)

      _adxr(i) = (adx_i + adx_j) / 2f

    }
  }

  def adxr(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _adxr(idx)
  }

}

