package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class RSIFunction(_baseSer: TBaseSer, var period: Factor) extends Function(_baseSer) {

  val _up = TVar[Double]()
  val _dn = TVar[Double]()

  val _rsi = TVar[Double]()

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i == 0) {

      _up(i) = Null.Double
      _dn(i) = Null.Double

      _rsi(i) = Null.Double

    } else {

      val change = C(i) - C(i - 1)
      if (change > 0) {
        _up(i) = change
        _dn(i) = 0f
      } else {
        _up(i) = 0f
        _dn(i) = -change
      }

      if (i < period.value - 1) {

        _rsi(i) = Null.Double

      } else {

        val up_sum_i = sum(i, _up, period)
        val dn_sum_i = sum(i, _dn, period)

        _rsi(i) = if (up_sum_i + dn_sum_i == 0) 0f else up_sum_i / (up_sum_i + dn_sum_i) * 100f
      }
    }
  }

  def rsi(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _rsi(idx)
  }

}

