package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class CCIFunction(_baseSer: TBaseSer, var alpha: Factor, var period: Factor) extends Function(_baseSer) {

  val _tp = TVar[Double]()
  val _deviation = TVar[Double]()

  val _cci = TVar[Double]()

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
    alpha = args(1).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _tp(i) = (H(i) + 2 * C(i) + L(i)) / 4f

    if (i < period.value - 1) {

      _deviation(i) = Null.Double

      _cci(i) = Null.Double

    } else {

      val tp_ma_i = ma(i, _tp, period)

      _deviation(i) = math.abs(_tp(i) - tp_ma_i)
      val deviation_ma_i = ma(i, _deviation, period)

      _cci(i) = (_tp(i) - tp_ma_i) / (alpha.value * deviation_ma_i)

    }
  }

  def cci(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _cci(idx)
  }

}

