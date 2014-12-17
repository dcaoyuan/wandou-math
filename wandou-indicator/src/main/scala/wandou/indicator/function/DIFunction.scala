package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class DIFunction(_baseSer: TBaseSer, var period: Factor) extends Function(_baseSer) {

  val _dmPlus = TVar[Double]()
  val _dmMinus = TVar[Double]()
  val _tr = TVar[Double]()

  val _diPlus = TVar[Double]()
  val _diMinus = TVar[Double]()

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _dmPlus(i) = dmPlus(i)
    _dmMinus(i) = dmMinus(i)
    _tr(i) = tr(i)

    if (i < period.value - 1) {

      _diPlus(i) = Null.Double
      _diMinus(i) = Null.Double

    } else {

      val dmPlus_ma = ma(i, _dmPlus, period)
      val dmMinus_ma = ma(i, _dmMinus, period)
      val tr_ma = ma(i, _tr, period)

      val diPlus_i = if (tr_ma == 0) 0f else dmPlus_ma / tr_ma * 100f
      val diMinus_i = if (tr_ma == 0) 0f else dmMinus_ma / tr_ma * 100f

      _diPlus(i) = diPlus_i
      _diMinus(i) = diMinus_i

    }
  }

  def diPlus(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _diPlus(idx)
  }

  def diMinus(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _diMinus(idx)
  }
}

