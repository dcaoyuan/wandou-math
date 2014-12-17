package wandou.indicator.function

import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class MFIFunction(_baseSer: TBaseSer, var period: Factor) extends Function(_baseSer) {

  val _tp = TVar[Double]()
  val _mfPos = TVar[Double]()
  val _mfNeg = TVar[Double]()

  val _mfi = TVar[Double]

  override def set(args: Any*): Unit = {
    period = args(0).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    _tp(i) = (H(i) + C(i) + L(i)) / 3f

    if (i == 0) {

      _mfPos(i) = 0f
      _mfNeg(i) = 0f

      _mfi(i) = 0f

    } else {

      if (_tp(i) > _tp(i - 1)) {
        _mfPos(i) = _tp(i) * V(i)
        _mfNeg(i) = 0f
      } else if (_tp(i) < _tp(i - 1)) {
        _mfPos(i) = 0f
        _mfNeg(i) = _tp(i) * V(i)
      } else {
        _mfPos(i) = 0f
        _mfNeg(i) = 0f
      }

      val mfPos_sum_i = sum(i, _mfPos, period)

      val mfNeg_sum_i = sum(i, _mfNeg, period)

      val mr_i = mfPos_sum_i / mfNeg_sum_i

      _mfi(i) = 100 / (1 + mr_i)

    }
  }

  def mfi(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _mfi(idx)
  }

}

