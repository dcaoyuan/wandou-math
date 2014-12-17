package wandou.indicator.function

import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer

/**
 *
 * @author Caoyuan Deng
 */
class DMFunction(_baseSer: TBaseSer) extends Function(_baseSer) {

  val _dmPlus = TVar[Double]()
  val _dmMinus = TVar[Double]()

  override def set(args: Any*): Unit = {
  }

  protected def computeSpot(i: Int): Unit = {
    if (i == 0) {

      _dmPlus(i) = Null.Double
      _dmMinus(i) = Null.Double

    } else {

      if (H(i) > H(i - 1) && L(i) > L(i - 1)) {
        _dmPlus(i) = H(i) - H(i - 1)
        _dmMinus(i) = 0f
      } else if (H(i) < H(i - 1) && L(i) < L(i - 1)) {
        _dmPlus(i) = 0f
        _dmMinus(i) = L(i - 1) - L(i)
      } else if (H(i) > H(i - 1) && L(i) < L(i - 1)) {
        if (H(i) - H(i - 1) > L(i - 1) - L(i)) {
          _dmPlus(i) = H(i) - H(i - 1)
          _dmMinus(i) = 0f
        } else {
          _dmPlus(i) = 0f
          _dmMinus(i) = L(i - 1) - L(i)
        }
      } else if (H(i) < H(i - 1) && L(i) > L(i - 1)) {
        _dmPlus(i) = 0f
        _dmMinus(i) = 0f
      } else if (H(i) == H(i - 1) && L(i) == L(i - 1)) {
        _dmPlus(i) = 0f
        _dmMinus(i) = 0f
      } else if (L(i) > H(i - 1)) {
        _dmPlus(i) = H(i) - H(i)
        _dmMinus(i) = 0f
      } else if (H(i) < L(i - 1)) {
        _dmPlus(i) = 0f
        _dmMinus(i) = L(i - 1) - L(i)
      } else {
        _dmPlus(i) = 0f
        _dmMinus(i) = 0f
      }

    }
  }

  def dmPlus(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _dmPlus(idx)
  }

  def dmMinus(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _dmMinus(idx)
  }
}

