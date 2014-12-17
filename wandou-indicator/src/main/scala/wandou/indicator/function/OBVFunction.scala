package wandou.indicator.function

import wandou.math.timeseries.TBaseSer

/**
 *
 * @author Caoyuan Deng
 */
class OBVFunction(_baseSer: TBaseSer) extends Function(_baseSer) {

  val _obv = TVar[Double]()

  override def set(args: Any*): Unit = {
  }

  protected def computeSpot(i: Int): Unit = {
    if (i == 0) {

      _obv(i) = 0f

    } else {

      if (C(i) > C(i - 1)) {
        _obv(i) = _obv(i - 1) + V(i)
      } else if (C(i) < C(i - 1)) {
        _obv(i) = _obv(i - 1) - V(i)
      } else {
        /** C(i) == C(i - 1) */
        _obv(i) = _obv(i - 1)
      }

    }
  }

  def obv(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _obv(idx)
  }

}

