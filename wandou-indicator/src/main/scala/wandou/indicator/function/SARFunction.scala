package wandou.indicator.function

import wandou.math.signal.Side
import wandou.math.timeseries.TBaseSer
import wandou.math.indicator.Factor

/**
 *
 * @author Caoyuan Deng
 */
class SARFunction(_baseSer: TBaseSer, var initial: Factor, var step: Factor, var maximum: Factor) extends Function(_baseSer) {

  val _side = TVar[Side]()
  val _ep = TVar[Double]()
  val _af = TVar[Double]()

  val _sar = TVar[Double]()

  override def set(args: Any*): Unit = {
    initial = args(0).asInstanceOf[Factor]
    step = args(1).asInstanceOf[Factor]
    maximum = args(2).asInstanceOf[Factor]
  }

  protected def computeSpot(i: Int): Unit = {
    if (i == 0) {

      _side(i) = Side.EnterLong

      val currLow = L(i)
      _sar(i) = currLow

      _af(i) = initial.value

      val currHigh = H(i)
      _ep(i) = currHigh

    } else {

      if (_side(i - 1) == Side.EnterLong) {
        /** in long-term */

        val currHigh = H(i)
        val prevHigh = H(i - 1)

        if (currHigh > _ep(i - 1)) {
          /** new high, acceleration adds 'step' each day, till 'maximum' */
          _af(i) = math.min(_af(i - 1) + step.value, maximum.value)
          _ep(i) = currHigh
        } else {
          /** keep same acceleration */
          _af(i) = _af(i - 1)
          _ep(i) = _ep(i - 1)
        }
        _sar(i) = _sar(i - 1) + _af(i) * (prevHigh - _sar(i - 1))

        if (_sar(i) >= currHigh) {
          /** turn to short-term */

          _side(i) = Side.ExitLong

          _sar(i) = currHigh

          _af(i) = initial.value
          _ep(i) = L(i)

        } else {
          /** still in long-term */

          _side(i) = Side.EnterLong
        }

      } else {
        /** in short-term */

        val currLow = L(i)
        val prevLow = L(i - 1)

        if (currLow < _ep(i - 1)) {
          _af(i) = math.min(_af(i - 1) + step.value, maximum.value)
          _ep(i) = currLow
        } else {
          _af(i) = _af(i - 1)
          _ep(i) = _ep(i - 1)
        }
        _sar(i) = _sar(i - 1) + _af(i) * (prevLow - _sar(i - 1))

        if (_sar(i) <= currLow) {
          /** turn to long-term */

          _side(i) = Side.EnterLong

          _sar(i) = currLow

          _af(i) = initial.value
          _ep(i) = H(i)

        } else {
          /** still in short-term */

          _side(i) = Side.ExitLong
        }
      }

    }
  }

  def sar(sessionId: Long, idx: Int): Double = {
    computeTo(sessionId, idx)

    _sar(idx)
  }

  def sarSide(sessionId: Long, idx: Int): Side = {
    computeTo(sessionId, idx)

    _side(idx)
  }
}

