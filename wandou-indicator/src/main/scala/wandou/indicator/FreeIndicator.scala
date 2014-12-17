package wandou.indicator

import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.DefaultTBaseSer
import wandou.math.timeseries.TFreq
import wandou.math.timeseries.Thing

/**
 * @author Caoyuan Deng
 */
class FreeIndicator(val baseSer: TBaseSer, _thing: Thing, _freq: TFreq) extends DefaultTBaseSer(_thing, _freq)
    with wandou.math.indicator.Indicator {

  reactions += indicatorBehavior

  private var _identifier: Option[String] = None
  def identifier = _identifier
  def identifier_=(identifier: String) {
    _identifier = identifier match {
      case null | "" => None
      case _         => Some(identifier)
    }
  }

  /**
   * @param time to be computed from
   */
  def computeFrom(time: Long) {}
  def computedTime: Long = this.lastOccurredTime
}
