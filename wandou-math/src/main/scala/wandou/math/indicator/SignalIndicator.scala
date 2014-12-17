package wandou.math.indicator

import wandou.math.timeseries.TVar
import wandou.math.signal.Signal

/**
 * @author Caoyuan Deng
 */
trait SignalIndicator extends Indicator {
  def signalVar: TVar[List[Signal]]
}
