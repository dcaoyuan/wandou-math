package wandou.indicator

import wandou.collection.ArrayList
import wandou.math.timeseries.Null
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TStampedMapBasedList
import wandou.math.timeseries.TVar
import wandou.math.indicator.Plot
import java.awt.Color
import scala.collection.immutable
import scala.reflect.ClassTag

/**
 *
 * @author Caoyuan Deng
 */
abstract class SpotIndicator(_baseSer: TBaseSer) extends Indicator(_baseSer) with wandou.math.indicator.SpotIndicator {

  def this() = this(null)

  /**
   * @todo Also override existsFromHead and existsFromTail?
   */
  override def exists(time: Long): Boolean = true

  override def computeFrom(fromTime: Long) {
    // do nothing
  }

  protected def compute(fromIdx: Int, size: Int) {
    // do nothing
  }

  def computeSpot(time: Long) {
    /** get baseIdx before preComputeFrom(), which may clear this data */
    val baseIdx = baseSer.indexOfOccurredTime(time)
    computeSpot(time, baseIdx)
  }

  /**
   * @param time
   * @param baseIdx   baseIdx may be < 0, means there is no timestamps for this
   *                  time yet, time could be future.
   */
  protected def computeSpot(time: Long, baseIdx: Int)

  object STVar {
    def apply[V: ClassTag](): TVar[V] = new SpotTVar[V]("", TVar.Kind.Close)
    def apply[V: ClassTag](name: String): TVar[V] = new SpotTVar[V](name, TVar.Kind.Close)
    def apply[V: ClassTag](name: String, kind: TVar.Kind): TVar[V] = new SpotTVar[V](name, kind)
  }

  final protected class SpotTVar[V: ClassTag](var name: String, val kind: TVar.Kind) extends TVar[V] {

    addVar(this)

    def timestamps = SpotIndicator.this.timestamps

    var plot: Plot = Plot.None
    var layer = -1 // -1 means not set
    // @todo: timestamps may be null when go here, use lazy val as a quick fix now, shoule review it
    private lazy val colors = new TStampedMapBasedList[Color](timestamps)
    def getColor(idx: Int) = colors(idx)
    def setColor(idx: Int, color: Color) {
      colors(idx) = color
    }

    private var timeToValue = immutable.TreeMap[Long, V]() // must sort by time

    def values: ArrayList[V] = {
      throw new UnsupportedOperationException()
    }

    def put(time: Long, value: V): Boolean = {
      timeToValue += time -> value
      true
    }

    def put(time: Long, fromHeadOrTail: Boolean, value: V): Boolean = {
      throw new UnsupportedOperationException("Can only be accessed via time.")
    }

    def apply(time: Long): V = {
      if (!timeToValue.contains(time)) {
        computeSpot(time)
      }
      timeToValue.getOrElse(time, Null.value)
    }

    def apply(time: Long, fromHeadOrTail: Boolean): V = {
      throw new UnsupportedOperationException("Can only be accessed via time.")
    }

    def update(time: Long, value: V) {
      timeToValue += time -> value
    }

    override def apply(idx: Int): V = {
      throw new UnsupportedOperationException("Can only be accessed via time.")
    }

    override def update(idx: Int, value: V) {
      throw new UnsupportedOperationException("Can only be accessed via time.")
    }

    override def reset(idx: Int) {
      throw new UnsupportedOperationException("Can only be accessed via time.")
    }

    override def reset(time: Long) {
      timeToValue -= time
    }

    def timesIterator: Iterator[Long] = timeToValue.keysIterator
    def valuesIterator: Iterator[V] = timeToValue.valuesIterator
  }
}

