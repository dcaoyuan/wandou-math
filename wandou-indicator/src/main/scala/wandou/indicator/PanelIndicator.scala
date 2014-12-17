package wandou.indicator

import akka.actor.Actor
import akka.actor.ActorLogging
import wandou.collection.ArrayList
import wandou.math.indicator.ComputeFrom
import wandou.math.indicator.Factor
import wandou.math.indicator.Id
import wandou.math.timeseries.TFreq
import wandou.math.timeseries.TSerEvent
import wandou.math.timeseries.Thing
import wandou.util
import wandou.util.ValidTime
import wandou.actors.Publisher
import java.util.concurrent.ConcurrentHashMap
import scala.reflect._
import scala.concurrent.duration._

/**
 * @author Caoyuan Deng
 */
abstract class PanelIndicator[T <: Indicator: ClassTag](_freq: TFreq) extends FreeIndicator(null, null, _freq) {

  def name: String

  private var _sectorKey: String = ""
  def sectorKey = _sectorKey
  def sectorKey_=(sectorKey: String) {
    _sectorKey = sectorKey
  }

  final lazy val key = name + "_" + sectorKey.trim + "_" + freq.shortName

  val indicators = new ArrayList[(T, ValidTime[Thing])]

  private var lastFromTime = Long.MaxValue
  private val panelBehavior: Actor.Receive = {
    case PanelIndicator.PanelHeartbeat =>
      computeFrom(lastFromTime)
      lastFromTime = computedTime
    case ComputeFrom(time) =>
      lastFromTime = time
    case TSerEvent.Loaded(_, _, fromTime, toTime, _, callback) =>
      lastFromTime = math.min(fromTime, lastFromTime)
    //computeFrom(fromTime)
    case TSerEvent.Refresh(_, _, fromTime, toTime, _, callback) =>
      lastFromTime = math.min(fromTime, lastFromTime)
    //computeFrom(fromTime)
    case TSerEvent.Updated(_, _, fromTime, toTime, _, callback) =>
      lastFromTime = math.min(fromTime, lastFromTime)
    //computeFrom(fromTime)
    case TSerEvent.Computed(src, _, fromTime, toTime, _, callback) =>
      lastFromTime = math.min(fromTime, lastFromTime)
    //computeFrom(fromTime)
    case TSerEvent.Cleared(src, _, fromTime, toTime, _, callback) =>
      clear(fromTime)
  }
  // TODO listenTo(PanelIndicator)

  reactions += panelBehavior

  def addSecs(validTimes: collection.Seq[ValidTime[Thing]]) {
    validTimes foreach addThing
    log.info("Add things: " + validTimes.length)
    publish(ComputeFrom(0))
  }

  def addThing(validTime: ValidTime[Thing]): Option[T] = {
    //    validTime.ref.serOf(freq) match {
    //      case Some(baseSer) =>
    //        val ind = baseSer.indicator(classTag[T].runtimeClass.asInstanceOf[Class[T]], factors: _*)
    //        // TODO listenTo(ind)
    //        indicators += ((ind, validTime))
    //        Some(ind)
    //      case _ => None
    //    }
    None // TODO
  }

  def removeThing(validTime: ValidTime[Thing]): Option[T] = {
    //    validTime.ref.serOf(freq) match {
    //      case Some(baseSer) =>
    //        val ind = baseSer.indicator(classTag[T].asInstanceOf[Class[T]], factors: _*)
    //        // TODO deafTo(ind)
    //        indicators -= ((ind, validTime))
    //        Some(ind)
    //      case _ => None
    //    }
    None // TODO
  }

  def descriptor = "(" + this.getClass.getSimpleName + "," + sectorKey + "," + freq.shortName + ")"

  override def computeFrom(fromTime0: Long) {

    val (firstTime, lastTime) = firstLastTimeOf(indicators)

    val fromTime = if (fromTime0 == 0 || fromTime0 == 1) { // fromTime maybe 1, when called by computeFrom(afterThisTime)
      firstTime
    } else fromTime0

    if (fromTime == Long.MaxValue || lastTime == Long.MinValue) return

    val t0 = System.currentTimeMillis
    compute(fromTime, lastTime)
    log.info(descriptor + ", size=" + size + ", computed " + util.formatTime(fromTime) + " - " + util.formatTime(lastTime) + " in " + (System.currentTimeMillis - t0) + "ms")
    //    val vmap = export(fromTime, lastTime)
    //    if (vmap.nonEmpty && vmap.values.head.asInstanceOf[Array[_]].size != 0) publish(key -> vmap)
  }

  /**
   * Implement this method for actual computing.
   * @param from time, included
   * @param to time, included
   */
  protected def compute(fromTime: Long, toTime: Long)

  protected def firstLastTimeOf(inds: ArrayList[(T, ValidTime[Thing])]): (Long, Long) = {
    var firstTime = Long.MaxValue
    var lastTime = Long.MinValue

    var i = -1
    while ({ i += 1; i < inds.length }) {
      val ind = inds(i)._1
      if (ind != null && ind.timestamps.length > 0) {
        val fTime = ind.firstOccurredTime
        firstTime = math.min(firstTime, fTime)

        val lTime = ind.lastOccurredTime
        lastTime = math.max(lastTime, lTime)
      }
    }

    (firstTime, lastTime)
  }
}

object PanelIndicator extends Actor with ActorLogging with Publisher {

  private val idToIndicator = new ConcurrentHashMap[Id[_ <: PanelIndicator[_ <: Indicator]], PanelIndicator[_ <: Indicator]](8, 0.9f, 1)

  private val runtime = Runtime.getRuntime
  private case object PanelHeartbeat
  private val interval = 30000L // 30 seconds
  private var count = 0
  var indicatorCount = 0

  def receive = publisherBehavior orElse {
    case PanelHeartbeat =>
      publish(PanelHeartbeat)
    //          count += 1
    //          if (count > 10){
    //            count = 0
    //            log.info("Before collect garbage, Max memory:" + (runtime.maxMemory/1024.0f/1024) + "M, total memory:" + (runtime.totalMemory/1024.0f/1024 + "M, free memory:" + (runtime.freeMemory/1024.0f/1024) + "M" ))
    //            System.gc
    //            log.info("After collect garbage, Max memory:" + (runtime.maxMemory/1024.0f/1024) + "M, total memory:" + (runtime.totalMemory/1024.0f/1024 + "M, free memory:" + (runtime.freeMemory/1024.0f/1024) + "M" ))
    //          }
  }

  def startTimer() = {
    import context.dispatcher
    context.system.scheduler.schedule(1.seconds, interval.seconds, self, PanelHeartbeat)
  }

  def idOf[T](klass: Class[T], sectorKey: String, freq: TFreq, factors: Factor*) = {
    val factorArr = factors.toArray
    val factorLen = factorArr.length
    val args = new Array[Any](factorLen + 1)
    args(0) = freq
    System.arraycopy(factorArr, 0, args, 1, factorLen)

    Id(klass, sectorKey, args: _*)
  }

  def apply[T <: PanelIndicator[_ <: Indicator]](klass: Class[T], sectorKey: String, freq: TFreq, factors: Factor*): (T, Boolean) = {
    val id = idOf(klass, sectorKey, freq, factors: _*)

    idToIndicator.get(id) match {
      case null =>
        /** if got none from idToIndicator, try to create new one */
        try {
          val indicator = klass.getConstructor(classOf[TFreq]).newInstance(freq)
          indicator.sectorKey = sectorKey
          indicator.factors = factors.toArray
          idToIndicator.putIfAbsent(id, indicator)

          indicatorCount += 1
          log.info("Started panel indicator: " + indicator.descriptor + ", indicators count: " + indicatorCount)
          (indicator, true)
        } catch {
          case ex: Throwable => log.warning(ex.getMessage); (null.asInstanceOf[T], false)
        }
      case x => (x.asInstanceOf[T], false)
    }
  }
}
