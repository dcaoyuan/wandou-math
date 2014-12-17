package wandou.math.indicator

import wandou.collection.ArrayList
import wandou.math.PersistenceManager
import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TFreq
import wandou.math.timeseries.descriptor.Descriptor

/**
 *
 * @author Caoyuan Deng
 */
class IndicatorDescriptor(_serviceClassName: => String, _freq: => TFreq, $factors: => Array[Factor], _active: => Boolean) extends Descriptor[Indicator](_serviceClassName, _freq, _active) {

  def this() = this(null, TFreq.DAILY, Array[Factor](), false)

  val folderName = "Indicators"

  private var _factors = new ArrayList[Factor] ++= $factors

  private var _identifier: Option[String] = None

  /**
   * You specify the indictor is from another symbols
   */
  def identifier = _identifier
  def identifier_=(identifier: String) {
    _identifier = identifier match {
      case null | "" => None
      case _         => Some(identifier)
    }
  }

  override def set(serviceClassName: String, freq: TFreq): Unit = {
    super.set(serviceClassName, freq)

    setFacsToDefault
  }

  def factors: Array[Factor] = _factors.toArray
  def factors_=(factors: Array[Factor]) {
    /**
     * @NOTICE:
     * always create a new copy of in factors to seperate the factors of this
     * and that transfered in (we don't know who transfer it in, so, be more
     * carefule is always good)
     */
    val mySize = this._factors.length
    if (factors != null) {
      var i = 0
      while (i < factors.length) {
        val newFac = factors(i).clone
        if (i < mySize) {
          this._factors(i) = newFac
        } else {
          this._factors += newFac
        }
        i += 1
      }
    } else {
      this._factors.clear
    }
  }

  override def displayName: String = {
    val name = lookupServiceTemplate(classOf[Indicator], "Indicators") match {
      case Some(tpInstance) => tpInstance.shortName
      case None             => serviceClassName
    }

    Indicator.displayName(name, factors)
  }

  /**
   * @NOTICE
   * Here we get a new indicator instance by searching DefaultFileSystem(on NetBeans).
   * This is because that this instance may from other modules (i.e. SolarisIndicator),
   * it may not be seen from this module. Actually we should not set dependency on
   * those added-on modules.
   * @param baseSer for indicator
   */
  protected def createServiceInstance(args: Any*): Option[Indicator] = {
    //    args match {
    //    case Seq(baseSerx: TBaseSer) =>
    //      lookupServiceTemplate(classOf[Indicator], "Indicators") match {
    //        case Some(indx) =>
    //          // is this indicator from another symbol ?
    //          val baseSer = (
    //            for {
    //              id <- identifier if id != baseSerx.thing.identifier
    //              thing <- baseSerx.thing.thingOf(id)
    //              base <- thing.serOf(baseSerx.freq)
    //            } yield base) getOrElse baseSerx
    //
    //          val instance = if (factors.length == 0) {
    //            // this means this indicatorDescritor's factors may not be set yet, so set a default one now
    //            val instancex = baseSer.indicator(indx.getClass.asInstanceOf[Class[Indicator]])
    //            factors = instancex.factors
    //            instancex
    //          } else {
    //            // should set facs here, because it's from one that is stored in xml
    //            baseSer.indicator(indx.getClass.asInstanceOf[Class[Indicator]], factors: _*)
    //          }
    //
    //          Option(instance)
    //        case None => None
    //      }
    //    case _ => None
    //    }
    None
  }

  def setFacsToDefault {
    val defaultFacs = PersistenceManager().defaultContent.lookupDescriptor(
      classOf[IndicatorDescriptor], serviceClassName, freq) match {
        case None =>
          lookupServiceTemplate(classOf[Indicator], "Indicators") match {
            case None    => None
            case Some(x) => Some(x.factors)
          }
        case Some(defaultDescriptor) =>
          Some(defaultDescriptor.factors)
      }

    defaultFacs foreach { x => factors = x }
  }

}

