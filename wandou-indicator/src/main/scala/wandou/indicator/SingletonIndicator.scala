package wandou.indicator

/**
 * Usually, indicator instances are created by call createNewInstance(),
 * But in this class, createNewInstance() don't really create a new singletonInstance,
 * it just return the singletonInstance.
 *
 * Here is not the traditional singleton pattern, which implement singleton by
 * using <em>private</em> constructor and a static getInstance() method. This is
 * because that, in many cases (such as NetBeans FileSystem, or serializtion etc.),
 * a <em>public</em> constructor with empty args is required.
 *
 * @author Caoyuan Deng
 */
import wandou.math.timeseries.TBaseSer

object SingletonIndicator {
  protected var singletonInstance: SingletonIndicator = _
}

import SingletonIndicator._
abstract class SingletonIndicator(_baseSer: TBaseSer) extends Indicator(_baseSer) {

  singletonInstance = this

  def createInstance: Indicator = {
    if (singletonInstance == null) {
      val clazz = this.getClass
      try {
        singletonInstance = this.getClass.newInstance.asInstanceOf[SingletonIndicator]
      } catch { case ex: Exception => ex.printStackTrace }
    }
    singletonInstance
  }

}

