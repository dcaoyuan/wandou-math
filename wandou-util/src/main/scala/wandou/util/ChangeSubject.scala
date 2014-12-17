package wandou.util

/**
 *
 * @author  Caoyuan Deng
 * @version 1.0, November 24, 2006, 5:06 PM
 * @since   1.0.4
 */
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

trait ChangeSubject {

  @transient
  private val observerToOwner = mutable.Map[ChangeObserver, AnyRef]()

  def addObserver(owner: AnyRef, observer: ChangeObserver) {
    synchronized { observerToOwner(observer) = owner }
  }

  def removeObserver(observer: ChangeObserver) {
    if (observer == null) {
      return
    }

    if (observerToOwner.keySet.contains(observer)) {
      synchronized { observerToOwner -= observer }
    }
  }

  def removeObserversOf(owner: AnyRef) {
    val toRemove = new ListBuffer[ChangeObserver]
    for ((observer, ownerx) <- observerToOwner if ownerx == owner) {
      toRemove += observer
    }

    synchronized { observerToOwner --= toRemove }
  }

  def removeObservers {
    synchronized { observerToOwner.clear }
  }

  /**
   * A ChangeObservable implement can support may type of ChangeObserver, so
   * we only apply O here, the implement class can choose to notify this type
   * of observers
   *
   * @Note since Class[T] is not co-variant, we have to explicitly use [T <: ChangeObserver]
   */
  def notifyChanged[T <: ChangeObserver](observerType: Class[T]) {
    for (observer <- observerToOwner.keysIterator if observerType.isInstance(observer)) {
      if (observer.updater isDefinedAt this) observer.updater(this)
    }
  }

  def notifyChanged {
    for (observer <- observerToOwner.keysIterator) {
      if (observer.updater isDefinedAt this) observer.updater(this)
    }
  }

  /**
   * for use of wrap class
   */
  //  def notifyObservers(subject: Observable): Unit = synchronized {
  //    if (changed) {
  //      /** must clone the observers in case deleteObserver is called */
  //      val clone = new Array[ObserverRef](observerRefs.size)
  //      observerRefs.copyToArray(clone, 0)
  //      clearChanged
  //      clone foreach {_.get.update(subject)}
  //    }
  //  }

  def observers: Iterable[ChangeObserver] = {
    observerToOwner.keySet
  }

  def observersOf[T <: ChangeObserver](observerType: Class[T]): Iterable[T] = {
    val result = new ListBuffer[T]
    for (observer <- observerToOwner.keysIterator if observerType.isInstance(observer)) {
      result += observer.asInstanceOf[T]
    }
    result
  }

  /**
   * Returns the total number of obervers.
   */
  def observerCount: Int = {
    observerToOwner.size
  }

  private def observerCountOf[T <: ChangeObserver](observerType: Class[T]): Int = {
    var count = 0
    for (observer <- observerToOwner.keysIterator if observerType.isInstance(observer)) {
      count += 1
    }
    count
  }

  override def toString = {
    val sb = new StringBuilder("ChangeObserverList: ")
    sb.append(observerToOwner.size).append(" observers: ")
    for (observer <- observerToOwner.keysIterator) {
      sb.append(" type ").append(observer.getClass.getName)
      sb.append(" observer ").append(observer)
    }

    sb.toString
  }
}

