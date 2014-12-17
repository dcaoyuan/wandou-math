package wandou.util.pool

/**
 * A simple base impementation of {@link ObjectPool}.
 * All optional operations are implemented as throwing
 * {@link UnsupportedOperationException}.
 *
 * @author Rodney Waldhoff
 * @version $Revision: 383290 $ $Date: 2006-03-05 02:00:15 -0500 (Sun, 05 Mar 2006) $
 */
abstract class BaseObjectPool[T] extends ObjectPool[T] {

  @throws(classOf[RuntimeException])
  def borrow: T

  @throws(classOf[RuntimeException])
  def returnIt(obj: T)

  @throws(classOf[RuntimeException])
  def invalidate(obj: T)

  /**
   * Not supported in this base implementation.
   */
  @throws(classOf[UnsupportedOperationException])
  def numOfIdle: Int = {
    throw new UnsupportedOperationException
  }

  /**
   * Not supported in this base implementation.
   */
  @throws(classOf[UnsupportedOperationException])
  def numOfActive: Int = {
    throw new UnsupportedOperationException
  }

  /**
   * Not supported in this base implementation.
   */
  @throws(classOf[UnsupportedOperationException])
  @throws(classOf[Exception])
  def clear {
    throw new UnsupportedOperationException
  }

  /**
   * Not supported in this base implementation.
   */
  @throws(classOf[UnsupportedOperationException])
  @throws(classOf[RuntimeException])
  def add {
    throw new UnsupportedOperationException
  }

  @throws(classOf[Exception])
  def close {
    assertOpen
    closed = true
  }

  /**
   * Not supported in this base implementation.
   */
  @throws(classOf[UnsupportedOperationException])
  @throws(classOf[IllegalStateException])
  def factory_=(factory: PoolableObjectFactory[T]) {
    throw new UnsupportedOperationException();
  }

  protected final def isClosed: Boolean = {
    closed
  }

  @throws(classOf[IllegalStateException])
  protected final def assertOpen {
    if (isClosed) {
      throw new IllegalStateException("Pool not open")
    }
  }

  @volatile
  private var closed = false
}

