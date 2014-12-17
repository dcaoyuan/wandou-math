package wandou.util.pool

/**
 * A pooling interface.
 * <p>
 * <code>ObjectPool</code> defines a trivially simple pooling interface. The only
 * required methods are {@link #borrowObject borrowObject} and {@link #returnObject returnObject}.
 * <p>
 * Example of use:
 * <table border="1" cellspacing="0" cellpadding="3" align="center" bgcolor="#FFFFFF"><tr><td><pre>
 * Object obj = <font color="#0000CC">null</font>;
 *
 * <font color="#0000CC">try</font> {
 *    obj = pool.borrowObject();
 *    <font color="#00CC00">//...use the object...</font>
 * } <font color="#0000CC">catch</font>(Exception e) {
 *    <font color="#00CC00">//...handle any exceptions...</font>
 * } <font color="#0000CC">finally</font> {
 *    <font color="#00CC00">// make sure the object is returned to the pool</font>
 *    <font color="#0000CC">if</font>(<font color="#0000CC">null</font> != obj) {
 *       pool.returnObject(obj);
 *    }
 * }</pre></td></tr></table>
 * See {@link org.apache.commons.pool.BaseObjectPool BaseObjectPool} for a simple base implementation.
 *
 * @author Rodney Waldhoff
 * @version $Revision: 155430 $ $Date: 2005-02-26 08:13:28 -0500 (Sat, 26 Feb 2005) $
 *
 */
trait ObjectPool[T] {
  /**
   * Obtain an instance from my pool.
   * By contract, clients MUST return
   * the borrowed instance using
   * {@link #returnObject(java.lang.Object) returnObject}
   * or a related method as defined in an implementation
   * or sub-interface.
   * <p>
   * The behaviour of this method when the pool has been exhausted
   * is not specified (although it may be specified by implementations).
   *
   * @return an instance from my pool.
   */
  @throws(classOf[RuntimeException])
  def borrow: T

  /**
   * Return an instance to my pool.
   * By contract, <i>obj</i> MUST have been obtained
   * using {@link #borrowObject() borrowObject}
   * or a related method as defined in an implementation
   * or sub-interface.
   *
   * @param obj a {@link #borrowObject borrowed} instance to be returned.
   */
  @throws(classOf[RuntimeException])
  def returnIt(obj: T)

  /**
   * Invalidates an object from the pool
   * By contract, <i>obj</i> MUST have been obtained
   * using {@link #borrowObject() borrowObject}
   * or a related method as defined in an implementation
   * or sub-interface.
   * <p>
   * This method should be used when an object that has been borrowed
   * is determined (due to an exception or other problem) to be invalid.
   * If the connection should be validated before or after borrowing,
   * then the {@link PoolableObjectFactory#validateObject} method should be
   * used instead.
   *
   * @param obj a {@link #borrowObject borrowed} instance to be returned.
   */
  @throws(classOf[RuntimeException])
  def invalidate(obj: T)

  /**
   * Create an object using my {@link #setFactory factory} or other
   * implementation dependent mechanism, and place it into the pool.
   * addObject() is useful for "pre-loading" a pool with idle objects.
   * (Optional operation).
   */
  @throws(classOf[RuntimeException])
  def add

  /**
   * Return the number of instances
   * currently idle in my pool (optional operation).
   * This may be considered an approximation of the number
   * of objects that can be {@link #borrowObject borrowed}
   * without creating any new instances.
   *
   * @return the number of instances currently idle in my pool
   * @throws UnsupportedOperationException if this implementation does not support the operation
   */
  @throws(classOf[UnsupportedOperationException])
  def numOfIdle: Int

  /**
   * Return the number of instances
   * currently borrowed from my pool
   * (optional operation).
   *
   * @return the number of instances currently borrowed in my pool
   * @throws UnsupportedOperationException if this implementation does not support the operation
   */
  @throws(classOf[UnsupportedOperationException])
  def numOfActive: Int

  /**
   * Clears any objects sitting idle in the pool, releasing any
   * associated resources (optional operation).
   *
   * @throws UnsupportedOperationException if this implementation does not support the operation
   */
  @throws(classOf[UnsupportedOperationException])
  @throws(classOf[Exception])
  def clear

  /**
   * Close this pool, and free any resources associated with it.
   */
  def close

  /**
   * Sets the {@link PoolableObjectFactory factory} I use
   * to create new instances (optional operation).
   * @param factory the {@link PoolableObjectFactory} I use to create new instances.
   *
   * @throws IllegalStateException when the factory cannot be set at this time
   * @throws UnsupportedOperationException if this implementation does not support the operation
   */
  @throws(classOf[UnsupportedOperationException])
  @throws(classOf[IllegalStateException])
  def factory_=(factory: PoolableObjectFactory[T])
}
