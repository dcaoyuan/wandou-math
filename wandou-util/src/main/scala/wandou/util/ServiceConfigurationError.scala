package wandou.util

/**
 * Error thrown when something goes wrong while loading a service provider.
 *
 * <p> This error will be thrown in the following situations:
 *
 * <ul>
 *
 *   <li> The format of a provider-configuration file violates the <a
 *   href="ServiceLoader.html#format">specification</a>; </li>
 *
 *   <li> An {@link java.io.IOException IOException} occurs while reading a
 *   provider-configuration file; </li>
 *
 *   <li> A concrete provider class named in a provider-configuration file
 *   cannot be found; </li>
 *
 *   <li> A concrete provider class is not a subclass of the service class;
 *   </li>
 *
 *   <li> A concrete provider class cannot be instantiated; or
 *
 *   <li> Some other kind of error occurs. </li>
 *
 * </ul>
 *
 *
 * @author Mark Reinhold
 * @version 1.5, 06/04/10
 * @since 1.6
 */

/**
 * Constructs a new instance with the specified message and cause.
 *
 * @param  msg  The message, or <tt>null</tt> if there is no message
 *
 * @param  cause  The cause, or <tt>null</tt> if the cause is nonexistent
 *                or unknown
 */
@SerialVersionUID(74132770414881L)
class ServiceConfigurationError(msg: String, cause: Throwable) extends Error(msg, cause) {

  /**
   * Constructs a new instance with the specified message.
   *
   * @param  msg  The message, or <tt>null</tt> if there is no message
   *
   */
  def this(msg: String) = this(msg, null)
}
