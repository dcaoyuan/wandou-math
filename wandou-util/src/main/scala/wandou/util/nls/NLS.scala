package wandou.util.nls

import java.util.Locale
import java.util.concurrent.ConcurrentHashMap

/**
 * The purpose of this class is to provide NLS (National Language Support)
 * configurable per thread.
 */
class NLS(locale: Locale) {
  private val map = new ConcurrentHashMap[Class[_], TranslationBundle](8, 0.9f, 1)

  private def get[T <: TranslationBundle](tpe: Class[T]): T = {
    var bundle = map.get(tpe)
    if (bundle == null) {
      bundle = GlobalBundleCache.lookupBundle(locale, tpe);
      // There is a small opportunity for a race, which we may
      // lose. Accept defeat and return the winner's instance.
      val old = map.putIfAbsent(tpe, bundle)
      if (old != null)
        bundle = old
    }
    bundle.asInstanceOf[T]
  }
}

object NLS {
  /** The root locale constant. It is defined here because the Locale.ROOT is not defined in Java 5 */
  def ROOT_LOCALE = new Locale("", "", "")

  private def local = new InheritableThreadLocal[NLS] {
    override protected def initialValue = new NLS(Locale.getDefault)
  }

  /**
   * Sets the locale for the calling thread.
   * <p>
   * The {@link #getBundleFor(Class)} method will honor this setting if if it
   * is supported by the provided resource bundle property files. Otherwise,
   * it will use a fall back locale as described in the
   * {@link TranslationBundle}
   *
   * @param locale
   *            the preferred locale
   */
  def setLocale(locale: Locale) {
    local.set(new NLS(locale))
  }

  /**
   * Sets the JVM default locale as the locale for the calling thread.
   * <p>
   * Semantically this is equivalent to <code>NLS.setLocale(Locale.getDefault())</code>.
   */
  def useJVMDefaultLocale {
    local.set(new NLS(Locale.getDefault))
  }

  /**
   * Returns an instance of the translation bundle of the required type. All
   * public String fields of the bundle instance will get their values
   * injected as described in the {@link TranslationBundle}.
   *
   * @param <T>
   *            required bundle type
   * @param type
   *            required bundle type
   * @return an instance of the required bundle type
   * @exception TranslationBundleLoadingException see {@link TranslationBundleLoadingException}
   * @exception TranslationStringMissingException see {@link TranslationStringMissingException}
   */
  def getBundleFor[T <: TranslationBundle](tpe: Class[T]): T = {
    local.get.get(tpe)
  }

}