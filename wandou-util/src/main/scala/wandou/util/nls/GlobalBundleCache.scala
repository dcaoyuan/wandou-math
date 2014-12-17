package wandou.util.nls

import java.util.HashMap
import java.util.Locale
import java.util.Map

/**
 * Global cache of translation bundles.
 * <p>
 * Every translation bundle will be cached here when it gets loaded for the
 * first time from a thread. Another lookup for the same translation bundle
 * (same locale and type) from the same or a different thread will return the
 * cached one.
 * <p>
 * Note that NLS instances maintain per-thread Map of loaded translation
 * bundles. Once a thread accesses a translation bundle it will keep reference
 * to it and will not call {@link #lookupBundle(Locale, Class)} again for the
 * same translation bundle as long as its locale doesn't change.
 */
object GlobalBundleCache {
  private val cachedBundles = new HashMap[Locale, Map[Class[_], TranslationBundle]]()

  /**
   * Looks up for a translation bundle in the global cache. If found returns
   * the cached bundle. If not found creates a new instance puts it into the
   * cache and returns it.
   *
   * @param <T>
   *            required bundle type
   * @param locale
   *            the preferred locale
   * @param type
   *            required bundle type
   * @return an instance of the required bundle type
   * @exception TranslationBundleLoadingException see {@link TranslationBundle#load(Locale)}
   * @exception TranslationStringMissingException see {@link TranslationBundle#load(Locale)}
   */
  def lookupBundle[T <: TranslationBundle](locale: Locale, tpe: Class[T]): T = synchronized {
    try {
      var bundles = cachedBundles.get(locale)
      if (bundles == null) {
        bundles = new HashMap[Class[_], TranslationBundle]();
        cachedBundles.put(locale, bundles)
      }
      var bundle = bundles.get(tpe)
      if (bundle == null) {
        bundle = tpe.newInstance
        bundle.load(locale)
        bundles.put(tpe, bundle)
      }
      bundle.asInstanceOf[T]
    } catch {
      case e: InstantiationException => throw new Error(e)
      case e: IllegalAccessException => throw new Error(e)
    }
  }
}
