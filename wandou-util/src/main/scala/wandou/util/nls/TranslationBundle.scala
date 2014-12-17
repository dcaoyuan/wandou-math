package wandou.util.nls

import java.util.Locale
import java.util.MissingResourceException
import java.util.ResourceBundle
import wandou.util.ClassVar

/**
 * Base class for all translation bundles that provides injection of translated
 * texts into public String fields.
 */
abstract class TranslationBundle {

  private var _effectiveLocale: Locale = _
  private var _resourceBundle: ResourceBundle = _

  /**
   * @return the locale locale used for loading the resource bundle from which
   *         the field values were taken
   */
  def effectiveLocale = _effectiveLocale

  /**
   * @return the resource bundle on which this translation bundle is based
   */
  def resourceBundle = _resourceBundle

  /**
   * Injects locale specific text in all instance fields of this instance.
   * Only public instance fields of type <code>String</code> are considered.
   * <p>
   * The name of this (sub)class plus the given <code>locale</code> parameter
   * define the resource bundle to be loaded. In other words the
   * <code>this.getClass().getName()</code> is used as the
   * <code>baseName</code> parameter in the
   * {@link ResourceBundle#getBundle(String, Locale)} parameter to load the
   * resource bundle.
   * <p>
   *
   * @param locale
   *            defines the locale to be used when loading the resource bundle
   * @exception TranslationBundleLoadingException see {@link TranslationBundleLoadingException}
   * @exception TranslationStringMissingException see {@link TranslationStringMissingException}
   */
  @throws(classOf[TranslationBundleLoadingException])
  def load(locale: Locale) {
    val bundleClass = getClass
    try {
      _resourceBundle = ResourceBundle.getBundle(bundleClass.getName, locale)
    } catch {
      case e: MissingResourceException => throw new TranslationBundleLoadingException(bundleClass, locale, e)
    }
    _effectiveLocale = resourceBundle.getLocale

    for (field @ ClassVar(name, getter, setter) <- ClassVar.getPublicVars(bundleClass) if field.getter.getReturnType == classOf[String]) {
      try {
        val translatedText = resourceBundle.getString(name)
        field.asInstanceOf[ClassVar[TranslationBundle, String]].set(this, translatedText)
      } catch {
        case e: MissingResourceException =>
          throw new TranslationStringMissingException(bundleClass, locale, name, e)
        case e: IllegalArgumentException =>
          throw new Error(e)
        case e: IllegalAccessException =>
          throw new Error(e)
      }
    }
  }
}