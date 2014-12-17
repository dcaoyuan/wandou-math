package wandou.util.nls

import java.util.Locale

@SerialVersionUID(1L)
class TranslationStringMissingException(bundleClass: Class[_], locale: Locale, val key: String, cause: Exception) extends TranslationBundleException(
  "Translation missing for [" + bundleClass.getName + ", " + locale.toString + ", " + key + "]",
  bundleClass, locale, cause)
