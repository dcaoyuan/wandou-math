package wandou.util.nls

import java.util.Locale

@SerialVersionUID(1L)
class TranslationBundleLoadingException(bundleClass: Class[_], locale: Locale, cause: Exception) extends TranslationBundleException(
  "Loading of translation bundle failed for [" + bundleClass.getName + ", " + locale.toString + "]",
  bundleClass, locale, cause)
