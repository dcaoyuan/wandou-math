package wandou.util.nls

import java.util.Locale

@SerialVersionUID(1L)
abstract class TranslationBundleException(message: String, val bundleClass: Class[_], val locale: Locale, cause: Exception) extends RuntimeException(message, cause)