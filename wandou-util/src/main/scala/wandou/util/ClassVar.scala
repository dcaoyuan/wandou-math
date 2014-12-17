package wandou.util

import java.lang.reflect.Method
import java.lang.reflect.Modifier
import scala.reflect.ClassTag

/**
 *
 * @param <C> type of class
 * @param <V> type of var
 *
 * @author Caoyuan Deng
 */
final class ClassVar[C, V: ClassTag](val name: String, val getter: Method, val setter: Method) {
  if (getter != null) getter.setAccessible(true)
  if (setter != null) setter.setAccessible(true)

  def get(from: C): V = {
    try {
      getter.invoke(from).asInstanceOf[V]
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  /**
   * @Note set(to: C, value: Any) instead of set(to: C, value: V) here to prevent
   * the AnyVal type case to be broadcasted to usages.
   */
  def set(to: C, value: V) {
    try {
      if (value != null) {
        setter.invoke(to, value.asInstanceOf[AnyRef])
      } else {
        val propValue = reflect.classTag[V].newArray(1).apply(0)
        setter.invoke(to, propValue.asInstanceOf[AnyRef])
      }
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
  }

  def copy(from: C, to: C) {
    set(to, get(from))
  }
}

object ClassVar {
  def apply[C, V: ClassTag](name: String, getter: Method, setter: Method) = new ClassVar[C, V](name, getter, setter)

  def unapply(x: ClassVar[_, _]) = Some((x.name, x.getter, x.setter))

  def getPublicVars[C](clz: Class[C]): List[ClassVar[C, _]] = {
    var fields: List[ClassVar[C, _]] = Nil
    val methods = clz.getMethods
    for (method <- methods; if Modifier.isPublic(method.getModifiers)) {
      val name = method.getName
      val params = method.getParameterTypes
      if (name.endsWith("_$eq") && params.length == 1) {
        val getterName = name.substring(0, name.length - 4)
        val paramType = params(0)
        methods find (x => x.getName == getterName && x.getReturnType == paramType) match {
          case Some(getter) => fields ::= ClassVar(getterName, getter, method)
          case _            =>
        }
      }
    }
    fields
  }
}
