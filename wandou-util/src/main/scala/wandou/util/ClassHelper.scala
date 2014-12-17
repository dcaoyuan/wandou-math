package wandou.util

/**
 *
 * @author Caoyuan Deng
 */
import java.lang.reflect.Modifier

object ClassHelper {

  // --- classes
  val UnitClass = classOf[Unit]
  val NullClass = classOf[scala.Null]

  val ByteClass = classOf[Byte]
  val ShortClass = classOf[Short]
  val IntClass = classOf[Int]
  val LongClass = classOf[Long]
  val FloatClass = classOf[Float]
  val DoubleClass = classOf[Double]
  val BooleanClass = classOf[Boolean]

  val JByteClass = classOf[java.lang.Byte]
  val JShortClass = classOf[java.lang.Short]
  val JIntegerClass = classOf[java.lang.Integer]
  val JLongClass = classOf[java.lang.Long]
  val JFloatClass = classOf[java.lang.Float]
  val JDoubleClass = classOf[java.lang.Double]
  val JBooleanClass = classOf[java.lang.Boolean]
  val JVoidClass = classOf[java.lang.Void]

  val SeqClass = classOf[collection.Seq[_]]
  val MapClass = classOf[collection.Map[_, _]]

  val StringClass = classOf[String]
  val CharSequenceClass = classOf[java.lang.CharSequence]
  val ByteBufferClass = classOf[java.nio.ByteBuffer]

  val JCollectionClass = classOf[java.util.Collection[_]]
  val JMapClass = classOf[java.util.Map[_, _]]
  val JListClass = classOf[java.util.List[_]]

  val BigDecimalClass = classOf[java.math.BigDecimal]
  val BigIntegerClass = classOf[java.math.BigInteger]
  val DateClass = classOf[java.util.Date]
  val NumberClass = classOf[java.lang.Number]
  val SqlDateClass = classOf[java.sql.Date]
  val SqlTimeClass = classOf[java.sql.Time]
  val SqlTimestampClass = classOf[java.sql.Timestamp]

  // scala> classOf[Int] == java.lang.Integer.TYPE
  // res17: Boolean = true
  // scala> classOf[Int] eq java.lang.Integer.TYPE
  // res18: Boolean = true

  val BooleanType = java.lang.Boolean.TYPE
  val ByteType = java.lang.Byte.TYPE
  val ShortType = java.lang.Short.TYPE
  val IntegerType = java.lang.Integer.TYPE
  val LongType = java.lang.Long.TYPE
  val FloatType = java.lang.Float.TYPE
  val DoubleType = java.lang.Double.TYPE
  val VoidType = java.lang.Void.TYPE

  val UnitType = scala.Unit

  // --- helpers

  private val TupleNameRegex = """scala\.Tuple(\d\d?)""".r
  def isTuple(v: AnyRef): Boolean = isTupleClass(v.getClass)

  def isTupleClass(c: Class[_]): Boolean = {
    c.getName match {
      case TupleNameRegex(count) =>
        val i = count.toInt; i >= 1 && i < 23
      case _ => false
    }
  }

  def isCollectionClass(c: Class[_]): Boolean = {
    JCollectionClass.isAssignableFrom(c) || SeqClass.isAssignableFrom(c)
  }

  def isMapClass(c: Class[_]): Boolean = {
    JMapClass.isAssignableFrom(c) || MapClass.isAssignableFrom(c)
  }

  def isInstance[T](c: Class[T], v: Any): Boolean = {
    c match {
      case UnitClass    => v.isInstanceOf[Unit] // corner case: classOf[Unit].isInstance(()) returns false, but, ().isInstanceOf[Unit] returns true
      case ByteClass    => JByteClass.isInstance(v) || ByteClass.isInstance(v)
      case ShortClass   => JShortClass.isInstance(v) || ShortClass.isInstance(v)
      case IntClass     => JIntegerClass.isInstance(v) || IntClass.isInstance(v)
      case LongClass    => JLongClass.isInstance(v) || LongClass.isInstance(v)
      case FloatClass   => JFloatClass.isInstance(v) || FloatClass.isInstance(v)
      case DoubleClass  => JDoubleClass.isInstance(v) || DoubleClass.isInstance(v)
      case BooleanClass => JBooleanClass.isInstance(v) || BooleanClass.isInstance(v)
      case _            => c.isInstance(v)
    }
  }

  def isScalaClass(c: Class[_]): Boolean = {
    val interfaces = c.getInterfaces
    var i = -1
    while ({ i += 1; i < interfaces.length }) {
      if (interfaces(i).getName == "scala.ScalaObject") return true
    }

    false
  }

  def isScalaSingletonObject(c: Class[_]): Boolean = {
    if (c.getSimpleName.endsWith("$")) {
      try {
        val field = c.getDeclaredField("MODULE$")
        val modifiers = field.getModifiers
        Modifier.isFinal(modifiers) && Modifier.isStatic(modifiers) && isScalaClass(c)
      } catch {
        case ex: NoSuchFieldException => false
      }
    } else false
  }

}

