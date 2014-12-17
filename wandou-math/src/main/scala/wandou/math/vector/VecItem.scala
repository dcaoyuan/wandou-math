package wandou.math.vector

/**
 * Element of vector
 * By add 'index' field in Element, we can easily to store sparse vector.
 *
 * sparse vector pay-off the searching performance (think about set(index, value),
 * get(index)), so, we'd better to use DefaultVec as default.
 *
 * @author Caoyuan Deng
 */
final case class VecItem(index: Int, value: Double) extends Serializable
