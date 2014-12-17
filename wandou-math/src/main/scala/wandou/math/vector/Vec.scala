package wandou.math.vector

/**
 * A vector of real numbers.
 *
 * @author Caoyuan Deng
 */
trait Vec extends Cloneable {
  /**
   * The elements of this <code>Vec</code> as an <code>Array[double]</code>.
   *
   * @return the <code>Array[double]</code>
   */
  def values: Array[Double]
  def values_=(values: Array[Double])

  /**
   * Element-wise addition.
   *
   * @param vec   the <code>Vec</code> to plus
   * @return the result of the operation
   */
  def plus(operand: Vec): Vec

  /**
   * Scalar addition.
   *
   * @param operand   the amount to plus
   * @return the result of the sum
   */
  def plus(operand: Double): Vec

  def minus(operand: Vec): Vec

  /**
   * Appends an element to the <code>Vec</code>.
   * @Notice
   * that is an inefficient operation and should be rarely used.
   *
   *
   * @param value   the value of the element to add
   */
  def add(value: Double)

  /**
   * Compute an Euclidean metric (or distance) from this <code>Vec</code> to
   * another.
   *
   * @param other   the <code>Vec</code> to measure the metric (or distance) with
   * @return the metric
   */
  def metric(other: Vec): Double

  /**
   * Compute the inner product of two <code>Vec</code>s.
   *
   * @param operand   the other <code>Vec</code>
   * @return the inner product
   */
  def innerProduct(operand: Vec): Double

  /**
   * <Xi dot Xi> the inner product of this vec itself
   */
  def square: Double

  /**
   * Scalar multipication.
   *
   * @param operand   the amount to times
   * @return the resulting <code>Vec</code>
   */
  def times(operand: Double): Vec

  /**
   * Compute a 1-norm (sum of absolute values) of the <code>Vec</code>.
   * norm (or length)
   *
   * @return the norm
   */
  def normOne: Double

  /**
   * Compute a 2-norm (square root of the sum of the squared values) of the
   * <code>Vec</code>.
   *
   *
   * @return the norm
   */
  def normTwo: Double

  /**
   * Returns the <i>idx </i>-nary element of the <code>Vec</code>.
   *
   * @param idx   the index of the desired element
   * @return the value of the element
   */
  def apply(idx: Int): Double

  /**
   * Sets element of index <code>i</code> to <code>value</code>.
   *
   * @param idx   index of the element to set
   * @param value    the value to set
   */
  def update(idx: Int, value: Double)

  /**
   * Sets all <code>Vec</code> elements to <code>value</code>.
   *
   * @param value   the value to set
   */
  def setAll(value: Double)

  /**
   * Sets elements to the ones of <code>orig</code>.
   *
   * @param orig   the <code>Vec</code> with the elements to set
   */
  def copy(orig: Vec)

  def copy(src: Vec, srcPos: Int, destPos: Int, length: Int)

  /**
   * @return the dimension of this <code>Vec</code>
   */
  def dimension: Int

  /**
   * Randomizes this <code>Vec</code> with values bounded by
   * <code>min</code> and <code>max</code>.
   *
   * @param min   lower bound
   * @param max   upper bound
   */
  def randomize(min: Double, max: Double)

  /**
   * Checks if a <code>Vec</code> has equal dimension of this <code>Vec</code>.
   *
   * @param comp   <code>Vec</code> to test with
   */
  def checkDimensionEquality(comp: Vec)

  def checkValidation: Boolean

  override def clone: Vec = super.clone.asInstanceOf[Vec]
}
