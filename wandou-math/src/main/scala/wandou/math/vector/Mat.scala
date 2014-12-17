package wandou.math.vector

/**
 * Matrix
 *
 * @author Caoyuan Deng
 */
trait Mat {

  /**
   *
   * get one column from the Matrix
   *
   * As we treat this method as a function, We name it column() instead
   * of column()
   */
  def column(column: Int, length: Int): Array[Double]

  /**
   *
   * get dense diagonal elements of the Matrix
   *
   * As we treat this method as a function (in Matrix Maths, diag() is a
   * operation symbol), We name it diag() instead of diag()
   */
  def diag: Array[Double]

  def swapColumn(i: Int, j: Int)
}

