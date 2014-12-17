package wandou.math.vector

import java.util.Random
import scala.reflect.ClassTag

/**
 * An (Input, Output) pair: one point of combined input-output space.
 *
 * @author Caoyuan Deng
 */
class InputOutputPoint protected (val input: Vec, val output: Vec)

object InputOutputPoint {
  def apply(input: Vec, output: Vec) = new InputOutputPoint(input, output)
  def apply(inputDimension: Int, outputDimension: Int) = new InputOutputPoint(new DefaultVec(inputDimension), new DefaultVec(outputDimension))

  def unapply(iop: InputOutputPoint): Option[(Vec, Vec)] = Some(iop.input, iop.output)

  def randomizeOrder_createNew[T <: InputOutputPoint: ClassTag](iops: Array[T]): Array[T] = {
    val size = iops.length
    val result = Array.ofDim[T](size)

    System.arraycopy(iops, 0, result, 0, size)
    val random = new Random(System.currentTimeMillis)
    var i = 0
    while (i < size) {
      val next = random.nextInt(size - i)
      val iop = result(next)
      result(next) = result(i)
      result(i) = iop
      i += 1
    }

    result
  }
}
