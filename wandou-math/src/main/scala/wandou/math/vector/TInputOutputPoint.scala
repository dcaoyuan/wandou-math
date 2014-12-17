package wandou.math.vector

/**
 *
 * @author Caoyuan Deng
 */
class TInputOutputPoint protected (_input: Vec, _output: Vec, val time: Long) extends InputOutputPoint(_input, _output)

object TInputOutputPoint {
  def apply(input: Vec, output: Vec, time: Long) =
    new TInputOutputPoint(input, output, time)
  def apply(inputDimension: Int, outputDimension: Int, time: Long) =
    new TInputOutputPoint(new DefaultVec(inputDimension), new DefaultVec(outputDimension), time)

  def unapply(iop: TInputOutputPoint): Option[(Vec, Vec, Long)] = Some(iop.input, iop.output, iop.time)
}
