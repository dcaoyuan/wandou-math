package wandou.indicator

import wandou.math.timeseries.TBaseSer
import wandou.math.timeseries.TVar

/**
 *
 * @author Caoyuan Deng
 */
class ProbMassIndicator(_baseSer: TBaseSer) extends SpotIndicator(_baseSer) {
  sname = "Probability Mass"
  lname = "Probability Mass"
  isOverlapping = true

  var baseVar: TVar[Double] = _

  val nIntervals = Factor("Number of Intervals", 30.0, 1.0, 1.0, 100.0)
  val period1 = Factor("Period1", 50.0)
  val period2 = Factor("Period2", 100.0)
  val period3 = Factor("Period3", 200.0)

  val MASS1 = STVar[Array[Array[Double]]]("MASS1")
  val MASS2 = STVar[Array[Array[Double]]]("MASS2")
  val MASS3 = STVar[Array[Array[Double]]]("MASS3")

  protected def computeSpot(time: Long, baseIdx: Int) {
    if (baseIdx > 0) {
      val probability_mass1 = probMass(baseIdx, baseVar, period1, nIntervals)
      val probability_mass2 = probMass(baseIdx, baseVar, period2, nIntervals)
      val probability_mass3 = probMass(baseIdx, baseVar, period3, nIntervals)

      MASS1(time) = probability_mass1
      MASS2(time) = probability_mass2
      MASS3(time) = probability_mass3
    }
  }

  override def shortName: String = {
    if (baseVar != null) {
      "PM: " + baseVar.name
    } else "PM"
  }
}

