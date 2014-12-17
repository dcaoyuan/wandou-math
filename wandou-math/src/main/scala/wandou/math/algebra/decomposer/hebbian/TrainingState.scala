package wandou.math.algebra.decomposer.hebbian

import wandou.collection.ArrayList
import wandou.math.algebra.DenseVector
import wandou.math.algebra.Matrix
import wandou.math.algebra.Vector
import wandou.math.algebra.decomposer.EigenStatus

class TrainingState(var currentEigens: Matrix, var trainingProjections: Matrix) {

  var numEigensProcessed = 0
  var currentEigenValues: ArrayList[Double] = _
  var trainingIndex = 0
  var helperVector: Vector = DenseVector(currentEigens.numRows)
  var isFirstPass = true
  var statusProgress = new ArrayList[EigenStatus]()
  var activationNumerator = 0.0
  var activationDenominatorSquared = 0.0

  def mostRecentEigen: Vector = {
    currentEigens.viewRow(numEigensProcessed - 1)
  }

  def currentTrainingProjection: Vector = {
    if (trainingProjections.viewRow(trainingIndex) == null) {
      trainingProjections.assignRow(trainingIndex, DenseVector(currentEigens.numCols))
    }
    trainingProjections.viewRow(trainingIndex)
  }
}
