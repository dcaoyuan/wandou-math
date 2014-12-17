package wandou.math.algebra.decomposer.hebbian

import wandou.math.algebra.Vector

trait EigenUpdater {
  def update(pseudoEigen: Vector, trainingVector: Vector, currentState: TrainingState)
}
