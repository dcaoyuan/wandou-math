package wandou.math.algebra.decomposer

import wandou.math.algebra.Vector
import wandou.math.algebra.VectorIterable

class SimpleEigenVerifier extends SingularVectorVerifier {

  def verify(corpus: VectorIterable, vector: Vector): EigenStatus = {
    val resultantVector = corpus.timesSquared(vector)
    val newNorm = resultantVector.norm(2)
    val oldNorm = vector.norm(2)
    if (newNorm > 0 && oldNorm > 0) {
      new EigenStatus(newNorm / oldNorm, resultantVector.dot(vector) / newNorm * oldNorm, false)
    } else {
      new EigenStatus(1.0, 0.0, false)
    }
  }
}
