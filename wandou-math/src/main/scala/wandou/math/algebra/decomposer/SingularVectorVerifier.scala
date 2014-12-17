package wandou.math.algebra.decomposer

import wandou.math.algebra.Vector
import wandou.math.algebra.VectorIterable

trait SingularVectorVerifier {
  def verify(eigenMatrix: VectorIterable, vector: Vector): EigenStatus
}
