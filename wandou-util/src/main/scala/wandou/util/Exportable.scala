package wandou.util

/**
 *
 * @author Caoyuan Deng
 */
trait Exportable[V] {
  /** export to array, map, primitive value etc */
  def exportTo: V
  /** import from array, map, primitive value etc */
  def importFrom(v: V): this.type
}
