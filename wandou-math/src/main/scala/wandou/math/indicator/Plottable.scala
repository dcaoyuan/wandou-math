package wandou.math.indicator

import java.awt.Color

/**
 *
 * @author Caoyuan Deng
 */
trait Plottable {

  def plot: Plot

  def getColor(idx: Int): Color
  def setColor(idx: Int, color: Color)

  def layer: Int
  def layer_=(order: Int)
}

