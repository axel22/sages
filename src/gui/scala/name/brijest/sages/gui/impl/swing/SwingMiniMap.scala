package name.brijest.sages.gui.impl.swing


import scala.swing._
import scala.swing.event._

import java.awt.Graphics2D

import name.brijest.sages.gui._


class SwingMiniMap extends Component with MiniMap {
  listenTo(Mouse.clicks)
  reactions += {
    case MouseClicked(src, p, modif, cl, tpop) if (modif == 0) => onMouseClick(p.x, p.y)
  }
  preferredSize = new java.awt.Dimension(150, 100)
  maximumSize = new java.awt.Dimension(150, 100)
  
  def invokeRepaint = repaint
  def width = size.width
  def height = size.height
  override protected def paintComponent(g: Graphics2D) = paint(new SwingDrawAdapter(g))
}
