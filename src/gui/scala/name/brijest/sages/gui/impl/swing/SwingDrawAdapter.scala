package name.brijest.sages.gui.impl.swing


import java.awt.Graphics2D
import java.awt.RenderingHints
import java.awt.Color
import java.awt.image.BufferedImage
import java.awt.image.RescaleOp


import name.brijest.sages.gui._


class SwingDrawAdapter(g: Graphics2D) extends DrawAdapter {
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  
  def drawPixel(x: Int, y: Int) = g.fillRect(x, y, 1, 1)
  def drawImage(x: Int, y: Int, img: BufferedImage, alpha: Float) {
    if (alpha == 1.f) g.drawImage(img, x, y, null)
    else {
      val scales = Array(1f, 1f, 1f, alpha)
      val offsets = new Array[Float](4)
      val resc = new RescaleOp(scales, offsets, null)
      g.drawImage(img, resc, x, y)
    }
  }
  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int) = g.drawLine(x1, y1, x2, y2)
  def fillRect(x: Int, y: Int, w: Int, h: Int) = g.fillRect(x, y, w, h)
  def setColor(c: Color) = g setColor c
  def scale(x: Double, y: Double) = g.scale(x, y)
}

















