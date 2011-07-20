package name.brijest.sages.gui


import java.awt.image.BufferedImage
import java.awt.Color


/**
 * Adapter used to draw on the screen. Various technologies offer different adapter
 * implementations.
 */
trait DrawAdapter {
  def drawPixel(x: Int, y: Int): Unit
  
  def drawLine(x1: Int, y1: Int, x2: Int, y2: Int): Unit
  def drawLine(p1: (Int, Int), p2: (Int, Int)): Unit = drawLine(p1._1, p1._2, p2._1, p2._2)
  
  def fillRect(x: Int, y: Int, w: Int, h: Int): Unit
  
  def drawImage(x: Int, y: Int, img: BufferedImage, alpha: Float): Unit
  def drawImage(loc: (Int, Int), img: BufferedImage, alpha: Float): Unit = drawImage(loc._1, loc._2, img, alpha)
  def drawImage(x: Int, y: Int, img: BufferedImage): Unit = drawImage(x, y, img, 1.f)
  def drawImage(loc: (Int, Int), img: BufferedImage): Unit = drawImage(loc._1, loc._2, img, 1.f)
  
  def setColor(c: Color): Unit
  def setColor(r: Int, g: Int, b: Int): Unit = setColor(new Color(r, g, b))
  
  def scale(x: Double, y: Double): Unit
}


















