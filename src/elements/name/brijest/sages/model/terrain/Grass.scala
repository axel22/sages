package name.brijest.sages.model.terrain


import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import scala.collection._

import name.brijest.sages.model.Terrain
import name.brijest.sages.gui.Animation
import name.brijest.sages.gui.SingleFrameAnimation
import name.brijest.sages.gui.TerrainDrawer
import name.brijest.sages.gui.ImageCache
import name.brijest.sages.common.collections.Neighbourhood

class Grass extends Terrain {
  def name = "Grass"
  def index = 10
  def movementCost = 10
  def categories = List()
  def acquireCategory = null
  def subindices = immutable.Set[Int](0, 1, 2, 3, 4)
  def neighbours = new Neighbourhood()
  def walkable = true
  val colour = new java.awt.Color(30, 120, 30)
}

object GrassDrawer extends ImageCache {
  private val imagenames = Array(
    "grass00.png",
    "grass01.png",
    "grass02.png",
    "grass03.png",
    "grass04.png"
  )
  def getAnimation(subi: Int) = new SingleFrameAnimation(getImage(this.getClass, imagenames(subi)))
}

class GrassDrawer private (si: Int, a: Animation) extends TerrainDrawer("Grass", si, a) {
  def this(subi: Int) = this(subi, GrassDrawer.getAnimation(subi))
  def copy(subi: Int) = new GrassDrawer(subi)
}















