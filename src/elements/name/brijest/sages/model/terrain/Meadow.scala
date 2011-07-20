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

class Meadow extends Terrain {
  def name = "Meadow"
  def index = 11
  def movementCost = 10
  def categories = List()
  def acquireCategory = null
  def subindices = immutable.Set(0, 1, 2, 3, 4)
  def neighbours = new Neighbourhood()
  def walkable = true
  val colour = new java.awt.Color(40, 150, 40)
}

object MeadowDrawer extends ImageCache {
  private val imagenames = Array(
    "meadow00.png",
    "meadow01.png",
    "meadow02.png",
    "meadow03.png",
    "meadow04.png"
  )
  def getAnimation(subi: Int) = new SingleFrameAnimation(getImage(this.getClass, imagenames(subi)))
}

class MeadowDrawer private (si: Int, a: Animation) extends TerrainDrawer("Meadow", si, a) {
  def this(subi: Int) = this(subi, MeadowDrawer.getAnimation(subi))
  def copy(subi: Int) = new MeadowDrawer(subi)
}















