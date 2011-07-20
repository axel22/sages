package name.brijest.sages.model.objects.trees


import name.brijest.sages.gui.ImageCache
import name.brijest.sages.gui.GuisableObjectDrawer
import name.brijest.sages.model.Event
import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model.objects._


class ForestTree extends Tree {
  setProp("Total guises", IntNum(2))
  def objectname = "ForestTree"
  def nicename= "Tree"
}

object ForestTree {
  val picnames = Array(
    "ftree1.png",
    "ftree2.png"
  )
}

class ForestTreeDrawer 
extends GuisableObjectDrawer(-56, -68, ObjectImageCacher.
                               singleFrameAniLib(classOf[ForestTree], ForestTree.picnames)) {
  def name = "ForestTree"
  def boundingbox = ((-56, -68), (16, 4))
  def copy = new ForestTreeDrawer
  override def effectDrawerFor(e: Event) = None
}









