package name.brijest.sages.model.objects.trees


import name.brijest.sages.gui.ImageCache
import name.brijest.sages.gui.GuisableObjectDrawer
import name.brijest.sages.model.Event
import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model.objects._


class NormalTree extends Tree {
  setProp("Total guises", IntNum(3))
  def objectname = "NormalTree"
  def nicename= "Tree"
}

object TreeCache extends ImageCache {
  private val arr = Array(
    "tree1.png",
    "tree2.png",
    "tree3.png"
  )
  def getAnimation(ind: Int) = singleFrameAni(getClass, arr(ind))
  def getAnimationLib = singleFrameAniLib(getClass, arr)
}

class NormalTreeDrawer extends GuisableObjectDrawer(-48, -65, TreeCache.getAnimationLib) {
  def name = "NormalTree"
  def boundingbox = ((-48, -65), (23, 7))
  def copy = new NormalTreeDrawer
  override def effectDrawerFor(e: Event) = None
}









