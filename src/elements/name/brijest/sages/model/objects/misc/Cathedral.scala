package name.brijest.sages.model.objects.misc

import name.brijest.sages.model.objects.NonListening
import name.brijest.sages.model.Object
import name.brijest.sages.gui.AnimatedObjectDrawer
import name.brijest.sages.model.Event
import name.brijest.sages.model.objects._


class Cathedral extends Object with NonListening {
  def objectname = "Cathedral"
  def nicename = "Cathedral"
  def size = (4, 3)
  def interactive = List((3, 1))
  def interact(thisloc: (Int, Int), that: Object, thatloc: (Int, Int)) = null
}

class CathedralDrawer
extends AnimatedObjectDrawer(-156, -96, ObjectImageCacher.singleFrameAni(classOf[Cathedral], "cathedral.png")) {
  def name = "Cathedral"
  def boundingbox = ((-156, -96), (80, 64))
  def copy = new CathedralDrawer
  def effectDrawerFor(e: Event) = None
}










