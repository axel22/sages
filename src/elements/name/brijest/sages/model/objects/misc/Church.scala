package name.brijest.sages.model.objects.misc




import name.brijest.sages.model.objects.NonListening
import name.brijest.sages.model.Object
import name.brijest.sages.gui.AnimatedObjectDrawer
import name.brijest.sages.model.Event
import name.brijest.sages.model.objects.ObjectImageCacher


class Church extends Object with NonListening {
  def objectname = "Church"
  def nicename = "Church"
  def size = (2, 2)
  def interactive = List((1, 1))
  def interact(thisloc: (Int, Int), that: Object, thatloc: (Int, Int)) = null
}

class ChurchDrawer
extends AnimatedObjectDrawer(-82, -88, ObjectImageCacher.singleFrameAni(classOf[Church], "church.png")) {
  def name = "Church"
  def boundingbox = ((-82, -88), (38, 32))
  def copy = new ChurchDrawer
  def effectDrawerFor(e: Event) = None
}











