package name.brijest.sages.model.objects.castles


import name.brijest.sages.model.objects.NonListening
import name.brijest.sages.model.Object
import name.brijest.sages.gui.AnimatedObjectDrawer
import name.brijest.sages.model.Event
import name.brijest.sages.model.RemoveInstanceEvent
import name.brijest.sages.gui.AnimatedDrawer
import name.brijest.sages.model.objects.ObjectImageCacher
import name.brijest.sages.model.objects.Town


class KnightCastle extends Town with NonListening {
  def objectname = "Knight castle"
  def nicename = "Knight castle"
  def size = (3, 3)
  def interactive = List((2, 1))
  def interact(thisloc: (Int, Int), that: Object, thatloc: (Int, Int)) = null
}


class KnightCastleDrawer
extends AnimatedObjectDrawer(-74, -40, ObjectImageCacher.
                               singleFrameAni(classOf[KnightCastle], "knight.png")) {
  def name = "Knight castle"
  def boundingbox = ((-74, -40), (72, 60))
  def copy = new KnightCastleDrawer
  def effectDrawerFor(e: Event) = None
  override def helper(name: String) = name match {
    case "icon" => Some(new AnimatedDrawer(ObjectImageCacher.singleFrameAni(classOf[KnightCastle], "knightpic.png")))
    case _ => None
  }
}








