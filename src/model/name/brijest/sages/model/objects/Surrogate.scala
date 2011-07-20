package name.brijest.sages.model.objects


import name.brijest.sages.model._



/**
 * A object acting like one that occupies a single slot, but not actually existing.
 * Used as a hack in the game engine.
 */
class Surrogate extends Object {
  def objectname = "Surrogate"
  def nicename = "Surrogate"
  def interactive = Nil
  def interact(thisloc: (Int, Int), that: Object, thatloc: (Int, Int)) = null
  def size = (1, 1)
  def events = Nil
  def onEvent(e: Event) = null
}




