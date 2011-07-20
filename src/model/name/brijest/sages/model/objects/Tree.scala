package name.brijest.sages.model.objects



import name.brijest.sages.model._



abstract class Tree extends GuisableObject with NonListening {
  def size = (1, 1)
  def interactive = List()
  def interact(thisloc: (Int, Int), that: Object, thatloc: (Int, Int)): Unit = null
}
