package name.brijest.sages.gui


import scala.collection._


/**
 * This class contains all terrain drawer objects and may instantiate them on demand.
 */
class TerrainDrawerPalette {
  private val drawermap = mutable.HashMap[String, TerrainDrawer]()
  def registerDrawer(td: TerrainDrawer) {
    if (drawermap.contains(td.name))
      throw new IllegalArgumentException("Palette already contains drawer for " + td.name + 
                                           ", " + td.subindex + ".")
    else drawermap.put(td.name, td)
  }
  def registerDrawer(cls: Class[_]) {
    val td = cls.getConstructor(classOf[Int]).newInstance(0.asInstanceOf[Object]).asInstanceOf[TerrainDrawer]
    registerDrawer(td)
  }
  def createDrawer(name: String, subindex: Int) = {
    drawermap.get(name) match {
      case Some(td) => td.copy(subindex)
      case None => throw new IllegalArgumentException("Drawer for " + name + ", " + subindex + " missing.")
    }
  }
}




















