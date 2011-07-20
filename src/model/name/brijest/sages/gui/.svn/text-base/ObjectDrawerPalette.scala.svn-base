package name.brijest.sages.gui


import scala.collection._

import name.brijest.sages.model.Instance


class ObjectDrawerPalette {
  private val drawermap = mutable.HashMap[String, ObjectDrawer]()
  def registerDrawer(td: ObjectDrawer) {
    if (drawermap.contains(td.name))
      throw new IllegalArgumentException("Palette already contains drawer for " + td.name + "!");
    else drawermap.put(td.name, td)
  }
  def registerDrawer(cls: Class[_]) {
    val td = cls.newInstance.asInstanceOf[ObjectDrawer]
    registerDrawer(td)
  }
  def createDrawer(name: String, inst: Instance) = {
    drawermap.get(name) match {
      case Some(td) => td.copy
      case None => throw new IllegalArgumentException("Drawer for " + name + " missing.")
    }
  }
}







