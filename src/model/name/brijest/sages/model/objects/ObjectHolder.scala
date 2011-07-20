package name.brijest.sages.model.objects



import name.brijest.sages.model._



/**
 * Adds some predefined objects to the dynamic loader.
 */
trait ObjectHolder {
  val objpal = new ObjectPalette
  
  // bootstrap certain objects
  objpal.registerObject(classOf[Sage])
}








