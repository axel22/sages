package name.brijest.sages.common


import java.io.File


/**
 * A utility class.
 */
object Utility {
  val currentDir = System.getProperty("user.dir")
  val elementsDir = currentDir + File.separator + "plugins" + File.separator + "elements"
}
