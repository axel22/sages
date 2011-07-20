package name.brijest.sages.gui

import java.io.File
import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import scala.collection._
import scala.ref.SoftReference

class ImageCache {
  private val tmpdir = new File("/tmp/")
  private val imagerefs = mutable.Map[String, SoftReference[BufferedImage]]()
  private val arrayrefs = mutable.Map[String, SoftReference[Array[BufferedImage]]]()
  def getImage(cls: Class[_], name: String) = {
    def addImage(name: String) = {
      val bimg = ImageIO.read(cls.getResourceAsStream(name))
      imagerefs.put(name, new SoftReference(bimg))
      bimg
    }
    imagerefs.get(name) match {
      case Some(ref) => ref.get match {
        case Some(img) => img
        case None => addImage(name)
      }
      case None => addImage(name)
    }
  }
  def getAPNGImages(cls: Class[_], name: String) = {
    def addImageArr(name: String) = {
      // unjar the contents
      val tmpname = "tmpimg_" + name + ".png"
      val tmpfile = if (!new File(tmpdir.getAbsolutePath + tmpname).exists) {
        val tfile = java.io.File.createTempFile("tmpimg_" + name, ".png", tmpdir)
        val fos = new java.io.FileOutputStream(tfile)
        val is = cls.getResourceAsStream(name)
        try {
          org.apache.commons.io.IOUtils.copy(is, fos)
          tfile
        } finally {
          is.close
          fos.close
        }
      } else new File(tmpdir.getAbsolutePath + tmpname)
      
      // put them to 
      val bimgs = new com.sixlegs.png.AnimatedPngImage().readAllFrames(tmpfile)
      arrayrefs.put(name, new SoftReference(bimgs))
      tmpfile.deleteOnExit
      bimgs
    }
    arrayrefs.get(name) match {
      case Some(ref) => ref.get match {
        case Some(imgs) => imgs
        case None => addImageArr(name)
      }
      case None => addImageArr(name)
    }
  }
  def singleFrameAni(cls: Class[_], name: String) = new SingleFrameAnimation(getImage(cls, name))
  def singleFrameAniLib(cls: Class[_], names: Array[String]) =
    new SingleFrameAnimationLib(for (n <- names) yield getImage(cls, n))
  def loopedAni(cls: Class[_], name: String, framelen: Int) =
    new LoopedAnimation(framelen, getAPNGImages(cls, name))
  def loopedAni(name: String, framelen: Int): LoopedAnimation = loopedAni(getClass, name, framelen)
}













