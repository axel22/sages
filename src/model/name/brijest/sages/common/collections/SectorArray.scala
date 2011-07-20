package name.brijest.sages.common.collections


import scala.collection._


class SectorArray[T](var secwdt: Int, var sechgt: Int, totwdt: Int, tothgt: Int)(implicit m: Manifest[T])
extends Array2D[mutable.Set[T]](totwdt / secwdt, tothgt / sechgt) {
  private val reversemap = mutable.Map[T, mutable.Set[(Int, Int)]]()
  
  def addToCoords(u: Int, v: Int, t: T) = {
    val (us, vs) = findSector(u, v)
    addToSector(us, vs, t)
  }
  def addToSector(pos: (Int, Int), t: T): Unit = addToSector(pos._1, pos._2, t)
  def addToSector(us: Int, vs: Int, t: T) {
    if (this(us, vs) == null) this.put(us, vs, mutable.Set[T]())
    this(us, vs) + t
    if (!reversemap.contains(t)) reversemap.put(t, mutable.Set())
    reversemap(t) + ((us, vs))
  }
  def removeFromSector(pos: (Int, Int), t: T): Unit = removeFromSector(pos._1, pos._2, t)
  def removeFromSector(us: Int, vs: Int, t: T) {
    if (this(us, vs) != null) {
      if (this(us, vs).contains(t)) reversemap(t) - ((us, vs))
      this(us, vs) - t
    }
  }
  def remove(t: T) {
    reversemap.get(t) match {
      case Some(s) => 
        s.foreach(pos => {
          this(pos._1, pos._2) - t
        })
        reversemap(t).clear
      case None =>
    }
  }
  override def clear = {
    super.clear
    reversemap.clear
  }
  override def resize(w: Int, h: Int) {
    throw new UnsupportedOperationException("Sector array cannot be resized with this method.")
  }
  def resize(sw: Int, sh: Int, totwdt: Int, tothgt: Int) {
    secwdt = sw
    sechgt = sh
    super.resize(totwdt / secwdt, tothgt / sechgt)
    reversemap.clear
  }
  def inSector(s: (Int, Int)): Set[T] = inSector(s._1, s._2)
  def inSector(us: Int, vs: Int): Set[T] = this(us, vs)
  /** Returns a sequence of sectors between the given coordinates. */
  def sectorsInRect(p1: (Int, Int), p2: (Int, Int)) = {
    val (us1, vs1) = findSector(p1)
    val (us2, vs2) = findSector(p2)
    for (x <- us1 to us2; y <- vs1 to vs2) yield (x, y)
  }
  def get(s: (Int, Int)) = if (this(s._1, s._2) == null) mutable.Set[T]()
                           else this(s._1, s._2)
  def findSector(p: (Int, Int)): (Int, Int) = findSector(p._1, p._2)
  def findSector(u: Int, v: Int) = {
    var (us, vs) = (u / secwdt, v / sechgt)
    if (us >= width) us = width - 1
    else if (us < 0) us = 0
    if (vs >= height) vs = height - 1
    else if (vs < 0) vs = 0
    (us, vs)
  }
  
}








