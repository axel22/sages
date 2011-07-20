package name.brijest.sages.gui




object Fundamental {
  val framespersec = 35
  def framelength = (1000.0 / framespersec).toInt
  
  def terrwdt = 48
  def terrhgt = 24
  
  def towniconsize = (54, 36)
  
  def planar2isoslot(u: Double, v: Double, mapsz: Int): (Double, Double) =
    (v / terrhgt + u / terrwdt - mapsz / 2, v / terrhgt - u / terrwdt + mapsz / 2)
  def planar2isoslot(t: (Double, Double), mapsz: Int): (Double, Double) = planar2isoslot(t._1, t._2, mapsz)
  def planar2isoslot(u: Int, v: Int, mapsz: Int): (Double, Double) =
    planar2isoslot(u.toDouble, v.toDouble, mapsz)
  
  def isoslot2planar(x: Double, y: Double, mapsz: Int): (Double, Double) =
    ((mapsz - y + x) * terrwdt / 2, (x + y) * terrhgt / 2)
  def isoslot2planar(p: (Double, Double), mapsz: Int): (Double, Double) = isoslot2planar(p._1, p._2, mapsz)
  def isoslot2planar(x: Int, y: Int, mapsz: Int): (Double, Double) =
    isoslot2planar(x.toDouble, y.toDouble, mapsz)
  
  def maxPlanarWidth(mapsz: Int) = isoslot2planar(mapsz, 0, mapsz)._1 + terrwdt
  def maxPlanarHeight(mapsz: Int) = isoslot2planar(mapsz, mapsz, mapsz)._2 + terrhgt
}








