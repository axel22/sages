package name.brijest.sages.common


import Math._


class RichLocation(x: Int, y: Int) extends (Int, Int)(x, y) {
  def direction(other: (Int, Int)): Direction = if (x < other._1) {
    if (y < other._2) SouthEast()
    else if (y > other._2) NorthEast()
    else East()
  } else if (x > other._1) {
	if (y < other._2) SouthWest()
	else if (y > other._2) NorthWest()
	else West()
  } else {
	if (y < other._2) South()
	else if (y > other._2) North()
	else NoDirection()
  }
  def +(other: (Int, Int)) = new RichLocation(x + other._1, y + other._2)
  def -(other: (Int, Int)) = new RichLocation(x - other._1, y - other._2)
}

object Calculus {
  def maxnorm(p1: (Int, Int), p2: (Int, Int)): Double = max((p1._1 - p2._1).abs, (p1._2 - p2._2).abs)
  def manhattan(p1: (Int, Int), p2: (Int, Int)): Double = (p1._1 - p2._1).abs + (p1._2 - p2._2).abs
  def near(p1: (Int, Int), p2: (Int, Int)) = (p1._1 - p2._1).abs <= 1 && (p1._2 - p2._2).abs <= 1
  
  implicit def tuple2RichLocation(t: (Int, Int)) = new RichLocation(t._1, t._2)
}

abstract class Direction {
  def abbrev: String
}
case class NoDirection() extends Direction {
  def abbrev = ""
}
case class North() extends Direction {
  def abbrev = "n"
}
case class NorthWest() extends Direction {
  def abbrev = "nw"
}
case class NorthEast() extends Direction {
  def abbrev = "ne"
}
case class South() extends Direction {
  def abbrev = "s"
}
case class SouthWest() extends Direction {
  def abbrev = "sw"
}
case class SouthEast() extends Direction {
  def abbrev = "se"
}
case class West() extends Direction {
  def abbrev = "w"
}
case class East() extends Direction {
  def abbrev = "e"
}















