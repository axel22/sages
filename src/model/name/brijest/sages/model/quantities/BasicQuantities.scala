package name.brijest.sages.model.quantities

/*******************************************************
 * 
 * This file contains some basic Value types.
 * 
 * @author Aleksandar Prokopec
 * 
 *******************************************************/

import name.brijest.sages.model.Constraint
import name.brijest.sages.common.Calculus._
import name.brijest.sages.model._

/**
 * Value for normal text.
 */
final case class Text(txt: String) extends Value {
  def this() = this("")
  
  def toXML = {
    <txt>{txt}</txt>
  }
  def fromXML(node: scala.xml.Node) = {
    new Text(node.text)
  }
  override def equals(other: Any) = other match {
    case that: Text => txt == that.txt
    case _ => false
  }
  override def hashCode = txt.hashCode
}

/**
 * Value for integers.
 */
final case class IntNum(num: Int) extends Value {
  def this() = this(0)
  
  def toXML = {
    <num>{num}</num>
  }
  def fromXML(node: scala.xml.Node) = {
    new IntNum(Integer.parseInt(node.text))
  }
  override def equals(other: Any) = other match {
    case that: IntNum => num == that.num
    case _ => false
  }
  override def hashCode = num.hashCode
}

object IntNum {
  implicit def int2IntNum(n: Int) = new IntNum(n)
}

/**
 * Value for integers.
 */
final case class DecNum(num: Double) extends Value {
  def this() = this(0.0)
  
  def toXML = {
    <num>{num}</num>
  }
  def fromXML(node: scala.xml.Node) = {
    new DecNum(java.lang.Double.parseDouble(node.text))
  }
  override def equals(other: Any) = other match {
    case that: DecNum => num == that.num
    case _ => false
  }
  override def hashCode = num.hashCode
}

/**
 * Value for colours.
 */
final case class Colour(r: Int, g: Int, b: Int) extends Value {
  def this() = this(0, 0, 0)
  
  def toXML = {
    <colour><r>{r}</r><g>{g}</g><b>{b}</b></colour>
  }
  def fromXML(node: scala.xml.Node) = {
    new Colour(Integer parseInt (node \ "r")(0).text,
               Integer parseInt (node \ "g")(0).text,
               Integer parseInt (node \ "b")(0).text)
  }
  override def equals(other: Any) = other match {
    case that: Colour => r == that.r && g == that.g && b == that.b
    case _ => false
  }
  override def hashCode = r.hashCode << 22 + g.hashCode << 11 + b.hashCode
}

/**
 * Value for bitsets.
 */
final class Bitmap(bitset: scala.collection.mutable.BitSet, val width: Int, val height: Int) extends Value {
  def this() = this(new scala.collection.mutable.BitSet, 1, 1)
  
  def get(x: Int, y: Int) = bitset(y * width + x)
  def bits: scala.collection.BitSet = bitset
  def cloneBits = bitset.clone
  
  def toXML = {
    <bitmap>
      <w>{width}</w><h>{height}</h>
      <bits>
        {
          val sb = new StringBuilder
          for (i <- 0 until width * height)
            if (bitset(i)) sb append "1"
            else sb append "0"
          sb
        }
      </bits>
    </bitmap>
  }
  def fromXML(node: scala.xml.Node) = {
    val w = Integer parseInt ((node \ "w")(0).text)
    val h = Integer parseInt ((node \ "h")(0).text)
    val bitstr = (node \ "bits")(0).text
    val bitset = new scala.collection.mutable.BitSet
    var i = 0
    for (c <- bitstr) {
      c match {
        case '1' => bitset(i) = true
        case '0' => bitset(i) = false
      }
      i = i + 1
    }
    new Bitmap(bitset, w, h)
  }
  override def equals(other: Any) = other match {
    case that: Bitmap => that.bits == bitset && that.width == width && that.height == height
    case _ => false
  }
  override def hashCode = width << 22 + height << 11 + bitset.hashCode
}

/**
 * This class represents a composition of quantities.
 */
final class Composition(val subvals: Map[String, Value]) extends Value {
  def this() = this(Map[String, Value]())
  
  def add(name: String, value: Value) = new Composition(subvals + (name -> value))
  
  def toXML = {
    <composition>
      {
        for ((k, v) <- subvals) yield
          <ntr>
        <k>{k}</k>
        <cls>{v.getClass.getName}</cls>
        {v.toXML}
      </ntr>
      }
    </composition>
  }
  def fromXML(node: scala.xml.Node) = {
    val ntries = for (n <- node \ "ntr"; v = Class.forName((n \ "cls").text).newInstance.asInstanceOf[Value])
      yield ((n \ "k").text, v.fromXML(n.child(5)))
    new Composition(Map() ++ ntries)
  }
  override def equals(other: Any) = other match {
    case that: Composition => that.subvals == subvals
    case _ => false
  }
  override def hashCode = subvals.hashCode
}

/**
 * This class encapsulates the number of turns elapsed. It calculate the
 * number of years, months, weeks and days from the current turn.
 */
final case class Turn(turn: Int) extends Value {
  require(turn >= 0)
  private def cyear = turn / 336
  private def cmonth = (turn - cyear * 336) / 28
  private def cweek = (turn - cyear * 336 - cmonth * 28) / 7
  private def cday = turn - cyear * 336 - cmonth * 28 - cweek * 7
  
  def this() = this(0)
  
  def year = cyear + 1
  def month = cmonth + 1
  def week = cweek + 1
  def day = cday + 1
  def increase = new Turn(turn + 1)
  def isWeekStart = day == 1
  def isWeekEnd = day == 7
  def isMonthStart = isWeekStart && week == 1
  def isYearStart = isMonthStart && month == 1
    
  def toXML = <turn>{turn}</turn>
  def fromXML(node: scala.xml.Node) = new Turn(node.text.toInt)
  override def equals(other: Any) = other match {
    case that: Turn => turn == that.turn
    case _ => false
  }
  override def hashCode = turn
}

case class Location(val x: Int, val y: Int) extends (Int, Int)(x, y) with Value {
  def this() = this(-1, -1)
  
  def toXML = <loc x=" x.toString " y=" y.toString "/>
  def fromXML(node: scala.xml.Node) = {
    val xp = (node \ "@x").text.toInt
    val yp = (node \ "@y").text.toInt
    new Location(xp, yp)
  }
}

class OnMapConstraint extends Constraint {
  def inspect(m: ModelView, v: Value) = v match {
    case Location(x, y) => x >= 0 && x < m.size && y >= 0 && y < m.size
    case _ => false
  }
  def constraintType(m: ModelView, d: Depot) = Range(Location(0, 0), Location(m.size - 1, m.size - 1))
}

case class Path(val nodes: List[(Int, Int)]) extends Value {
  def this() = this(Nil)
  
  def withoutHead = {
    val head :: tail = nodes
    tail
  }
  def toXML = <path>{for ((x, y) <- nodes) yield <n x={ x.toString } y={ y.toString }/>}</path>
  def fromXML(node: xml.Node) = {
    val nodenodes = node \ "n"
    var lst = List[(Int, Int)]()
    for (nn <- nodenodes.reverse) lst = {
      val x = (nn \ "@x").text.toInt
      val y = (nn \ "@y").text.toInt
      (x, y)
    } :: lst
    new Path(lst)
  }
}

class ValidPathConstraint extends Constraint {
  private def checkPath(m: ModelView, path: List[(Int, Int)]): Boolean = path match {
    case head :: second :: tail => m.walkable(head) && near(head, second) && checkPath(m, second :: tail)
    case head :: tail => m.walkable(head)
    case Nil => false
  }
  def inspect(m: ModelView, v: Value) = v match {
    case Path(nodeshead :: nodestail) => checkPath(m, nodestail)
    case _ => false
  }
  def constraintType(m: ModelView, d: Depot) = Criterium() 
}

/** Hack for ant :) */
final class BasicQuantities











