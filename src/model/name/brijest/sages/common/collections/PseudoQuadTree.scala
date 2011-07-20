package name.brijest.sages.common.collections


import scala.collection._




/**
 * Allows efficient insertion of 2-dimensional points, deletion and queries
 * such as "is there a point northeast of the given point", as well as region queries.
 * The KEYey is always a (KEY, KEY), and the data it stores with each KEYey is a generic
 * type T.
 * A data structure by Overmars and van Leeuwen.
 */
class PseudoQuadTree[T](delta: Double)
extends mutable.Map[(Double, Double), T] {
  type KEY = Double
  private abstract class Node {
    def x: KEY
    def y: KEY
    def totalbelow: Int
    def appendLeaves(lst: List[((KEY, KEY), T)]): List[((KEY, KEY), T)]
    def isBalanced: Boolean
    def depth: Int
  }
  private case class Leaf(x: KEY, y: KEY, v: T) extends Node {
    def totalbelow = 1
    def appendLeaves(lst: List[((KEY, KEY), T)]) = ((x, y), v) :: lst
    def isBalanced = true
    def depth = 0;
  }
  private case class Pseudo(x: KEY, y: KEY, children: Array[Node]) extends Node {
    def this(x: KEY, y: KEY) = this(x, y, new Array[Node](4))
    var totalbelow = 0
    def totalchildren = children.foldRight(0)((n: Node, count: Int) => if (n != null) count + 1 else count)
    def updateTotalBelow = totalbelow = children.foldRight(0)((n: Node, count: Int) => 
      if (n != null) count + n.totalbelow else count)
    def childindex(xp: KEY, yp: KEY) = (if (yp > y) 2 else 0) + (if (xp > x) 1 else 0)
    def apply(xp: KEY, yp: KEY) = children(childindex(xp, yp))
    def update(xp: KEY, yp: KEY, n: Node) = children(childindex(xp, yp)) = n
    def isBalanced = {
      val mostallowed = Math.ceil(1 / (2 + 1 - delta) * totalbelow)
      children.forall((n: Node) => if (n != null) n.totalbelow <= mostallowed else true)
    }
    def appendLeaves(lst: List[((KEY, KEY), T)]) = children.foldRight(lst)((n: Node, leaves: List[((KEY, KEY), T)]) =>
      if (n != null) n.appendLeaves(leaves) else leaves)
    def depth = children.foldRight(0)((n: Node, d: Int) => if (n != null) {
      val nd = n.depth
      if (nd >= d) nd + 1 else d
    } else d)
    def nw = children(0)
    def ne = children(1)
    def sw = children(2)
    def se = children(3)
  }
  
  private var root: Node = null
  private var rebalances = 0
  
  // ctor
  if (delta < 0.0 || delta > 2.0) throw new IllegalStateException("Delta value must be between 0 and 2.")
  
  // private
  private def bulkinsert(leaves: Array[((KEY, KEY), T)]): Node = {
    if (leaves.isEmpty) return null
    if (leaves.size == 1) return new Leaf(leaves(0)._1._1, leaves(0)._1._2, leaves(0)._2)
    
    // find midpoint
    // sort
    val xsorted = leaves
    util.Sorting.stableSort(leaves, (a: ((KEY, KEY), T), b: ((KEY, KEY), T)) =>
      if (a._1._1 < b._1._1) true else if (a._1._1 > b._1._1) false else if (a._1._2 < b._1._2) true else false)
    val hx = (xsorted(xsorted.size / 3)._1._1 + xsorted(xsorted.size / 3 + 1)._1._1) / 2
    val ysorted = xsorted.filter(_._1._1 >= hx)
    util.Sorting.stableSort(ysorted, (a: ((KEY, KEY), T), b: ((KEY, KEY), T)) =>
      if (a._1._2 < b._1._2) true else if (a._1._2 > b._1._2) false else if (a._1._1 < b._1._1) true else false)
    val hy = if (ysorted.size > 2) (ysorted(ysorted.size / 2)._1._2 + ysorted(ysorted.size / 2 + 1)._1._2) / 2
             else if (ysorted.size == 2) (ysorted(0)._1._2 + ysorted(0)._1._2) / 2
             else ysorted(0)._1._2
    
    // insert children
    val nw = bulkinsert(leaves.filter((t: ((KEY, KEY), T)) => t._1._1 <= hx && t._1._2 <= hy))
    val ne = bulkinsert(leaves.filter((t: ((KEY, KEY), T)) => t._1._1 > hx && t._1._2 <= hy))
    val sw = bulkinsert(leaves.filter((t: ((KEY, KEY), T)) => t._1._1 <= hx && t._1._2 > hy))
    val se = bulkinsert(leaves.filter((t: ((KEY, KEY), T)) => t._1._1 > hx && t._1._2 > hy))
    
    // create node
    val pseudo = new Pseudo(hx, hy, Array(nw, ne, sw, se))
    pseudo.updateTotalBelow
    pseudo
  }
  private def rebalance(n: Node): Node = {
    // enlist all leafs of the tree
    val leaves = n.appendLeaves(Nil).toArray
    rebalances += leaves.size
    
    // bulkinsert them
    bulkinsert(leaves)
  }
  private def insert(k: (KEY, KEY), v: T, into: Node): Node = if (into == null) {
    new Leaf(k._1, k._2, v)
  } else into match {
    case oldleaf @ Leaf(x, y, currval) if (x != k._1 || y != k._2) => // find middle, create pseudo node
      val newleaf = new Leaf(k._1, k._2, v)
      val pseudo = new Pseudo((x + k._1) / 2, (y + k._2) / 2)
      pseudo(x, y) = oldleaf
      pseudo(k._1, k._2) = newleaf
      pseudo.updateTotalBelow
      pseudo
    case Leaf(x, y, currval) => // replace
      new Leaf(k._1, k._2, v)
    case pseudo @ Pseudo(x, y, children) => // continue searching and then rebalance if necessary
      val subnode = insert(k, v, pseudo(k._1, k._2))
      pseudo(k._1, k._2) = subnode
      pseudo.updateTotalBelow
      if (!subnode.isBalanced && pseudo.isBalanced) pseudo(k._1, k._2) = rebalance(subnode)
      pseudo
  }
  private def remove(k: (KEY, KEY), n: Node): Node = if (n != null) n match {
    case Leaf(k._1, k._2, v) => null
    case pseudo @ Pseudo(x, y, children) =>
      val subnode = remove(k, pseudo(k._1, k._2))
      pseudo(k._1, k._2) = subnode
      pseudo.updateTotalBelow
      if (subnode != null && !subnode.isBalanced && pseudo.isBalanced) pseudo(k._1, k._2) = rebalance(subnode)
      pseudo
    case _ => n
  } else n
  private def findNorthwest(k: (KEY, KEY), n: Node): Option[((KEY, KEY), T)] = if (n == null) None else n match {
    case Leaf(x, y, v) if x <= k._1 && y > k._2 => Some((x, y), v)
    case p @ Pseudo(x, y, ch) if x > k._1 && y <= k._2 => findNorthwest(k, p(k._1, k._2))
    case p @ Pseudo(x, y, ch) if x > k._1 && y > k._2 =>
      val findnw = findNorthwest(k, p.nw)
      if (findnw != None) return findnw
      else findNorthwest(k, p(k._1, k._2))
    case p @ Pseudo(x, y, ch) if x <= k._1 && y <= k._2 =>
      val findnw = findNorthwest(k, p.nw)
      if (findnw != None) return findnw
      else findNorthwest(k, p(k._1, k._2))
    case p @ Pseudo(x, y, ch) if x <= k._1 && y > k._2 =>
      val findnw = findNorthwest(k, p.nw)
      if (findnw != None) return findnw
      val findne = findNorthwest(k, p.ne)
      if (findne != None) return findne
      val findsw = findNorthwest(k, p.sw)
      if (findsw != None) return findsw
      findNorthwest(k, p(k._1, k._2))
    case _ => None
  }
  private def toString(len: Int, n: Node): String = if (n != null) n match {
    case Leaf(x, y, v) => " " * len + "(" + x + ", " + y + ") => " + v + "\n"
    case p @ Pseudo(x, y, v) =>
      " " * len + "h(" + x + ", " + y + ")\n" + p.children.foldRight("")((n: Node, s: String) =>
        s + toString(len + 1, n))
  } else ""
  
  // methods
  def this() = this(1.5)
  def rebalanceCounter = rebalances
  def depth = if (root == null) 0 else root.depth
  def get(k: (KEY, KEY)) = if (root != null) {
    def search(n: Node): Option[T] = n match {
      case Leaf(k._1, k._2, v) => Some(v)
      case p @ Pseudo(x, y, children) => search(p(k._1, k._2))
      case _ => None
    }
    search(root)
  } else None
  override def size = if (root != null) root.totalbelow else 0
  def iterator = if (root != null) root.appendLeaves(Nil).iterator else Nil.iterator
  def -=(k: (KEY, KEY)) = {
    root = remove(k, root)
    if (root != null && !root.isBalanced) root = rebalance(root)
    this
  }
  def +=(p: ((KEY, KEY), T)) = {
    root = insert(p._1, p._2, root)
    if (!root.isBalanced) root = rebalance(root)
    this
  }
  def northwest(k: (KEY, KEY)): Option[((KEY, KEY), T)] = findNorthwest(k, root)
  override def toString = toString(0, root)
}

















