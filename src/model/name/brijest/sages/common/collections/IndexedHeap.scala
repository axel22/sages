package name.brijest.sages.common.collections


import scala.collection._


/**
 * Contains elements of type T and their cost of type C - the value according to which they are sorted.
 * It allows constant time fetching of elements, logarithmic time fetching of elements with the
 * lowest and highest cost, and logarithmic time insertion, update and deletion.
 */
class IndexedHeap[T, C](implicit ord: Ordering[(C, T)]) extends mutable.Map[T, C] {
  private var tree = new immutable.TreeSet[(C, T)]
  private val map = new mutable.HashMap[T, C]
  
  def -=(elem: T) = (map.get(elem): @unchecked) match {
    case Some(c) =>
      tree -= ((c, elem))
      map.removeKey(elem)
      this
  }
  def +=(kv: (T, C)) = map.get(kv._1) match {
    case Some(c) =>
      tree -= ((c, kv._1))
      tree += ((kv._2, kv._1))
      map.put(kv._1, kv._2)
      this
    case None =>
      map.put(kv._1, kv._2)
      tree += ((kv._2, kv._1))
      this
  }
  def get(element: T) = map.get(element)
  override def size = map.size
  def iterator = map.iterator
  def firstElement = tree.firstKey
  def lastElement = tree.lastKey
  def removeFirst = {
    val f = firstElement
    tree -= f
    map.removeKey(f._2)
    f
  }
  def removeLast = {
    val l = lastElement
    tree -= l
    map.removeKey(l._2)
    l
  }
}










