package name.brijest.sages.common.collections



/**
 * Used to serve as a collection of neighbours of the current slot, or object instance
 * on a slot.
 */
class Neighbourhood[T](implicit m: Manifest[T]) extends Iterable[((Int, Int), T)] {
  private val neigh = new Array2D[T](3, 3)
  
  def north = neigh(1, 0)
  def north_=(t: T) = neigh.put(1, 0, t)
  def northeast = neigh(2, 0)
  def northeast_=(t: T) = neigh.put(2, 0, t)
  def east = neigh(2, 1)
  def east_=(t: T) = neigh.put(2, 1, t)
  def southeast = neigh(2, 2)
  def southeast_=(t: T) = neigh.put(2, 2, t)
  def south = neigh(1, 2)
  def south_=(t: T) = neigh.put(1, 2, t)
  def southwest = neigh(0, 2)
  def southwest_=(t: T) = neigh.put(0, 2, t)
  def west = neigh(0, 1)
  def west_=(t: T) = neigh.put(0, 1, t)
  def northwest = neigh(0, 0)
  def northwest_=(t: T) = neigh.put(0, 0, t)
  def center = neigh(1, 1)
  def center_=(t: T) = neigh.put(1, 1, t)
  
  def iterator = new Iterator[((Int, Int), T)] {
    var x = -1
    var y = -1
    
    // position yerself on the first element
    for (i <- 0 until 3; j <- 0 until 3; if x == -1)
      if (neigh(i, j) != null) {
        x = i
        y = j
      }
    
    def hasNext = x != -1
    def next = {
      val curr = neigh(x, y)
      val t = (x, y)
      
      // goto next element
      var found = false
      for (i <- x until 3; j <- y until 3; if !found)
        if (neigh(i, j) != null) {
          found = true
          x = i
          y = j
        }
      if (!found) x = -1
      
      // return current element
      (t, curr)
    }
  }
}






















