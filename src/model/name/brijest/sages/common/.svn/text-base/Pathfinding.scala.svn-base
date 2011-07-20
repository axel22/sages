package name.brijest.sages.common


import name.brijest.sages.model.ModelView
import name.brijest.sages.common.Calculus._
import name.brijest.sages.common.collections.IndexedHeap


object Pathfinding {
  private class Slot(x: Int, y: Int, par: Slot) extends (Int, Int)(x, y) with Ordered[Slot] {
    def this(s: (Int, Int)) = this(s._1, s._2, null)
    def this(s: (Int, Int), p: Slot) = this(s._1, s._2, p)
    
    var parent: Slot = par
    var costToHere = 0.0
    def compare(t: Slot) = if (x < t._1) -1 else if (x > t._1) 1 else if (y < t._2) -1 else if (y > t._2) 1 else 0
    def adjacent = (x - 1, y - 1) :: (x, y - 1) :: (x + 1, y - 1) ::
      (x - 1, y) :: (x + 1, y) :: (x - 1, y + 1) :: (x, y + 1) :: (x + 1, y + 1) :: Nil
    def toLoc = (x, y)
  }
  
  /**
   * Searches starting at the start slot (which may not be walkable), using the specified
   * heuristic, until a slot that obeys a specified condition is found.
   * Note that the heuristic must be conservative. All metrics are conservative.
   */
  private def aStarPath(v: ModelView, start: (Int, Int), heuristic: ((Int, Int)) => Double,
                        condition: ((Int, Int)) => Boolean, movecost: ((Int, Int)) => Double): List[(Int, Int)] =
  {
    // initialize two collections - OPEN and CLOSED slot collections
    // OPEN - contains all unchecked slots (in essence, search front)
    // CLOSED - contains all slots that have been opened and checked
    // put 'start' into OPEN
    def within(t: (Int, Int)) = t._1 >= 0 && t._2 >= 0 && t._1 < v.size && t._2 < v.size
    val open = new IndexedHeap[Slot, Double]
    val closed = new IndexedHeap[Slot, Double]
    open.put(new Slot(start), heuristic(start))
    
    // while there are slots in OPEN, remove the cheapest slot from OPEN, check the condition
    // and expand it - put it's unseen neighbours in OPEN, adjust neighbours in OPEN, ignore those in CLOSED
    // (note that unwalkable slots must be ignored, as well as the slots outside the map)
    while (!open.isEmpty) {
      val (currcost, current) = open.removeFirst
      closed.put(current, currcost)
//      println(current + ", c = " + currcost + ", parent = " + current.parent)
      
      if (condition(current)) {
        // the target location has been found - return path
        def build(path: List[(Int, Int)], slot: Slot): List[(Int, Int)] = if (slot == null) path 
                                                                          else build(slot.toLoc :: path, slot.parent)
        return build(Nil, current)
      } else for (ngb <- current.adjacent; if within(ngb) && v.walkable(ngb)) {
        // expand neighbours
        val neighbour = new Slot(ngb, current)
        def costToHere(s: Slot) = (if (s.parent == null) 0.0 else s.parent.costToHere + movecost(s))
        def expectedCost(s: Slot) = s.costToHere + heuristic(s)
        if (open.contains(neighbour)) {
          // see if cost should be adjusted
          neighbour.costToHere = costToHere(neighbour)
          val newcost = expectedCost(neighbour)
          val oldcost = open(neighbour)
          if (newcost < oldcost) open.put(neighbour, newcost)
        } else if (closed.contains(neighbour)) {
          // ignore this slot - it's already been seen
        } else {
          // add to OPEN as new slot
          neighbour.costToHere = costToHere(neighbour)
          open.put(neighbour, expectedCost(neighbour))
        }
      }
    }
    
    Nil
  }
  
  /**
   * Searches for a path starting at the start slot until a path to target slot is found.
   * 
   * @param unitcost
   * The cost of moving across a single normal terrain slot.
   */
  def aStarPathToTarget(v: ModelView, start: (Int, Int), target: (Int, Int), unitcost: Double) =
    if (!v.walkable(target)) Nil
    else aStarPath(v, start, maxnorm(target, _) * unitcost, _ == target, v.slot(_).terrain.movementCost)
  /**
   * Searches for a path starting at the start slot until a slot near the target slot is
   * found.
   * 
   * @param unitcost
   * The cost of moving across a single normal terrain slot.
   */
  def aStarPathNearTarget(v: ModelView, start: (Int, Int), target: (Int, Int), unitcost: Double) =
    aStarPath(v, start, maxnorm(target, _) * unitcost, maxnorm(target, _) <= 1.01, v.slot(_).terrain.movementCost)
  /**
   * Searches for a path starting at the start slot until a path to target slot is found.
   * A specified movement cost function is used.
   */
  def aStarPathToTarget(v: ModelView, start: (Int, Int), target: (Int, Int),
                        movecost: ((Int, Int)) => Double, unitcost: Double) =
    if (!v.walkable(target)) Nil
    else aStarPath(v, start, maxnorm(target, _) * unitcost, _ == target, movecost)
  /**
   * Searches for a path starting at the start slot until a slot near the target slot is
   * found.
   * A specified movement cost function is used.
   */
  def aStarPathNearTarget(v: ModelView, start: (Int, Int), target: (Int, Int),
                          movecost: ((Int, Int)) => Double, unitcost: Double) =
    aStarPath(v, start, maxnorm(target, _) * unitcost, maxnorm(target, _) <= 1.01, movecost)
}


















