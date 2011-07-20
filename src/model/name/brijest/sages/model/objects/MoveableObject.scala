package name.brijest.sages.model.objects


import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model.Action
import name.brijest.sages.model.Operation._
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Depot
import name.brijest.sages.model.OpResult
import name.brijest.sages.model.quantities.Path
import name.brijest.sages.model.quantities.ValidPathConstraint
import name.brijest.sages.model._


import scala.collection.mutable.ArrayBuffer


abstract class MoveableObject extends Object {
  define("Total MP", "Total number of movement points per turn.", new IntNum)
  define("MP left", "Remaining number of movement points in this turn.", new IntNum)
  define(Action("Move", "Moves the object along the path as long as it has MPs.",
                new AnonymousOperation {
                  def apply(m: ModelView, a: Model#Adapter, own: Depot, ind: Option[Int], params: Array[Value]) =
                    move(m, a, own, ind, params)
                  def parameters = List(classOf[Path])
                  def constraints = List(new ValidPathConstraint)
                }
  ))
  
  private def move(m: ModelView, a: Model#Adapter, d: Depot, i: Option[Int], params: Array[Value]) = {
    val path = params(0).asInstanceOf[Path]
    val mpsbefore = prop("MP left").asInteger
    def domove(p: List[(Int, Int)], mps: Int): (Int, (Int, Int)) = (p: @unchecked) match {
      case head :: second :: tail =>
        val after = mps - movementCost(m.slot(second).terrain)
        if (after > 0) domove(second :: tail, after)
        else (mps, head)
      case head :: Nil => (mps, head)
    }
    val (mpsafter, targetloc) = domove(path.nodes, mpsbefore)
    val Some(index) = i
    a.changeInstanceProperty(index, "MP left", IntNum(mpsafter))
    a.relocateInstance(index, targetloc)
    OpResult.successful("Target", targetloc._1 + "," + targetloc._2)
  }
  def movementCost(terrain: Terrain): Int
}












