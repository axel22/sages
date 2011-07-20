package name.brijest.sages.gui.initializer


import name.brijest.sages.model.Instance
import name.brijest.sages.model.objects.Sage
import name.brijest.sages.model.objects.MoveableObject
import name.brijest.sages.gui.TerrainDecorationDrawer


class GuiState {
  var selectedObject: Option[Instance] = None
  var selectedPath: List[(Int, Int)] = Nil
  var pathDecorations: Option[List[(Int, Int, TerrainDecorationDrawer)]] = None
  
  def isObjectSelected = selectedObject match {
    case Some(inst) => true
    case None => false
  }
  def selectedObjectIndex = selectedObject match {
    case Some(inst) => inst.index
    case None => -1
  }
  def isSageSelected = selectedObject match {
    case Some(inst) => inst.prototype.isInstanceOf[Sage]
    case None => false
  }
  def isPathChosen = selectedPath != Nil
  def isMoveableSelected = selectedObject match {
    case Some(inst) => inst.prototype.isInstanceOf[MoveableObject]
    case None => false
  }
  def selectedMoveable = selectedObject match {
    case Some(inst) => inst.prototype match {
      case obj: MoveableObject => Some(obj)
      case _ => None
    }
    case None => None
  }
  def hasPathDecorations = pathDecorations match {
    case Some(pd) => true
    case None => false
  }
}
















