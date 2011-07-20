package name.brijest.sages.gui.initializer


import name.brijest.sages.common.Pathfinding._
import name.brijest.sages.model.Instance
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.quantities.Path
import name.brijest.sages.model.objects.MoveableObject
import name.brijest.sages.controller.impl.commands.CombinedCommand.combine
import name.brijest.sages.model.executions.ExecutionCombinatorFactory._
import name.brijest.sages.controller._
import name.brijest.sages.model._
import name.brijest.sages.gui._


object MoveObjectListening extends InitializionPart {
  def init(in: Initializer) {
    // set object movement listening
    import in._
    
    factory.canvas.addTerrainActionListener(new TerrainActionAdapter {
      override def onTerrainClick(slot: (Int, Int)) {
        if (!guistate.isMoveableSelected) {
          // 1) no moveable object is selected - no movement once a terrain click event occurs
          // -> do nothing
        } else if (!guistate.isPathChosen) {
          // 2) a moveable object is selected, but no path is selected - once a terrain click occurs,
          //    a path must be found - no path must be found if the click occured at the same
          //    position as the selected object
          // -> try to find a path between the object and the target - if there is one, select it
          ctrl.view(v => selectPath(v, in, slot))
        } else {
          // 3) an object is selected and a path is selected - reset path selection, and then issue a
          //    movement command to the controller if the click is at the same slot as the path end,
          //    but reselect path otherwise
          // -> use to path to send the move command
          def findEnd(path: List[(Int, Int)]): (Int, Int) = (path: @unchecked) match {
            case lastSlot :: Nil => lastSlot
            case head :: tail => findEnd(tail)
          }
          val path = guistate.selectedPath
          val selind = guistate.selectedObjectIndex
          val pathend = findEnd(path)
          if (pathend == slot) ctrl.send(invokeAction(selind, "Move", Path(path))(player)) {
            opres => // do nothing
          } else {
            ctrl.view(v => selectPath(v, in, slot))
          }
        }
      }
    })
    ctrl.addListener(new EventListener {
      def listens = classOf[RelocateInstanceEvent] :: classOf[RemoveInstanceEvent] :: Nil
      def onEvent(e: Event) = e match {
        case RelocateInstanceEvent(oldloc, nloc, inst) => if (guistate.selectedObjectIndex == inst.index)
          removeDecorations(in)
        case RemoveInstanceEvent(loc, inst) => if (guistate.selectedObjectIndex == inst.index)
          removeDecorations(in)
      }
    })
  }
  
  private def removeDecorations(in: Initializer) {
    import in._
    
    guistate.selectedPath = Nil
    ctrl.view(updateDecorationDrawers(_, in))
  }
  
  private def selectPath(v: ModelView, in: Initializer, slot: (Int, Int)) {
    import in._
    
    val Some(selected) = guistate.selectedObject
    val selpos = v.instanceMap.getLocationFor(selected)
    val moveable = selected.prototype.asInstanceOf[MoveableObject]
    if (selpos != slot) {
      val path = aStarPathToTarget(v, selpos, slot, s => moveable.movementCost(v.slot(s).terrain), 10.0)
      guistate.selectedPath = path
    }
    
    updateDecorationDrawers(v, in)
  }
  private def target(t: (Int, Int), r: Boolean) = (t._1, t._2, new TerrainDecorationDrawer(ArrowPics.target(r)))
  private def arrow(from: (Int, Int), to: (Int, Int), r: Boolean) = (from._1, from._2,
                                                                     (to._1 - from._1, to._2 - from._2) match {
    case (0, -1) => new TerrainDecorationDrawer(ArrowPics.north(r))
    case (-1, -1) => new TerrainDecorationDrawer(ArrowPics.northwest(r))
    case (-1, 0) => new TerrainDecorationDrawer(ArrowPics.west(r))
    case (-1, 1) => new TerrainDecorationDrawer(ArrowPics.southwest(r))
    case (0, 1) => new TerrainDecorationDrawer(ArrowPics.south(r))
    case (1, 1) => new TerrainDecorationDrawer(ArrowPics.southeast(r))
    case (1, 0) => new TerrainDecorationDrawer(ArrowPics.east(r))
    case (1, -1) => new TerrainDecorationDrawer(ArrowPics.northeast(r))
    case _ => throw new IllegalStateException("Illegal path.")
  })
  private def updateDecorationDrawers(v: ModelView, in: Initializer) {
    import in._
    
    // remove old path decorations if any
    if (guistate.hasPathDecorations) {
      val Some(pdl) = guistate.pathDecorations
      factory.canvas.removeTerrainDecorationList(pdl)
    }
    
    // add new path decorations if needed
    if (guistate.isPathChosen) {
      val Some(moveable) = guistate.selectedMoveable
      def decoList(path: List[(Int, Int)], mp: Int): List[(Int, Int, TerrainDecorationDrawer)] = path match {
        case head :: second :: tail => arrow(head, second, mp < moveable.movementCost(v.slot(head).terrain)) :: 
          decoList(second :: tail, mp - moveable.movementCost(v.slot(head).terrain))
        case head :: Nil => target(head, mp < moveable.movementCost(v.slot(head).terrain)) :: Nil
        case Nil => throw new IllegalStateException("Cannot create decoration list from empty path.")
      }
      
      val head :: decorations = decoList(guistate.selectedPath, moveable.prop("MP left").asInteger)
      guistate.pathDecorations = Some(decorations)
      factory.canvas.addTerrainDecorationList(decorations)
    }
  }
}















