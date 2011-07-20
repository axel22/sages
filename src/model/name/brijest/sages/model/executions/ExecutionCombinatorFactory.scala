package name.brijest.sages.model.executions




import name.brijest.sages.model._




/**
 * Allows creation of execution objects.
 */
object ExecutionCombinatorFactory {
  def addPlayer(p: Player) = new AddPlayerCombinator(p)
  def removePlayer(index: Int) = new RemovePlayerCombinator(index)
  def dig(p: (Int, Int)) = new DigCombinator(p)
  def setSlot(p: (Int, Int), t: TerrainSlot) = new SetSlotCombinator(p, t)
  def addInstance(p: (Int, Int), inst: Instance) = new AddInstanceCombinator(p, inst)
  def removeInstance(index: Int) = new RemoveInstanceCombinator(index)
  def relocateInstance(index: Int, nloc: (Int, Int)) = new RelocateInstanceCombinator(index, nloc)
  def changeState(s: State) = new ChangeStateCombinator(s)
  def changeAtmosphere(a: Atmosphere) = new ChangeAtmosphereCombinator(a)
  def changeQuest(q: Quest) = new ChangeQuestCombinator(q)
  def invokeAction(ind: Int, actnm: String, ps: Array[Value]) = new InvokeInstanceActionCombinator(ind, actnm, ps)
  def invokeAction(ind: Int, actnm: String, v: Value) = new InvokeInstanceActionCombinator(ind, actnm, Array(v))
  def changeProperty(ind: Int, pnm: String, v: Value) = new ChangeInstancePropertyCombinator(ind, pnm, v)
  def interactObjects(index1: Int, index2: Int) = new InteractObjectsCombinator(index1, index2)
  def endTurn = new EndTurnCombinator
  def none = new NullCombinator
}











