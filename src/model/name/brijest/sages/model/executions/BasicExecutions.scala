package name.brijest.sages.model.executions



import name.brijest.sages.model._



/**
 * Represents a combinator used for creating commands. Combinators can be
 * joined together to form more complex combinators. Each combinator can
 * be turned into a command implicitly.<br/>
 * Each command combinator must have an empty ctor.
 */
abstract class ExecutionCombinator {
  /**
   * Used to chain combinators together.
   */
  def &(second: ExecutionCombinator) = new CompositeCombinator(this, Some(second))
  /**
   * Returns an execution object that will be used in the command. It does not need
   * to check if it's possible to perform the execution, the method <code>check</code>
   * is responsible for that.
   */
  def getExecution: (Model#Adapter)=>Unit
  /**
   * Checks to see if the execution performed by the Execution object returned by the
   * <code>getExecution</code> method is possible.
   */
  def check(view: ModelView): Boolean
  def toXML: xml.Node
  def fromXML(node: xml.Node): ExecutionCombinator
}

/**
 * A zero-element-combinator - does nothing, and can be applied to any model :)
 */
class NullCombinator extends ExecutionCombinator {
  def getExecution = (a: Model#Adapter) => {}
  def check(modelview: ModelView) = true
  def toXML = <nullcomb/>
  def fromXML(node: xml.Node) = new NullCombinator
  override def equals(other: Any) = other match {
    case that: NullCombinator => true
    case _ => false
  }
  override def hashCode = 938651
}

/** Adds a player. */
class AddPlayerCombinator(val p: Player) extends ExecutionCombinator {
  def this() = this(null)
  
  def getExecution = (a: Model#Adapter) => if (p != null) a.addPlayer(p)
  def check(modelview: ModelView) =
    !modelview.containsPlayer(p) && !modelview.containsPlayerColour(p.colour)
  def toXML = {
    <addplayer>
      {p.toXML}
    </addplayer>
  }
  def fromXML(node: xml.Node) = {
    val pl = Player.fromXML(node.child(1))
    new AddPlayerCombinator(pl)
  }
  override def equals(other: Any) = other match {
    case that: AddPlayerCombinator => that.p == p
    case _ => false
  }
  override def hashCode = p.hashCode
}

/** Removes a player. */
class RemovePlayerCombinator(val index: Int) extends ExecutionCombinator {
  def this() = this(Int.MinValue)
  def this(p: Player) = this(p.index)
  
  def getExecution = (a: Model#Adapter) => if (index != Int.MinValue) a.removePlayer(index)
  def check(modelview: ModelView) = modelview.players.contains(index)
  def toXML = {
    <removeplayer>
      <index>{index}</index>
    </removeplayer>
  }
  def fromXML(node: xml.Node) = {
    val ind = (node \ "index")(0).text.toInt
    new RemovePlayerCombinator(ind)
  }
  override def equals(other: Any) = other match {
    case that: RemovePlayerCombinator => that.index == index
    case _ => false
  }
  override def hashCode = index
}

/** Digs at location. */
class DigCombinator(val p: (Int, Int)) extends ExecutionCombinator {
  def this() = this(null)
  
  def getExecution = (a: Model#Adapter) => if (p != null) a.digAt(p)
  def check(modelview: ModelView) = modelview.within(p)
  def toXML = <dig x={ p._1.toString } y={ p._2.toString }></dig>
  def fromXML(node: xml.Node) = {
    val x = (node \ "@x")(0).text.toInt
    val y = (node \ "@y")(0).text.toInt
    new DigCombinator(x, y)
  }
  override def equals(other: Any) = other match {
    case that: DigCombinator => that.p == p
    case _ => false
  }
  override def hashCode = p.hashCode
}

/** Sets slot at location. */
class SetSlotCombinator(val p: (Int, Int), val t: TerrainSlot) extends ExecutionCombinator {
  def this() = this(null, null)
  
  def getExecution = (a: Model#Adapter) => if (p != null) a.setSlot(p, t)
  def check(modelview: ModelView) = modelview.within(p)
  def toXML =
    <setslot x={ p._1.toString } y={ p._2.toString }>
      <subind>{t.subindex}</subind>
      <terrcls>{t.getClass.getName}</terrcls>
    </setslot>
  def fromXML(node: xml.Node) = {
    val x = (node \ "@x")(0).text.toInt
    val y = (node \ "@y")(0).text.toInt
    val subind = (node \ "subind")(0).text.toInt
    val terr = Class.forName((node \ "terrcls")(0).text).newInstance.asInstanceOf[Terrain]
    new SetSlotCombinator((x, y), new TerrainSlot(subind, terr))
  }
  override def equals(other: Any) = other match {
    case that: SetSlotCombinator => that.p == p && that.t == t
    case _ => false
  }
  override def hashCode = p.hashCode << 16 + t.hashCode
}

/** Adds an instance to the model. */
class AddInstanceCombinator(val p: (Int, Int), val inst: Instance) extends ExecutionCombinator {
  def this() = this(null, null)
  
  def getExecution = (a: Model#Adapter) => if (p != null) a.addInstance(p, inst)
  def check(modelview: ModelView) = modelview.checkPlacement(p, inst)
  def toXML =
    <addinst x={ p._1.toString } y={ p._2.toString }>
      {inst.toXML}
    </addinst>
  def fromXML(node: xml.Node) = {
    val x = (node \ "@x")(0).text.toInt
    val y = (node \ "@y")(0).text.toInt
    val i = Instance.fromXML(node.child(1))
    new AddInstanceCombinator((x, y), i)
  }
  override def equals(other: Any) = other match {
    case that: AddInstanceCombinator => that.p == p && that.inst == inst
    case _ => false
  }
  override def hashCode = p.hashCode + inst.hashCode
}

/** Remove instance from the model. */
class RemoveInstanceCombinator(val index: Int) extends ExecutionCombinator {
  def this() = this(Int.MinValue)
  def this(i: Instance) = this(i.index)
  
  def getExecution = (a: Model#Adapter) => if (index != Int.MinValue) a.removeInstance(index)
  def check(modelview: ModelView) = modelview.containsInstance(index)
  def toXML = <removeinst index={ index.toString }></removeinst>
  def fromXML(node: xml.Node) = {
    val index = (node \ "@index")(0).text.toInt
    new RemoveInstanceCombinator(index)
  }
  override def equals(other: Any) = other match {
    case that: RemoveInstanceCombinator => that.index == index
    case _ => false
  }
  override def hashCode = index
}

/** Relocate instance in the model. */
class RelocateInstanceCombinator(val index: Int, val nloc: (Int, Int)) extends ExecutionCombinator {
  def this() = this(Int.MinValue, null)
  def this(inst: Instance, nl: (Int, Int)) = this(inst.index, nl)

  def getExecution = (a: Model#Adapter) => if (nloc != null) a.relocateInstance(index, nloc)
  def check(modelview: ModelView) = modelview.containsInstance(index) &&
    modelview.checkPlacement(nloc, modelview.getInstance(index))
  def toXML = {
    <relocate x={ nloc._1.toString } y={ nloc._2.toString } index={ index.toString }>
    </relocate>
  }
  def fromXML(node: xml.Node) = {
    val x = (node \ "@x")(0).text.toInt
    val y = (node \ "@y")(0).text.toInt
    val ind = (node \ "@index")(0).text.toInt
    new RelocateInstanceCombinator(ind, (x, y))
  }
  override def equals(other: Any) = other match {
    case that: RelocateInstanceCombinator => that.nloc == nloc && that.index == index
    case _ => false
  }
  override def hashCode = index << 16 + nloc.hashCode
}

/** Changes state of the game. */
class ChangeStateCombinator(val s: State) extends ExecutionCombinator {
  def this() = this(null)
  
  def getExecution = (a: Model#Adapter) => if (s != null) a.changeState(s)
  def check(modelview: ModelView) = true
  def toXML = {
    <changestate cls={ s.getClass.getName }>
      {s.toXML}
    </changestate>
  }
  def fromXML(node: xml.Node) = {
    val stsample = Class.forName((node \ "@cls")(0).text).newInstance.asInstanceOf[State]
    val state = stsample.fromXML(node.child(1))
    new ChangeStateCombinator(state)
  }
  override def equals(other: Any) = other match {
    case that: ChangeStateCombinator => that.s == s
    case _ => false
  }
  override def hashCode = s.hashCode
}

/** Changes atmosphere of the game. */
class ChangeAtmosphereCombinator(val at: Atmosphere) extends ExecutionCombinator {
  def this() = this(null)
  
  def getExecution = (a: Model#Adapter) => if (at != null) a.changeAtmosphere(at)
  def check(modelview: ModelView) = true
  def toXML = {
    <changeatm cls={ at.getClass.getName }>
      {at.toXML}
    </changeatm>
  }
  def fromXML(node: xml.Node) = {
    val atm = Atmosphere.fromXML(node.child(1))
    new ChangeAtmosphereCombinator(atm)
  }
  override def equals(other: Any) = other match {
    case that: ChangeAtmosphereCombinator => that.at == at
    case _ => false
  }
  override def hashCode = at.hashCode
}

/** Changes the quest of the game. */
class ChangeQuestCombinator(val q: Quest) extends ExecutionCombinator {
  def this() = this(null)
  
  def getExecution = (a: Model#Adapter) => if (q != null) a.changeQuest(q)
  def check(modelview: ModelView) = true
  def toXML = {
    <changequest cls={ q.getClass.getName }>
      {q.toXML}
    </changequest>
  }
  def fromXML(node: xml.Node) = {
    val qsample = Class.forName((node \ "@cls")(0).text).newInstance.asInstanceOf[Quest]
    val quest = qsample.fromXML(node.child(1))
    new ChangeQuestCombinator(quest)
  }
  override def equals(other: Any) = other match {
    case that: ChangeQuestCombinator => that.q == q
    case _ => false
  }
  override def hashCode = q.hashCode
}

/** Invokes an action of an instance. */
class InvokeInstanceActionCombinator(val index: Int, val actionName: String, val params: Array[Value])
extends ExecutionCombinator {
  def this() = this(Int.MinValue, null, null)
  
  def getExecution = (a: Model#Adapter) => if (params != null) a.invokeInstanceAction(index, actionName, params)
  def check(modelview: ModelView) = modelview.containsInstance(index) &&
    (modelview.getInstance(index).actions.get(actionName) match {
      case Some(action) => action.operation.check(modelview, params)
      case None => false
    })
  def toXML = {
    <invokeInstAction index={ index.toString } action={ actionName }>
      {
        for (v <- params) yield <p cls={ v.getClass.getName }>v.toXML</p>
      }
    </invokeInstAction>
  }
  def fromXML(node: xml.Node) = {
    val ind = (node \ "@index")(0).text.toInt
    val action = (node \ "@action")(0).text
    val paramnodes = node \ "p"
    val params = for (pn <- paramnodes.toArray[xml.Node]) yield {
      val vsample = Class.forName((pn \ "@cls")(0).text).newInstance.asInstanceOf[Value]
      vsample.fromXML(pn.child(0))
    }
    new InvokeInstanceActionCombinator(ind, action, params)
  }
  override def equals(other: Any) = other match {
    case that: InvokeInstanceActionCombinator => that.index == index && actionName == that.actionName &&
      params == that.params
    case _ => false
  }
  override def hashCode = index + actionName.hashCode << 16
}

/** Changes a property of an instance. */
class ChangeInstancePropertyCombinator(val index: Int, val propertyName: String, val nv: Value)
extends ExecutionCombinator {
  def this() = this(Int.MinValue, null, null)
  
  def getExecution = (a: Model#Adapter) => if (nv != null) a.changeInstanceProperty(index, propertyName, nv)
  def check(modelview: ModelView) = modelview.containsInstance(index) &&
    modelview.getInstance(index).properties.contains(propertyName)
  def toXML = {
    <changeInstProp index={ index.toString } pname={ propertyName } cls={ nv.getClass.getName }>
      {nv.toXML}
    </changeInstProp>
  }
  def fromXML(node: xml.Node) = {
    val ind = (node \ "@index")(0).text.toInt
    val propname = (node \ "@pname")(0).text
    val sampleval = Class.forName((node \ "cls")(0).text).newInstance.asInstanceOf[Value]
    val nval = sampleval.fromXML(node.child(1))
    new ChangeInstancePropertyCombinator(ind, propname, nval)
  }
  override def equals(other: Any) = other match {
    case that: ChangeInstancePropertyCombinator => that.index == index && propertyName == that.propertyName &&
      that.nv == nv
    case _ => false
  }
  override def hashCode = index << 24 + propertyName.hashCode << 16 + nv.hashCode
}

/** Ends a turn. */
class InteractObjectsCombinator(val ind1: Int, val ind2: Int) extends ExecutionCombinator {
  def this() = this(-1, -1)
  def getExecution = _.interactObjects(ind1, ind2)
  def check(modelview: ModelView) = modelview.containsInstance(ind1) && modelview.containsInstance(ind2)
  def toXML =
    <interact ind1={ ind1.toString } ind2={ ind2.toString }>
    </interact>
  def fromXML(node: xml.Node) = {
    val ind1 = (node \ "@ind1")(0).text.toInt
    val ind2 = (node \ "@ind2")(0).text.toInt
    new InteractObjectsCombinator(ind1, ind2)
  }
  override def equals(other: Any) = other match {
    case that: InteractObjectsCombinator => ind1 == that.ind1 && ind2 == that.ind2
    case _ => false
  }
  override def hashCode = ind1 << 16 + ind2 + 3215
}

/** Ends a turn. */
class EndTurnCombinator extends ExecutionCombinator {
  def getExecution = _.endTurn
  def check(modelview: ModelView) = true
  def toXML = <endturn/>
  def fromXML(node: xml.Node) = new EndTurnCombinator
  override def equals(other: Any) = other match {
    case that: EndTurnCombinator => true
    case _ => false
  }
  override def hashCode = 294159867
}

/** A combinator composed of many combinators. */
class CompositeCombinator(val current: ExecutionCombinator, val next: Option[ExecutionCombinator])
extends ExecutionCombinator {
  def this() = this(new NullCombinator, None)
  def this(currcomb: ExecutionCombinator) = this(currcomb, None)
  
  def getExecution = {
    val currex = current.getExecution
    next match {
      case Some(cc) => (a: Model#Adapter) => {
        currex(a)
        cc.getExecution(a)
      }
      case None => currex(_: Model#Adapter)
    }
  }
  def check(view: ModelView) = {
    next match {
      case Some(cc) => current.check(view) && cc.check(view)
      case None => current.check(view)
    }
  }
  def toXML = {
    <compcomb cls={ current.getClass.getName }>
      {current.toXML}
      {
        next match {
          case Some(cc) => <next cls= { cc.getClass.getName }>{cc.toXML}</next>
          case None => 
        }
      }
    </compcomb>
  }
  def fromXML(node: xml.Node) = {
    val ccsample = Class.forName((node \ "@cls")(0).text).newInstance.asInstanceOf[ExecutionCombinator]
    val curr = ccsample.fromXML(node.child(1))
    if (node.child.length < 4) {
      val nnode = node.child(3)
      val nextsample = Class.forName((nnode \ "@cls")(0).text).newInstance.asInstanceOf[ExecutionCombinator]
      val nx = nextsample.fromXML(nnode.child(0))
      new CompositeCombinator(curr, Some(nx))
    } else {
      new CompositeCombinator(curr, None)
    }
  }
  override def equals(other: Any) = other match {
    case that: CompositeCombinator => that.current == current && that.next == next
    case _ => false
  }
  override def hashCode = current.hashCode + 322012
}

/** Hack for ant :) */
final class BasicExecutions








