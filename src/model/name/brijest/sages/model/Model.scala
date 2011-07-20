package name.brijest.sages.model



/*********************************************************
 * 
 * This file describes the model.
 * 
 * @author Aleksandar Prokopec
 * 
 *********************************************************
 */



import scala.collection._

import name.brijest.sages.model.quantities._
import name.brijest.sages.model.states._


/**
 * Provides a view into the model.
 */
trait ModelView {
  def size: Int
  def terrainPalette: TerrainPalette
  def objectPalette: ObjectPalette
  def categories: CategoryFactory
  def version: String
  def playerlist: List[Player]
  def players: Map[Int, Player]
  def player(index: Int): Player
  def containsPlayer(p: Player) = players.contains(p.index) && players(p.index) == p
  def containsPlayerColour(c: Colour) = players.forall(_._2.colour != c)
  def state: State
  def slot(x: Int, y: Int): TerrainSlot = slot((x, y))
  def slot(p: (Int, Int)): TerrainSlot
  def digged(x: Int, y: Int): Boolean = digged((x, y))
  def digged(p: (Int, Int)): Boolean
  def instanceMap: InstanceMap
  def containsInstance(i: Instance): Boolean
  def containsInstance(index: Int): Boolean
  def containsInstanceAt(t: (Int, Int)): Boolean
  def walkable(x: Int, y: Int): Boolean = walkable((x, y))
  def walkable(t: (Int, Int)): Boolean
  /** Returns the instance at the specified slot, or null if there is none. */
  def getInstance(t: (Int, Int)): Instance
  def getInstance(index: Int): Instance
  def isSlotInteractive(t: (Int, Int)): Boolean
  def getInteractive(t: (Int, Int)): Instance
  def checkPlacement(t: (Int, Int), inst: Instance): Boolean
  def checkPlacement(x: Int, y: Int, inst: Instance): Boolean
  def quest: Quest
  def turn: Turn 
  def atmosphere: Atmosphere
  def within(p: (Int, Int)) = p._1 >= 0 && p._2 >= 0 && p._1 < size && p._2 < size
}

/**
 * An implementation of the immutable view of the model.
 */
class DefaultModelView protected (val size: Int,
                           val terrainPalette: TerrainPalette,
                           val objectPalette: ObjectPalette,
                           val categories: CategoryFactory,
                           val version: String,
                           playerList: Seq[Player],
                           startState: State,
                           terrainMap: TerrainMap,
                           bookOfKnowledgeLocation: (Int, Int),
                           theInstanceMap: InstanceMap,
                           typeOfQuest: Quest,
                           elapsedTurns: Turn,
                           atm: Atmosphere) extends ModelView {
  protected val playermap = mutable.Map() ++ (for (p <- playerList) yield (p.index, p))
  protected var currstate = startState
  protected val terrmap = terrainMap
  protected val boklocation = bookOfKnowledgeLocation
  protected val instmap = theInstanceMap
  protected var questtype = typeOfQuest
  protected var turnselapsed = elapsedTurns
  protected var atmos = atm
  
  def this(s: Int, tp: TerrainPalette, op: ObjectPalette, c: CategoryFactory, v: String) = 
    this(s, tp, op, c, v,
         List(),
         new StartState,
         new TerrainMap(s, tp),
         (0, 0),
         new InstanceMap(s),
         new AimlessQuest,
         new Turn,
         new Calm)
  
  def playerlist: List[Player] = playermap.values.toList
  def players: Map[Int, Player] = playermap
  def player(index: Int) = playermap(index)
  def state = currstate
  override def slot(x: Int, y: Int) = terrmap.slot(x, y)
  def slot(p: (Int, Int)) = terrmap.slot(p)
  override def digged(x: Int, y: Int) = terrmap.digged(x, y)
  def digged(p: (Int, Int)) = terrmap.digged(p)
  def instanceMap = instmap
  def containsInstance(inst: Instance) = instmap.contains(inst)
  def containsInstance(i: Int) = instmap.hasIndex(i)
  def containsInstanceAt(t: (Int, Int)) = instmap.occupied(t)
  def walkable(t: (Int, Int)) = !containsInstanceAt(t) && terrmap.walkable(t)
  override def walkable(x: Int, y: Int) = !instmap.occupied(x, y)
  def getInstance(t: (Int, Int)) = instmap(t)
  def getInstance(index: Int) = instmap.getByIndex(index)
  def isSlotInteractive(t: (Int, Int)) = instmap.interactive(t)
  def getInteractive(t: (Int, Int)) = instmap.getInteractive(t)
  def checkPlacement(t: (Int, Int), inst: Instance) = instmap.checkFree(t, inst)
  def checkPlacement(x: Int, y: Int, inst: Instance) = instmap.checkFree(x, y, inst)
  def quest = questtype
  def turn = turnselapsed
  def atmosphere = atmos
  override def equals(other: Any) = other match {
    case that: DefaultModelView => that.size == size && that.boklocation == boklocation &&
      that.currstate == currstate && that.playermap == playermap && that.version == version &&
      that.terrmap == terrmap && that.instmap == instmap && that.questtype == questtype &&
      that.turnselapsed == turnselapsed
    case _ => false
  }
  override def hashCode = size << 25 + boklocation.hashCode << 21 + currstate.hashCode << 17 +
    version.hashCode << 13 + questtype.hashCode << 9 + turnselapsed.hashCode << 5 +
    terrmap.hashCode
}

trait Beholdable {
  private var beholder: Beholder = null
  def informBeholder(e: Event) = if (beholder != null) beholder.receiveEvent(e)
  def setBeholder(b: Beholder) = beholder = b
}

/**
 * A trait that delays raised events and releases them later.
 */
trait AccumulativeObservable extends Observable with Beholdable {
  private val delayed = new mutable.Queue[Event]()
  
  override def raise(e: Event) {
    delayed.enqueue(e)
  }
  def release {
    for (e <- delayed) super.raise(e)
    for (e <- delayed) informBeholder(e)
    delayed.clear
  }
}

/**
 * A companion object for the Model.
 */
object Model {
  def fromXML(node: scala.xml.Node, terrpal: TerrainPalette, objpal: ObjectPalette, catfac: CategoryFactory) = {
    val version = (node \ "@version")(0).text
    val size = (node \ "size")(0).text.toInt
    val players = for (n <- (node \ "players" \ "p")) yield Player fromXML n
    val nst = Class.forName((node \ "state" \ "@cls")(0).text).newInstance.asInstanceOf[State]
    val state = nst.fromXML((node \ "state")(0).child(1))
    val terrainmap = TerrainMap.fromXML(node.child(7), terrpal)
    val bokloc = ((node \ "bokloc" \ "@x")(0).text.toInt, (node \ "bokloc" \ "@y")(0).text.toInt)
    val instmap = InstanceMap.fromXML(node.child(11))
    val nq = Class.forName((node \ "quest" \ "@cls")(0).text).newInstance.asInstanceOf[Quest]
    val quest = nq.fromXML((node \ "quest")(0).child(1))
    val turns = (new Turn) fromXML ((node \ "elapsed")(0).child(0))
    val atm = Atmosphere.fromXML((node \ "atmosphere")(0).child(0))
    
    new Model(size, terrpal, objpal, catfac, version, players, state, terrainmap, bokloc,
              instmap, quest, turns, atm)
  }
}

/**
 * A mutable view of the model. Defines basic operations which raise events, allows registration of observers
 * and can receive Execution objects. It can also persist itself to xml.
 */
class Model(s: Int,
            tpal: TerrainPalette,
            opal: ObjectPalette,
            cf: CategoryFactory,
            vers: String,
            playerList: Seq[Player],
            startState: State,
            tmap: TerrainMap,
            bl: (Int, Int),
            imap: InstanceMap,
            qt: Quest,
            et: Turn,
            atm: Atmosphere)
extends DefaultModelView(s, tpal, opal, cf, vers, playerList, startState, tmap, bl, imap, qt, et, atm) 
with AccumulativeObservable {
  // register quest and object instances as observers
  addObserver(questtype)
  for ((loc, inst) <- instmap) addObserver(inst.prototype)

  private def requireWithin(p: (Int, Int)) = require(within(p), "Location " + p + 
                                                       " not within the map (size = " + size + ").")
  
  def this(s: Int, tpal: TerrainPalette, opal: ObjectPalette, cf: CategoryFactory, vers: String) =
    this(s, tpal, opal, cf, vers,
         List(),
         new StartState,
         new TerrainMap(s, tpal),
         (0, 0),
         new InstanceMap(s),
         new AimlessQuest,
         new Turn,
         new Calm)
  
  class Adapter {
      def modelview: ModelView = Model.this
	  /**
	   * Adds a player on the player list.<br/>
	   * Raises the <code>CreatePlayerEvent</code>.<br/>
	   * If a player with the specified index or colour already exists,
	   * an exception is thrown.
	   * 
	   * @throws IllegalArgumentException
	   * If a player with the specified index or colour already exists.
	   */
	  def addPlayer(p: Player) {
	    require(!playermap.contains(p.index), "Index " + p.index + " exists!")
	    require(containsPlayerColour(p.colour), "Player with colour " + p.colour + " exists!")
	    
	    playermap.put(p.index, p)
	    
	    raise(CreatePlayerEvent(p))
	  }
	  /**
	   * Removes the player from the player list.<br/>
	   * Raises the <code>DeletePlayerEvent</code>.<br/>
	   * If such a player does not exist, an exception is thrown.
	   * 
	   * @throws IllegalArgumentException
	   * If a player with the specified index does not exist.
	   */
	  def removePlayer(p: Player): Unit = removePlayer(p.index)
	  /**
	   * Removes the player with the index from the player list.<br/>
	   * Raises the <code>DeletePlayerEvent</code>.<br/>
	   * If such an index does not exist, an exception is thrown.
	   * 
	   * @throws IllegalArgumentException
	   * If a player with the specified index does not exist.
	   */
	  def removePlayer(index: Int) {
	    require(playermap.contains(index), "Index " + index + " does not exist!")
	    val p = playermap(index)
	    playermap.removeKey(index)
	    raise(DeletePlayerEvent(p))
	  }
	  /**
	   * Performs digging at the specified location, and raises the <code>DigEvent</code>. If the location 
	   * has already been digged at, nothing will change and no event will be raised.
	   * 
	   * @throws IllegalArgumentException
	   * If the specified location is not on the map.
	   */
	  def digAt(p: (Int, Int)) {
	    requireWithin(p)
	    terrmap.setDigged(p)
	    raise(DigEvent(true, p))
	  }
	  /**
	   * Sets a slot of the terrain and raises a <code>ChangeTerrainEvent</code>.
	   * 
	   * @throws IllegalArgumentException
	   * If the specified location is not on the map.
	   */
	  def setSlot(p: (Int, Int), slot: TerrainSlot) {
	    requireWithin(p)
	    require(slot != null, "Slot cannot be null!")
	    terrmap.setSlot(p, slot)
	    raise(ChangeTerrainEvent(p, slot))
	  }
	  /**
	   * Adds an object instance on the map and raises the <code>AddInstanceEvent</code>.<br/>
	   * An exception is thrown if the instance cannot be placed on the given location.
	   * 
	   * @thrown IllegalArgumentException
	   * If the object cannot be placed here.
	   */
	  def addInstance(p: (Int, Int), inst: Instance) {
	    instmap.put(p, inst)
	    addObserver(inst.prototype)
	    raise(AddInstanceEvent(p, inst))
	  }
	  /**
	   * Removes the object instance from the map.<br/>
	   * Raises the <code>RemoveInstanceEvent</code>.<br/>
	   * An exception is thrown if the instance does not exist.
	   * 
	   * @throws IllegalArgumentException
	   * If the instance does not exist.
	   */
	  def removeInstance(inst: Instance) {
	    val loc = instmap.getLocationFor(inst)
	    instmap remove inst
	    removeObserver(inst.prototype)
	    raise(RemoveInstanceEvent(loc, inst))
	  }
	  /**
	   * Removes the instance with the specified index.<br/>
	   * Raises the <code>RemoveInstanceEvent</code>.
	   * 
	   * @throws IllegalArgumentException
	   * If the instance with the specified index does not exist.
	   */
	  def removeInstance(index: Int) {
	    val inst = instmap getByIndex index
	    if (inst == null) throw new IllegalArgumentException("No instance with index " + index + ".")
	    removeInstance(inst)
	  }
	  /**
	   * Removes an existing object instance and places it on a new location.<br/>
	   * Raises the <code>RelocateInstanceEvent</code>.
	   * 
	   * @throws IllegalArgumentException
	   * If the instance with the specified object index does not exist, or the specified
	   * location is not free.
	   */
	  def relocateInstance(inst: Instance, nloc: (Int, Int)): Unit = relocateInstance(inst.index, nloc)
	  /**
	   * Removes an existing object instance and places it on a new location.<br/>
	   * Raises the <code>RelocateInstanceEvent</code>.
	   * 
	   * @throws IllegalArgumentException
	   * If the instance with the specified index does not exist, or the specified
	   * location is not free.
	   */
	  def relocateInstance(index: Int, nloc: (Int, Int)) {
	    val inst = instmap getByIndex index
	    if (inst == null) throw new IllegalArgumentException("No instance with index " + index + ".")
	    if (!instmap.checkFree(nloc, inst))
          throw new IllegalArgumentException("Cannot place instance at " + nloc + ".")
	    val oldloc = instmap getLocationFor inst
	    instmap remove inst
	    instmap.put(nloc, inst)
	    raise(RelocateInstanceEvent(oldloc, nloc, inst))
	  }
	  /**
	   * Changes the current state of the model.<br/>
	   * It raises the <code>StateChangedEvent</code>.
	   */
	  def changeState(s: State) {
	    val oldstate = currstate
	    currstate = s
	    raise(StateChangedEvent(oldstate, currstate))
	  }
	  /**
	   * Changes quest and raises the <code>QuestChangedEvent</code>.
	   */
	  def changeQuest(q: Quest) {
	    val oldquest = questtype
	    questtype = q
	    removeObserver(oldquest)
	    addObserver(q)
	    raise(QuestChangedEvent(oldquest, questtype))
	  }
	  def changeAtmosphere(a: Atmosphere) {
	    val oldatm = atmosphere
	    atmos = a
	    raise(AtmosphereChangedEvent(oldatm, a))
	  }
	  /**
	   * Invokes an action on the instance with the given index.
	   * Since action itself may perform further operations, this
	   * method does not raise any events.
	   * 
	   * @throws IllegalArgumentException
	   * If the object with the specified index does not exist, the action does not exist or the parameters
	   * for the action are wrong.
	   */
	  def invokeInstanceAction(index: Int, actionName: String, params: Array[Value]): Unit = 
	    invokeInstanceAction(instmap.getByIndex(index), actionName, params)
	  /**
	   * Invokes an action on the instance.
	   * Since action itself may perform further operations, this
	   * method does not raise any events.
	   * 
	   * @throws IllegalArgumentException
	   * If the object instance does not exist, the action does not exist or the parameters
	   * for the action are wrong.
	   */
	  def invokeInstanceAction(inst: Instance, actionName: String, params: Array[Value]) {
	    require(instmap.contains(inst), "Instance " + inst + " does not exist.")
	    require(inst.actions.contains(actionName), "Instance does not have action: " + actionName + ".")
	    require(inst.actions(actionName).operation.check(Model.this, params),
             "Bad parameters for action " + actionName + ".")
	    val res = inst.actions(actionName)(Model.this, this, inst.prototype, Some(inst.index), params)
	    raise(InstanceActionInvokedEvent(inst, actionName, params, res, instmap.getLocationFor(inst)))
	  }
	  /**
	   * Changes the property value of an object instance and raises the
	   * <code>PropertyChangeEvent</code>.<br/>
	   * 
	   * @throws IllegalArgumentException
	   * If the object with the specified index does not exist, the property does
	   * not exist or the value has a wrong type.
	   */
	  def changeInstanceProperty(index: Int, propertyName: String, nvalue: Value): Unit =
	    changeInstanceProperty(instmap getByIndex index, propertyName, nvalue)
	  /**
	   * Changes the property value of an object instance and raises the
	   * <code>InstancePropertyChangedEvent</code>.<br/>
	   * 
	   * @throws IllegalArgumentException
	   * If the object instance does not exist, the property does
	   * not exist or the value has a wrong type.
	   */
	  def changeInstanceProperty(inst: Instance, propertyName: String, nvalue: Value) {
	    require(instmap.contains(inst), "Instance " + inst + " does not exist.")
	    require(inst.properties.contains(propertyName), "Instance hass no property: " + propertyName + ".")
	    val prop = inst.properties(propertyName)
	    require(nvalue.getClass == prop.valueClass, "Value " + nvalue + " doesn't have type: " +
               prop.valueClass + ".")
	    inst.properties(propertyName) match {
	      case mq: MutableQuantity =>
	        val oldval = mq.value
	        mq.value = nvalue
	        raise(InstancePropertyChangedEvent(inst, oldval, nvalue, propertyName,
                                            instmap.getLocationFor(inst)))
	      case _ => throw new IllegalStateException("Every quantity should be mutable, " + propertyName +
	                                                  " in " + inst + " is not.")
	    }
	  }
	  /**
	   * Ends the turn, increases the turn counter and raises the <code>EndTurnEvent</code>.
	   */
	  def endTurn {
	    turnselapsed = turnselapsed.increase
	    raise(EndTurnEvent(turnselapsed))
	  }
	  def interactObjects(index1: Int, index2: Int) {
	    require(instmap.hasIndex(index1), "There is not object instance with index " + index1)
	    require(instmap.hasIndex(index2), "There is not object instance with index " + index2)
	    val inst = instmap.getByIndex(index1)
	    val withInst = instmap.getByIndex(index2)
	    val instAt = instmap.getLocationFor(inst)
	    val withAt = instmap.getLocationFor(withInst)
	    inst.prototype.interact(instAt, withInst.prototype, withAt)
	    raise(ObjectsInteractedEvent(inst, withInst, instAt, withAt))
	  }
  }
  
  def execute(e: Execution, fromPlayer: Int): OpResult = {
    // validate execution
    if (!currstate.validateExecution(e)) 
      return (false, currstate + " does not allow requested execution.")
    // check if player is active
    if (fromPlayer >= 0) if (currstate.activePlayers.forall(_.index != fromPlayer))
        return (false, "Player not active in " + currstate + ".")
    val adapter = new this.Adapter
    val opresult = e(adapter)
    release
    opresult
  }
  
  def toXML = {
    <model version={ version }>
      <size>{size}</size>
      <players>
        {
          for ((i, p) <- playermap) yield
            <p>
              {p.toXML}
            </p>
        }
      </players>
      <state cls={ currstate.getClass.getName }>
        {currstate.toXML}
      </state>
      {terrmap.toXML}
      <bokloc x={ boklocation._1.toString } y={ boklocation._2.toString }></bokloc>
      {instmap.toXML}
      <quest cls={ questtype.getClass.getName }>
        {questtype.toXML}
      </quest>
      <elapsed>{turnselapsed.toXML}</elapsed>
      <atmosphere>{atmosphere.toXML}</atmosphere>
    </model>
  }
  override def equals(other: Any) = other match {
    case that: Model => super.equals(that)
    case _ => false
  }
}

/**
 * An event raised when a player is added to the model.
 */
case class CreatePlayerEvent(val p: Player) extends Event

/**
 * Raised when a player is removed from the model.
 */
case class DeletePlayerEvent(val p: Player) extends Event

/**
 * Raised when dig status changes on a given location of the map.
 */
case class DigEvent(val digged: Boolean, override val location: (Int, Int)) extends Event

/**
 * Raised when terrain changes.
 */
case class ChangeTerrainEvent(override val location: (Int, Int), slot: TerrainSlot) extends Event

/**
 * Raised when an object instance is added to the map.
 */
case class AddInstanceEvent(override val location: (Int, Int), override val instance: Instance)
extends Event

/** Raised when an object instance is removed from the map. */
case class RemoveInstanceEvent(override val location: (Int, Int), override val instance: Instance)
extends Event

/** Raised when an object instance is relocated on the map. */
case class RelocateInstanceEvent(oldloc: (Int, Int), override val location: (Int, Int),
                                 override val instance: Instance) extends Event

/** Raised when model state changes. */
case class StateChangedEvent(oldstate: State, newstate: State) extends Event

/** Raised when model state changes. */
case class AtmosphereChangedEvent(oldatm: Atmosphere, newatm: Atmosphere) extends Event

/** Raised when quest changes. */
case class QuestChangedEvent(oldquest: Quest, newquest: Quest) extends Event

/** Raised when an object instance property changes. */
case class InstancePropertyChangedEvent(override val instance: Instance, oldval: Value, newval: Value,
                                        name: String, override val location: (Int, Int))
extends Event

/** Raised when two objects interact. */
case class ObjectsInteractedEvent(override val instance: Instance, val withInstance: Instance,
                                  override val location: (Int, Int), val withAtLocation: (Int, Int))
extends Event

/** Raised when an object action gets invoked. */
case class InstanceActionInvokedEvent(override val instance: Instance, name: String, params: Array[Value],
                                      result: OpResult, override val location: (Int, Int)) extends Event

/** Raised when the turn ends, carries information about the new turn number. */
case class EndTurnEvent(turn: Turn) extends Event








