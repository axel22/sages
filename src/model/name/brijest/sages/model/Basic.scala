package name.brijest.sages.model


/************************************************************************
 * 
 * This file defines some basic classes and traits for the Sages project.
 * 
 * @author Aleksandar Prokopec
 * 
 ************************************************************************/



import scala.collection._

import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model.quantities.DecNum
import name.brijest.sages.model.quantities.Text


/**
 * Any serializable value. Each Value must also have an empty ctor.<br/>
 * Each implementation of this trait must be immutable.<br/>
 * Each invocation of toXML method must return a single top xml node - for
 * instance:<br/>
 * <code>
 * somevalue
 *   childnode1 ... /childnode1
 *   ...
 *   childnodeN ... /childnodeN
 * /somevalue
 * </code><br/>
 * Care must be taken so that this xml does not start with an empty text node!
 */
trait Value {
  def toXML: scala.xml.Node
  def fromXML(node: scala.xml.Node): Value
  def equals(other: Any): Boolean
  def hashCode: Int
}

/**
 * Companion for instantiation of Quantity objects.
 */
object Quantity {
  def apply(n: String, d: String, v: Value): Quantity = {
    new Quantity(n, d, v)
  }
}

/**
 * Serves as a wrapper of values and their respective kinds.
 */
class Quantity protected (val name: String, val description: String, vp: Value) {
  protected var v = vp
  
  def valueClass = v.getClass
  def value = v
  def asInteger = v.asInstanceOf[IntNum].num
  def asString = v.asInstanceOf[Text].txt
  def asDecimal = v.asInstanceOf[DecNum].num
  
  def toXML = {
    <quantity>
      <name>{name}</name>
      <desc>{description}</desc>
      <value>{value.toXML}</value>
      <kind>{valueClass}</kind>
    </quantity>
  }
  override def equals(other: Any) = other match {
    case that: Quantity => name == that.name && description == that.description &&
      value == that.value
    case _ => false
  }
  override def hashCode = name.hashCode << 16 + value.hashCode + 7
}

object MutableQuantity {
  def fromXML(node: scala.xml.Node): Quantity = {
    val nm = (node \ "name").text
    val desc = (node \ "desc").text
    val kind = (node \ "kind")(0).text
    Class.forName(kind).newInstance match {
      case v: Value =>
        val vdes = v fromXML (node \ "value")(0)
        new MutableQuantity(nm, desc, vdes)
      case _ => throw new IllegalArgumentException("Unknown value class: " + kind)
    }
  }
  def apply(n: String, d: String, v: Value) = new MutableQuantity(n, d, v)
  def apply(q: Quantity): MutableQuantity = apply(q.name, q.description, q.value)
  implicit def tuple2Quantity(tup: (String, String, Value)) = new MutableQuantity(tup._1, tup._2, tup._3)
}

/**
 * A mutable quantity.
 */
final class MutableQuantity(n: String, d: String, vl: Value) extends Quantity(n, d, vl) {
  def value_=(t: Value) {
//    println(t.asInstanceOf[IntNum].num)
    if (t.getClass != valueClass)
      throw new IllegalArgumentException("Value must be of type " + valueClass)
    else v = t
//    println("-> " + v.asInstanceOf[IntNum].num)
  }
}

object OpResult {
  implicit def wrap(t: (Boolean, String)) = new OpResult(t._1, t._2)
  def successful = new OpResult(true, "")
  def successful(key: String, v: String) = {
    val opres = new OpResult(true, "")
    opres.infomap.put(key, v)
    opres
  }
  def fromXML(node: xml.Node) = {
    val success = (node \ "@s").text.toBoolean
    val desc = (node \ "@d").text
    val opres = new OpResult(success, desc)
    val infomap = for (pn <- node.child if !pn.child.isEmpty)
      yield ((node \ "@k").text, (node \ "@v").text)
    for ((k, v) <- infomap) opres.infomap.put(k, v)
    opres
  }
}

/**
 * Describes the result of some operation.
 */
final class OpResult(val successful: Boolean, val description: String) {
  val infomap = mutable.Map[String, String]()
  override def equals(other: Any) = other match {
    case that: OpResult => successful == that.successful && description == that.description
    case _ => false
  }
  override def hashCode = description.hashCode << 16 + successful.hashCode
  override def toString = "OpResult(" + successful + ", " + description + ")"
  def toXML = <opres s={ successful.toString } d={ description }>{
    for ((k, v) <- infomap) yield <nfo k={ k } v={ v }/>
  }</opres>
}

/**
 * Describes a constraint for the parameter of a function.<br/>
 * A parameter value may be unbound, it may be within a certain range if it's numeric,
 * or it may be enumerated (an element of some finite set).<br/>
 * The behaviour of a constraint may depend on the state of the model, thus - a constraint is
 * not immutable. However, a constraint must not change the state of the model.
 */
abstract class Constraint {
  abstract class Type
  case class Unbound() extends Type
  case class Range(lower: Value, higher: Value) extends Type
  case class Enumeration(allowed: Array[Value]) extends Type
  case class Criterium() extends Type
  
  /**
   * Returns the appropriate constraint type. Note that the constraint may change as the
   * model state changes. However, invoking this method must not change the model in
   * any way.
   * 
   * @param model
   * The entire model. The constraint may use model information to decide on the constraint
   * type and relevant information. It may be pattern matched to a model view or the actual
   * model in implementations.
   * 
   * @param owner
   * An appropriate owner of the action must be specified for the constraint type to be computed.
   * 
   * @return
   * Returns an appropriate constraint type with all the relevant information.
   */
  def constraintType(model: ModelView, owner: Depot): Type
  /**
   * Inspects the value and returns whether or not the value is allowed by the constraint.
   * It checks it for the appropriate type, as well as for the appropriate value.
   */
  def inspect(model: ModelView, v: Value): Boolean
}

/**
 * Operation envelopes a function that does something with the model. The operation
 * receives a model, the model adapter, operation owner, it's index if applicable and a field of values. It
 * returns an operation result.<br/>
 * The operation object also specifies the types and constraints for each object in the field
 * of values array.<br/>
 * The Operation subclass objects are immutable if and only if the function they envelope has no
 * side effects and their parameter constraints do not depend on the state of the model.
 * Each Operation subclass must have an empty ctor.<br/>
 * Operations should not have private data members, hence - they should have no state. However,
 * they may cause changes in mutable objects they are passed - the model, for instance.<br/>
 * If an operation returns an unsuccessful operation result, it must leave <b>no changes</b> on the model
 * or the operation owner.
 */
abstract class Operation {
  def check(m: ModelView, params: Array[Value]): Boolean = {
    for ((param, constraint) <- params.zip(constraints)) if (!constraint.inspect(m, param)) return false
    true
  }
  
  /**
   * Performs the operation.
   * 
   * @throws IllegalArgumentException
   * If the parameters for the operation are wrong.
   */
  def apply(model: ModelView, adapter: Model#Adapter, owner: Depot, index: Option[Int], params: Array[Value]):
    OpResult
  def parameters: Seq[Class[_]]
  def constraints: Seq[Constraint]
  def equals(other: Any): Boolean
  def hashCode: Int
}

abstract class AnonymousOperation extends Operation {
  override def equals(other: Any) = other match {
    case that: AnonymousOperation => true
    case _ => false
  }
  override def hashCode = 2143512
}

/**
 * Companion object of the operation class.
 */
object Operation {
  def apply(params: Seq[Class[_]], constrs: Seq[Constraint], 
           fun: (ModelView, Model#Adapter, Depot, Option[Int], Array[Value]) => OpResult): Operation =
    new AnonymousOperation {
      def apply(m: ModelView, a: Model#Adapter, own: Depot, ind: Option[Int], params: Array[Value]) =
        fun(m, a, own, ind, params)
      def parameters = params
      def constraints = constrs
    }
}

/**
 * Companion for Action objects that allows creation of an Action
 * object from XML.
 */
object Action {
  def fromXML(node: scala.xml.Node) = {
    val name = (node \ "name").text
    val desc = (node \ "desc").text
    val opclsname = (node \ "opclass").text
    Class.forName(opclsname, true, DynamicLoader).newInstance match {
      case operation: Operation => new Action(name, desc, operation)
      case _ => throw new IllegalArgumentException("Unknown operation class: " + opclsname)
    }
  }
  def apply(n: String, d: String, op: Operation) = new Action(n, d, op)
}

/**
 * Action objects serve as wrappers for operations.
 */
final class Action(val name: String, val description: String, val operation: Operation) extends Value {
  def apply(model: ModelView, ad: Model#Adapter, owner: Depot, index: Option[Int], params: Array[Value]) = 
    operation.apply(model, ad, owner, index, params)
  def parameters = operation.parameters
  def constraints = operation.constraints
  def toXML = {
    <action>
      <name>{name}</name>
      <desc>{description}</desc>
      <opclass>{operation.getClass.getName}</opclass>
    </action>
  }
  def fromXML(node: scala.xml.Node) = Action.fromXML(node)
  override def equals(other: Any) = other match {
    case that: Action => name == that.name && description == that.description && operation == that.operation
    case _ => false
  }
  override def hashCode = name.hashCode << 22 + description.hashCode << 11 + operation.hashCode
}

/**
 * Companion object for the properties trait.
 */
object Properties {
  def fromXML(node: scala.xml.Node): Properties = {
    val props = for (pn <- node.child if !pn.child.isEmpty)
      yield MutableQuantity.fromXML(pn)
    val propmap = mutable.LinkedHashMap() ++ props.map(x => (x.name, x))
    
    new Properties {
      def properties = propmap
    }
  }
  def empty: Properties = {
    new Properties {
      def properties = mutable.LinkedHashMap()
    }
  }
}

/**
 * This trait describes any object that owns or may own quantities.
 */
trait Properties extends Value {
  def properties: scala.collection.mutable.LinkedHashMap[String, Quantity]
  def hasProp(name: String) = properties.contains(name)
  def prop(name: String) = if (properties.contains(name)) properties(name) else null
  protected def setProp(name: String, v: Value) = prop(name).asInstanceOf[MutableQuantity].value = v
  def toXML = {
    <properties>
      { for ((k, v) <- properties) yield v.toXML }
    </properties>
  }
  def fromXML(node: scala.xml.Node): Value = Properties.fromXML(node)
  protected def equalProps(that: Properties) = this.properties == that.properties
  override def hashCode = properties.hashCode
}

object Depot {
  def fromXML(node: scala.xml.Node): Depot = {
    val props = Properties.fromXML((node \ "properties")(0))
    val acts = for (an <- (node \ "actions")(0).child if !an.child.isEmpty)
      yield Action.fromXML(an)
    val actmap = mutable.LinkedHashMap() ++ acts.map(x => (x.name, x))
    
    new Depot {
      def properties = props.properties
      def actions = actmap
    }
  }
  def empty: Depot = new Depot {
    def actions = mutable.LinkedHashMap()
    def properties = mutable.LinkedHashMap()
  }
}

/**
 * A Depot contains actions and quantities.
 */
trait Depot extends Properties {
  def actions: scala.collection.mutable.LinkedHashMap[String, Action]
  def act(name: String) = if (actions.contains(name)) actions(name) else null
  def hasAction(name: String) = actions.contains(name)
  override def toXML = {
    <depot>
      {super.toXML}
      <actions>
        { for ((k, v) <- actions) yield v.toXML }
      </actions>
    </depot>
  }
  override def fromXML(node: scala.xml.Node): Value = Depot.fromXML(node)
  protected def equalDepot(that: Depot) = this.actions == that.actions && super.equalProps(that)
  override def hashCode = actions.hashCode << 16 + super.hashCode
}

/**
 * Represents any kind of an event. Events happen once the model state changes and
 * hold information regarding the change.<br/>
 * Each Event implementation must be a case class.
 */
abstract class Event {
  /** Instance involved in this event. */
  def instance: Instance = null
  def instanceIndex = if (instance == null) -1 else instance.index
  /** Location of this event. Null if there is no particular location. */
  def location: (Int, Int) = null
}

/**
 * Represents the listener of events.
 */
trait Observer {
  /**
   * Handles the event.
   */
  def onEvent(e: Event): Unit
  /**
   * Returns the list of event classes this observer handles. Must return at least one event class.<br/>
   * If a value <code>classOf[Event]</code> is included in this
   * list, it means the observer listens to all possible events.
   */
  def events: List[Class[_]]
}


/**
 * Allows registration of observers and provides methods for informing them of events.
 */
trait Observable {
  private val observers = mutable.Map[Class[_], mutable.Set[Observer]]()
  observers.put(classOf[Event], mutable.Set[Observer]())
  
  private def add(cls: Class[_], o: Observer) {
    if (!observers.contains(cls)) observers.put(cls, mutable.Set[Observer]())
    observers(cls) + o
  }
  def addObserver(o: Observer) { for (evcls <- o.events) add(evcls, o) }
  def removeObserver(o: Observer) { for (ec <- o.events if (observers.contains(ec))) observers(ec) - o }
  def containsObserver(o: Observer) = observers.get(o.events.elements.next) match {
    case Some(s) => s.contains(o)
    case None => false
  }
  protected def raise(e: Event) {
    // inform listeners to everything
    for (o <- observers(classOf[Event])) o.onEvent(e)
    
    // inform specific listeners
    observers.get(e.getClass) match {
      case Some(s) => for (o <- s) o.onEvent(e)
      case _ =>
    }
  }
}

/** Hack for ant :) */
final class Basic






