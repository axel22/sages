package name.brijest.sages.model.states


import scala.collection._

import name.brijest.sages.model._



/*******************************************************
 * 
 * This file describes the states of the game.
 * 
 * @author Aleksandar Prokopec
 * 
 *******************************************************/



/**
 * The state the game is in once it begins.
 */
final class StartState(d: Depot) extends State(d) {
  def this() = this(Depot.empty)
  
  def name = "Start"
  def description = "The beginning of the game."
  def activePlayers = Set()
  def saveable = false
  def validateExecution(e: Execution) = e match {
    case _ => true
  }
  def fromXML(node: xml.Node) = {
    val props = Depot.fromXML(node.child(1))
    new StartState(props)
  }
  override def equals(other: Any) = other match {
    case that: StartState => true;
    case _ => false
  }
  override def hashCode = 43724
}

























