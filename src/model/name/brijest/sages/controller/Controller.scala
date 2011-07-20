package name.brijest.sages.controller


import scala.collection._

import name.brijest.sages.model.OpResult
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Observable
import name.brijest.sages.model.Beholder
import name.brijest.sages.model.Event


/**
 * Interface for sending commands and interacting with the model. Classes implementing this
 * interface must inform listeners of events within the model.
 */
trait Controller extends Beholder {
  private val listeners = mutable.Map[Class[_], mutable.Set[EventListener]]()
  listeners.put(classOf[Event], mutable.Set[EventListener]())
  
  private def add(cls: Class[_], l: EventListener) {
    if (!listeners.contains(cls)) listeners.put(cls, mutable.Set[EventListener]())
    listeners(cls) + l
  }
  def addListener(l: EventListener) { for (evcls <- l.listens) add(evcls, l) }
  def removeListener(l: EventListener) { for (ec <- l.listens if (listeners.contains(ec))) listeners(ec) - l }
  def containsListener(l: EventListener) = listeners.get(l.listens.elements.next) match {
    case Some(s) => s.contains(l)
    case None => false
  }
  protected def raise(e: Event) {
    // inform listeners to everything
    for (l <- listeners(classOf[Event])) l.onEvent(e)
    
    // inform specific listeners
    listeners.get(e.getClass) match {
      case Some(s) => for (l <- s) l.onEvent(e)
      case None =>
    }
  }
  /**
   * Sends a command to the model. It delivers the result of
   * performing command's execution on the model with a callback method.
   * This method <b>may</b> thus be a non-blocking operation.
   */
  def send(command: Command)(inform: OpResult => Unit): Unit
  /**
   * Performs some sort of rendering operation with the model. It
   * does not change the model in any way, but uses it's information.
   */
  def view(op: ModelView => Unit): Unit
}












