package name.brijest.sages.gui

import name.brijest.sages.controller._

import name.brijest.sages.model.Instance
import name.brijest.sages.model.Value
import name.brijest.sages.model.Event
import name.brijest.sages.model.RemoveInstanceEvent


/**
 * Concrete widgets must implement this trait's <code>onSelect</code> method,
 * which displays the selected object.
 * They may use this object's <code>invokeAction</code> method to invoke
 * actions on the object.
 */
trait ObjectDisplay extends Widget with EventListener {
  private var sel: Option[Instance] = None
  
  def onSelect(inst: Option[Instance]): Unit
  def selected = sel
  def selected_=(inst: Option[Instance]) {
    sel = inst
    onSelect(inst)
  }
  def invokeAction(actionName: String, params: Seq[Value]) {
    // TODO
  }
  def listens = Nil
  def onEvent(e: Event) = null
}






