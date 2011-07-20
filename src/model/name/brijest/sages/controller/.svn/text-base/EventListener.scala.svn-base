package name.brijest.sages.controller


import name.brijest.sages.model.Event


/** Listeners. Those that listen to Class[Event] will be notified of every event. */
trait EventListener {
  def onEvent(e: Event): Unit
  def listens: List[Class[_]]
}


