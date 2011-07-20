package name.brijest.sages.controller.impl.controllers



import name.brijest.sages.model.OpResult
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Event
import name.brijest.sages.controller._


class LocalController extends Controller {
  
  def send(c: Command)(onres: OpResult=>Unit) {
    // TODO
  }
  
  def view(modelview: ModelView => Unit) {
    // TODO
  }
  
  def receiveEvent(e: Event) {
    // TODO
  }
  
}
