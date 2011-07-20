package name.brijest.sages.gui.initializer


import name.brijest.sages.controller.Controller
import name.brijest.sages.model.Instance
import name.brijest.sages.gui._


/**
 * Initializes a part of the gui.
 */
trait InitializionPart {
  def init(in: Initializer)
}

/**
 * Initializer initializes the GUI and the MVC mechanism,
 * regardless of the concrete GUI implementation.
 */
class Initializer(val factory: WidgetFactory, val ctrl: Controller, val player: Int,
                  val odpal: ObjectDrawerPalette, val tdpal: TerrainDrawerPalette) {
  val guistate = new GuiState
  
  // main initialization
  WidgetProperties.init(this)
  ControllerListening.init(this)
  OnCreate.init(this)
  
  // gui listening
  SelectObjectListening.init(this)
  MoveObjectListening.init(this)
  InteractObjectListening.init(this)
}








