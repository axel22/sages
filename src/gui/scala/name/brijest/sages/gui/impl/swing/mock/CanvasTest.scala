package name.brijest.sages.gui.impl.swing.mock


import scala.swing._
import scala.swing.event._

import name.brijest.sages.controller.impl.controllers.MockController
import name.brijest.sages.common.Pathfinding
import name.brijest.sages.model.Model
import name.brijest.sages.model.MutableQuantity
import name.brijest.sages.model.quantities.IntNum
import name.brijest.sages.model.Instance
import name.brijest.sages.gui.main.ElementsLoader
import name.brijest.sages.gui.initializer.Initializer
import name.brijest.sages.gui._
import name.brijest.sages.gui.impl.swing._


object CanvasTest extends SimpleGUIApplication {
  javax.swing.UIManager.setLookAndFeel(javax.swing.UIManager.getSystemLookAndFeelClassName());
  
  def top = new MainFrame {
    title = "Canvas test"
    val elems = new SwingWidgetFactory
    contents = elems
    val model = initModel(elems)
    val ctrl = new MockController(model)
    val initializer = new Initializer(elems, ctrl, -1, ElementsLoader.odrawpal, ElementsLoader.tdrawpal)
  }
  def initModel(elems: WidgetFactory) = {
    val sz = 40
    val model = new Model(sz, ElementsLoader.tpal, ElementsLoader.opal, ElementsLoader.catfact, "")
    model.execute(new name.brijest.sages.model.Execution {
      def apply(ad: Model#Adapter) = {
        val rand = new scala.util.Random
        for (x <- 0 until sz; y <- 0 until sz) {
          val slot = model.terrainPalette.createSlot(11, rand.nextInt(5))
          ad.setSlot((x, y), slot)
        }
        ad.addInstance((0, 10), model.objectPalette.createInstance(151, "NormalTree"))
        ad.addInstance((1, 13), model.objectPalette.createInstance(152, "NormalTree"))
        ad.addInstance((2, 0), model.objectPalette.createInstance(2131, "Willow"))
        ad.addInstance((0, 20), model.objectPalette.createInstance(2134, "Willow"))
        val inst0 = model.objectPalette.createInstance(2136, "Willow")
        inst0.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(4)
        ad.addInstance((1, 20), inst0)
        val inst = model.objectPalette.createInstance(2137, "Willow")
        inst.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(1)
        ad.addInstance((0, 21), inst)
        val inst2 = model.objectPalette.createInstance(2138, "Willow")
        inst2.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(2)
        ad.addInstance((1, 21), inst2)
        val inst3 = model.objectPalette.createInstance(2139, "Willow")
        inst3.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(3)
        ad.addInstance((2, 21), inst3)
        ad.addInstance((2, 19), model.objectPalette.createInstance(1, "Church"))
        ad.addInstance((0, 14), model.objectPalette.createInstance(2, "Knight castle"))
        ad.addInstance((2, 22), model.objectPalette.createInstance(3, "Windmill"))
        ad.addInstance((25, 14), model.objectPalette.createInstance(4, "Wizard's fort"))
        ad.addInstance((4, 20), model.objectPalette.createInstance(5, "Sage"))
        ad.addInstance((14, 15), model.objectPalette.createInstance(6, "Cathedral"))
        ad.addInstance((5, 12), model.objectPalette.createInstance(7, "ForestTree"))
        ad.addInstance((10, 12), model.objectPalette.createInstance(8, "ForestTree"))
        ad.addInstance((12, 12), model.objectPalette.createInstance(9, "ForestTree"))
        ad.addInstance((14, 14), model.objectPalette.createInstance(10, "ForestTree"))
        ad.addInstance((15, 14), model.objectPalette.createInstance(11, "ForestTree"))
        val inst4 = model.objectPalette.createInstance(12, "ForestTree")
        inst4.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(1)
        ad.addInstance((12, 16), inst4)
        ad.addInstance((15, 12), model.objectPalette.createInstance(13, "ForestTree"))
        val inst5 = model.objectPalette.createInstance(14, "ForestTree")
        inst5.prop("Guise").asInstanceOf[MutableQuantity].value = new IntNum(1)
        ad.addInstance((6, 14), inst5)
        null
      }
    }, -1)
    model
  }
}









