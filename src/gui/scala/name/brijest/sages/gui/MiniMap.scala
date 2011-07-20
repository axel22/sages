package name.brijest.sages.gui


import name.brijest.sages.controller._
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Event
import name.brijest.sages.model.ChangeTerrainEvent
import name.brijest.sages.model.AddInstanceEvent
import name.brijest.sages.model.RemoveInstanceEvent
import name.brijest.sages.model.RelocateInstanceEvent
import name.brijest.sages.gui.Fundamental._


/**
 * Concrete widgets must implement:
 * Method <code>invokeRepaint</code> which invokes a repaint event.
 * They must call:
 * Method <code>paint</code> on repaint events.
 * Method <code>onMouseClick</code> which is called when the widget is mouse-clicked.
 */
trait MiniMap extends Widget with EventListener {
  private val BLACK = new java.awt.Color(0, 0, 0)
  
  def listens = List(classOf[ChangeTerrainEvent], classOf[AddInstanceEvent],
                     classOf[RemoveInstanceEvent], classOf[RelocateInstanceEvent])
  def onEvent(e: Event) = {
    invokeRepaint
  }
  def invokeRepaint: Unit
  def paint(ad: DrawAdapter) {
    controller view ((v: ModelView) => {
      val w = width
      val h = height
      ad.setColor(BLACK)
      ad.fillRect(0, 0, w, h)
      
      val fullw = maxPlanarWidth(v.size)
      val fullh = maxPlanarHeight(v.size)
      
      ad.scale(1.0 * w / fullw, 1.0 * h / fullh)
//      var lastpos: (Double, Double) = null
      for (x <- 0 until v.size; y <- 0 until v.size) {
        val t = v.slot(x, y)
        val uvpos = isoslot2planar(x, y, v.size)
        ad.setColor(t.terrain.colour)
        ad.fillRect(uvpos._1.toInt, uvpos._2.toInt, Fundamental.terrwdt, Fundamental.terrhgt)
//        if (lastpos == null) lastpos = uvpos
//        ad.drawLine(lastpos._1.toInt, lastpos._2.toInt, uvpos._1.toInt, uvpos._2.toInt)
//        lastpos = uvpos
      }
    })
  }
  def onMouseClick(x: Int, y: Int) {
    // TODO
  }
}









