package name.brijest.sages.gui


import scala.collection._
import scala.ref._
import scala.actors._
import scala.actors.Actor._

import java.awt.image.BufferedImage
import javax.imageio.ImageIO

import name.brijest.sages.common.collections.Array2D
import name.brijest.sages.common.collections.SectorArray
import name.brijest.sages.common.collections.PseudoQuadTree
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Event
import name.brijest.sages.model.ChangeTerrainEvent
import name.brijest.sages.model.AddInstanceEvent
import name.brijest.sages.model.RemoveInstanceEvent
import name.brijest.sages.model.RelocateInstanceEvent
import name.brijest.sages.model.InstancePropertyChangedEvent
import name.brijest.sages.model.InstanceActionInvokedEvent
import name.brijest.sages.model.Instance
import name.brijest.sages.model.objects.Surrogate
import name.brijest.sages.controller.Controller
import name.brijest.sages.controller.EventListener
import name.brijest.sages.gui.images.Images
import Fundamental._


trait Canvas extends Widget with EventListener

/**
 * Background canvas - has methods for drawing it's background.
 */
trait BackgroundCanvas extends Canvas {
  lazy val sback = ImageIO.read(classOf[Images].getResourceAsStream("stars1.png"))
  lazy val star1 = ImageIO.read(classOf[Images].getResourceAsStream("star1.png"))
  lazy val star2 = ImageIO.read(classOf[Images].getResourceAsStream("star2.png"))
  lazy val star3 = ImageIO.read(classOf[Images].getResourceAsStream("star3.png"))
  lazy val star4 = ImageIO.read(classOf[Images].getResourceAsStream("star4.png"))
  lazy val star5 = ImageIO.read(classOf[Images].getResourceAsStream("star5.png"))
  lazy val star6 = ImageIO.read(classOf[Images].getResourceAsStream("star6.png"))
  
  private def drawLayer(img: BufferedImage, upos: Int, vpos: Int, ad: DrawAdapter, mult: Int) {
    val l1wdt = img.getWidth
    val l1hgt = img.getHeight
    for (i <- -1 to (width / l1wdt + 1); j <- -1 to (height / l1hgt + 1)) {
      val up = i * l1wdt - (upos / mult % l1wdt)
      val vp = j * l1hgt - (vpos / mult % l1hgt)
      ad.drawImage(up, vp, img)
    }
  }
  private def drawStarLayer(img: BufferedImage, upos: Int, vpos: Int, ad: DrawAdapter, mult: Double, density: Int) {
    val iwdt = img.getWidth
    val ihgt = img.getHeight
    val random = new java.util.Random(mult.toInt * 11 + 2314)
    for (i <- 0 to density) {
      val upabs = ((random.nextInt - upos / mult) % width).toInt
      val vpabs = ((random.nextInt - vpos / mult) % height).toInt
      val up = if (upabs >= 0) upabs % width else (upabs + (upabs.abs % width + 1) * (width))
      val vp = if (vpabs >= 0) vpabs % height else (vpabs + (vpabs.abs % height + 1) * (height))
      ad.drawImage(up, vp, img)
    }
  }
  protected def drawBackground(upos: Int, vpos: Int, ad: DrawAdapter) {
    drawLayer(sback, upos, vpos, ad, 16)
    drawStarLayer(star6, upos, vpos, ad, 14, 400)
    drawStarLayer(star5, upos, vpos, ad, 10, 250)
    drawStarLayer(star4, upos, vpos, ad, 8, 100)
    drawStarLayer(star3, upos, vpos, ad, 6, 80)
    drawStarLayer(star2, upos, vpos, ad, 4, 40)
    drawStarLayer(star1, upos, vpos, ad, 2, 15)
  }
}

/**
 * Defines methods for drawing itself and the model terrain using a DrawAdapter. It is intended to be mixed
 * in with a concrete widget from a certain UI toolkit - it may thus accelerate creation of UI
 * in multiple toolkits.<br/>
 * Concrete widgets should call <code>onResize</code> method when a widget gets resized. They should call
 * the <code>paintCanvas</code> method when repaint events occur.<br/>
 * Concrete widgets must implement the <code>invokeRepaint</code> method.
 */
trait TerrainCanvas extends BackgroundCanvas {
  private val tdcache = mutable.HashMap[(Int, Int), SoftReference[TerrainDrawer]]()
  protected val visibletds = new Array2D[TerrainDrawer](1, 1)
  protected var xmin = 0
  protected var ymin = 0
  protected var xmax = 0
  protected var ymax = 0
  protected var upos = 0
  protected var vpos = 0
  private var cacheinvalid = false
  private var highlighted: (Int, Int) = null
  
  var terraindrawerpalette: TerrainDrawerPalette = null
  
  def onEvent(e: Event) = e match {
    case cte: ChangeTerrainEvent =>
      cacheinvalid = true
      invokeRepaint
    case _ =>
  }
  def listens: List[Class[_]] = List(classOf[ChangeTerrainEvent])
  def uPosition = upos
  def vPosition = vpos
  def uPosition_=(u: Int) = {
    upos = u
  }
  def vPosition_=(v: Int) = {
    vpos = v
  }
  def position = (upos, vpos)
  def position_=(pos: (Int, Int)) = {
    upos = pos._1
    vpos = pos._2
  }
  def highlightTerrain(t: (Int, Int)) = highlighted = t
  def totalwidth(v: ModelView) = isoslot2planar(v.size - 1, 0, v.size)._1
  def totalheight(v: ModelView) = isoslot2planar(v.size - 1, v.size - 1, v.size)._2
  def totalsize(v: ModelView) = (totalwidth(v), totalheight(v))
  def paintCanvas(ad: DrawAdapter) = controller view (overridablePaint(ad, _))
  def onResize = invokeRepaint
  /** Invokes repainting of the canvas. The concrete widget should redraw itself. */
  def invokeRepaint: Unit
  
  protected def overridablePaint(ad: DrawAdapter, v: ModelView) {
    // refresh drawer cache
    prepareForDrawing(v)
    
    // draw background
    drawBackground(upos, vpos, ad)
    
    // draw terrain
    for (x <- xmin to xmax; y <- ymin to ymax) {
      val (up, vp) = calcScreenPosition(x, y, v.size)
      val td = getTerrainDrawer(x, y, v)
      if (td != null) td.draw(up.toInt - terrwdt / 2, vp.toInt - terrhgt / 2, ad)
    }
    
    // draw terrain highlight
    if (highlighted != null) drawHighlight(ad, v.size)
  }
  private def drawHighlight(ad: DrawAdapter, sz: Int) = {
    val dpos = calcScreenPosition(highlighted, sz)
    val pos = (dpos._1.toInt, dpos._2.toInt)
    ad.setColor(new java.awt.Color(0, 240, 190))
    ad.drawLine(pos._1, pos._2 - terrhgt / 2, pos._1 + terrwdt / 2, pos._2)
    ad.drawLine(pos._1 + terrwdt / 2, pos._2, pos._1, pos._2 + terrhgt / 2)
    ad.drawLine(pos._1, pos._2 + terrhgt / 2, pos._1 - terrwdt / 2, pos._2)
    ad.drawLine(pos._1 - terrwdt / 2, pos._2, pos._1, pos._2 - terrhgt / 2)
  }
  /** Returns slot center position. */
  protected def calcScreenPosition(xysl: (Int, Int), sz: Int): (Double, Double) =
    calcScreenPosition(xysl._1, xysl._2, sz)
  protected def calcScreenPosition(xslot: Int, yslot: Int, sz: Int) = {
    val (uabs, vabs) = isoslot2planar(xslot, yslot, sz)
    (uabs - upos, vabs - vpos + terrhgt / 2)
  }
  private def emptyDrawerCache {
    tdcache.clear
    visibletds.clear
    cacheinvalid = false
  }
  private def prepareForDrawing(v: ModelView) {
    if (visibletds.width != v.size) visibletds.resize(v.size, v.size)
    if (cacheinvalid) emptyDrawerCache
    
    // determine new visible slots
//    println("____________________")
//    println("pos: " + upos + ", " + vpos + "; width: " + width + ", " + height)
    val topleft = planar2isoslot(upos, vpos, v.size)
    val topright = planar2isoslot(upos + width, vpos, v.size)
    val botleft = planar2isoslot(upos, vpos + height, v.size)
    val botright = planar2isoslot(upos + width, vpos + height, v.size)
    val nxmin = topleft._1.toInt
    val nymin = topright._2.toInt
    val nxmax = botright._1.toInt + 1
    val nymax = botleft._2.toInt + 1
    def withinnew(xp: Int, yp: Int) = xp >= nxmin && xp <= nxmax && yp >= nymin && yp <= nymax
    
    // add new slots to the visibles array
    val mapsz = v.size
    for (x <- nxmin to nxmax; y <- nymin to nymax) {
      if (x >= 0 && x < mapsz && y >= 0 && y < mapsz) visibletds.put(x, y, getTerrainDrawer(x, y, v))
    }
    
    // remove old slots from the visibles array
    for (x <- xmin to xmax; y <- ymin to ymax; if (!withinnew(x, y))) {
      if (x >= 0 && x < mapsz && y >= 0 && y < mapsz) visibletds.put(x, y, null)
    }
    
    // assign new values for visible slots boundaries
    xmin = nxmin
    xmax = nxmax
    ymin = nymin
    ymax = nymax
//    println(xmin + ", " + ymin + ", " + xmax + ", " + ymax)
  }
  private def within(x: Int, y: Int) = x >= xmin && x <= xmax && y >= ymin && y <= ymax
  private def within(t: (Int, Int)): Boolean = within(t._1, t._2)
  /**
   * Gets the terrain from the view and creates the appropriate terrain drawer in the drawer map.
   * It puts it on the list of the visible drawers as well.
   */
  private def loadTerrainDrawer(x: Int, y: Int, view: ModelView): TerrainDrawer = {
    val tslot = view.slot(x, y)
    if (tslot == null) return null
    val td = terraindrawerpalette.createDrawer(tslot.terrain.name, tslot.subindex)
    tdcache.put((x, y), new SoftReference(td))
    if (within(x, y)) visibletds.put((x, y), td)
    td
  }
  /** Gets the terrain drawer from the terrain drawer cache. Creates a new drawer if it's not in cache. */
  private def getTerrainDrawer(x: Int, y: Int, view: ModelView) = {
    tdcache.get(x, y) match {
      case Some(wr) => wr.get match {
        case Some(td) => td
        case None => loadTerrainDrawer(x, y, view)
      }
      case None => loadTerrainDrawer(x, y, view)
    }
  }
}

object ArrowPics {
  private def ani(name: String) = new SingleFrameAnimation(ImageIO.read(classOf[Images].getResource(name)))
  val N = ani("arrow0000.png")
  val NW = ani("arrow0001.png")
  val W = ani("arrow0002.png")
  val SW = ani("arrow0003.png")
  val S = ani("arrow0004.png")
  val SE = ani("arrow0005.png")
  val E = ani("arrow0006.png")
  val NE = ani("arrow0007.png")
  val TARGET = ani("target.png")
  val RN = ani("arrow_red0000.png")
  val RNW = ani("arrow_red0001.png")
  val RW = ani("arrow_red0002.png")
  val RSW = ani("arrow_red0003.png")
  val RS = ani("arrow_red0004.png")
  val RSE = ani("arrow_red0005.png")
  val RE = ani("arrow_red0006.png")
  val RNE = ani("arrow_red0007.png")
  val RTARGET = ani("target_red.png")
  def north(red: Boolean) = if (red) RN else N
  def northwest(red: Boolean) = if (red) RNW else NW
  def west(red: Boolean) = if (red) RW else W
  def southwest(red: Boolean) = if (red) RSW else SW
  def south(red: Boolean) = if (red) RS else S
  def southeast(red: Boolean) = if (red) RSE else SE
  def east(red: Boolean) = if (red) RE else E
  def northeast(red: Boolean) = if (red) RNE else NE
  def target(red: Boolean) = if (red) RTARGET else TARGET
}

trait DecoratedTerrainCanvas extends TerrainCanvas {
  private val decorations = mutable.Set[List[(Int, Int, TerrainDecorationDrawer)]]()
  
  override protected def overridablePaint(ad: DrawAdapter, v: ModelView) {
    super.overridablePaint(ad, v)
    
    // draw terrain decorations
    for (dec <- decorations; (x, y, decdrawer) <- dec) {
      val uvposcenter = calcScreenPosition(x, y, v.size)
      val uvpos = (uvposcenter._1.toInt - terrwdt / 2, uvposcenter._2.toInt - terrhgt / 2)
      decdrawer.draw(uvpos._1, uvpos._2, ad)
    }
  }
  
  def addTerrainDecorationList(lst: List[(Int, Int, TerrainDecorationDrawer)]): Unit = decorations + lst
  def removeTerrainDecorationList(lst: List[(Int, Int, TerrainDecorationDrawer)]): Unit = decorations - lst
  def clearTerrainDecorations = decorations.clear
}

/**
 * Concrete widgets mixing this trait should call the <code>onResize</code> and <code>paintCanvas</code>
 * methods when appropriate.
 */
trait ObjectCanvas extends DecoratedTerrainCanvas {
  abstract class Scheduled extends Ordered[Scheduled] {
    def pos: (Int, Int)
    def inst: Instance
    def drawer: ObjectDrawer
    override def toString = pos + ", instance = " + inst
    def compare(that: Scheduled) = {
      val diff = (this.pos._1 + this.pos._2) - (that.pos._1 + that.pos._2)
      if (diff != 0) diff
      else {
        if (this.pos._1 < that.pos._1) -1
        else if (this.pos._1 > that.pos._1) 1
        else if (this equals that) 0
        else that match {
          case Concrete(p, i, d) => 1
          case Delayed(p, o) => -1
        }
      }
    }
    def criticalSlot = (pos._1 + inst.size._1, pos._2 - 1)
  }
  case class Concrete(val pos: (Int, Int), val inst: Instance, val drawer: ObjectDrawer) extends Scheduled {
    override def equals(that: Any) = that match {
      case Concrete(p, i, d) => p == pos
      case _ => false
    }
  }
  case class Delayed(val pos: (Int, Int), val original: Concrete) extends Scheduled {
    override def equals(that: Any) = that match {
      case Delayed(p, o) => p == pos
      case _ => false
    }
    def inst = null
    def drawer = null
  }
  
  private val objsectors = new SectorArray[Int](5, 5, 10, 10)
  private val odcache = mutable.Map[Int, SoftReference[ObjectDrawer]]()
  protected val visibleods = mutable.Map[Int, ObjectDrawer]()
  private val forbiddenObjects = mutable.Set[Int]()
  private var drawschedule = new mutable.ListBuffer[Scheduled]()
  private var sectorinitneeded = true
  private var addedinstances = mutable.Map[Int, Instance]()
  private val enqueuedEffects = mutable.Map[(Int, Int), List[EffectDrawer]]()
  
  var objectdrawerpalette: ObjectDrawerPalette = null
  
  def forbidDrawing(index: Int) = forbiddenObjects + index
  def allowDrawing(index: Int) = forbiddenObjects - index
  def enqueueEffectDrawer(ed: EffectDrawer) {
    if (!enqueuedEffects.contains(ed.location)) enqueuedEffects.put(ed.location, Nil)
    enqueuedEffects.put(ed.location, ed :: enqueuedEffects(ed.location))
  }
  
  protected override def overridablePaint(ad: DrawAdapter, v: ModelView) {
    // draws the terrain and decorations
    // this call also sets the xmin, ymin, xmax, ymax vars appropriately
    super.overridablePaint(ad, v)
    
    // see if initialization is necessary
    if (sectorinitneeded) initializeSectors(v)
    else updateSectors(v)
    
    // prepare draw schedule
    prepareDrawSchedule(v)
    
    // draw visible objects
    for (sch <- drawschedule) {
      val pos = calcScreenPosition(sch.pos, v.size)
      if (!forbiddenObjects.contains(sch.inst.index))
        sch.drawer.draw(pos._1.toInt, pos._2.toInt, sch.inst, ad)
      
      // invoke effect drawers that are at this object's locations
      for (aloc <- sch.inst.prototype.occupied;
           relloc = (sch.pos._1 + aloc._1, sch.pos._2 + aloc._2)
           if enqueuedEffects.contains(relloc);
           ed <- enqueuedEffects(relloc))
        {
    	  val p = calcScreenPosition(relloc, v.size)
          ed.draw(p._1.toInt, p._2.toInt, ad)
        }
    }
    enqueuedEffects.clear
  }
  
  override def onEvent(e: Event) = {
    e match {
      case AddInstanceEvent(p, inst) =>
        addedinstances.put(inst.index, inst)
      case RemoveInstanceEvent(p, inst) =>
        removeFromSectors(inst.index)
        addedinstances.removeKey(inst.index)
      case RelocateInstanceEvent(oldp, np, inst) =>
        removeFromSectors(inst.index)
        addedinstances.removeKey(inst.index)
        addedinstances.put(inst.index, inst)
      case _ => 
    }
    super.onEvent(e)
  }
  override def listens = classOf[AddInstanceEvent] :: classOf[RemoveInstanceEvent] :: 
    classOf[RelocateInstanceEvent] :: super.listens
  
  /**
   * Resizes the sector array, visible drawers array, etc.
   */
  private def initializeSectors(v: ModelView) {
    // refill object sectors
    val tsz = totalsize(v)
    objsectors.resize(640, 480, tsz._1.toInt, tsz._2.toInt)
    for ((loc, inst) <- v.instanceMap) addToSectors(v, inst)
    
    sectorinitneeded = false
    addedinstances.clear
  }
  
  private def updateSectors(v: ModelView) {
    for ((ind, inst) <- addedinstances) addToSectors(v, inst)
    addedinstances.clear
  }
  
  private def addToSectors(v: ModelView, inst: Instance) {
    // determine the bounds of this object
    val od = getObjectDrawer(inst)
    val doubpos = calcScreenPosition(v.instanceMap.getLocationFor(inst), v.size)
    val pos = (doubpos._1.toInt, doubpos._2.toInt)
    val bbox = od.boundingbox
    val topleft = (pos._1 + bbox._1._1, pos._2 + bbox._1._2)
    val botright = (pos._1 + bbox._2._1, pos._2 + bbox._2._2)
    for (sector <- objsectors.sectorsInRect(topleft, botright)) objsectors.addToSector(sector, inst.index)
  }
  
  private def removeFromSectors(index: Int) = objsectors.remove(index)
  
  private def prepareDrawSchedule(view: ModelView) {
    // 1. find objects currently on the screen - determine visible objects
    // (visible objects are in the 'visibleods' at all times)
    val scrsectors = objsectors.sectorsInRect((uPosition, vPosition), (uPosition + width, vPosition + height))
    
    // 2. order the objects into a data structure, in respect to their (x + y) value
    val added = mutable.Set[Int]()
    var initialschedule: immutable.Set[Scheduled] = immutable.TreeSet[Scheduled]()
    for ((u, v) <- scrsectors) objsectors.get(u, v).foreach({index =>
      val inst = view.getInstance(index)
      if (!added.contains(index)) {
        initialschedule = initialschedule + 
          new Concrete(view.instanceMap.getLocationFor(inst), inst, getObjectDrawer(inst))
        added + index
      }
    })
    
    // 2.b) add 'surrogate' objects and drawers on top of which effects will be drawn to initial list
    for ((pos, edlst) <- enqueuedEffects; if !view.containsInstanceAt(pos)) {
      initialschedule += new Concrete(pos, new Instance(-1, new Surrogate), new EmptyObjectDrawer)
    }
    
    // 3. create a real draw schedule using the isometric drawing algorithm
    val quad = new PseudoQuadTree[List[Delayed]]()
    drawschedule.clear
    while (!initialschedule.isEmpty) {
      val sch = initialschedule.asInstanceOf[immutable.TreeSet[Scheduled]].firstKey
      initialschedule = initialschedule - sch
      sch match {
        case c @ Concrete(pos, inst, od) =>
          // a concrete object must be delayed until it's critical slot is processed and free
          val critical = c.criticalSlot
          initialschedule = initialschedule + new Delayed(critical, c)
          quad((pos._1, -pos._2)) = Nil
        case d @ Delayed(critical, original) =>
          // a delayed object must be drawn if possible, and all objects that depend must be reprocessed
          // if drawing the object is not possible, it is placed in the quad queue and processed later
          quad.northwest((critical._1, -critical._2)) match {
            case Some((k, v)) => // depends on some slot - drawing IS NOT possible
              // add object on the todo list
              quad((k._1, k._2)) = d :: v
            case None => // does not depend - drawing IS possible
              // put the object on the drawschedule
              drawschedule.append(original)
              val dependantobjs = quad(original.pos._1, -original.pos._2)
              quad.removeKey((original.pos._1, -original.pos._2))
              // put all of the dependant objects from the quad back to initial schedule
              for (delsch <- dependantobjs) initialschedule = initialschedule + delsch
          }
      }
    }
    if (!quad.isEmpty) throw new IllegalStateException("Quad must get empty after scheduling!")
    
    // 4. reset the list of currently visible object drawers
    visibleods.clear
    for (sch <- drawschedule) visibleods.put(sch.inst.index, sch.drawer)
  }
  
  protected def getObjectDrawer(inst: Instance) = {
    odcache.get(inst.index) match {
      case Some(ref) => ref.get match {
        case Some(od) => od
        case None => addObjectDrawerFor(inst)
      }
      case None => addObjectDrawerFor(inst)
    }
  }
  
  private def addObjectDrawerFor(inst: Instance) = {
    val od = objectdrawerpalette.createDrawer(inst.prototype.objectname, inst)
    odcache.put(inst.index, new SoftReference(od))
    od
  }
  
}

/**
 * Concrete widgets mixing this trait should call the <code>onResize</code> and <code>paintCanvas</code>
 * methods when appropriate.
 */
trait EffectCanvas extends ObjectCanvas {
  protected val effectdrawers = new mutable.LinkedHashMap[(Int, Int), mutable.Set[EffectDrawer]]
  
  protected override def overridablePaint(a: DrawAdapter, v: ModelView) {
    // draw effects near objects
    for (((x, y), edset) <- effectdrawers; ed <- edset) if (!ed.isAboveObjects) {
      // we do not call ed.draw(x, y, a) here - instead we enqueue the drawer,
      // so that it get's drawn by the ObjectCanvas in the appropriate order
      enqueueEffectDrawer(ed)
    }
    
    // draw terrain and objects
    // also, set the xmin, xmax, ymin and ymax
    super.overridablePaint(a, v)
    
    // draw effects above objects, and clear obsolete effect drawers
    val toremove = mutable.Set[(Int, Int, EffectDrawer)]()
    for ((xypos @ (x, y), edset) <- effectdrawers; ed <- edset) {
      if (ed.isAboveObjects) {
        val uvpos = calcScreenPosition(xypos, v.size)
        ed.draw(uvpos._1.toInt, uvpos._2.toInt, a)
      }
      if (!ed.hasNext) toremove + ((x, y, ed))
    }
    for ((x, y, ed) <- toremove) {
      removeEffect(x, y, ed)
    }
  }
  
  def addEffect(x: Int, y: Int, ed: EffectDrawer): Unit = addEffect((x, y), ed)
  def addEffect(t: (Int, Int), ed: EffectDrawer) = {
    (if (effectdrawers.contains(t)) effectdrawers(t) else {
      val nset = mutable.Set[EffectDrawer]()
      effectdrawers.put(t, nset)
      nset
    }) + ed
  }
  def removeEffect(x: Int, y: Int, ed: EffectDrawer) {
    if (effectdrawers.contains(x, y)) effectdrawers(x, y) - ed
  }
}

/**
 * Concrete widgets mixing this trait should call the <code>onResize</code> and <code>paintCanvas</code>
 * methods when appropriate.<br/>
 * This canvas creates a thread which updates the frames of all drawers periodically, thus animating
 * the canvas.
 */
trait AnimatedCanvas extends EffectCanvas {
  var framesToMove = 0
  var lock = new AnyRef
  
  actor {
    loop {
      // wait
      Thread.sleep(Fundamental.framelength)
      
      // update frames of the canvas elements
      lock.synchronized { 
        framesToMove += 1
      }
      
      // invoke repaint
      invokeRepaint
    }
  }
  
  override def overridablePaint(a: DrawAdapter, v: ModelView) {
    // update
    lock.synchronized {
      while (framesToMove > 0) {
        for (td <- visibletds if td != null) td.nextFrame
        for ((ind, od) <- visibleods) od.nextFrame
        for ((pos, edset) <- effectdrawers; ed <- edset) ed.nextFrame
        framesToMove -= 1
      }
    }
    
    // override paint
    super.overridablePaint(a, v)
  }
}

trait TerrainActionListener {
  def onTerrainOver(xslot: Int, yslot: Int): Unit = onTerrainOver((xslot, yslot))
  def onTerrainOver(slot: (Int, Int)): Unit
  def onTerrainClick(xslot: Int, yslot: Int): Unit = onTerrainClick((xslot, yslot))
  def onTerrainClick(slot: (Int, Int)): Unit
  def onTerrainRightClick(xslot: Int, yslot: Int): Unit = onTerrainRightClick((xslot, yslot))
  def onTerrainRightClick(slot: (Int, Int)): Unit
}

trait ObjectActionListener {
  def onObjectOver(inst: Instance): Unit
  def onObjectClick(inst: Instance): Unit
}

trait TerrainActionAdapter extends TerrainActionListener {
  def onTerrainOver(slot: (Int, Int)) = null
  def onTerrainClick(slot: (Int, Int)) = null
  def onTerrainRightClick(slot: (Int, Int)) = null
}

trait ObjectActionAdapter extends ObjectActionListener {
  def onObjectOver(inst: Instance) = null
  def onObjectClick(inst: Instance) = null
}
trait InteractiveCanvas extends AnimatedCanvas {
  private val terrainlisteners = mutable.Set[TerrainActionListener]()
  private val objectlisteners = mutable.Set[ObjectActionListener]()
  
  def addTerrainActionListener(tl: TerrainActionListener) = terrainlisteners + tl
  def removeTerrainActionListener(tl: TerrainActionListener) = terrainlisteners - tl
  def addObjectActionListener(ol: ObjectActionListener) = objectlisteners + ol
  def removeObjectActionListener(ol: ObjectActionListener) = objectlisteners - ol
  def onMouseMove(mx: Int, my: Int) {
    controller.view((v: ModelView) => {
      // determine the slot of the mouse
      val mdoubslot = planar2isoslot(uPosition + mx, vPosition + my, v.size)
      val mslot = (mdoubslot._1.toInt, mdoubslot._2.toInt)
      
      if (v.within(mslot)) {
        // invoke terrain listeners
        for (l <- terrainlisteners) l.onTerrainOver(mslot)
        
        // invoke object listeners
        val inst = v.getInstance(mslot)
        if (inst != null) for (l <- objectlisteners) l.onObjectOver(inst)
      }
    })
  }
  def onLeftMouseClick(mx: Int, my: Int) {
    controller.view((v: ModelView) => {
      // determine the slot of the mouse
      val mdoubslot = planar2isoslot(uPosition + mx, vPosition + my, v.size)
      val mslot = (mdoubslot._1.toInt, mdoubslot._2.toInt)

      if (v.within(mslot)) {
        // invoke terrain listeners
        for (l <- terrainlisteners) l.onTerrainClick(mslot)
        
        // invoke object listeners
        val inst = v.getInstance(mslot)
        if (inst != null) for (l <- objectlisteners) l.onObjectClick(inst)
      }
    })
  }
  def onRightMouseClick(mx: Int, my: Int) {
    controller.view((v: ModelView) => {
      // determine the slot of the mouse
      val mdoubslot = planar2isoslot(uPosition + mx, vPosition + my, v.size)
      val mslot = (mdoubslot._1.toInt, mdoubslot._2.toInt)
      
      for (l <- terrainlisteners) l.onTerrainRightClick(mslot)
    })
  }
}

/**
 * Concrete widgets mixing this trait should call the <code>onResize</code> and <code>paintCanvas</code>
 * methods when appropriate.<br/>
 * Also, concrete widgets should call this trait's <code>onMouseMove</code> and <code>onMouseClick</code>
 * methods whenever they receive such an event. In turn, this trait will inform any registered terrain
 * clicked and object clicked listeners. Note: mouse movement events include left-click dragging.<br/>
 * Whenever right mouse button is pressed a concrete widget should call the <code>onRightMousePressed</code>
 * and whenever right mouse button is released a concrete widget should call the
 * <code>onRightMouseReleased</code> method of this trait. If a mouse moves and the right key is down, then
 * a drag event should be invoked - widget should call <code>onRightMouseDrag</code>.
 */
trait ScrollableCanvas extends InteractiveCanvas {
  private var pressed = false
  private var lastpos: (Int, Int) = null
  
  def onRightMousePressed(mx: Int, my: Int) = {
    pressed = true
    lastpos = (mx, my)
  }
  def onRightMouseReleased(mx: Int, my: Int) = pressed = false
  def onRightMouseDrag(mx: Int, my: Int) {
    if (pressed) {
      // perform scrolling
      val posdiff = (mx - lastpos._1, my - lastpos._2)
      position = (uPosition - posdiff._1, vPosition - posdiff._2)
      lastpos = (mx, my)
    }
  }
}

trait SlotHighlightingCanvas extends ScrollableCanvas with TerrainActionListener {
  addTerrainActionListener(this)
  
  private var should = true
  
  def onTerrainOver(t: (Int, Int)) = if (should) highlightTerrain(t)
  def onTerrainClick(t: (Int, Int)) = null
  def onTerrainRightClick(t: (Int, Int)) = null
  def shouldHighlight(sh: Boolean) = should = sh; if (!should) highlightTerrain(null)
}

/**
 * Invokes effects once they happen.
 */
trait EffectInvokingCanvas extends SlotHighlightingCanvas {
  private var eventlist = List[Event]()
  
  override def listens = classOf[InstanceActionInvokedEvent] :: classOf[InstancePropertyChangedEvent] ::
    super.listens
  override def onEvent(e: Event) = e match {
    case InstancePropertyChangedEvent(inst, oval, nval, name, loc) => enqueueEffect(e)
    case InstanceActionInvokedEvent(inst, name, params, result, loc) => enqueueEffect(e)
    case AddInstanceEvent(loc, inst) => enqueueEffect(e); super.onEvent(e)
    case RemoveInstanceEvent(loc, inst) => enqueueEffect(e); super.onEvent(e)
    case RelocateInstanceEvent(oldloc, newloc, inst) => enqueueEffect(e); super.onEvent(e)
    case _ => super.onEvent(e)
  }
  private def enqueueEffect(e: Event) = if (e.instanceIndex != -1) eventlist = e :: eventlist
  override def overridablePaint(a: DrawAdapter, v: ModelView) {
    // find effect drawers for all events
    for (e <- eventlist) {
      val od = getObjectDrawer(e.instance)
      val op = od.effectDrawerFor(e)
      op match {
        case Some(ed) => addEffect(e.location, ed)
        case None =>
      }
    }
    eventlist = Nil
    
    super.overridablePaint(a, v)
  }
}

/**
 * This class displays the model with all the details.<br/>
 * Concrete widgets mixing this trait should call the <code>onResize</code> and <code>paintCanvas</code>
 * methods when appropriate.<br/>
 * Also, concrete widgets should call this trait's <code>onMouseMove</code> and <code>onMouseClick</code>
 * methods whenever they receive such an event. In turn, this trait will inform any registered terrain
 * clicked and object clicked listeners.<br/>
 * Whenever right mouse button is pressed a concrete widget should call the <code>onRightMousePressed</code>
 * and whenever right mouse button is released a concrete widget should call the
 * <code>onRightMouseReleased</code> method of this trait. If a mouse moves and the right key is down, then
 * a drag event should be invoked - widget should call <code>onMouseDrag</code>.
 */
trait ModelCanvas extends EffectInvokingCanvas {
  
}






/** Hack for ant :) */
//final class Canvas










