package name.brijest.sages.gui


import java.awt.image.BufferedImage

import name.brijest.sages.model.Instance
import name.brijest.sages.model.Event


/**
 * An animation is a collection of frames that may be iterated over. These
 * frames are drawn using a draw adapter.
 * A concrete implementation decides whether or not the animation is loopable.
 */
trait Animation {
  /** Are there more frames? */
  def hasNext: Boolean
  /** Jump to next frame. Calling this method does nothing if the current frame is the last one. */
  def nextFrame: Unit
  /** Draw image representing current frame. */
  def draw(x: Int, y: Int, a: DrawAdapter): Unit
  /** Draw image representing current frame. */
  def draw(t: (Int, Int), a: DrawAdapter): Unit = draw(t._1, t._2, a)
  /** Resets animation back to the first frame. */
  def reset: Unit
}

trait LoopedImagesAnimation extends Animation {
  var currentFrame = 0
  var cfcounter = 0
  /** Image sequence must not be empty. */
  def images: Array[BufferedImage]
  /** A positive number denoting how many frames before changing to the next image. */
  def frameLength: Int
  def hasNext = true
  def nextFrame = {
    if (cfcounter < frameLength) cfcounter = cfcounter + 1
    else {
      cfcounter = 0
      currentFrame = (currentFrame + 1) % images.size
    }
  }
  def draw(x: Int, y: Int, a: DrawAdapter) = a.drawImage(x, y, images(currentFrame))
  def reset = {
    currentFrame = 0
    cfcounter = 0
  }
}

trait SingleLoopImagesAnimation extends Animation {
  var current = 0
  /** Image sequence must not be empty. */
  def images: Array[BufferedImage]
  def hasNext = current < (images.size - 1)
  def nextFrame = if (hasNext) current = current + 1
  def draw(x: Int, y: Int, a: DrawAdapter) = a.drawImage(x, y, images(current))
  def reset = current = 0
}

class LoopedAnimation(val frameLength: Int, val images: Array[BufferedImage]) extends LoopedImagesAnimation

class SingleFrameAnimation(val image: BufferedImage) extends Animation {
  def hasNext = false
  def nextFrame = {}
  def draw(x: Int, y: Int, a: DrawAdapter) = a.drawImage(x, y, image)
  def reset = {}
}

/**
 * Always draws the image specified by index from the buffered image array.
 */
class SingleFrameAnimationLib(val images: Array[BufferedImage]) extends Animation {
  var index = 0
  def hasNext = false
  def nextFrame = {}
  def draw(x: Int, y: Int, a: DrawAdapter) = a.drawImage(x, y, images(index))
  def reset = {}
}

object NullAnimation extends Animation {
  def hasNext = false
  def nextFrame = {}
  def draw(x: Int, y: Int, a: DrawAdapter) = {}
  def reset = {}
}

/**
 * A drawer is used to draw something. It, too, is a kind of an animation.
 */
trait Drawer extends Animation

class EmptyDrawer extends Drawer {
  def hasNext = true
  def nextFrame = {}
  def draw(x: Int, y: Int, a: DrawAdapter) = {}
  def reset = {}
}

/**
 * Used to draw terrain. Subclasses MUST have a ctor taking the subindex of the
 * terrain (an Int).
 */
abstract class TerrainDrawer(val name: String, val subindex: Int, anim: Animation) extends Drawer {
  def hasNext = anim.hasNext
  def nextFrame = anim.nextFrame
  def draw(x: Int, y: Int, a: DrawAdapter) = anim.draw(x, y, a)
  def reset = anim.reset
  /** Returns a copy of this terrain drawer for a terrain subindex. */
  def copy(subindex: Int): TerrainDrawer
}

class TerrainDecorationDrawer(val anim: Animation) extends Drawer {
  def hasNext = anim.hasNext
  def nextFrame = anim.nextFrame
  def draw(x: Int, y: Int, a: DrawAdapter) = anim.draw(x, y, a)
  def reset = anim.reset
}

/** Used as a mixin - offsets the drawing. */
trait OffsetableDrawer extends Drawer {
  var xoffs = 0
  var yoffs = 0
  abstract override def draw(x: Int, y: Int, a: DrawAdapter) = super.draw(x + xoffs, y + yoffs, a)
}

/** Draws itself using it's animation object. */
trait AnimationDrawer extends Drawer {
  var animation: Animation = null
  def hasNext = animation.hasNext
  def nextFrame = animation.nextFrame
  def draw(x: Int, y: Int, a: DrawAdapter) = animation.draw(x, y, a)
  def reset = animation.reset
}

class AnimatedDrawer(ani: Animation) extends AnimationDrawer {
  animation = ani
}

/**
 * Used for drawing effects. Concrete implementations must provide an animation object for it.
 * Also, concrete implementations may alter it's behaviour by reimplementing it's Animation
 * interface or by using mixins.<br/>
 * No effect drawer should be loopable - at some point the <code>hasNext</code> method should
 * return false.
 */
trait EffectDrawer extends Drawer with AnimationDrawer with OffsetableDrawer {
  /** Returns whether the effect should be drawn above objects, or along with them. */
  def isAboveObjects: Boolean
  /**
   * Returns the location of the effect. If the effect is not drawn above other objects, this will be
   * the location at which the effect will be drawn.
   */
  def location: (Int, Int)
  /** Returns a copy of this effect drawer. */
  def copy: EffectDrawer
}

/**
 * An object drawer draws object instances of objects on the map.<br/>
 * Each concrete implementation must have an empty ctor.<br/>
 * It's draw method shall be called for the (u, v) of the center of the slot on which the object must be
 * drawn. This slot shall be the object's main slot (it's (0, 0)).
 */
trait ObjectDrawer extends Drawer {
  /** Name of the object it draws. */
  def name: String
  /** Returns a copy of this object drawer. */
  def copy: ObjectDrawer
  def draw(x: Int, y: Int, ad: DrawAdapter): Unit =
    throw new UnsupportedOperationException("Object drawers cannot draw with this method.")
  def draw(x: Int, y: Int, inst: Instance, ad: DrawAdapter)
  /**
   * Defines a bounding box into which this drawer will draw, with the center of 
   * the coordinate system being the center of the slot onto which the drawer
   * will draw.
   * @return
   * A tuple of tuples. The first tuple is the upper left boundary of the object,
   * and the second is the lower right boundary. Note that this is the screen position
   * relative to the drawers (0, 0) drawing point. It has nothing to do with terrain slot
   * coordinates. This method must always return the same value.
   */
  def boundingbox: ((Int, Int), (Int, Int))
  /** An object drawer may return special effect that render certain effects associated with the object. */
  def effectDrawerFor(e: Event): Option[EffectDrawer]
  /** Additional drawers may be associated with each drawer. */
  def helper(name: String): Option[Drawer] = None
}

trait LockObjectDrawer extends ObjectDrawer {
  protected trait LockerEffectDrawer extends EffectDrawer {
    private var drawerlocked = false
    
    protected def acquireLock = if (!drawerlocked && shouldDraw) {
      shouldDraw = false
      drawerlocked = true
      true
    } else drawerlocked
    protected def releaseLock = if (drawerlocked) {
      drawerlocked = false
      shouldDraw = true
    }
    abstract override def draw(x: Int, y: Int, ad: DrawAdapter) = if (drawerlocked) super.draw(x, y, ad)
  }
  
  var shouldDraw = true
  abstract override def draw(x: Int, y: Int, inst: Instance, ad: DrawAdapter) = if (shouldDraw)
    super.draw(x, y, inst, ad)
}

/**
 * Same as object drawer, but draws itself using the provided animation.
 */
abstract class AnimatedObjectDrawer(xoff: Int, yoff: Int, ani: Animation) extends ObjectDrawer {
  def hasNext = ani.hasNext
  def nextFrame = ani.nextFrame
  def draw(x: Int, y: Int, inst: Instance, a: DrawAdapter) = ani.draw(x + xoff, y + yoff, a)
  def reset = ani.reset
}

abstract class GuisableObjectDrawer(xoff: Int, yoff: Int, anilib: SingleFrameAnimationLib)
extends ObjectDrawer {
  def hasNext = anilib.hasNext
  def nextFrame = anilib.nextFrame
  def reset = anilib.reset
  def draw(x: Int, y: Int, inst: Instance, a: DrawAdapter) = {
    val guise = inst.prop("Guise").asInteger
    anilib.index = guise
    anilib.draw(x + xoff, y + yoff, a)
  }
}

/**
 * Used as a hack.
 */
class EmptyObjectDrawer extends AnimatedObjectDrawer(0, 0, NullAnimation) {
  def name = null
  def copy = new EmptyObjectDrawer
  def boundingbox = ((-100, -100), (100, 100))
  def effectDrawerFor(e: Event) = None
}

/** Hack for ant :) */
final class Drawers



















