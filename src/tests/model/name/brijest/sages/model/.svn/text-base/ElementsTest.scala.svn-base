package name.brijest.sages.model


import org.scalatest.junit.JUnit3Suite


class ElementsSuite extends JUnit3Suite {

  def testCategory(c: Category) {
    c.getClass.newInstance
    assert(c.name != null)
    assert(c.description != null)
    assert(c.acquireQuestion != null)
  }
  
  def testTerrain(t: Terrain) {
    t.getClass.newInstance
    assert(t.name != null)
    assert(t.movementCost > 0)
    assert(t.categories != null)
    assert(t.categories.size != 0)
    assert(t.subindices != null)
    assert(t.subindices.contains(0))
    for (i <- 1 until 100) t.acquireCategory ensuring t.categories.contains(_: Category)
  }
  
  def testQuest(q: Quest) {
    q.getClass.newInstance
    assert(q.name != null)
    assert(q.description != null)
    assert(q == q.fromXML(q.toXML))
  }
  
  def testState(s: State) {
    assert(s.name != null)
    assert(s.description != null)
    assert(s.activePlayers != null)
    assert(s == s.fromXML(s.toXML))
  }
  
  def testObject(o: Object) {
    o.getClass.newInstance
    assert(o.objectname != null)
    assert(o.occupied.contains(0, 0))
    assert(o.interactive != null)
    assert(o.interactive forall {
      o.occupied.contains(_)
    })
  }
  
  def testValue(v: Value) {
    assert(v == v.fromXML(v.toXML))
  }
  
  def testElements {
    
  }
  
}
