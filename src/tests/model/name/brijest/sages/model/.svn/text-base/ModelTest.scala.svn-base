package name.brijest.sages.model

import org.scalatest.junit.JUnit3Suite

import name.brijest.sages.model.quantities._



class ModelTest extends JUnit3Suite {
  
  def testModel {
    val cats = new CategoryFactory
    val opal = new ObjectPalette
    val tpalette = new TerrainPalette
    tpalette.registerTerrain(new Terrain {
      def neighbours = null
      def subindices = Set[Int](2)
      def acquireCategory = null
      def categories = null
      def movementCost = 1
      def index = 1
      def walkable = true
      def name = "Mock"
      def colour = null
    })
    val model = new Model(10, tpalette, opal, cats, "")
//    println(model.toXML)
//    println(Model.fromXML(model.toXML, tpalette, opal, cats).toXML)
    assert(model == Model.fromXML(model.toXML, tpalette, opal, cats))
  }

  def testTerrainMap {
    val palette = new TerrainPalette
    palette.registerTerrain(new Terrain {
      def neighbours = null
      def subindices = Set[Int](2)
      def acquireCategory = null
      def categories = null
      def movementCost = 1
      def index = 1
      def name = "Mock"
      def walkable = true
      def colour = null
    })
    val map = new TerrainMap(10, palette)
    
    //println(map.toXML)
    //println(TerrainMap.fromXML(map.toXML, palette).toXML)
    assert(map == TerrainMap.fromXML(map.toXML, palette))
  }
  
  def testValues {
    val comp = new Composition(Map("a" -> new Text("Ok"), "f" -> new IntNum(19)))
    assert(comp == comp.fromXML(comp.toXML))
    val t = new Turn(10)
    assert(t.week == 2)
    assert(t.day == 4)
    val turn = new Turn(349)
    assert(turn.year == 2)
    assert(turn.month == 1)
    assert(turn.week == 2)
    assert(turn.day == 7)
  }
  
}












