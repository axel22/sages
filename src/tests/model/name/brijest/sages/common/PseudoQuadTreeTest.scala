package name.brijest.sages.common

import org.scalatest.junit.JUnit3Suite
import name.brijest.sages.common.collections.PseudoQuadTree

class PseudoQuadTreeTest extends JUnit3Suite {
  
  def testInsertion1 {
    val quad = new PseudoQuadTree[List[Int]]
    val numinserts = 10
    for (i <- 1 to numinserts) {
      quad((i * 2, 10 - i)) = i :: Nil
    }
    println("Total elements: " + quad.size)
    assert(quad.size == numinserts)
    for (i <- 1 to numinserts) assert(quad.contains((i * 2, 10 - i)))
  }
  
  def testInsertion2 {
    val quad = new PseudoQuadTree[List[Int]]
    val numinserts = 1500
    for (i <- 1 to numinserts) {
      quad(((i + 100) % 500 * 2 + 5000, 150 - i % 300)) = i :: Nil
    }
    assert(quad.size == numinserts)
    for (i <- 1 to numinserts) assert(quad.contains(((i + 100) % 500 * 2 + 5000, 150 - i % 300)))
  }
  
  def testInsertion3 {
    println("Starting... insertion 3")
    val quad = new PseudoQuadTree[Int]
    quad((-1, 1)) = 1
    println(quad)
    quad((1, -1)) = 2
    println(quad)
    quad((1, 1)) = 3
    println(quad)
    quad((3, -1)) = 4
    println(quad)
    quad((5, -3)) = 5
    println(quad)
    quad((5, -1)) = 6
    println(quad)
    println("NW " + quad.northwest(6, -5))
  }
  
  def testRandomInsertion {
    println("Starting random insertion...")
    val quad = new PseudoQuadTree[Int]
    val numinserts = 100
    val rand = new util.Random
    for (i <- 1 to numinserts) quad((rand.nextInt % 300 - 150, rand.nextInt % 300 - 150)) = i
    println("Total elements after random insertion: " + quad.size)
  }
  
  def testDeletion1 {
	println("Starting deletion 1...")
    val quad = new PseudoQuadTree[Int]
    val numinserts = 22500
    val rand = new util.Random(3212)
    for (i <- 1 to numinserts) quad((rand.nextInt % 1501 + 100, rand.nextInt % 3237 - 10)) = i
    println("Total rebalances for insertion: " + quad.rebalanceCounter)
    println("Tree depth: " + quad.depth)
    rand.setSeed(3212)
    for (i <- 1 to numinserts) quad -= ((rand.nextInt % 1501 + 100.0, rand.nextInt % 3237 - 10.0))
    println("Total rebalances for ins. & del.: " + quad.rebalanceCounter)
    assert(quad.size == 0)
  }
  
  def testDeletion2 {
    val quad = new PseudoQuadTree[Int]
    val numinserts = 100
    for (i <- 1 to numinserts) quad((i % 50 + 10, i % 23 - 10)) = i
    for (i <- 1 to numinserts) {
      quad -= ((i % 50 + 10.0, i % 23 - 10.0))
      assert(quad.size == numinserts - i)
    }
  }
  
  def testDeletion3 {
    println("Starting... deletion 3")
    val quad = new PseudoQuadTree[Int]
    quad((-1, 1)) = 1
    quad((1, -1)) = 2
    quad((1, 1)) = 3
    quad((3, -1)) = 4
    quad((5, -3)) = 5
    quad((5, -1)) = 6
    println(quad)
    quad -= ((-1, 1))
    println(quad)
    quad -= ((1, -1))
    println(quad)
    quad -= ((1, 1))
    println(quad)
    quad -= ((3, -1))
    println(quad)
    quad -= ((5, -1))
    println(quad)
    quad -= ((5, -3))
    println(quad)
  }
  
  def testRandomDeletion {
    println("Starting random deletion...")
    val quad = new PseudoQuadTree[Int]
    val numtests = 5000
    val rand = new util.Random
    var sz = 0.0
    for (i <- 1 to numtests) {
      quad((rand.nextInt % 50, rand.nextInt % 50)) = i
      quad -= ((rand.nextInt % 50, rand.nextInt % 50))
      sz += quad.size
    }
    println("Average size: " + (sz / numtests))
  }
  
}























