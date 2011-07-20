package name.brijest.sages.controller

import org.scalatest.junit.JUnit3Suite
import name.brijest.sages.model.executions.ExecutionCombinatorFactory._

object CommandCombinatorsTest extends JUnit3Suite {

  def testCombinators() {
    val command = (
      endTurn&
      dig(1,1)&
      removePlayer(5)&
      none
    )
    println(command.toXML)
  }
  
  def main(args: Array[String]) {
    testCombinators
  }
  
}

























