package name.brijest.sages.controller.impl.commands


import name.brijest.sages.model.Execution
import name.brijest.sages.model.ModelView
import name.brijest.sages.model.Model
import name.brijest.sages.model.OpResult
import name.brijest.sages.controller.Command
import name.brijest.sages.model.executions._


object CombinedCommand {
  implicit def combine(ec: ExecutionCombinator) = new CombinedCommand(ec, _: Int)
}

/** A command made out of combinators */
class CombinedCommand(val cc: ExecutionCombinator, val senderIndex: Int) extends Command {
  def this() = this(new NullCombinator, Int.MinValue)
  
  def getExecution = (a: Model#Adapter) => {
    if (!cc.check(a.modelview)) (false, "Conditions for execution are inappropriate.")
    cc.getExecution(a)
    OpResult.successful
  }
  def toXML = {
    <combinedCommand sender={ senderIndex.toString } cls={ cc.getClass.getName }>
      {cc.toXML}
    </combinedCommand>
  }
  def fromXML(node: xml.Node) = {
    val si = (node \ "@sender")(0).text.toInt
    val ccsample = Class.forName((node \ "@cls")(0).text).newInstance.asInstanceOf[ExecutionCombinator]
    val c = ccsample.fromXML(node.child(1))
    new CombinedCommand(c, si)
  }
}























