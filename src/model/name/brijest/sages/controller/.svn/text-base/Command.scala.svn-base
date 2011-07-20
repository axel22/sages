package name.brijest.sages.controller


import name.brijest.sages.model.Execution


/**
 * Describes the interface to objects carrying information about operations on the model.
 * Each Command object must provide an Execution object. It is also able to persist itself
 * to XML and vice versa.<br/>
 * Each Command object must have an empty ctor, even if it means creating invalid objects.<br/>
 * A Command object is immutable.
 */
trait Command {
  def getExecution: Execution
  def senderIndex: Int
  def toXML: xml.Node
  def fromXML(node: xml.Node): Command
}
