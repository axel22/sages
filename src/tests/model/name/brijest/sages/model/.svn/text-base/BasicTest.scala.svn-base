package name.brijest.sages.model


import junit.framework.TestCase
import junit.framework.Assert.assertEquals



class BasicTestCase extends TestCase {
  
  def testQuantity {
    val somevalue = new TestValue(5)
    val quant = new MutableQuantity("A quantity", "Some test quantity > to see xml.", somevalue)
    assertEquals(quant, MutableQuantity.fromXML(quant.toXML))
  }
  
  def testAction {
    val someoperation = new TestOperation
    val action = new Action("TAction", "An action used in this test.", someoperation)
    //println(action.toXML)
    assertEquals(action, Action.fromXML(action.toXML))
  }
  
  def testObservable {
    val m = new Observable{}
    val o = new Observer {
      def onEvent(e: Event) {}
      def events = List(classOf[TestEvent])
    }
    assert(!m.containsObserver(o))
    m.addObserver(o)
    assert(m.containsObserver(o))
    m.removeObserver(o)
    assert(!m.containsObserver(o))
  }
  
}

case class TestEvent() extends Event

class TestOperation extends Operation {
  def apply(model: ModelView, a: Model#Adapter, owner: Depot, ind: Option[Int], params: Array[Value]) = null
  def parameters = Array[Class[_]]()
  def constraints = Array[Constraint]()
  override def equals(other: Any) = other match {
    case v: TestOperation => true
    case _ => false
  }
  override def hashCode = classOf[TestOperation].hashCode;
}

class TestValue(a: Int) extends Value {
  val aval = a
  
  def this() = this(0)
  
  def toXML = <a>{a}</a>
  def fromXML(node: scala.xml.Node) = {
	val ta = Integer.parseInt(node.text)
	new TestValue(ta)
  }
  override def equals(other: Any) = other match {
    case that: TestValue => that.aval == this.aval
    case _ => false
  }
  override def toString = "TestValue: " + aval
}






















