import ox.cads.collection.TotalStack
import ox.cads.util.ThreadID
import ox.cads.atomic.AtomicPair

/** A lock-free stack.  Based on Herlihy & Shavit, Section 11.2 */
class LockFreeStack[T](p: Int) extends TotalStack[T]{
  // We'll build linked lists from the following type of Nodes
  private class Node(val v: T){
    var next = new AtomicPair[Node, Int](null, 0)
    var value: T = v
  }

  // Atomic reference to the top of the stack
  private val top = new AtomicPair[Node, Int](null, 0)
  private val pool = new Array[Node](p)

  private def pause = ox.cads.util.NanoSpin(500) 


  // if the slot is not empty, return an unused node, else create a new one
  private def recycleOrCreate(value: T) = {
  	val id = ThreadID.get % p
  	if(pool(id) == null){
  		new Node(value)
  	}else{
  		val n = pool(id)
  		pool(id) = n.next.getFirst
  		n.value = value
  		n
  	}
  }

  /** Push value onto the stack */
  def push(value: T) = {
    // get the recycled node or create a new one
    val node = recycleOrCreate(value)
    var done = false
    do{
      val (oldTop, stamp) = top.get
      val oldNextStamp = node.next.getSecond
      node.next.set(oldTop, oldNextStamp + 1)
      // try to add node to the stack
      done = top.compareAndSet((oldTop, stamp), (node, stamp + 1))
      if(!done) pause // back off
    } while(!done)
  }

  /** Pop a value from the stack.  Return None if the stack is empty.  */ 
  def pop : Option[T] = {
    var result : Option[T] = None; var done = false
    do {
      val (oldTop, stamp) = top.get
      if (oldTop == null) done = true // empty stack; return None
      else {
        val newTop = oldTop.next.getFirst
        // try to remove oldTop from list
        if(top.compareAndSet((oldTop, stamp), (newTop, stamp+1))) {
          result = Some(oldTop.value)
          done = true
          
          // Recycle oldTop
          val id = ThreadID.get % p
          val firstFreeNode = pool(id)
          oldTop.next.set(firstFreeNode, stamp+1)
          pool(id) = oldTop
        }
        else pause
      }
    } while(!done)
    result
  }
}