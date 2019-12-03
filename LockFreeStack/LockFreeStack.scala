import ox.cads.collection.TotalStack
import ox.cads.util.ThreadID
import ox.cads.atomic._

// Lock free stack with a garbage collector
// Create pool with size p (a array of Node), where each thread can access pool[ThreadID % p]

// Create a garbage colllector: 
    // When pop: insert the popped node to the collector
    // When push: recover a node from the garbage collector or create a new nodes(if empty)

// A solution to the ABA problem is to pair the reference with a counter, also known as "stamp"
// Each time a thread changes a reference, it also increments the stamp.
// This avoids the ABA problem because the stamp will changed (same case applies to the recycled nodes),
// and so the CAS will fail.


/** A lock-free stack.  Based on Herlihy & Shavit, Section 11.2 */
class LockFreeStack[T](p: Int) extends TotalStack[T]{
  // We'll build linked lists from the following type of Nodes
  private class Node(val v: T){
    var nextStamp = new AtomicPair[Node, Int](null, 0)
    var value: T = v

    def next = nextStamp.getFirst
    def stamp = nextStamp.getSecond
  }

  // Atomic reference to the top of the stack
  private val top = new AtomicPair[Node, Int](null, 0)

  // initialize the garbage collector
  private val pool = new Array[Node](p)

  private def pause = ox.cads.util.NanoSpin(500) 


  // if the slot is not empty, return an unused node, else create a new one
  private def recycleOrCreate(value: T) = {
  	val id = ThreadID.get % p
  	if(pool(id) == null){
  		new Node(value)
  	}else{
  		val n = pool(id)
  		pool(id) = n.next
  		n.value = value
  		n
  	}
  }

  /** Push value onto the stack */
  def push(value: T) = {
    // get the recycled node or create a new one
    val nextTop = recycleOrCreate(value)
    var done = false
    do{
      val (oldTop, tStamp) = top.get
      
      nextTop.nextStamp.set(oldTop, tStamp + 1)
      // try to add node to the stack
      done = top.compareAndSet((oldTop, tStamp), (nextTop, tStamp + 1))
      if(!done) pause // back off
    } while(!done)
  }

  /** Pop a value from the stack.  Return None if the stack is empty.  */ 
  def pop : Option[T] = {
    var result : Option[T] = None; var done = false
    do {
      val (oldTop, tStamp) = top.get
      if (oldTop == null) done = true // empty stack; return None
      else {
        val newTop = oldTop.next

        // try to remove oldTop from list
        if(top.compareAndSet((oldTop, tStamp), (newTop, tStamp+1))) {
          result = Some(oldTop.value)
          done = true
          
          // Recycle oldTop
          val id = ThreadID.get % p
          val firstFreeNode = pool(id)
          oldTop.nextStamp.set(firstFreeNode, tStamp+1)
          pool(id) = oldTop
        }
        else pause
      }
    } while(!done)
    result
  }
}