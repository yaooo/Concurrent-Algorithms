import java.util.concurrent.atomic.AtomicReference

/** A lock-free stack.  Based on Herlihy & Shavit, Section 11.2 */
class LockFreeStack[T >: Null] extends Stack[T]{
  // We'll build linked lists from the following type of Nodes
  private class Node(val value: T){
    var next: Node = null
  }

  // Atomic reference to the top of the stack
  private val top = new AtomicReference[Node](null)
  
  // Back off; could use binary back off here
  private def pause = Thread.sleep(1)

  /** Push value onto the stack */
  def push(value: T) = {
    val node = new Node(value)
    var done = false
    do{
      val oldTop = top.get
      node.next = oldTop
      // try to add node to the stack
      done = top.compareAndSet(oldTop, node) 
      if(!done) pause // back off
    } while(!done)
  }

  /** Pop a value from the stack.  Return null if the stack is empty.  */ 
  def pop : T = {
    var result : T = null; var done = false
    do{
      val oldTop = top.get
      if(oldTop == null) done = true // empty stack; return null
      else{
	val newTop = oldTop.next
	// try to remove oldTop from list
	if(top.compareAndSet(oldTop, newTop)){
	  result = oldTop.value; done = true
	}
	else pause
      }
    } while(!done)
    result
  }
}