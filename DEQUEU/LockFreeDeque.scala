import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicReference

class LockFreeDeque[T: ClassTag](p: Int) {
  private class Node(val value: T) {
    val prev = new AtomicReference[Node](null)
    val next = new AtomicReference[Node](null)
    val data = value
  }

  private val firstNode = new Node(null.asInstanceOf[T]) // initial dummy header

  // Atomic reference to tohe head of the stack
  private val head = new AtomicReference[Node](firstNode)
  private val tail = new AtomicReference[Node](firstNode)

  // Back off; could use binary back off here
  private def pause = ox.cads.util.NanoSpin(500)

  /** Push value onto the stack */
  def addFirst(value: T) = {
    val node = new Node(value); var done = false
    while (!done) {
      val myTop = head.get
      val next = myTop.prev.get

      if (myTop == head.get)
        if (next == null) {
          if (myTop.prev.compareAndSet(next, node)) {
            node.next.compareAndSet(null, myTop)
            head.compareAndSet(myTop, node)
            done = true
          }
        } else {
          head.compareAndSet(myTop, next) // and retry
        }
    }
  }

  //todo should work now
  def addLast(value: T) = {
    val node = new Node(value); var done = false
    while (!done) {
      val myTail = tail.get
      val next = myTail.next.get

      if (myTail == tail.get)
        if (next == null) {
          if (myTail.next.compareAndSet(next, node)) {
            node.prev.compareAndSet(null, myTail)
            tail.compareAndSet(myTail, node)
            done = true
          }
        } else {
          tail.compareAndSet(myTail, next) // and retry
        }
    }
  }

  def isEmpty: Boolean = head.get == tail.get

  //todo look good as well
  def removeFirst: Option[T] = {
    var done = false; var result: Option[T] = None
    while (!done) {
      var myHead = head.get; val myTail = tail.get; val next = myHead.next.get
      if (myHead == head.get)
        if (myHead == myTail) { // empty queue
          if (next == null) { // empty queue, return null
            result = None; done = true
          } else // new item partially enqueued
            tail.compareAndSet(myTail, next) // try to advance tail
        } else { // non-empty queue
          result = Some(next.data) // provisional result
          if (head.compareAndSet(myHead, next)){
          	myHead = null
          	done = true
          }
          // else result already taken, re-try
        }
      // else retry
    } // end of while loop
    result
  }

  /** Pop a value from the stack.  Return None if the stack is empty.  */
  def removeLast: Option[T] = {
    var done = false; var result: Option[T] = None
    while (!done) {
      var myHead = head.get; val myTail = tail.get; val next = myTail.prev.get
      if (myTail == tail.get)
        if (myHead == myTail) { // empty queue
          if (next == null) { // empty queue, return null
            result = None; done = true
          } else // new item partially enqueued
            head.compareAndSet(myHead, next) // try to advance tail
        } else { // non-empty queue
          result = Some(next.data) // provisional result
          if (head.compareAndSet(myHead, next)){
          	myHead = null
          	done = true
          }
          // else result already taken, re-try
        }
      // else retry
    } // end of while loop
    result
  }
}
