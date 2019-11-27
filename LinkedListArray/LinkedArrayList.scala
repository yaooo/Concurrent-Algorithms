import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock


// abstraction: have a list of nodes (a node holds two index: first and last, as well an array that holds the data); 
// the tail is always reachable from the head

// for enqueue: 
// if it is not full, update tail.data, and update tail.last
// Otherwise, create a new node. 
    // If it is the first time updating, update both head and tail (b/c they are null initially)
    // If it is not the first time, add the new node to tail.next, then advance the tail

// for dequeue:
// if head is null(first time), return None
// if head and taill point at the same node and (for this node, there is no data in between the its local last and first), return None
// if there in not data to be dequeued in the current node, advance to the next node for head, and try dequeue

// datatype invariant: comments

class LinkedArrayList [T: ClassTag] extends ox.cads.collection.Queue[T] {

  private class Node{
    // the index of the head and the tail for each node
    var first = 0
    var last = 0

    val data = new Array[T](size)
    var next : Node = null
  }

  private var head : Node = null
  private var tail : Node = null
  private val size = 10
  private val enqueue_lock = new ReentrantLock()
  private val dequeue_lock = new ReentrantLock()

  
  def enqueue(x: T) = {
    enqueue_lock.lock
    try{
      // if it is not full (linearization point)
      if(tail != null && tail.last < size){
        tail.data(tail.last) = x
        tail.last += 1
      }else{
        val node = new Node
        node.data(0) = x
        node.last += 1

        // first time enqueue
        if(head == null){
          head = node
          tail = head
        }else{
          tail.next = node
          tail = tail.next
        }
      }

    } finally{
      enqueue_lock.unlock
    }
  }

  def dequeue : Option[T] = {
    dequeue_lock.lock
    try{
      if(head == null)
        return None
      
      // point at the same node and the same slot(linearization point for an empty queue)
      if(head == tail && head.first >= head.last)
        return None

      // the current block is full 
      if(head.first >= size){
        head = head.next
        // (linearization point if dequeued all elements from the head already but the next node does not exist)
        if(head == null) return None
      }

      // linearization point for dequeuing an element when the queue is not empty
      val res = head.data(head.first)
      head.first += 1
      Some(res)

    }finally{
      dequeue_lock.unlock
    }
  }
}