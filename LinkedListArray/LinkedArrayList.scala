import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock

class LinkedArrayList [T: ClassTag] extends ox.cads.collection.Queue[T] {
  

  private class Node{
    // the index of the head and the tail
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
      // if it is not full
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
      
      if(head == tail && head.first >= head.last)
        return None

      // the block is full
      if(head.first >= size){
        head = head.next
        if(head == null) return None
      }

      val res = head.data(head.first)
      head.first += 1
      Some(res)

    }finally{
      dequeue_lock.unlock
    }
  }
}