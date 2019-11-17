// Lock-based LinkedArrayList
import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.atomic.{AtomicInteger, AtomicReference, AtomicReferenceArray}
import ox.cads.atomic.AtomicPair

class LockFreeLinkedArrayList [T: ClassTag] extends ox.cads.collection.Queue[T] {

  private class Node {
    val data = new AtomicReferenceArray[T](size)
    var next = new AtomicReference[Node](null)
  }
  private val size = 10

  private var init = new Node
  private var head = new AtomicPair[Node, Int](init, 0)
  private var tail = new AtomicPair[Node, Int](init, 0)

  // TODO: not working
  def enqueue(x: T) = {
    var done = false
    do{
      var (tailNode, enqIndex) = tail.get
      

      if(enqIndex >= size){// if full, add new node

        // MARK
        var node = new Node
        tailNode.next.compareAndSet(null, node)

        // move the tail pointer to its next node
        tail.compareAndSet((tailNode, enqIndex), (tailNode.next.get, 0))
      }else{ // not fulll yet
        while (!done && enqIndex < size) {
          if(tailNode.data.compareAndSet(enqIndex, null.asInstanceOf[T], x))
            done = true
          tail.compareAndSet((tailNode, enqIndex), (tailNode, enqIndex+1))
          var (tailNode1, enqIndex1) = tail.get
          tailNode = tailNode1
          enqIndex = enqIndex1
        }
      }

    }while(!done)
  }

  // TODO: not working
  def dequeue : Option[T] = {
    while(true){
      val (headNode, deqIndex) = head.get
      val (tailNode, enqIndex) = tail.get

      if(tailNode == headNode && enqIndex == deqIndex)
        return None

      if(deqIndex == size){
        head.compareAndSet((headNode, deqIndex), (headNode.next.get, 0))
      }else{
        if(head.compareAndSet((headNode, deqIndex), (headNode, deqIndex+1)))
          return Some(headNode.data.get(deqIndex))
      }
    }

    None
  }
}