import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicReferenceArray
import java.util.concurrent.atomic.AtomicReference
import ox.cads.atomic.AtomicPair


// abstraction: have a list of nodes (a node holds the "array" of data and "next")
// have two global atomic pairs for head and tail； the tail is always reachable from the head
// head is the AtomicPair that holds the head node and the dequeue index
// tail is the AtomicPair that holds the tail node and the enqueue index

// The queue is a list Nodes where each node holds an array[size] with type T.
// The head is refering to the front of the list, and its deqIndex is refering to the first available element in the array of the Node.
// The tail is refering to the end of the list, and its enqIndex is refering to the last available element in the array of the Node.

// A node is created before the first enqueue/dequeue so that head/tail refer to the same node in the begining.

// Enqueue:
// get tailNode and the enqueueIndex from the "tail". 
// If the array inside of the tailNode is full, add a new node and advance the "tail"
// If the array inside of the tailNode is not full, add the new data into the tailNode and update the "tail"

// Dequeue:
// get tailNode and the enqueueIndex from the "tail", get headNode and the dequeueIndex from the "head"
// If head and tail point at the same slot of the same node, return None

// Right before dequeuing, we have to chack the following condition:
// If everthing in the current node already dequeued(we are trying to dequeue when the dequeueIndex >= size), then we have to advance the "head", then try dequeue again

// Upadate "head" to finish dequeue.


// datatype invariant: comments

class LockFreeLinkedArrayList [T: ClassTag] extends ox.cads.collection.Queue[T] {

  private class Node {
    val data = new AtomicReferenceArray[T](size)
    var next = new AtomicReference[Node](null)
  }
  private val size = 10

  // head and tail point to the same node
  private var init = new Node
  private var head = new AtomicPair[Node, Int](init, 0)
  private var tail = new AtomicPair[Node, Int](init, 0)

  def enqueue(x: T) = {
    var done = false
    do{
      // get tail and the enqueue index for the tail node
      var (tailNode, enqIndex) = tail.get
      
      if(enqIndex >= size){// if full, add new node
        // MARK
        var node = new Node
        tailNode.next.compareAndSet(null, node)

        // move the tail pointer to its next node
        tail.compareAndSet((tailNode, enqIndex), (tailNode.next.get, 0))
      }else{ 
        // if the block is not fulll yet
        while (!done && enqIndex < size) {

          // add to the last node
          if(tailNode.data.compareAndSet(enqIndex, null.asInstanceOf[T], x))
            done = true

          // linearization point
          // advance the tail pointer
          tail.compareAndSet((tailNode, enqIndex), (tailNode, enqIndex+1))
          var (tailNode1, enqIndex1) = tail.get
          tailNode = tailNode1
          enqIndex = enqIndex1
        }
      }
    }while(!done)
  }

  def dequeue : Option[T] = {
    while(true){
      val (headNode, deqIndex) = head.get
      val (tailNode, enqIndex) = tail.get

      // if head and tail point at the same slot
      if(tailNode == headNode && enqIndex == deqIndex)
        return None

      if(deqIndex >= size){ // go to the next node, if everthing in the current node already dequeued
        head.compareAndSet((headNode, deqIndex), (headNode.next.get, 0))
      }else{
        // linearization point
        if(head.compareAndSet((headNode, deqIndex), (headNode, deqIndex+1)))
          return Some(headNode.data.get(deqIndex))
      }
    }

    return None
  }
}