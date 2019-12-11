import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock
import ox.cads.locks.Lock
import java.util.concurrent.atomic.AtomicInteger

class ListDeque[T] extends Deque[T]{
	class Node(val value: T){
    	@volatile var prev: Node = null
    	@volatile var next: Node = null
    	val data = value
    	// @volatile var mark: Boolean = false
    	// val l: Lock = Lock.FairLock	
    	// def lock = {l.lock}
    	// def unlock = {l.unlock}	
  	}

	// Head and tail of the list
	private var head = new Node(null.asInstanceOf[T]) // dummy header
	private var tail = head // last Node in list
	
	private var count = 0
	private val CAPACITY = 1000
	
	private val lock = new ReentrantLock()
	private val notFull = lock.newCondition // used to wait until not full
  	private val notEmpty = lock.newCondition // used to wait until not empty
  	private val size = new AtomicInteger(0) // current # elements


	def addFirst(x: T): Unit = {
		count += 1
		
		val f = head
		val e = new Node(x)
		e.next = head
		head = e

		if(tail == null)
			tail = e
		else
			f.prev = e
		notEmpty.signal
	}

	// the same as the regular enque
	def addLast(x: T): Unit = {
		count += 1

		val l = tail
		val e = new Node(x)
		tail.next = e
		tail = e
		if(head == null)
			head = e
		else 
			l.next = e
		notEmpty.signal
	}

	// the same as the regular deque
	def removeFirst:Option[T] = {
		var f = head
		if(f == null) return None
		
		var n = f.next
		head = n

		if(n == null)
			tail = null
		else 
			n.prev = null
		count -= 1
		notFull.signal
		Some(f.data)
	}


	def removeLast: Option[T] = {
		var l = tail
		if(l == null)
			return None
		var p = l.prev
		tail = p
		if(p == null)
			head = null
		else
			p.next = null

		count -= 1
		notFull.signal
		Some(l.data)
	}
}
