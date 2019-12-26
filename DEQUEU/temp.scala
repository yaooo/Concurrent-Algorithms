import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock
import ox.cads.locks.Lock
import java.util.concurrent.atomic.AtomicInteger

class ListDeque[T: ClassTag](p: Int){
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

  	private var head : Node = null
  	private var tail : Node = null
	
	private var count = 0
	private val CAPACITY = 1000
	
	private val lock = new ReentrantLock()
	private val notFull = lock.newCondition // used to wait until not full
  	private val notEmpty = lock.newCondition // used to wait until not empty
  	// private val size = new AtomicInteger(0) // current # elements


	def addFirst(x: T): Unit = {		
		var node = new Node(x)
		lock.lock
		try{
			val f = head
			node.next = f
			head = node
			if(tail == null)
				tail = node
			else
				f.prev = node
			count+= 1
			notEmpty.signal
		}finally{
			lock.unlock
		}

	}

	// the same as the regular enque
	def addLast(x: T): Unit = {
		var node = new Node(x)
		lock.lock
		try{
			val l = tail
			node.prev = l
			tail = node
			if(head == null)
				head = node
			else
				l.next = node
			count+=1
			notEmpty.signal
		}finally{
			lock.unlock
		}
	}


	// the same as the regular deque
	def removeFirst:Option[T] = {
		lock.lock
		try{
			var f = head
			if(f == null) return None;
			
			var n = f.next
			head = n
			
			if(n == null) tail = null;
			else n.prev = null
			
			count -= 1
			notFull.signal
			return Some(f.data)
		}finally{
			lock.unlock
		}
	}

	def removeLast: Option[T] = {
		lock.lock
		try {
			var l = tail
			if(l == null) return None;
			
			var p = l.prev
			tail = p
			if(p == null) head = null;
			else p.next = null;

			count -= 1
			notFull.signal
			Some(l.data)
		}finally{
			lock.unlock
		}
	}
}
