import scala.reflect.ClassTag
import java.util.concurrent.locks.ReentrantLock
import ox.cads.locks.Lock
import java.util.concurrent.atomic.AtomicInteger


// save a copy of a partially working code
class ListDeque[T: ClassTag] extends Deque[T]{
	class Node(val value: T){
    	@volatile var prev: Node = null
    	@volatile var next: Node = null
    	val data = value
    	@volatile var mark: Boolean = false

    	val l: Lock = Lock.FairLock	
    	def lock = {l.lock}
    	def unlock = {l.unlock}	
  	}

	// Head and tail of the list

  	private val tail = new Node(null.asInstanceOf[T])
  	private val head = new Node(null.asInstanceOf[T])
  	head.next = tail
  	tail.prev = head
	
	private var count = 0
	private var threashold = 50
	// private val CAPACITY = 1000
	
	private val lock = new ReentrantLock()
	private val notFull = lock.newCondition // used to wait until not full
  	private val notEmpty = lock.newCondition // used to wait until not empty
  	// private val size = new AtomicInteger(0) // current # elements



  	private def find : (Node, Node) = {
	    // optimistically search for item
	    var pred = head; var curr = head.next
	    while(curr.key < key || curr.key == key & curr.item != item){
	      pred = curr; curr = curr.next
	    }
	    // lock then validate
	    pred.lock; curr.lock
	    if(validate(pred, curr)) return (pred, curr)
	    else{ pred.unlock; curr.unlock; find(item, key) } // try again
	} 


	def addFirst(x: T): Unit = {		
		var node = new Node(x)


		if(count < threashold){
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
		}else{
			head.lock
			val f = head

			node.next = head
			count+=1
			head = node
			f.unlock
		}
	}

	// the same as the regular enque
	def addLast(x: T): Unit = {
		var node = new Node(x)

		if(count < threashold){
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
		}else{
			tail.lock

			val l = tail
			node.prev = tail
			tail = node
			count+=1
			l.unlock

		}
	}


	def removeFirst:Option[T] = {

		if(count < threashold){

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
		}else{
			head.lock
			
			var f = head

			if(f == null) return None

			var n = f.next
			head = n

			if(n == null) tail = null
			else n.prev = null

			count -= 1
			f.unlock
			return Some(f.data)

		}
	}

	def removeLast: Option[T] = {
		if(count < threashold){

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
				return Some(l.data)
			}finally{
				lock.unlock
			}
		}else{
			tail.lock
			var l = tail
			if(l == null) return None
			var p = l.prev
			tail = p
			if(p == null) head = null
			else p.next = null

			count -= 1
			l.unlock
			return Some(l.data)
		}
	}
}
