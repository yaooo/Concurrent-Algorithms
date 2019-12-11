import scala.reflect.ClassTag
trait Deque[T]{

	def addFirst(x: T): Unit

	def addLast(x: T): Unit

	def removeFirst:Option[T]


	def removeLast: Option[T]
}
