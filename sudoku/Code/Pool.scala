/** A trait representing a generic pool. */
trait Pool[T >: Null]{
  /** put x into the pool */
  def add(x: T)

  /** Get a value from the pool */
  def get : T
}