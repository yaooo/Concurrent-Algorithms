import scala.collection.immutable.Stack
import ox.cads.util.Profiler
import ox.cads.testing._

/** Object to perform linearizability testing on a queue. */
object Test{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the queue
  var enqueueProb = 0.3 // probability of doing an enqueue
  var queueType = "unbounded" // which queue type are we using?

  type C = ox.cads.collection.TotalStack[Int]
  type LFS =  LockFreeStack[Int]
  type S = Stack[Int]
  

  type SeqType = scala.collection.immutable.Stack[Int]

  def seqPush(x: Int)(stack: S) : (Unit, S) = ((), stack.push(x))
  def seqPop(stack: S) : (Option[Int], S) = {
    if (stack.isEmpty) (None, stack)
    else {val (r, stack1) = stack.pop2; (Some(r), stack1)}
  }


  /** A worker for testers based on an immutable sequential datatype. */
  def worker(me: Int, log: GenericThreadLog[S, LFS]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters)
      if(random.nextFloat <= enqueueProb){
  val x = random.nextInt(MaxVal)
  log.log(_.push(x), "enqueue("+x+")", seqPush(x))
      }
      else log.log(_.pop , "dequeue", seqPop)
  }

  // List of queues
  val queues0 = List("lockFree", "recycle", "unbounded") 
  val queues = queues0.map("--" + _)

  def main(args: Array[String]) = {
    // parse arguments
    var verbose = false; var i = 0
    var reps = 1250  // Number of repetitions
    var p = 4      // Number of workers 

    // Now run the tests
    val t0 = java.lang.System.nanoTime
    var r = 0
    var result = 1
    while(r < reps && result > 0){
      // The sequential and concurrent queue
      val seqQueue = Stack[Int]()
      val concQueue = new LFS(p)

      // Create and run the tester object
      val tester = LinearizabilityTester.JITGraph[SeqType, LFS](
        seqQueue, concQueue, p, worker _, iters)
      result = tester()
      r += 1
      if(r%100 == 0) print(".")
    } // end of for loop
    val t1 = java.lang.System.nanoTime
    println("\nTime taken: "+(t1-t0)/1000000+"ms")
  }
  
}
  