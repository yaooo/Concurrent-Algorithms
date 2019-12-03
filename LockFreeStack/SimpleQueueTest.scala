/* A linearizability tester for total queues (i.e. queues that do not 
 * block, and such that dequeues on the empty queue return None). */

import scala.collection.immutable.Queue
import ox.cads.util.Profiler
import ox.cads.testing._

/** Object to perform linearizability testing on a queue. */
object SimpleQueueTest{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 10 // Maximum value placed in the queue
  var enqueueProb = 0.4 // probability of doing an enqueue
  var queueType = "unbounded" // which queue type are we using?


  type SeqQueue = Queue[Int]
  type ConcQueue = ox.cads.collection.Queue[Int]
  type LAL = LinkedArrayList[Int]
  type LFLAL = LockFreeLinkedArrayList[Int]

  def seqEnqueue(x: Int)(q: Queue[Int]) : (Unit, Queue[Int]) = 
    ((), q.enqueue(x))
  def seqDequeue(q: Queue[Int]) : (Option[Int], Queue[Int]) =   
    if(q.isEmpty) (None,q) 
    else{ val (r,q1) = q.dequeue; (Some(r), q1) }

  /** A worker for testers based on an immutable sequential datatype. */
  def worker(me: Int, log: GenericThreadLog[SeqQueue, ConcQueue]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters)
      if(random.nextFloat <= enqueueProb){
      	val x = random.nextInt(MaxVal)
      	log.log(_.enqueue(x), "enqueue("+x+")", seqEnqueue(x))
      }
      else log.log(_.dequeue, "dequeue", seqDequeue)
  }


  // List of queues
  val queues0 = List("LinkedArrayList", "LockFreeLinkedArrayList") 
  val queues = queues0.map("--" + _)

  val usage = 
    """|scala -J-Xmx10g QueueTest
       | [""" + queues.mkString(" | ") + "]\n" +
    """| [--iters n] [--reps n] [--enqueueProb p] [-p n]"""

  def main(args: Array[String]) = {
    // parse arguments
    var verbose = false; var i = 0
    var reps = 1250  // Number of repetitions
    var p = 4      // Number of workers 
    while(i < args.length){
      if(queues.contains(args(i))){ queueType = args(i).drop(2); i += 1 }
      else if(args(i) == "-p"){ p = args(i+1).toInt; i += 2 }
      else if(args(i) == "--iters"){ iters = args(i+1).toInt; i += 2 }
      else if(args(i) == "--reps"){ reps = args(i+1).toInt; i += 2 }
      else if(args(i) == "--enqueueProb"){ 
        enqueueProb = args(i+1).toDouble; i += 2 
      }
      else sys.error("Usage:\n"+usage.stripMargin)
    }

    // Now run the tests
    val t0 = java.lang.System.nanoTime
    var r = 0
    var result = 1
    while(r < reps && result > 0){
      // The sequential and concurrent queue
      val seqQueue = Queue[Int]()
      val concQueue = new LAL
      // Create and run the tester object
      val tester = LinearizabilityTester.JITGraph[SeqQueue, ConcQueue](
        seqQueue, concQueue, p, worker _, iters)
      result = tester()
      r += 1
      if(r%100 == 0) print(".")
    } // end of for loop
    val t1 = java.lang.System.nanoTime
    println("\nSimple LinkedArrayList ====> time taken: "+(t1-t0)/1000000+"ms")
  

    // // Now run the tests
    val tt0 = java.lang.System.nanoTime
    var rr = 0
    var rresult = 1
    while(rr < reps && rresult > 0){
      // The sequential and concurrent queue
      val seqQueue = Queue[Int]()
      val concQueue = new LFLAL
      // Create and run the tester object
      val tester = LinearizabilityTester.JITGraph[SeqQueue, ConcQueue](
        seqQueue, concQueue, p, worker _, iters)
      rresult = tester()
      rr += 1
      if(rr%100 == 0) print(".")
    } // end of for loop
    val tt1 = java.lang.System.nanoTime
    println("\nLockFreeLinkedArrayList ====> time taken: "+(tt1-tt0)/1000000+"ms")
 
  }
  
}
	