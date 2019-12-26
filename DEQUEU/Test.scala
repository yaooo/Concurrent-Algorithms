import ox.cads.util.Profiler
import ox.cads.testing._
import scala.collection.immutable.List


/** Object to perform linearizability testing on a queue. */
object Test{
  var iters = 200  // Number of iterations by each worker
  val MaxVal = 20 // Maximum value placed in the queue
  var queueType = "unbounded" // which queue type are we using?


  var enqueueFirstThreshold = 0
  var enqueueLastThreshold = 0.2
  var dequeueFirstThreshold = 0.4
  var dequeueLastThreshold = 0.7

  type LD =  ListDeque[Int]
  type LFDequeu = LockFreeDeque[Int]  
  type SeqType = scala.collection.immutable.List[Int]

  /** sequential behaviour we expect */
  def seqAddFirst(x: Int)(s: SeqType) : (Unit, SeqType) =
    ((), x :: s)
  
  def seqAddLast(x: Int)(s: SeqType) : (Unit, SeqType) =
    ((), s :+ x)

  def seqRemoveFirst(s: SeqType) : (Option[Int], SeqType) =
    s match {
      case Nil => (None, s)
      case x :: newS => (Some(x), newS)
    }

  def seqRemoveLast(s: SeqType) : (Option[Int], SeqType) =
    s match {
      case Nil => (None, s)
      case newS :+ x => (Some(x), newS)
    }

  /** A worker for testers based on an immutable sequential datatype. */
  def worker(me: Int, log: GenericThreadLog[SeqType, LD]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters)
      // val x = random.nextInt(MaxVal)

      random.nextFloat match {
        case x if x > dequeueLastThreshold => log.log(_.removeLast, "removeLast", seqRemoveLast)
        case x if x > dequeueFirstThreshold => log.log(_.removeFirst, "removeFirst", seqRemoveFirst)
        case x if x > enqueueLastThreshold => {
          val x = random.nextInt(MaxVal)
          log.log(_.addLast(x), "addLast("+x+")", seqAddLast(x))
        }
        case _ => {
          val x = random.nextInt(MaxVal)
          log.log(_.addFirst(x), "addFirst("+x+")", seqAddFirst(x))
        }
      }
  }


/** A worker for testers based on an immutable sequential datatype. */
  def worker1(me: Int, log: GenericThreadLog[SeqType, LFDequeu]) = {
    val random = new scala.util.Random(scala.util.Random.nextInt+me*45207)
    for(i <- 0 until iters)
      // val x = random.nextInt(MaxVal)

      random.nextFloat match {
        case x if x > dequeueLastThreshold => log.log(_.removeLast, "removeLast", seqRemoveLast)
        case x if x > dequeueFirstThreshold => log.log(_.removeFirst, "removeFirst", seqRemoveFirst)
        case x if x > enqueueLastThreshold => {
          val x = random.nextInt(MaxVal)
          log.log(_.addLast(x), "addLast("+x+")", seqAddLast(x))
        }
        case _ => {
          val x = random.nextInt(MaxVal)
          log.log(_.addFirst(x), "addFirst("+x+")", seqAddFirst(x))
        }
      }
  }

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
      val seqQueue = List[Int]()
      val concQueue = new LD

      // Create and run the tester object
      val tester = LinearizabilityTester.JITGraph[SeqType, LD](
        seqQueue, concQueue, p, worker _, iters)
      result = tester()
      r += 1
      if(r%100 == 0) print(".")
    } // end of for loop
    val t1 = java.lang.System.nanoTime
    println("\nListDeque ====> Time taken: "+(t1-t0)/1000000+"ms")
  

   // // Now run the tests
    // val tt0 = java.lang.System.nanoTime
    // var rr = 0
    // var rresult = 1
    // while(rr < reps && rresult > 0){
    //   // The sequential and concurrent queue
    //   val seqQueue = List[Int]()
    //   val concQueue = new LFDequeu(p)
    //   // Create and run the tester object
    //   val tester = LinearizabilityTester.JITGraph[SeqType, LFDequeu](
    //     seqQueue, concQueue, p, worker1 _, iters)
    //   rresult = tester()
    //   rr += 1
    //   if(rr%100 == 0) print(".")
    // } // end of for loop
    // val tt1 = java.lang.System.nanoTime
    // println("\nLockFreeDeque ====> time taken: "+(tt1-tt0)/1000000+"ms")
 
  }
  
}
  