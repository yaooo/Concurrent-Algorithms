/** Sudoku solver.
  * Basic usage:
  * # scala Sudoku filename: solves the puzzle in filename
  * # scala Sudoku --all: solves all the puzzles in this directory
  * # scala Sudoku --allPoss: solves all the possible puzzles in this directory
  * Options:
  * # -n n: repeat n times
  * # -a: use the AdvancedPartial
  */
import ox.cads.util.Profiler
import java.util.concurrent.atomic.AtomicBoolean
import ox.cads.collection.TerminationDetectingPool
import ox.cads.collection.Pool
import scala.collection.mutable.ArrayBuffer

class StackPool[T] extends Pool[T]{
  private var stack = new ox.cads.collection.LockFreeStack[T]

  /** put x into the pool */
    def add(x: T) : Unit = {
      stack.push(x);
      // Profiler.count("Push")
    }

    /** Get a value from the pool.
    * @return Some(x) where x is the value obtained, or None if the pool is
    * empty. */
  def get : Option[T] = {
    stack.pop
  }
}



object ConcurrentSudoku {

  /** Solve the puzzle defined by init */
  def solve(init: Partial) {
    // Stack to store partial solutions that we might back-track to.
    val stack = new scala.collection.mutable.Stack[Partial]
    stack.push(init)
    var done = false

    while (!stack.isEmpty && !done) {
      val partial = stack.pop
      if (partial.complete) { // done!
        partial.printPartial; done = true
      } else {
        // Choose position to play
        val (i, j) = partial.nextPos;
        // Consider all values to play there
        for (d <- 1 to 9)
          if (partial.canPlay(i, j, d)) {
            val p1 = partial.play(i, j, d); stack.push(p1)
          }
      }
    } // end of while
  }

  /** question 1 **/
  def solve1(init: Partial, par: Int) {
    // Stack to store partial solutions that we might back-track to.
    val stack = new ox.cads.collection.LockFreeStack[Partial]
    val done = new AtomicBoolean(false)
    stack.push(init)

    def solver(){
      while (!done.get) {
        stack.pop match {
          case None =>{}
          case Some(partial)=>{
            val complete = partial.complete
            if(complete && done.compareAndSet(false, true)){
              partial.printPartial
            }else if(!complete){
              val (i, j) = partial.nextPos;
              for (d <- 1 to 9)
                if (partial.canPlay(i, j, d)) {
                  val p1 = partial.play(i, j, d)
                  if(!done.get) stack.push(p1)
                }
            }
          } 
        }
      }
    }
    ox.cads.util.ThreadUtil.runSystem(par, solver)
  }


  /** question 2 **/
  def solve2(init: Partial, par: Int) {
    // Stack to store partial solutions that we might back-track to.
    val pool = new TerminationDetectingPool[Partial](
      new StackPool[Partial],
      par
    )
    val done = new AtomicBoolean(false)

    // Stack to store partial solutions that we might back-track to.
    pool.add(init)

    def solver() {
      while (!done.get) {
        // pool.get will spin until pool is not empty or some thread invoke signalDone.
        pool.get match {
          // Return immediate upon one unsuccessful get.
          case None => { return }
          case Some(partial) => {
            val complete = partial.complete
            if (complete && done.compareAndSet(false, true)) {
              pool.signalDone
              partial.printPartial;
            } else if (!complete) {
              // Choose position to play
              val (i, j) = partial.nextPos;
              // Consider all values to play there
              for (d <- 1 to 9)
                if (partial.canPlay(i, j, d)) {
                  val p1 = partial.play(i, j, d)
                  pool.add(p1)
                }
            }
          }
        }
      }
    }
    ox.cads.util.ThreadUtil.runSystem(par, solver)
  }


  /** question 3 **/
  // To optimize the stack, we try not to have so many workers at the same time because they might just block each other.
  // We reduced the number of workers that are needed in order to fully use the resources 
  // "Amdahl's law"
  def solve3(init: Partial, par: Int) {
    // Stack to store partial solutions that we might back-track to.
    var num = par
    val cores = Runtime.getRuntime().availableProcessors();
    if(cores < par)
      num = cores/3*2
    println("Number of threads:" + num)
    // if(par > cores){
    //   par = cores
    // }

    val pool = new TerminationDetectingPool[Partial](
      new StackPool[Partial],
      num
    )
    val done = new AtomicBoolean(false)

    // Stack to store partial solutions that we might back-track to.
    pool.add(init)

    def solver() {
      while (!done.get) {
        // pool.get will spin until pool is not empty or some thread invoke signalDone.
        pool.get match {
          // Return immediate upon one unsuccessful get.
          case None => { return }
          case Some(partial) => {
            val complete = partial.complete
            if (complete && done.compareAndSet(false, true)) {
              pool.signalDone
              partial.printPartial;
            } else if (!complete) {
              // Choose position to play
              val (i, j) = partial.nextPos;
              // Consider all values to play there
              for (d <- 1 to 9)
                if (partial.canPlay(i, j, d)) {
                  val p1 = partial.play(i, j, d)
                  pool.add(p1)
                }
            }
          }
        }
      }
    }
    ox.cads.util.ThreadUtil.runSystem(num, solver)
  }


  /** A list of files containing possible puzzles */
  private val allPossibleFiles =
    List(
      "test1.sud",
      "test2.sud",
      "test3.sud",
      "test4.sud",
      "test5.sud",
      "test6.sud",
      "test7.sud",
      "test8.sud",
      "test9.sud",
      "test10.sud"
    )

  /** A list of files containing puzzles, including one impossible one. */
  private val allFiles = allPossibleFiles ++ List("impossible.sud")

  def main(args: Array[String]) = {
    val t0 = System.currentTimeMillis()

    // options
    var par = 10 // number of threads
    var section = 0 // the section of the problem, ex: 1,2,3
    var count = 1 // number of tests
    var fname = "" // filename
    var adv = true // are we using the AdvancedPartial?
    var all = false // are we doing all files in allFiles?
    var allPoss = false // are we doing all files in allPossibleFiles?
    // parse command line arguments
    var i = 0
    while (i < args.length) {
      if (args(i) == "-s") {
        section = args(i + 1).toInt; i += 2
      } else if (args(i) == "-p") {
        par = args(i + 1).toInt; i += 2
      } else if (args(i) == "-n") {
        count = args(i + 1).toInt; i += 2
      } else if (args(i) == "-a") {
        adv = true; i += 1
      } else if (args(i) == "--all") {
        all = true; i += 1
      } else if (args(i) == "--allPoss") {
        allPoss = true; i += 1
      } else {
        fname = args(i); i += 1
      }
    }
    assert(all || allPoss || fname != "")

    // Initialise partial from file fname
    def mkPartial(fname: String) = {
      val partial = if (adv) new AdvancedPartial else new SimplePartial
      partial.init(fname)
      partial
    }

    // Solve count times
    section match {
      case 3 => {
        for (i <- 0 until count)
          if (all) for (f <- allFiles) { println(f); solve3(mkPartial(f), par) } else if (allPoss)
            for (f <- allPossibleFiles) { println(f); solve3(mkPartial(f), par) } else
            solve3(mkPartial(fname), par)
      }
      case 2 => {
        for (i <- 0 until count)
          if (all) for (f <- allFiles) { println(f); solve2(mkPartial(f), par) } else if (allPoss)
            for (f <- allPossibleFiles) { println(f); solve2(mkPartial(f), par) } else
            solve2(mkPartial(fname), par)
      }
      case 1 => {
        for (i <- 0 until count)
          if (all) for (f <- allFiles) { println(f); solve1(mkPartial(f), par) } else if (allPoss)
            for (f <- allPossibleFiles) { println(f); solve1(mkPartial(f), par) } else
            solve1(mkPartial(fname), par)
      }
      case 0 => {
        for (i <- 0 until count)
          if (all) for (f <- allFiles) { println(f); solve(mkPartial(f)) } else if (allPoss)
            for (f <- allPossibleFiles) { println(f); solve(mkPartial(f)) } else
            solve(mkPartial(fname))
      }
    }
    

    println("Time taken: " + (System.currentTimeMillis() - t0))
    Profiler.report
  }
}
