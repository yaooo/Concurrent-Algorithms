/*Conclusion:

To solve the sudoko 100 times:
  When using simple solver (without "-a"), the sequential solver takes around 16000 ms.
  When using simple solver (without "-a"), the concurrent solver (10 threads) takes around 7723 ms.


  When using the advanced partial (with "-a"), the sequential solver takes around 1143 ms.
  When using the advanced partial (with "-a"), the concurrent solver (10 threads) takes around 1106 ms.

Without the advanced partial, it greatly reduced the overall time using the concurrent solver
With the advanced partial, because the tree is traversed in a better way, the overall time does not have much effects regarding these two approaches.

*/



/** Sudoku solver.
  * Basic usage:
  * # scala Sudoku filename: solves the puzzle in filename
  * # scala Sudoku --all: solves all the puzzles in this directory
  * # scala Sudoku --allPoss: solves all the possible puzzles in this directory
  * Options:
  * # -n n: repeat n times
  * # -a: use the AdvancedPartial
  * # -p: the number of threads
  * # -s: the section of the problem, 0(sequential), 1(problem 1), 2(problem 2), 3(problem 3)
  */
import ox.cads.util.Profiler
import java.util.concurrent.atomic.AtomicBoolean
import ox.cads.collection.TerminationDetectingPool
import ox.cads.collection.Pool
import scala.collection.mutable.ArrayBuffer


// extrend the traint pool for the object of the TerminationDetectingPool
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

//   scala Sudoku --all -s 1 -p 10
// test1.sud
// 643287951
// 592316487
// 817459263
// 981634725
// 235798146
// 764521398
// 456973812
// 328145679
// 179862534

// test2.sud
// 987645321
// 654321978
// 312987654
// 896734512
// 745219863
// 231568497
// 578193246
// 469872135
// 123456789

// test3.sud
// 987654321
// 654321987
// 321987654
// 896745213
// 745213896
// 213896745
// 579468132
// 468132579
// 132579468

// test4.sud
// 943867251
// 582913467
// 617452983
// 891634725
// 235798146
// 764521398
// 459376812
// 328145679
// 176289534

// test5.sud
// 956431827
// 748296351
// 213785469
// 589612734
// 674953218
// 321847596
// 492568173
// 167324985
// 835179642

// test6.sud
// 731245869
// 642891357
// 589763421
// 425378196
// 813629574
// 976154283
// 357486912
// 294517638
// 168932745

// test7.sud
// 465237891
// 219658374
// 783149652
// 928563417
// 546791238
// 137824569
// 372986145
// 651472983
// 894315726

// test8.sud
// 937162485
// 581493276
// 624587913
// 213648759
// 479325168
// 865971324
// 752839641
// 196754832
// 348216597

// test9.sud
// 352617948
// 468329517
// 179584263
// 813265479
// 295741386
// 647938152
// 731496825
// 986152734
// 524873691

// test10.sud
// 731986245
// 842753169
// 695142837
// 384561972
// 917234586
// 256879314
// 523417698
// 178695423
// 469328751

// impossible.sud
// Time taken: 174


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
            val complete = partial.complete // if the partial solution is complete
            if(complete && done.compareAndSet(false, true)){
              partial.printPartial
            }else if(!complete){
              // Choose position to play
              val (i, j) = partial.nextPos;
              // Consider all values to play there
              for (d <- 1 to 9)
                if (partial.canPlay(i, j, d)) {
                  val p1 = partial.play(i, j, d); stack.push(p1)
                }
            }
          } 
        }
      }
    }
    ox.cads.util.ThreadUtil.runSystem(par, solver)
  }





// scala Sudoku --all -s 2 -p 10
// test1.sud
// 643287951
// 592316487
// 817459263
// 981634725
// 235798146
// 764521398
// 456973812
// 328145679
// 179862534

// test2.sud
// 987645321
// 654321978
// 312987654
// 896734512
// 745219863
// 231568497
// 578193246
// 469872135
// 123456789

// test3.sud
// 987654321
// 654321987
// 321987654
// 896745213
// 745213896
// 213896745
// 579468132
// 468132579
// 132579468

// test4.sud
// 943867251
// 582913467
// 617452983
// 891634725
// 235798146
// 764521398
// 459376812
// 328145679
// 176289534

// test5.sud
// 956431827
// 748296351
// 213785469
// 589612734
// 674953218
// 321847596
// 492568173
// 167324985
// 835179642

// test6.sud
// 731245869
// 642891357
// 589763421
// 425378196
// 813629574
// 976154283
// 357486912
// 294517638
// 168932745

// test7.sud
// 465237891
// 219658374
// 783149652
// 928563417
// 546791238
// 137824569
// 372986145
// 651472983
// 894315726

// test8.sud
// 937162485
// 581493276
// 624587913
// 213648759
// 479325168
// 865971324
// 752839641
// 196754832
// 348216597

// test9.sud
// 352617948
// 468329517
// 179584263
// 813265479
// 295741386
// 647938152
// 731496825
// 986152734
// 524873691

// test10.sud
// 731986245
// 842753169
// 695142837
// 384561972
// 917234586
// 256879314
// 523417698
// 178695423
// 469328751

// impossible.sud
// Time taken: 175

  /** question 2 **/
  def solve2(init: Partial, par: Int) {
    // Stack to store partial solutions that we might back-track to.
    val pool = new TerminationDetectingPool[Partial](
      new StackPool[Partial],
      par
    )
    val done = new AtomicBoolean(false)
    pool.add(init)

// • a value can be returned;
// • all p threads are attempting a get, at which point all return None;
// • another thread calls the signalDone operation, at which point all
// threads attempting a get return None.
    def solver() {
      while (!done.get) {
        pool.get match {
          // return for gettin a None
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




// scala Sudoku --all -s 3 -p 8 -n 100
// Time taken: 9151

// scala Sudoku --all -s 3 -p 10 -n 100
// Time taken: 16551


// scala Sudoku --all -s 3 -p 4 -n 100
// Time taken: 16775

// scala Sudoku --all -s 3 -p 4 -a -n 100
// Time taken: 1318


// scala Sudoku --all -s 3 -p 40 -n 50 -a
// Time taken: 750

// scala Sudoku --all -s 3 -p 40 -n 50
// Time taken: 4632

  /** question 3 **/
  // To optimize the stack, we try not to have so many workers at the same time because they might just block each other.
  // We reduced the number of workers that are needed in order to fully use the resources 
  // "Amdahl's law"

  // Or we can just ust "-a" option which pushes the partial solution with the least variations into the stack
  // In this case, we push fewer possible partial solutions to the stack, thus reduced the stack size
  def solve3(init: Partial, par: Int) {
    // Stack to store partial solutions that we might back-track to.
    var num = par
    val cores = Runtime.getRuntime().availableProcessors();
    if(cores < par)
      num = cores
    println("Number of threads:" + num)

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
    var adv = false // are we using the AdvancedPartial?
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
    // Profiler.report
  }
}
