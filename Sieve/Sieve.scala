/*Conclusion*/
// For a smaller N, the sequential program runs faster. However, it is not the same case after N becomes big enough.
// The concurrent program with a local cache generally performs slightly better than the regular concurrent program.



// Test cases using 4 workers

// scala Sieve 10000
// 104729
// Time taken(sequential): 8
// 104729
// Time taken(concurrent): 65
// 104729
// Time taken(concurrent-cached): 41

// scala Sieve 100000
// 1299709
// Time taken(sequential): 61
// 1299709
// Time taken(concurrent): 147
// 1299709
// Time taken(concurrent-cached): 104

// scala Sieve 1000000
// 15485863
// Time taken(sequential): 1296
// 15485863
// Time taken(concurrent): 1650
// 15485863
// Time taken(concurrent-cached): 1542

// scala Sieve 10000000
// 179424673
// Time taken(sequential): 35583
// 179424673
// Time taken(concurrent): 22669
// 179424673
// Time taken(concurrent-cached): 23616

import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicInteger

object Sieve {

    def sequential(N: Int) = {
        val primes = new Array[Int](N) // will hold the primes
        primes(0) = 2
        var nextSlot = 1 // next free slot in primes
        var next = 3 // next candidate prime to consider

        while (nextSlot < N) {
            // Test if next is prime;
            // invariant: next is coprime with primes[0..i) && p = primes(i)
            var i = 0;
            var p = primes(i)
            while (p * p <= next && next % p != 0) {
                i += 1; p = primes(i)
            }
            if (p * p > next) { // next is prime
                primes(nextSlot) = next;
                nextSlot += 1
            }
            next += 1
        }

        println(primes(N - 1))
    }


// find a place to inset, not care about overwrite
  def insertPrime(primes: AtomicIntegerArray, prime: Int, start: Int): Boolean = {
    var i = start
    var p = prime

    while (p != 0) {
      var cur = primes.get(i)
      // find the index to insert
      while (cur > 0 && cur < p) {
        i += 1
        if (i >= primes.length) return false
        cur = primes.get(i)
      }

      // replace the number previously stored in the Array
      // store p = cur, and find the next position to insert the replaced value
      if (primes.compareAndSet(i, cur, p)) {
        p = cur 
      } 
    }
    true
  }

    def isPrime(n: Int): Boolean = ! ((2 until scala.math.sqrt(n).toInt) exists (n % _ == 0))


    def concurrent(N: Int, par: Int) = {
        val primes = new AtomicIntegerArray(N)
        primes.set(0, 2)

        val current = new AtomicIntegerArray(par) //store the current ids for the threads
        val next = new AtomicInteger(3) //next long to be tested
        var nextSlot = new AtomicInteger(1) // index of the next prime that should be inserted

        def solve(id: Int):Unit ={
            while(primes.get(N-1) == 0){
                var cur_number = next.getAndIncrement()
                var start = nextSlot.get()

                current.set(id, cur_number)

                // spining lock, spin if "(m ^ 2 <= n)"
                var valid = false
                while(!valid){
                    valid = true
                    for(i <- 0 until par)
                        valid = valid & !(current.get(i) <= Math.sqrt(cur_number))
                }

                var isPrime = true 
                var i = 0; var p = primes.get(i)
                while (isPrime && p > 0 && i < primes.length-1 && p * p <= cur_number) {
                  isPrime &= !(cur_number % p == 0)
                  i += 1
                  p = primes.get(i)
                }

                if (isPrime && insertPrime(primes, cur_number, start)) {
                   nextSlot.getAndIncrement
                }
            }
        }

        ox.cads.util.ThreadUtil.runIndexedSystem(par, solve)
        println(primes.get(N - 1))
    }


    def concurrent_cache(N: Int, par: Int) = {
        val primes = new AtomicIntegerArray(N)
        primes.set(0, 2)
        val current = new Array[Int](par)   //current ids for the threads
        val next = new AtomicInteger(3) //next long to be tested
        var nextSlot = new AtomicInteger(1) // index of the next prime that should be inserted

        // local cache used to store non-zero numbers from the array primes
        val localPrimes = new Array[Int](N)
        localPrimes(0) = 2
        var localPrimesIndex = 1

        def solve(id: Int):Unit ={

            while(primes.get(N-1) == 0){
                var cur_number = next.getAndIncrement()
                var start = nextSlot.get()

                current(id) = cur_number

                // spining lock, spin if "(m ^ 2 <= n)" (overflow??)
                var valid = false
                while(!valid){
                    valid = true
                    for(i <- 0 until par)
                        valid = valid & !(current(i) <= Math.sqrt(cur_number))
                }

                var isPrime = true

                while(localPrimesIndex < primes.length && primes.get(localPrimesIndex) != 0){
                    localPrimes(localPrimesIndex) = primes.get(localPrimesIndex)
                    localPrimesIndex += 1
                }

                var i = 0; var p = localPrimes(i)
                while (isPrime && p > 0 && i < localPrimes.length-1 && p * p <= cur_number) {
                  isPrime &= !(cur_number % p == 0)
                    // isPrime = false
                  i += 1
                  p = localPrimes(i)
                }

                if (isPrime && insertPrime(primes, cur_number, start)) {
                   nextSlot.getAndIncrement
                }
            }
        }

        ox.cads.util.ThreadUtil.runIndexedSystem(par, solve)
        println(primes.get(N - 1))
    }


    def main(args: Array[String]) = {
        assert(args.length == 1, "must have one argument")

        val N = args(0).toInt // number of primes required

        val t0 = java.lang.System.currentTimeMillis()
        sequential(N)
        println("Time taken(sequential): " + (java.lang.System.currentTimeMillis() - t0))

        val t1 = java.lang.System.currentTimeMillis()
        concurrent(N, 4)
        println("Time taken(concurrent): " + (java.lang.System.currentTimeMillis() - t1))

        val t2 = java.lang.System.currentTimeMillis()
        concurrent(N, 4)
        println("Time taken(concurrent-cached): " + (java.lang.System.currentTimeMillis() - t2))
    }
}
