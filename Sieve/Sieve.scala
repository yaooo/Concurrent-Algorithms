/** A sequential implementation of the Sieve of Eratosthenes */

import java.util.concurrent.atomic.AtomicIntegerArray
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.atomic.AtomicBoolean

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

  def insertPrime(primes: AtomicIntegerArray, prime: Int, start: Int): Boolean = {
    var i = start
    var p = prime

    while (p != 0) {
      var cur = primes.get(i)
      while (cur > 0 && cur < p) {
        i += 1
        if (i >= primes.length) return false
        cur = primes.get(i)
      }
      // find the position to insert the number

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
        val current = new Array[Int](par)   //current ids for the threads
        val next = new AtomicInteger(3) //next long to be tested
        var nextSlot = new AtomicInteger(1) // index of the next prime that should be inserted

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
                var i = 0; var p = primes.get(i)
                while (isPrime && p > 0 && i < primes.length-1 && p <= Math.sqrt(cur_number)) {
                  if (cur_number % p == 0)
                    isPrime = false
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


    def main(args: Array[String]) = {
        assert(args.length == 1, "must have one argument")

        val N = args(0).toInt // number of primes required

        val t0 = java.lang.System.currentTimeMillis()
        sequential(N)
        println("Time taken: " + (java.lang.System.currentTimeMillis() - t0))

        val t1 = java.lang.System.currentTimeMillis()
        concurrent(N, 8)
        println("Time taken: " + (java.lang.System.currentTimeMillis() - t1))
    }
}
