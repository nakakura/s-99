/**
 * Created by nakakura on 2014/07/27.
 */

class S99Int(val n: Int) {
  import S99Int._

  //P31 (**) Determine whether a given integer number is prime.
  def isPrime: Boolean =
    (n > 1) && (primes takeWhile { _ <= math.sqrt(n) } forall { n % _ != 0 })

  //P33 (*) Determine whether two positive integer numbers are coprime.
  def isComprimeTo(x: Int): Boolean = {
    if(S99Int.gcd(n, x) == 1) return true
    else false
  }

  //P34 (**) Calculate Euler's totient function phi(m).
  def totient: Int = {
    (1 to n).filter(n.isComprimeTo(_)).length
  }

  //P35 (**) Determine the prime factors of a given positive integer.
  def primeFactors : Int = {
    (1 to n).filter(n.isComprimeTo(_)).length
  }
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

  def main(args: Array[String]): Unit = {
    println(10.totient)
  }

  //P32 (**) Determine the greatest common divisor of two positive integer numbers.
  def gcd(m: Int, n: Int): Int = {
    if(n == 0) return m
    else if(m < n) return gcd(n, m)
    gcd(n, m % n)
  }


}

