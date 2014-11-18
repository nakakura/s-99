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
  def primeFactors: List[Int] = {
    def primeFactorsR(x: Int, s: Stream[Int]): List[Int] = {
      if(x.isPrime) List(x)
      else if(x % s.head == 0) s.head :: primeFactorsR(x / s.head, s)
      else primeFactorsR(x, s.tail)
    }

    primeFactorsR(n, sieve(Stream.from(2)))
  }

  //P36 (**) Determine the prime factors of a given positive integer (2).
  def primeFactorMultiplicity: List[(Int, Int)] = {
    def encode(items: List[Int]): List[(Int, Int)] = {
      val (packed, next) = items.span(_ == items.head)
      if(next == Nil) List((packed.head, packed.length))
      else (packed.head, packed.length) :: encode(next)
    }

    encode(n.primeFactors)
  }

  //P37 (**) Calculate Euler's totient function phi(m) (improved).
  def phi_improved: Int = {
    n.primeFactorMultiplicity.foldLeft(1)((z, x) => z * (x._1 - 1) * math.pow(x._1, x._2 - 1) toInt)
  }

}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

  //P32 (**) Determine the greatest common divisor of two positive integer numbers.
  def gcd(m: Int, n: Int): Int = {
    if(n == 0) return m
    else if(m < n) return gcd(n, m)
    gcd(n, m % n)
  }

  def sieve(s: Stream[Int]): Stream[Int] = {
    Stream.cons(s.head, s.tail.filter(_ % s.head != 0))
  }
}

