/**
 * Created by nakakura on 2014/07/27.
 */

class S99Int(val n: Int) {
  import S99Int._

  def isPrime: Boolean =
    (n > 1) && (primes takeWhile { _ <= math.sqrt(n) } forall { n % _ != 0 })
}

object S99Int {
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

  val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })

  def main(args: Array[String]): Unit = {
    for (n <- 1 to 10)
      println(n + ": " + n.isPrime)
  }
}

