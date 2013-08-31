/**
 * Created with IntelliJ IDEA.
 * User: nakakura
 * Date: 8/31/13
 * Time: 1:02 PM
 * To change this template use File | Settings | File Templates.
 */


class S99Int(n:Int){
  implicit def intToS99Int(n:Int) = new S99Int(n)
  def isPrime():Boolean = (n != 1) &&
    (List.range(2,math.sqrt(n).toInt+1) forall (i => n % i != 0))

  //P33 (*) Determine whether two positive integer numbers are coprime.
  def isCoprimeTo(x: Int): Boolean = S99Int.gcd(n, x) == 1

  //P34 (**) Calculate Euler's totient function phi(m).
  def totient(): Int = {
    List.range(1, n).filter(n.isCoprimeTo(_)).length
  }

  //P35 (**) Determine the prime factors of a given positive integer.
  def primeFactors(): List[Int] = {
    def primeFactorsR(x: Int, value: Int): List[Int] = {
      if(x > math.sqrt(n).toInt + 1) Nil
      else if(value % x == 0) x :: primeFactorsR(x, value / x)
      else primeFactorsR(x + 1, value)
    }

    primeFactorsR(2, n)
  }

  //P36 (**) Determine the prime factors of a given positive integer (2).
  def pack[A](ls: List[A]): List[List[A]] = {
    if (ls.isEmpty) List(List())
    else {
      val (packed, next) = ls span { _ == ls.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  def encode[A](ls: List[A]): List[(A, Int)] =
    pack(ls) map { e => (e.head, e.length) }

  def primeFactoryMultiplicity(): List[(Int, Int)] = {
    encode(n.primeFactors())
  }

}

object S99Int{
  //P32 (**) Determine the greatest common divisor of two positive integer numbers.
  def gcd(x: Int, y: Int): Int = (x, y) match {
    case (_, _) if x < y => gcd(y, x)
    case (_, 0) => x
    case (_, _) => gcd(y, x % y)
  }
}


object Main {
  implicit def intToS99Int(n:Int) = new S99Int(n)
  //P31 (**) Determine whether a given integer number is prime.
  def main(args: Array[String]){
    println(7.isPrime)
    println(S99Int.gcd(36, 63))
    println(35.isCoprimeTo(64))
    println(35.isCoprimeTo(7))
    println(10.totient())
    println(315.primeFactors())
    println(315.primeFactoryMultiplicity())
  }
}
