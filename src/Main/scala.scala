package Main

/**
 * Created with IntelliJ IDEA.
 * User: nakakura
 * Date: 8/25/13
 * Time: 3:23 PM
 * To change this template use File | Settings | File Templates.
 */

object Main{

  //P01 (*) Find the last element of a list.
  def last(list: List[Int]): Int = {
    return list.last
  }

  //P02 (*) Find the last but one element of a list.
  def penultimate(list: List[Int]): Int = {
    return list.init.last
  }

  //P03 (*) Find the Kth element of a list.
  def nth(counter: Int, list: List[Int]): Int = {
    (counter, list) match{
      case (0, h::_ ) => h
      case (x, _::tail) => nth(x-1, tail)
      case (x, Nil) => throw new NoSuchElementException
    }
  }

  //P04 (*) Find the number of elements of a list.
  def length(list: List[Int]): Int = {
    return list.length
  }

  //P05 (*) Reverse a list.
  def reverse(list: List[Int]): List[Int] = {
    return list.reverse
  }

  //P06 (*) Find out whether a list is a palindrome.
  def isPalindrome(list: List[Int]): Boolean = {
    list match {
      case Nil => true
      case x::Nil => true
      case x::tail if x == tail.last => isPalindrome(tail.init)
      case x::tail if x != tail.last => false
    }
  }

  //P07 (**) Flatten a nested list structure.
  def flatten(list: List[Any]): List[Any] = {
    list.flatMap({
      case x: List[Any] => flatten(x)
      case x: Any => List(x)
    })
  }

  //P08 (**) Eliminate consecutive duplicates of list elements.
  def compress(list: List[Symbol]): List[Symbol] = {
    list match{
      case Nil => Nil
      case x :: tail => x :: compress(tail.dropWhile(_ == x))
    }
  }

  //P09 (**) Pack consecutive duplicates of list elements into sublists.
  def pack(list: List[Symbol]): List[List[Symbol]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  //P10 (*) Run-length encoding of a list.
  def encode(list: List[Symbol]): List[(Int, Symbol)] = {
    if (list.isEmpty) List()
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List((packed.length, packed.head))
      else List.concat(List((packed.length, packed.head)), encode(next))
    }
  }

  def encode_ans(ls: List[Symbol]): List[(Int, Symbol)] =
    pack(ls) map { e => (e.length, e.head) }

  //P11 (*) Modified run-length encoding.
  def encodeModified(list: List[Symbol]): List[Any] = {
    encode(list) map { t =>
      if(t._1 == 1) t._2
      else t
    }
  }

  //P12 (**) Decode a run-length encoded list.
  def decode(list: List[(Int, Symbol)]): List[Symbol] = {
    list flatMap  { t =>
      for(x <- 1 to t._1) yield t._2
    }
  }

  //P12 answer
  def decode_answer[A](ls: List[(Int, A)]): List[A] =
    ls flatMap { e => List.make(e._1, e._2) }

  //P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect[A](ls: List[A]): List[(Int, A)] =
    if (ls.isEmpty) Nil
    else {
      val (packed, next) = ls span { _ == ls.head }
      (packed.length, packed.head) :: encodeDirect(next)
    }

  //P14 (*) Duplicate the elements of a list.
  def duplicate(list: List[Symbol]): List[Symbol] = {
    println("p14")
    list flatMap  {t =>
      List(t, t)
    }
  }

  //P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateN(n: Int, list: List[Symbol]): List[Symbol] = {
    println("p15")
    list flatMap  {t =>
      for(x <- 1 to n) yield t
    }
  }

  //P16 (**) Drop every Nth element from a list.
  def drop(n: Int, list: List[Symbol]): List[Symbol] = {
    println("p16")
    def drop_sub(n: Int, ret: List[Symbol], src: List[Symbol]): List[Symbol] = {
      (n, src) match {
        case (_, Nil) => ret.reverse
        case (1, x :: tail) => drop_sub(n-1, ret, tail)
        case (_, x :: tail) => drop_sub(n-1, x::ret, tail)
      }
    }

    drop_sub(n, Nil, list)
  }

  //P17 (*) Split a list into two parts.
  def split(n:Int, list: List[Symbol]): (List[Symbol], List[Symbol]) = {
    (list.take(n), list.drop(n))
  }

  def main(args: Array[String]){
    println(last(List(1,2,3,4,5)))
    println(penultimate(List(1,2,3,4,5)))
    println(nth(2, List(1,2,3,4,5)))
    println(length(List(1,2,3,4,5)))
    println(reverse(List(1,2,3,4,5)))
    println(isPalindrome(List(1,2,3,4,3,2,1)))
    println(isPalindrome(List(1,2,3,3,2,1)))
    println(isPalindrome(List(1,2,4,3,2,1)))
    println(flatten(List(List(1, 1), 2, List(3, List(5, 8)))))
    println(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encode_ans(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)))
    println(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
    println(duplicate(List('a, 'b, 'c, 'c, 'd)))
    println(duplicateN(3, List('a, 'b, 'c, 'c, 'd)))
    println(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
    println(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
}
