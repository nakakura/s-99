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
  def compress[A](list: List[A]): List[A] = {
    list match{
      case Nil => Nil
      case x :: tail => x :: compress(tail.dropWhile(_ == x))
    }
  }

  //P09 (**) Pack consecutive duplicates of list elements into sublists.
  def pack[A](list: List[A]): List[List[A]] = {
    if (list.isEmpty) List(List())
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }

  //P10 (*) Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] = {
    if (list.isEmpty) List()
    else {
      val (packed, next) = list span { _ == list.head }
      if (next == Nil) List((packed.length, packed.head))
      else List.concat(List((packed.length, packed.head)), encode(next))
    }
  }

  def encode_ans[A](ls: List[A]): List[(Int, A)] =
    pack(ls) map { e => (e.length, e.head) }

  //P11 (*) Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[Any] = {
    encode(list) map { t =>
      if(t._1 == 1) t._2
      else t
    }
  }

  //P12 (**) Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]): List[A] = {
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
  def duplicate[A](list: List[A]): List[A] = {
    println("p14")
    list flatMap  {t =>
      List(t, t)
    }
  }

  //P15 (**) Duplicate the elements of a list a given number of times.
  def duplicateN[A](n: Int, list: List[A]): List[A] = {
    println("p15")
    list flatMap  {t =>
      for(x <- 1 to n) yield t
    }
  }

  //P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A]): List[A] = {
    println("p16")
    def drop_sub(n: Int, ret: List[A], src: List[A]): List[A] = {
      (n, src) match {
        case (_, Nil) => ret.reverse
        case (1, x :: tail) => drop_sub(n-1, ret, tail)
        case (_, x :: tail) => drop_sub(n-1, x::ret, tail)
      }
    }

    drop_sub(n, Nil, list)
  }

  //P17 (*) Split a list into two parts.
  def split[A](n:Int, list: List[A]): (List[A], List[A]) = {
    (list.take(n), list.drop(n))
  }

  //P20 (*) Remove the Kth element from a list.
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    n match {
      case 0 => (list.tail, list.head)
      case _ if n > 0 => (list.take(n) ::: list.drop(n+1), list(n))
      case _ if n < 0 => throw new NoSuchElementException
    }
  }

  //P21 (*) Insert an element at a given position into a list.
  def insertAt[A](x: A, n: Int, list: List[A]): List[A] = list.splitAt(n) match {
    case (head, tail) => head ::: List(x) ::: tail
  }

  //P22 (*) Create a list containing all integers within a given range.
  def range(start: Int, end: Int): List[Int] = {
      if(start > end) Nil
      else start :: range(start + 1, end)
  }

  //P23 (**) Extract a given number of randomly selected elements from a list.
  def randomSelect[A](n: Int, list: List[A]): List[A] = {
    def randomSelect_sub(n: Int, srcList:List[A]): List[A] = {
      if(n <= 0) Nil
      else{
        val extract = removeAt(util.Random.nextInt(srcList.length), srcList)
        extract._2 :: randomSelect_sub(n-1, extract._1)
      }
    }
    randomSelect_sub(n, list)
  }

  //P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  def lotto(length: Int, max: Int): List[Int] = {
    if(length <= 0 ) Nil
    else{
      util.Random.nextInt(max-1) + 1 :: lotto(length-1, max)
    }
  }

  //P25 (*) Generate a random permutation of the elements of a list.
  def randomPermute[A](list: List[A]): List[A] = {
    randomSelect(list.length, list)
  }

  //P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  def flatMapSublists[A](ls: List[A])(f: (List[A]) => List[List[A]]): List[List[A]] = {
    ls match {
      case Nil => Nil
      case _ if ls.length > 0 => f(ls) ::: flatMapSublists(ls.tail)(f)
    }
  }

  def combinations[A](n: Int, ls: List[A]): List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(ls) { s =>
      combinations(n - 1, s.tail) map {s.head :: _}
    }
  }

  //P27 (**) Group the elements of a set into disjoint subsets.
  def group3[A](ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(2, ls)
      noA = ls.filterNot(x => a.contains(x))
      b <- combinations(3, noA)
    } yield List(a, b, noA.filterNot(x => b.contains(x)))

  def group[A](groupLen: List[Int], ls: List[A]): List[List[List[A]]] =
    for {
      a <- combinations(groupLen(0), ls)
      noA = ls.filterNot(x => a.contains(x))
      b <- combinations(groupLen(1), noA)
    } yield List(a, b, noA.filterNot(x => b.contains(x)))

  def test[A](listA: List[A], listB: List[A]): Unit = {
    val map1 = listA.flatMap{x => x :: List(99)}
    println(map1)
    val map2 = listA.map{x => x :: List(99)}
    println(map2)

      val list = listA.flatMap{x =>
        listB map {x :: List(_)}
      }

      println(list)

    val list2 = listA.map{x =>
      listB map {x :: List(_)}
    }

    println(list2)
  }

  def main(args: Array[String]){
    //val before = System.currentTimeMillis
    //println(randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)))
    test(List(1,2,3), List(10,20,30))
    //println( System.currentTimeMillis - before  )
  }
}
