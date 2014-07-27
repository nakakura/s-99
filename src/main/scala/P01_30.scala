/**
 * Created by nakakura on 6/8/14.
 */
import java.util.Date

object P01_30 {
  def main(args: Array[String]): Unit = {
    println(lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
  }

  //P01 (*) Find the last element of a list.
  def last[A](list: List[A]): A = list match {
    case h :: Nil => h
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }

  //P02 (*) Find the last but one element of a list.
  def penultimate[A](list: List[A]): A = {
    if(list.length == 2) list.head
    else if(list.length > 2) penultimate(list.tail)
    else throw new NoSuchElementException
  }

  //P03 (*) Find the Kth element of a list.
  def nth[A](n: Int, list: List[A]): A = (n, list) match{
    case (0, h :: _) => h
    case (n, _ :: tail) => nth(n-1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }

  //P04 (*) Find the number of elements of a list.
  def length[A](list: List[A]): Int = list match {
    case Nil => 0
    case _::tail => 1 + length(tail)
  }

  //P05 (*) Reverse a list.
  def reverse[A](list: List[A]): List[A] = {
    list.foldLeft(List.empty[A])((z, n) => n :: z)
  }

  //P06 (*) Find out whether a list is a palindrome.
  def isPalindrome[A](list: List[A]): Boolean = list match {
    case Nil => true
    case _::Nil => true
    case x::tail => (x == tail.last) && isPalindrome(tail.init)
  }
  //def isPalindrome[A](ls: List[A]): Boolean = ls == ls.reverse

  //P07 (**) Flatten a nested list structure.
  def flatten(list: List[Any]): List[Any] = list.flatMap {
    case ms:List[_] => flatten(ms)
    case e => List(e)
  }

  //P08 (**) Eliminate consecutive duplicates of list elements.
  def compress[A](list: List[A]): List[A] = list.foldLeft(List.empty[A])((z, n) => z match {
    case Nil => n :: z
    case x::l => if(x == n) z else n :: z
  }).reverse

  def compress2[A](list: List[A]): List[A] = list.foldRight(List[A]()) { (h, r) =>
    if (r.isEmpty || r.head != h) h :: r
    else r
  }

  //P09 (**) Pack consecutive duplicates of list elements into sublists.
  def pack[A](list: List[A]): List[List[A]] = list.foldLeft(List.empty[List[A]])((z, n)=> z match {
    case Nil => List(n) :: z
    case x :: l => if(n == x.head) (n :: x) :: l else List(n) :: (x :: l)
  }).reverse

  //P10 (*) Run-length encoding of a list.
  def encode[A](list: List[A]): List[(Int, A)] = list.foldLeft(List.empty[(Int, A)])((z, n)=> z match {
    case Nil => (1, n) :: z
    case x :: l => if(n == x._2) (x._1 + 1, n) :: l else (1, n) :: (x :: l)
  }).reverse

  //P11 (*) Modified run-length encoding.
  def encodeModified[A](list: List[A]): List[Any] = {
    encode(list).map(item => item match{
      case (1, l) => l
      case (_, l) => item
    })
  }

  //P12 (**) Decode a run-length encoded list.
  def decode[A](list: List[(Int, A)]): List[A] = {
    def decodeItem[A](item: (Int, A)): List[A] = item match {
      case (0, l) => Nil
      case (n, l) => l :: decodeItem((n-1, l))
    }
    list.flatMap(item =>{
      decodeItem(item)
    })
  }

  //P13 (**) Run-length encoding of a list (direct solution).
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    list.foldLeft(List.empty[(Int, A)])((z, n) => {
      if(z.head._2 == n) (z.head._1 + 1, z.head._2) :: z.tail
      else (1, n) :: z
    })
  }

  //P14 (*) Duplicate the elements of a list.
  def duplicate[A](list: List[A]): List[A] = list flatMap(item=>{
    List(item, item)
  })

  //P15 (**) Duplicate the elements of a list a given number of times.
  def deplicateN[A](times: Int, list: List[A]): List[A] = list.flatMap(item=>{
    List.fill(times)(item)
  })

  //P16 (**) Drop every Nth element from a list.
  def drop[A](n: Int, list: List[A]): List[A] = {
    list.foldLeft((n, List.empty[A]))((z, x) => z match {
      case (1, l) => (n, l)
      case (k, l) => (k - 1, x :: l)
    })._2.reverse
  }

  //P17 (*) Split a list into two parts.
  def split[A](n: Int, list: List[A]): (List[A], List[A]) = {
    (list.take(n), list.drop(n))
  }

  //P18 (**) Extract a slice from a list.
  def slice[A](start: Int, end: Int, list: List[A]) : List[A] = {
    list.drop(start).take(end - start)
  }

  //P19 (**) Rotate a list N places to the left.
  def rotate[A](n: Int, list: List[A]): List[A] = {
    if(n > 0) list.drop(n) ++ list.take(n)
    else list.drop(list.length + n) ++ list.take(list.length + n)
  }

  //P20 (*) Remove the Kth element from a list.
  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    val ret = list.zipWithIndex.foldLeft((List.empty[A], list.head))((z, item) =>{
      if(item._2 == n) (z._1, item._1)
      else (item._1 :: z._1, z._2)
    })

    (ret._1.reverse, ret._2)
  }

  //P21 (*) Insert an element at a given position into a list.
  def insertAt[A](appendItem: A, position: Int, list: List[A]): List[A] = {
    list.zipWithIndex.flatMap{
      case (item, index) if index == position => appendItem :: (item :: Nil)
      case (item, _) => item :: Nil
    }
  }

  //P22 (*) Create a list containing all integers within a given range.
  def range(start: Int, end: Int): List[Int] = {
    (for(i <- start to end) yield i).toList
  }

  //P23 (**) Extract a given number of randomly selected elements from a list.
  def randomSelect[A](num: Int, list: List[A]): List[A] = {
    if(num == 0) return Nil
    val (l, e) = removeAt(num, list)
    e :: randomSelect(num - 1, l)
  }

  //P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  def lotto(num: Int, max: Int): List[Int] = {
    if(num <= 0) return Nil
    val randomInt: Int = math.floor(math.random * max).toInt + 1
    if(randomInt > max) lotto(num, max)
    else randomInt :: lotto(num - 1, max)
  }

  //P25 (*) Generate a random permutation of the elements of a list.
  def randomPermute[A](list: List[A]): List[A] = {
    randomSelect(list.length, list)
  }

  //P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  def combinations[A](num: Int, list: List[A]): List[List[A]] = {
    if(num == 1) return list.map(item => item :: Nil)
    list.zipWithIndex.flatMap{
      case (item, index) if (list.length - index) >= num => {
        combinations(num - 1, list.drop(index + 1)).map(nextItems => {
          item :: nextItems
        })
      }
      case _ =>{
        Nil
      }
    }.toList
  }

  //P27 (**) Group the elements of a set into disjoint subsets.
  def group3[A](list: List[A]): List[List[List[A]]] = {
    val x = for{
      first <- combinations(2, list)
      other = list.filter(item => ! first.contains(item))
      second <- combinations(3, other)
    } yield List(first, second, other.filter(item => ! second.contains(item)))
    x.toList
  }

  def group[A](ns: List[Int], list: List[A]): List[List[List[A]]] = ns match{
    case Nil => List(Nil)
    case n :: ns => combinations(n, list) flatMap{c =>
      group(ns, list.filter(item => ! c.contains(item))) map { c :: _ }
    }
  }

  //P28 (**) Sorting a list of lists according to length of sublists.
  def lsort[A](list: List[List[A]]): List[List[A]] = {
    list.sortBy(item => item.length)
  }

  def lsortFreq[A](list: List[List[A]]): List[List[A]] = {
    val list2 = lsort(list)
    val list3 = list2.foldLeft(List.empty[(Int, List[List[A]])])((z, n) => (z, n) match {
      case (x :: l, n) if x._2.head.length == n.length => (x._1 + 1, n :: x._2) :: l
      case (z, n) => (1, n :: Nil) :: z
    })
    list3.sortBy(item => item._1).flatMap(item => item._2.reverse)
  }
}
