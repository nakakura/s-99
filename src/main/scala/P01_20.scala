/**
 * Created by nakakura on 6/8/14.
 */
import java.util.Date

object P01_20 {
  def main(args: Array[String]): Unit = {
    println(removeAt(1, List('a, 'b, 'c, 'd)))
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

  def removeAt[A](n: Int, list: List[A]): (List[A], A) = {
    val ret = list.zipWithIndex.foldLeft((List.empty[A], list.head))((z, item) =>{
      if(item._2 == n) (z._1, item._1)
      else (item._1 :: z._1, z._2)
    })

    (ret._1.reverse, ret._2)
  }
}
