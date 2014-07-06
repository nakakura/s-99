/**
 * Created by nakakura on 6/8/14.
 */
import java.util.Date

object P01_20 {
  def main(args: Array[String]): Unit = {
    val a06 = isPalindrome[Int](List(1, 2, 1))
    val a08 = compress[Symbol](List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    val a09 = pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    val a10 = encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
    println(a10)
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

  //pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  def encode[A](list: List[A]): List[(Int, A)] = list.foldLeft(List.empty[(Int, A)])((z, n)=> z match {
    case Nil => (1, n) :: z
    case x :: l => if(n == x._2) (x._1 + 1, n) :: l else (1, n) :: (x :: l)
  }).reverse

}
