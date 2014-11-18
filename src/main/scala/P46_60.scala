/**
 * Created by nakakura on 2014/07/27.
 */

class MyBool(val a: Boolean) {
  import MyBool._

  implicit def bool2MyBool(i: Boolean): MyBool = new MyBool(i)

  def and(b: Boolean) = (a, b) match{
    case (true, true) => true
    case _ => false
  }

  def or(b: Boolean) = (a, b) match{
    case (false, false) => false
    case _ => true
  }

  def eq(b: Boolean) = (a and b) or ((not(a)) and (not(b)))

  def xor(b: Boolean) = not(a eq b)

  def nor(b: Boolean) = not(a or b)

  def nand(b: Boolean) = not(a and b)

  def impl(b: Boolean) = not(a) or b
}

object MyBool{
  def not(a: Boolean) = a match{
    case true => false
    case false => true
  }
}

object Logic {
  implicit def bool2MyBool(i: Boolean): MyBool = new MyBool(i)


  def main(args: Array[String]): Unit = {
    println(gray(5))
  }

  def and(a: Boolean, b: Boolean): Boolean = (a, b) match{
    case (true, true) => true
    case _ => false
  }

  def or(a: Boolean, b: Boolean): Boolean = (a, b) match{
    case (false, false) => false
    case _ => true
  }

  def xor(a: Boolean, b: Boolean): Boolean = {
    a != b
  }

  def table2(f: (Boolean, Boolean) => Boolean) {
    println("A     B     result")
    for {a <- List(true, false)
         b <- List(true, false)} {
      printf("%-5s %-5s %-5s\n", a, b, f(a, b))
    }
  }

  def gray(num: Int): List[String] = {
    if(num == 0) return List("")
    subGray(num - 1, List[String]("0", "1"))
  }

  def subGray(num: Int, items: List[String]): List[String] = {
    if(num == 0) return items
    subGray(num - 1,
      for(a <- List[String]("0", "1");
        b <- items) yield (a + b))
  }

}

