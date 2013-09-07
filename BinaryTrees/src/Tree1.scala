/**
 * Created with IntelliJ IDEA.
 * User: nakakura
 * Date: 9/7/13
 * Time: 4:47 PM
 * To change this template use File | Settings | File Templates.
 */

sealed abstract class Tree[+T]{
  //P57 (**) Binary search trees (dictionaries).
  def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"

  //P56 (**) Symmetric binary trees.
  def isSymmetric(): Boolean = {
    def subIsSymmetric(leftNode: Tree[T], rightNode: Tree[T]): Boolean = {
      (leftNode, rightNode) match {
        case (End, End) => true
        case (x: Node[T], y: Node[T]) => subIsSymmetric(x.left, y.left) && subIsSymmetric(x.right, y.right)
        case _ => false
      }
    }

    subIsSymmetric(left, right)
  }

  def addValue[U >: T <% Ordered[U]](x: U): Tree[U] = {
    if(x < value) Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
  }
}

case object End extends Tree[Nothing] {
  def addValue[U <% Ordered[U]](x: U) = Node(x)
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  //P55 (**) Construct completely balanced binary trees.
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      val subtrees = cBalanced(n / 2, value)
      subtrees.flatMap(l => subtrees.map(r => Node(value, l, r)))
    }
    case n if n % 2 == 0 => {
      val lesserSubtrees = cBalanced((n - 1) / 2, value)
      val greaterSubtrees = cBalanced((n - 1) / 2 + 1, value)
      lesserSubtrees.flatMap(l => greaterSubtrees.flatMap(g => List(Node(value, l, g), Node(value, g, l))))
    }
  }

  def fromList[T <% Ordered[T]](l: List[T]): Tree[T] =
    l.foldRight(End: Tree[T])((r, e) => {println(e); r.addValue(e)})
}