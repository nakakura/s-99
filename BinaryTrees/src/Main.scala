/**
 * Created with IntelliJ IDEA.
 * User: nakakura
 * Date: 9/7/13
 * Time: 4:45 PM
 * To change this template use File | Settings | File Templates.
 */


object Main {
  def main(args: Array[String]) {
    println(Tree.cBalanced(4, "x"))
    println(Node('a', Node(Node('a', Node('b'), Node('c'))), Node(Node('a', Node('b'), Node('c')))).isSymmetric)
    println(Tree.fromList(List(3, 2, 5, 7, 1)))
  }
}
