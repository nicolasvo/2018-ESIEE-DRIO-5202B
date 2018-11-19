abstract class Tree
object Empty extends Tree
case class Leaf(v: Int) extends Tree
case class Node(l: Tree, r: Tree) extends Tree

//val t = Node(Leaf(1), Node(Leaf(2), Leaf(3)))

def height(t: Tree): Int =
  t match {
    case Empty => 0
    case Leaf(_) => 1
    case Node(l, r) => 1 + Math.max(height(l), height(r))
  }

height(Node(Leaf(1), Node(Leaf(2), Leaf(3))))

def sum(t: Tree): Int =
  t match {
    case Empty => 0
    case Leaf(v) => v
    case Node(l, r) => sum(l) + sum(r)
  }

sum(Node(Leaf(1), Node(Leaf(2), Leaf(3))))


// zero of the function
// zB: for a product, zero equals 1
def fold(zero: Int)(f: (Int, Int) => Int)(t: Tree): Int = {
  t match {
    case Empty => zero
    case Leaf(v) => v
    case Node(l, r) => f(fold(zero)(f)(l), fold(zero)(f)(r))
  }
}

def tsum = fold(0)(_+_)_

tsum(Node(Leaf(1), Node(Leaf(4), Leaf(3))))
