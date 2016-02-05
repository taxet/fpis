package ex3

/**
 * @author CHAOLUN
 * 
 * ex 3.25 - 3.29
 */
object Tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  
  def size[A](root: Tree[A]): Int = root match{
    case Leaf(v) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }
  
  def maximum(root:Tree[Int]): Int = root match{
    case Leaf(v) => v
    case Branch(l, r ) => maximum(l) max maximum(r)
  }
  
  def depth[A](root: Tree[A]):Int = root match{
    case Leaf(v) => 1
    case Branch(l, r) => 1 + depth(l).max(depth(r))
  }
  
  def map[A,B](root: Tree[A])(f: A => B):Tree[B] = root match{
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }
  
  def fold[A, B](root: Tree[A])(f: A => B)(g: (B, B) => B): B = root match{
    case Leaf(v) => f(v)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
  
  def maximumWithFold(root: Tree[Int]):Int = fold(root)((i: Int) => i)((i1: Int, i2: Int) => i1.max(i2))
  def depthWithFold[A](root: Tree[A]): Int = fold(root)(_ => 1)((i1: Int, i2:Int) => i1.max(i2))
  def sizeWithFold[A](root: Tree[A]): Int = fold(root)(_ => 1)((i1: Int, i2: Int) => 1 + i1 + i2)
  def mapWithFold[A, B](root: Tree[A])(f: A => B): Tree[B] = fold(root)(a => Leaf(f(a)): Tree[B])((l, r) => Branch(l, r))
}