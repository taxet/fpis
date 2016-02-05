package ex3

/**
 * @author CHAOLUN
 * 
 * ex 3.7 - 3.23
 */
object RightFold {
  def rightFold[A, B](l: List[A], n: B)(f: (A, B) => B) : B = l match{
    case Nil => n
    case head::tail => f(head, rightFold(tail, n)(f))
  }
  
  def length[A](as: List[A]): Int = rightFold(as, 0)((a: A, b: Int) => b+1)
  
  def leftFold[A, B](as: List[A], z: B)(f: (A, B) => B) : B = as match{
    case Nil => z
    case head:+tail =>leftFold(head, f(tail, z))(f)
  }
  
  def sum(nums: List[Int]): Int = leftFold(nums,0)(_+_)
  def product(nums: List[Int]): Int = leftFold(nums,1)(_*_)
  
  def reverse[A](ls:List[A]): List[A] = 
    rightFold(ls, List() :List[A])((ele:A, newLs:List[A]) => newLs:+ele)
  //def reverse[A](ls: List[A]): List[A] = 
  //  leftFold(ls, List(): List[A])((ele: A, newLs: List[A]) =>newLs:+ele)
  
  def append[A](ls: List[A], a: A): List[A] = 
    leftFold(ls, List(a))((ele: A, newLs: List[A]) => ele::newLs)
  
  def concat[A](l1: List[A], l2: List[A]): List[A] = 
    leftFold(l1, l2)((ele:A, newLs: List[A]) => ele::newLs)
  
  def addOne(l: List[Int]): List[Int] = 
    leftFold(l, List() : List[Int])((ele:Int, newLs: List[Int]) => ele + 1::newLs)
  
  def d2String(d: List[Double]): String = 
    leftFold(d, "")((d:Double, str:String) => d.toString + " " + str)
  
  def map[A, B](as: List[A])(f: A => B) : List[B] = 
    leftFold(as, List():List[B])((ele: A, ls:List[B]) => f(ele)::ls)
  
  def fileter[A](as:List[A])(f: A => Boolean): List[A] = 
    leftFold(as, List() : List[A])((ele: A, ls: List[A]) => if(f(ele)) ele::ls else ls)
  
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = 
    leftFold(as, List(): List[B])((ele: A, ls: List[B]) => f(ele):::ls)
  
  def filterByFlatMap[A](as:List[A])(f: A => Boolean): List[A] = 
    flatMap(as)(a => if(f(a)) List(a) else List())
  
  def addTwoList(l1: List[Int], l2: List[Int]): List[Int] = (l1, l2) match{
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (head1::tail1, head2::tail2) => head1+head2::addTwoList(tail1, tail2)
  }
  
  def zipWith[A](l1: List[A], l2: List[A])(f: (A, A) => A): List[A] = (l1, l2) match{
    case (_, Nil) => l1
    case (Nil, _) => l2
    case (head1::tail1, head2::tail2) => f(head1, head2)::zipWith(tail1, tail2)(f)
  }
    
  def main(args: Array[String]) = {
    println(flatMap(List(1, 2, 3))(a => List(a, a)))
  }
}