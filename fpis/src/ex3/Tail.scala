package ex3

/**
 * @author CHAOLUN
 * 
 * FPIS 2.3 2.4 2.5 2.6
 */
object Tail {
  def tail[A](ls: List[A]) : List[A] = ls match{
    case Nil => Nil
    case head::tail => tail
  }
  def setHead[A](ls:List[A]) : List[A] = ls match{
    case Nil => Nil
    case head::tail => head::ls
  }
  
  def drop[A](l: List[A], n: Int) : List[A] = n match{
    case 0 => l
    case _ => drop(tail(l), n - 1)
  }
  
  def dropWhile[A](l:List[A], f: A=>Boolean) : List[A] = l match{
    case Nil => Nil
    case head::tail => if (f(head)) dropWhile(tail, f) else l
  }
  
  def init[A](l:List[A]): List[A] = l match{
    case Nil => Nil
    case head:+tail => head
  }
  
  def main(args: Array[String]) = {
    val testList : List[Int] = List(3, 44, 38, 5, 47, 15, 36, 26, 27, 2, 46, 4, 19, 50, 48)
    println(tail(testList));
    println(setHead(testList));
    println(drop(testList, 6));
    println(dropWhile(testList, (i: Int) => i < 45));
    println(init(testList))
  }
}