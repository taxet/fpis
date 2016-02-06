package ex4

import scala.{Option => _, Either => _, _}

/**
 * @author CHAOLUN
 */
object Ex4 {
  // ex 4.1
  trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match{
      case None => None
      case Some(a) => Some(f(a))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = this match{
      case None => default
      case Some(a) => a
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this match{
      case None => ob
      case _ => this
    }
    def filter(f: A => Boolean): Option[A] = this match{
      case Some(a) if f(a) => this
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]
  
  //ex 4.2
  def mean(xs: Seq[Double]) : Option[Double] = 
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)
  def variance(xs: Seq[Double]): Option[Double] = 
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C) : Option[C] = (a, b) match{
      case (None, _) => None
      case (_, None) => None
      case (Some(aa), Some(bb)) => Some(f(aa, bb))
    }
  // a flatMap (aa => b map (bb => f(aa, bb)))
  
  def sequence[A](a: List[Option[A]]) : Option[List[A]] = a match{
    case Nil => Some(Nil)
    case h::tail => map2(h, sequence(tail))((hh, tt) => hh::tt)
  }
  
  def traverse[A,B](a:List[A])(f:A => Option[B]) : Option[List[B]] = sequence(a.map(f))
  
  sealed trait Either[+E,+A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(a) => Right(f(a))
    }
    def flatMap[EE >: E, B](f: A => Either[EE, B]) : Either[EE, B] = this match{
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }
    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match{
      case Left(_) => b
      case Right(a) => Right(a)
    }
    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {a <- this; bb <- b} yield f(a,bb)
  }
  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]
  
  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum/xs.length)
  
  def safeDiv(x: Int, y: Int) : Either[Exception, Int] = 
    try Right(x / y)
    catch {case e:Exception => Left(e)}
  
  def Try[A](a: => A): Either[Exception, A] = 
    try Right(a)
    catch {case e:Exception => Left(e)}
  
  // ex 4.7
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = es match{
    case Nil => Right(Nil)
    case h::t => for {hh <- h; tt <- sequence(t)} yield hh::tt
    //case h::t => h flatMap(hh => sequence(t) map (tt => hh::tt))
  }
  
  def traverseE[E, A, B](as: List[A])(
      f: A => Either[E, B]): Either[E, List[B]] = as match{
    case Nil => Right(Nil)
    //case h::t => for {hh <- f(h); tt <-traverseE(t)(f)} yield hh::tt
    case h::t => (f(h) map2(traverseE(t)(f)))(_::_)
  }
}