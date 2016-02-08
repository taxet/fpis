package ex5

/**
 * @author CHAOLUN
 */
object Ex5 {
  sealed trait Stream[+A] {
   def headOption: Option[A] = this match{
     case Empty => None
     case Cons(h, t) => Some(h())
   }
   def toList: List[A] = this match {
     case Empty => Nil
     case Cons(h, t) => h()::t().toList
   }
   def take(n: Int): Stream[A] = this match{
     case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
     case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
     case _ => Empty
   }
   def drop(n: Int): Stream[A] = this match {
     case Cons(h, t) if n > 0 => t().drop(n - 1)
     case _ => Empty
   }
   
   def takeWhile(p: A => Boolean): Stream[A] = this match{
     case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
     case _ => Empty
   }
   
   def exists(p: A=>Boolean):Boolean = this match{
     case Cons(h, t) => p(h()) || t().exists(p)
     case _ => false
   }
   
   def foldRight[B](z: => B)(f: (A, => B) => B): B =
     this match{
       case Cons(h, t) => f(h(), t().foldRight(z)(f))
       case _ => z
     }
   
   def existF(p: A => Boolean): Boolean = 
     foldRight(false)((a, b) => p(a)||b)
   
   def forAll(p: A => Boolean): Boolean = 
     foldRight(true)((a, b) => p(a) && b)
   
   def takeWhileF(p: A => Boolean) : Stream[A] = 
     foldRight(Stream[A]())((a, b) => if(p(a)) Stream.cons(a, b) else Empty)

   def headOptionF : Option[A] = 
     foldRight(None: Option[A])((h, _) => Some(h))
   def map[B](f: A => B): Stream[B] = 
     foldRight(Stream[B]())((a, b) => Stream.cons(f(a), b))
   def filter(f: A => Boolean): Stream[A] = 
     foldRight(Stream[A]())((h, t) => if (f(h)) Stream.cons(h, t) else t)
   def append[B >: A](s: => Stream[B]): Stream[B] = 
     foldRight(s)((h, t) => Stream.cons(h, t))
   def flatMap[B](f: A => Stream[B]): Stream[B] = 
     foldRight(Stream[B]())((h, t) => f(h) append t)
     
   def constant[A](a: A): Stream[A] = 
     Stream.cons(a, constant(a))
   def from(n: Int): Stream[Int] = 
     Stream.cons(n, from(n + 1))
   def fibs : Stream[Int] = {
     def fibsRecu(a: Int, b: Int): Stream[Int] = 
       Stream.cons(a, fibsRecu(b, a+b))
     return fibsRecu(0, 1)
   }
   def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = 
     f(z) match{
     case Some((h, t)) => Stream.cons(h, unfold(t)(f))
     case None => Empty
   }
   def fibsU : Stream[Int] = unfold((0, 1)){case (f0, f1) => Some(f0, (f1, f1+f0))}
   def fromU(a: Int): Stream[Int] = unfold(a){case i => Some(i, i + 1)}
   def constantU[A](a:A): Stream[A] = unfold(a){case i => Some(i, i)}
   def onesU : Stream[Int] = unfold(1){case _ => Some(1, 1)}
   
   def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this){
     case Cons(h, t) => Some(f(h()), t())
     case _ => None
   }
   def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)){
     case(Cons(h, t), 1) => Some(h(), (Empty, 0))
     case(Cons(h, t), n) if n > 1 => Some(h(), (t(), n-1))
     case _ => None
   }
   def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
     case Cons(h, t) if p(h()) => Some(h(), t())
     case _ => None
   }
   def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, s2) {
     case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
     case _ => None
   }
   def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold(this, s2) {
     case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
     case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
     case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
     case _ => None
   }
   
   def startsWith[A](s: Stream[A]): Boolean = (this, s) match {
     case (_, Empty) => true
     case (Cons(h1, t1), Cons(h2, t2)) => h1() == h2() && t1().startsWith(t2())
     case _ => false
   }
   
   def tails: Stream[Stream[A]] = unfold(this){
     case Empty => None
     case s => Some((s, s drop 1))
   } append Stream.empty
   
   def hasSubsequence[A](s: Stream[A]):Boolean = tails exists (_ startsWith s)
   
   def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] = foldRight((z, Stream(z)))((a, p0) => {
     lazy val p1 = p0
     val b2 = f(a, p1._1)
     (b2, Stream.cons(b2, p1._2))
   })._2
  }
  
  case object Empty extends Stream[Nothing]
  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  object Stream{
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }
    
    def empty[A]: Stream[A] = Empty
    def apply[A](as: A*): Stream[A] = 
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }
}