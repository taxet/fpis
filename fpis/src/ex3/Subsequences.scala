package ex3

/**
 * @author CHAOLUN
 * 
 * ex 3.24
 */
object Subsequences {
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match{
    case (_, Nil) => true
    case (Nil, _) => false
    case (head1::tail1, head2::tail2) => 
      if(head1 != head2) hasSubsequence(tail1, sub)
      else hasSubsequence(tail1, tail2) || hasSubsequence(tail1, sub)
  }
    
}